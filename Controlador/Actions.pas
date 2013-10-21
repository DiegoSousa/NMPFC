unit Actions;

{$mode objfpc}{$H+}

interface

uses Graphics,Main,Param,Math,DecConsts,obsavoid,SysUtils,MPC,Model;

const
  IDCTRL_OMNI_FORWARD=1;
  IDCTRL_OMNI_POSITION=2;
  IDCTRL_OMNI_XYTETA=3;

type
  TAction=(acStop, acGoToXYTeta,acAdvisedSpeed,acDesiredSpeed,acDoFormation,acDoFormationFollower);

const
  CActionString: array[low(TAction)..High(TAction)] of string =
        ('acStop','acGoToXYTeta','acAdvisedSpeed','acDesiredSpeed', 'acDoFormation','acDoFormationFollower');

const
  GoToXYTetaPrecision=0.025;
  //Tese Tiago
  minNextPointDistance=0.05;

procedure ActionStop(num: integer);
procedure ActionGoToXYTeta(num: integer);
procedure ActionAdvisedSpeed(num: integer);
procedure ActionDesiredSpeed(num: integer);
procedure VVnToVxy(teta,v,vn: double; var Vx,Vy: double);
procedure VxyToVVn(teta,vx,vy: double; var V,Vn: double);
//Tese Tiago
procedure ActionDoFormation(num:integer);
procedure ActionDoFormationFollower(num:integer);

type
  TActionFunc = procedure(num: integer);

const
  CActionFunc: array[low(TAction)..High(TAction)] of TActionFunc =
      ( @ActionStop, @ActionGoToXYTeta,@ActionAdvisedSpeed,@ActionDesiredSpeed, @ActionDoFormation, @ActionDoFormationFollower);

//****************************************************
//
// type TActionsPars
//
//----------------------------------------------------
// Parameters for the action that's currently being
// executed
//****************************************************
type
  TActionPars=record
    x,y,w,teta: double;

    sx,sy: double;    // start position for acFollowVector

    anyway: boolean;
    speed: double;
    speed_on_target: double;
    target_precision: double;

    desiredV,desiredVn,desiredW:double;
    chipkick_dist: double;
    avoid: TAvoidSet;
  end;

var
  //Array of TActionPars, action parameters for each robot
  ActionPars: array [0..MaxRobots-1] of TActionPars;

  //Trajectories of the current robot
  traj: TTrajectory;
  staticTraj: TTrajectory;

  //Speed for static trajectory
  staticSpeed : double;
  staticSpeedOnTarget : double;
  b_global,k_global,y_cross_global,x_diff_global,y_diff_global:double;


function ActionString(a: TAction): string;

implementation

uses Tactic, Utils, Robots, Roles, Tasks;

function ActionString(a: TAction): string;
begin
  result:=CActionString[a];
end;

//-----------------------------------------------------------------------------
//  Controller
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// VxyToVVn()
//
// Convert speeds in Vxy to VVn
//-------------------------------------------------------------------------

procedure VxyToVVn(teta,vx,vy: double; var V,Vn: double);
var ct,st: double;
begin
  ct:=cos(teta);
  st:=sin(teta);
  v:=vx*ct+vy*st;
  vn:=-vx*st+vy*ct;
end;

procedure VxyToVVnNH(teta,vx,vy: double; var V: double);
var ct,st: double;
begin

  ct:=cos(teta);
  st:=sin(teta);
  v:=(vx+vy)/(ct+st);

end;

//-------------------------------------------------------------------------
// VVnToVxy()
//
// Convert speeds in VVn to Vxy
//-------------------------------------------------------------------------

procedure VVnToVxy(teta,v,vn: double; var Vx,Vy: double);
var ct,st: double;
begin
  ct:=cos(teta);
  st:=sin(teta);
  vx:=v*ct-vn*st;
  vy:=v*st+vn*ct;
end;

function SatVal(v, vmax: double): double;
begin
  if v > vmax then v := vmax;
  if v < -vmax then v := -vmax;
  result := v;
end;

//-----------------------------------------------------------------------------
// ProportionalSat
//
// Keeps vector directions if norms have to be scaled
//-----------------------------------------------------------------------------

procedure ProportionalSat(var v1,v2,v3: double; vmax: double);
var maxv,minv: double;
    scale,scalemax,scalemin: double;
begin
  // conversao de velocidade linear para a maxima na roda
  vmax:=0.87*vmax;

  maxv:=Max(abs(v1),Max(abs(v2),abs(v3)));

  if maxv>vmax then
    scale:=maxv/vmax
  else
    scale:=1;

  v1:=v1/scale;
  v2:=v2/scale;
  v3:=v3/scale;
end;

//----------------------------------------------------------------------------
// SetTacticCommandsInRobotRef
//
// Defines the calculated v, vn and w in the TTacticCommand object of the
// current Robot. This is the object that's read to send the speeds to
// the motors
//----------------------------------------------------------------------------

procedure SetTacticCommandsInRobotRef(var com: TTacticCommand; v,vn,w: double);
begin
  com.v:=v;
  com.vn:=vn;
  com.w:=w;
  with com do begin
    v1 := v * MechCosAlpha + vn * MechSinAlpha + MechD * w;
    v2 := -v * MechCosAlpha + vn * MechSinAlpha + MechD * w;
    v3 := -vn + MechB * w;
    ProportionalSat(v1, v2, v3, SpeedMax);
  end;
end;

procedure SetTacticCommandsInRobotRefNH(var com: TTacticCommand; v,w: double);
begin
  com.v:=v;
  com.vn:=0;
  com.w:=w;
  with com do begin
    v1 := v + 0.2999*w;
    v2 := v - 0.2999*w;
    v3 := 0;
  end;
end;

procedure ActionAdvisedSpeed(num: integer);
var advV,advVn,advW:double;
begin
  advV:=RobotState[num].LocAdvV;
  advVn:=RobotState[num].LocAdvVn;
  advW:=RobotState[num].LocAdvW;

  SetTacticCommandsInRobotRef(TacticCommands[num],advV,advVn,advW);
end;

procedure ActionDesiredSpeed(num: integer);
begin
 SetTacticCommandsInRobotRef(TacticCommands[num],ActionPars[num].desiredV,ActionPars[num].desiredVn,ActionPars[num].desiredw);
end;

//-----------------------------------------------------------------------------
// TrajectoryController
//
// Calculates the correct v, vn, and w to follow the trajectory calculated
// before. Limits speeds for deceleration when close to the target to arrive
// to it at the desired speed.
//-----------------------------------------------------------------------------

procedure TrajectoryController(speed, speed_on_target: double; var state: TRobotState; var com: TTacticCommand; var traj: TTrajectory;tgx,tgy: double);
var statev, statevn, v, vn, dteta, derdist,distancia,distNew,stopang: double;
    vmax, pw, tx, ty, tempx,tempy, segteta, deltat, vball: double;
    idx: integer;
    previousSpeed:double;
begin
  idx := Min(3, traj.count-1);
  tx := traj.pts[idx].x;
  ty := traj.pts[idx].y;
  dteta := DiffAngle(traj.pts[idx].teta, state.teta);

  segteta := ATan2(ty-state.y,tx-state.x);

  distancia:=dist((tgx-state.x),(tgy-state.y));

  tempx:=tx-state.x;
  tempy:=ty-state.y;
  NormalizeVector(tempx,tempy);

  previousSpeed:=dist(RobotState[myNumber].sv,RobotState[myNumber].svn);
  distNew:=0.25-0.5*max_linear_acceleration*
     power((previousSpeed-speed_on_target)/max_linear_acceleration,2)+
            previousSpeed*(previousSpeed-speed_on_target)/max_linear_acceleration;

  if (traj.distance < deltadist) then begin
    vmax := 0;
  end else begin
    if (distancia>distNew) then
       vmax :=speed
    else
       vmax :=speed_on_target;
  end;

  tempx:=tempx*vmax;
  tempy:=tempy*vmax;
  if vmax>speed then vmax:=speed;

  VxyToVVn(state.teta, tempx, tempy, v, vn);

  if abs(dteta) < deltateta * Pi / 180 then begin
    pw := 0;
  end else begin
    pw := 3 * dteta - 0.5 * state.sw;
  end;

  SetTacticCommandsInRobotRef(com, v, vn, pw);

end;

//-----------------------------------------------------------------------------
//
// TrajectoryControllerNH
//
//-----------------------------------------------------------------------------

procedure TrajectoryControllerNH(speed, speed_on_target: double; var state: TRobotState; var com: TTacticCommand; var traj: TTrajectory;tgx,tgy: double);
var statev, statevn, v, vn, dteta, derdist,distancia,distNew,stopang: double;
    vmax, pw, tx, ty, tempx,tempy, segteta, deltat, vball: double;
    idx: integer;
    previousSpeed:double;
begin
  idx := Min(3, traj.count-1);
  tx := traj.pts[idx].x;
  ty := traj.pts[idx].y;
  //dteta := DiffAngle(traj.pts[idx].teta, state.teta);

  segteta := ATan2(ty-state.y,tx-state.x);

  dteta := (DiffAngle(segteta,state.teta));

  distancia:=dist((tgx-state.x),(tgy-state.y));

  tempx:=tx-state.x;
  tempy:=ty-state.y;
  NormalizeVector(tempx,tempy);

  previousSpeed:=RobotState[myNumber].sv;
  distNew:=0.25-0.5*max_linear_acceleration*
     power((previousSpeed-speed_on_target)/max_linear_acceleration,2)+
            previousSpeed*(previousSpeed-speed_on_target)/max_linear_acceleration;

  if (traj.distance < deltadist) then begin
    vmax := 0;
  end else begin
    if (distancia>distNew) then
       vmax :=speed
    else
       vmax :=speed_on_target;
  end;

  tempx:=tempx*vmax;
  tempy:=tempy*vmax;
  if vmax>speed then vmax:=speed;

  VxyToVVnNH(state.teta, tempx, tempy, v);

  if abs(dteta) < deltateta * Pi / 180 then begin
    pw := 0;
  end else begin
    pw := 3 * dteta - 0.5 * state.sw;
  end;

  SetTacticCommandsInRobotRefNH(com, v, pw);

end;

//-------------------------------------------------------------------------
// Actions!!!
//
// ActionStop
//
// Stop Robot movement
//-------------------------------------------------------------------------

procedure ActionStop(num: integer);
begin
  with TacticCommands[num] do begin
    v1:=0;
    v2:=0;
    v3:=0;
    v:=0;
    vn:=0;
    w:=0;
  end;
  RobotCalcData[num].trajectory_length := 0;
end;

//-------------------------------------------------------------------------
// ActionGoToXYTeta
//
// Moves the robot to the position defined in the ActionPars[num] object
// following a trajectory calculated by RobotBestPath().
//-------------------------------------------------------------------------

procedure ActionGoToXYTeta(num: integer);
var rx,ry,d: double;
    i: integer;
begin
  rx:=ActionPars[num].x;
  ry:=ActionPars[num].y;

  //Get Robot trajectory (A*)
  RobotBestPath(num,rx,ry,traj,ActionPars[num].avoid);

  //Set target teta for each point as the final teta and the rotation power (w)
  for i:=0 to traj.count - 1 do begin
    traj.pts[i].teta := ActionPars[num].teta;
    traj.pts[i].teta_power := 1;
  end;

  //Call TrajectoryController
  if FormMPC.difCB.Checked=true then begin
       TrajectoryControllerNH(ActionPars[num].speed, ActionPars[num].speed_on_target, RobotState[num], TacticCommands[num], traj,rx,ry);
  end else begin
       TrajectoryController(ActionPars[num].speed, ActionPars[num].speed_on_target, RobotState[num], TacticCommands[num], traj,rx,ry);
  end;

  RobotCalcData[num].trajectory_length := traj.distance;
end;

//-------------------------------------------------------------------------
// ActionDoFormation
//
// keeps formation
//-------------------------------------------------------------------------
procedure ActionDoFormation(num: integer);
var  v,vn,w,distancia: double;
     i: integer;
begin


        //Run controller
        FormMPC.MPCcontroller(RobotState[num],staticTraj,ActionPars[num].avoid,ActionPars[num].speed,v,vn,w);

        FormMPC.showDebug;

        //Set speed references
        SetTacticCommandsInRobotRef(TacticCommands[num],v,vn,w);

        //Update RobotState State
        RobotState[num].v := v;
        RobotState[num].vn := vn;
        RobotState[num].w := w;
end;

//-------------------------------------------------------------------------
// ActionDoFormationFollower
//
// keeps formation when searching the target
//-------------------------------------------------------------------------
procedure ActionDoFormationFollower(num: integer);
var  v,vn,w,distancia: double;
begin

        //Run controller
        FormMPC.MPCcontrollerFollower(RobotState[num],staticTraj,ActionPars[num].avoid,ActionPars[num].speed,v,vn,w);

        FormMPC.showDebug;

        //Set speed references
        SetTacticCommandsInRobotRef(TacticCommands[num],v,vn,w);

        //Update RobotState State
        RobotState[num].v := v;
        RobotState[num].vn := vn;
        RobotState[num].w := w;
end;


end.



