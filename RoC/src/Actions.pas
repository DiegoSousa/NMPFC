unit Actions;

{$mode objfpc}{$H+}

interface

uses Graphics,Main,Param,Math,DecConsts,obsavoid,genTraj,SysUtils,MPC,Model;

const
  IDCTRL_OMNI_FORWARD=1;
  IDCTRL_OMNI_POSITION=2;
  IDCTRL_OMNI_XYTETA=3;



type
  TAction=(acStop, acGoToXYTeta,acAdvisedSpeed,acDesiredSpeed,acFollowTrajectory,acControlTrajectory,acDoFormation,acDoFormationFollower);

const
  CActionString: array[low(TAction)..High(TAction)] of string =
        ('acStop','acGoToXYTeta','acAdvisedSpeed','acDesiredSpeed','acFollowTrajectory','acControlTrajectory','acDoFormation','acDoFormationFollower');

const
  GoToXYTetaPrecision=0.025;
  minNextPointDistance=0.05;

procedure ActionStop(num: integer);
procedure ActionGoToXYTeta(num: integer);
procedure ActionAdvisedSpeed(num: integer);
procedure ActionDesiredSpeed(num: integer);
procedure VVnToVxy(teta,v,vn: double; var Vx,Vy: double);
procedure VxyToVVn(teta,vx,vy: double; var V,Vn: double);
procedure ActionDoFormation(num:integer);
procedure ActionDoFormationFollower(num:integer);

procedure ActionFollowTrajectory(num:integer);
procedure ActionControlTrajectory(num: integer);
procedure TrajectoryControllerStatic(speed, speed_on_target: double; var state: TRobotState; var com: TTacticCommand; var traj: TTrajectory);

// Procedures related to Controller
procedure ProportionalSat(var v1,v2,v3: double; vmax: double);
procedure SetTacticCommandsInRobotRef(var com: TTacticCommand; v,vn,w: double);


type
  TActionFunc = procedure(num: integer);

const
  CActionFunc: array[low(TAction)..High(TAction)] of TActionFunc =
      ( @ActionStop, @ActionGoToXYTeta,@ActionAdvisedSpeed,@ActionDesiredSpeed, @ActionFollowTrajectory, @ActionControlTrajectory, @ActionDoFormation, @ActionDoFormationFollower);

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
    controllerIndex : integer;
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

  //Controller Index
  controllerIndex : integer;
function ActionString(a: TAction): string;

implementation

uses Tactic, Utils, Robots, Roles, Tasks;

function ActionString(a: TAction): string;
begin
  result:=CActionString[a];
end;


//-------------------------------------------------------------------------
// ProportionalSat
//
// Keeps vector directions if norms have to be scaled
//-------------------------------------------------------------------------
procedure ProportionalSat(var v1,v2,v3: double; vmax: double);
var maxv,minv: double;
    scale,scalemax,scalemin: double;
begin
  maxv:=Max(v1,Max(v2,v3));
  minv:=Min(v1,Min(v2,v3));

  if maxv>vmax then scalemax:=maxv/vmax else scalemax:=1.0;
  if minv<-vmax then scalemin:=minv/(-vmax) else scalemin:=1.0;

  scale:=Max(scalemin,scalemax);

  v1:=v1/scale;
  v2:=v2/scale;
  v3:=v3/scale;

end;
//-------------------------------------------------------------------------
// SetTacticCommandsInRobotRef
//
// Defines the calculated v, vn and w in the TTacticCommand object of the
// current Robot. This is the object that's read to send the speeds to
// the motors
//-------------------------------------------------------------------------
procedure SetTacticCommandsInRobotRef(var com: TTacticCommand; v,vn,w: double);
var
   v1,v2,v3 : double;
begin

  com.v:=v;
  com.vn:=vn;
  com.w:=w;

  with com do begin

    v1 := -v * MechCosAlpha + vn * MechSinAlpha + MechD * w;
    v2 := -vn + MechB * w;
    v3 := v * MechCosAlpha + vn * MechSinAlpha + MechD * w;

    ProportionalSat(v1, v2, v3, SpeedMax);

  end;
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

////-----------------------------------------------------------------------------
//// TrajectoryController
////
//// Calculates the correct v, vn, and w to follow the trajectory calculated
//// before. Limits speeds for deceleration when close to the target to arrive
//// to it at the desired speed.
////-----------------------------------------------------------------------------
//
//procedure TrajectoryController(speed, speed_on_target: double; var state: TRobotState; var com: TTacticCommand; var traj: TTrajectory;tgx,tgy: double);
//var statev, statevn, v, vn, dteta, derdist,distancia,distNew,stopang: double;
//    vmax, pw, tx, ty, tempx,tempy, segteta, deltat, vball: double;
//    idx: integer;
//    previousSpeed:double;
//begin
//  idx := Min(3, traj.count-1);
//  tx := traj.pts[idx].x;
//  ty := traj.pts[idx].y;
//  dteta := DiffAngle(traj.pts[idx].teta, state.teta);
//
//  segteta := ATan2(ty-state.y,tx-state.x);
//
//  distancia:=dist((tgx-state.x),(tgy-state.y));
//
//  tempx:=tx-state.x;
//  tempy:=ty-state.y;
//  NormalizeVector(tempx,tempy);
//
//  previousSpeed:=dist(RobotState[myNumber].sv,RobotState[myNumber].svn);
//  distNew:=0.25-0.5*max_linear_acceleration*
//     power((previousSpeed-speed_on_target)/max_linear_acceleration,2)+
//            previousSpeed*(previousSpeed-speed_on_target)/max_linear_acceleration;
//
//  if (traj.distance < deltadist) then begin
//    vmax := 0;
//  end else begin
//    if (distancia>distNew) then
//       vmax :=speed
//    else
//       vmax :=speed_on_target;
//  end;
//
//  tempx:=tempx*vmax;
//  tempy:=tempy*vmax;
//  if vmax>speed then vmax:=speed;
//
//  VxyToVVn(state.teta, tempx, tempy, v, vn);
//
//  if abs(dteta) < deltateta * Pi / 180 then begin
//    pw := 0;
//  end else begin
//    pw := 3 * dteta - 0.5 * state.sw;
//  end;
//
//  SetTacticCommandsInRobotRef(com, v, vn, pw);
//
//end;

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
// TrajectoryController
//
// Calculates the correct v, vn, and w to follow the trajectory calculated
// before. Limits speeds for deceleration when close to the target to arrive
// to it at the desired speed.
//-------------------------------------------------------------------------
procedure TrajectoryController(speed, speed_on_target: double; var state: TRobotState; var com: TTacticCommand; var traj: TTrajectory);
var statev, statevn, v, vn, dteta, teta_power: double;
    vmax, pw, tx, ty, segteta: double;
    idx: integer;
begin
  //manuel
  //LogString:='';

  idx := Min(2, traj.count-1); //target is the 2nd trajectory point from current localization

  //target x,y,dteta
  tx := traj.pts[idx].x;
  ty := traj.pts[idx].y;
  dteta := DiffAngle(traj.pts[idx].teta, state.teta);
  teta_power := traj.pts[idx].teta_power;

  //manuel
  //LogString:=LogString+format('%d;%d;%.2f;%.2f;%.2f;%.2f;%.2f;%.2f',[traj.count,idx,tx,ty,traj.pts[idx].teta, state.teta,dteta,teta_power]);

  //teta of the direction it has to follow
  segteta := ATan2(ty-state.y,tx-state.x);

  //calculate new V and Vn
  VxyToVVn(segteta, state.vx, state.vy, statev, statevn);

  //manuel
  //LogString:=LogString+format(';%.2f;%.2f;%.2f;%.2f;%.2f;%.2f;%.2f',[state.y, state.x, segteta, state.vx, state.vy, statev,statevn]);

  // calculate maximum speed for a constant desaccelaration to target
  //manuel
  //if traj.distance < 0.05 then begin

  if traj.distance < 0.02 then begin
    vmax := 0;

  end else begin
    vmax := sqr(speed_on_target) +  0.7*0.7*(traj.distance - 0.02);
    if vmax > 0 then begin
      vmax := sqrt(vmax);
      if vmax>speedmax then vmax:=SpeedMax;
    end else vmax := 0;
  end;

  //manuel
  //LogString:=LogString+format(';%.2f;%.2f;%.2f',[traj.distance, speed_on_target, vmax]);

  v := speed;
  if v > vmax then v := vmax;
  vn := -statevn * 0.75;
  if vn > vmax then vn := vmax;
  if vn < -vmax then vn := -vmax;


  //calculate w for rotation to desired teta
  if abs(dteta) < 3 * Pi / 180 then begin
    pw := 0;
  end else begin
    pw := 4 * dteta - 0.5 * state.w;
    pw := pw * teta_power;
  end;

  //manuel
  //LogString:=LogString+Format(';%.2f;%.2f;%.2f;%.2f',[v,vn,state.w, pw]);

  VVnToVxy(segteta, v, vn, statev, statevn);

  //manuel
  //LogString:=LogString+Format(';%.2f;%.2f;%.2f;%.2f;%.2f',[segteta, v, vn, statev, statevn]);

  VxyToVVn(state.teta, statev, statevn, v, vn);

  //manuel
  //LogString:=LogString+format(';%.2f;%.2f;%.2f;%.2f;%.2f',[state.teta, statev, statevn, v, vn]);

  SetTacticCommandsInRobotRef(com, v, vn, pw);

  //manuel
  //WriteLn(LogFile,LogString);
end;


//-------------------------------------------------------------------------
// TrajectoryControllerStatic
//
// Calculates the correct v, vn, and w to follow a static trajectory
//-------------------------------------------------------------------------
procedure TrajectoryControllerStatic(speed, speed_on_target: double; var state: TRobotState; var com: TTacticCommand; var traj: TTrajectory);
var statev, statevn, v, vn, dteta, teta_power, dx, dy, distance: double;
    vmax, pw, tx, ty, segteta, next_x, next_y, prev_x, prev_y: double;
    destPoint : integer;
    b,k,y_diff,x_diff,y_cross : double;
begin

  if traj.currentPoint = 0 then
     traj.currentPoint :=1;

  //Target trajectory point changes when robot crosses the line perpendicular to
  //the one that passes through two consecutive trajectory points, crossing it
  //at the middle

  next_x := traj.pts[traj.currentPoint].x;
  next_y := traj.pts[traj.currentPoint].y;
  prev_x := traj.pts[traj.currentPoint-1].x;
  prev_y := traj.pts[traj.currentPoint-1].y;

  y_diff := next_y-prev_y;
  x_diff := next_x-prev_x;
  if y_diff = 0 then
     y_diff := 0.000001;

  //calculate equation of the described line
  k:=-(next_x-prev_x)/y_diff;
  b:=((next_y*next_y+next_x*next_x)-(prev_x*prev_x+prev_y*prev_y))/(2*y_diff);
  y_cross := k*state.x + b;

  //debug
  b_global := b;
  k_global := k;
  y_cross_global := y_cross;
  x_diff_global := x_diff;
  y_diff_global := y_diff;


  //choose next point
  if x_diff = 0 then begin  //vertical trajectory
     if ((state.y > y_cross) and (y_diff > 0)) or ((state.y < y_cross) and (y_diff < 0)) then
        traj.currentPoint := traj.currentPoint+1;
  end
  else  //general trajectory
  if (((k>=0) and (x_diff>0)) or ((k<=0) and (x_diff<0))) and (state.y < y_cross) then
        traj.currentPoint := traj.currentPoint+1
  else
  if (((k>=0) and (x_diff<0)) or ((k<=0) and (x_diff>0))) and (state.y > y_cross) then
        traj.currentPoint := traj.currentPoint+1;


  //Reached the end of the path, stop robot and exit procedure
  destPoint := traj.currentPoint + 1;
  if destPoint > MaxTrajectoryCount-1 then begin
     SetTacticCommandsInRobotRef(com, 0, 0, 0);
     Exit;
  end;

  //target x,y,dteta
  tx := traj.pts[destPoint].x;
  ty := traj.pts[destPoint].y;
  dteta := DiffAngle(traj.pts[destPoint].teta, state.teta);
  teta_power := traj.pts[destPoint].teta_power;

  //teta of the direction it has to follow
  segteta := ATan2(ty-state.y,tx-state.x);

  //calculate new V and Vn
  VxyToVVn(segteta, state.vx, state.vy, statev, statevn);

  //calculate current distance from end of target
  calcDistance(traj);

  //limit max speed
  if traj.distance < 0.02 then begin
    vmax := 0;
  end else begin
   vmax := sqr(speed_on_target) +  0.7*0.7*(traj.distance - 0.02);
    if vmax > 0 then begin
      vmax := sqrt(vmax);
      if vmax>speedmax then vmax:=SpeedMax;
    end else vmax := 0;
  end;


  v := speed;
  if v > vmax then v := vmax;
  vn := -statevn * 0.75;
  if vn > vmax then vn := vmax;
  if vn < -vmax then vn := -vmax;


  //calculate w for rotation to desired teta
  if abs(dteta) < 3 * Pi / 180 then begin
    pw := 0;
  end else begin
    pw := 4 * dteta - 0.5 * state.w;
    pw := pw * teta_power;
  end;

  VVnToVxy(segteta, v, vn, statev, statevn);

  VxyToVVn(state.teta, statev, statevn, v, vn);

  SetTacticCommandsInRobotRef(com, v, vn, pw);

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

  //manuel
  if (abs(rx)>FieldLength/2-0.75) and (abs(ry)<1) then begin
    rx:=FieldLength/2-0.8;
    //ry:=RobotState[num].y;
  end;

  //Get Robot trajectory (A*)
  RobotBestPath(num,rx,ry,traj,ActionPars[num].avoid);

  //Set target teta for each point as the final teta and the rotation power (w)
  for i:=0 to traj.count - 1 do begin
    traj.pts[i].teta := ActionPars[num].teta;
    traj.pts[i].teta_power := 1;
  end;

  //Call TrajectoryController
  TrajectoryController(ActionPars[num].speed, ActionPars[num].speed_on_target, RobotState[num], TacticCommands[num], traj);
  RobotCalcData[num].trajectory_length := traj.distance;
end;

//-------------------------------------------------------------------------
// ActionFollowTrajectory
//
// Follows a given trajectory (staticTraj)
//-------------------------------------------------------------------------
procedure ActionFollowTrajectory(num: integer);
var
   v,vn,w: double;
begin

  case ActionPars[num].controllerIndex of
   0:
     TrajectoryControllerStatic(ActionPars[num].speed, ActionPars[num].speed_on_target, RobotState[num], TacticCommands[num], staticTraj);
   1:
     begin
      //Run controller
      //FormMPC.MPCcontrollerFollowTraj(RobotState[num],staticTraj,ActionPars[num].speed,v,vn,w);

      FormMPC.showDebug2;

      //Set speed references
      SetTacticCommandsInRobotRef(TacticCommands[num],v,vn,w);

      //Update RobotState State
      RobotState[num].v := V;
      RobotState[num].vn := Vn;
      RobotState[num].w := w;

    end;

   end;

end;

//-------------------------------------------------------------------------
// ActionControlTrajectory
//
// Follows a given trajectory (staticTraj)
//-------------------------------------------------------------------------
procedure ActionControlTrajectory(num: integer);
var rx,ry,d: double;
    i: integer;
    v,vn,w: double;
begin
      rx:=ActionPars[num].x;
      ry:=ActionPars[num].y;

      if (abs(rx)>FieldLength/2-0.75) and (abs(ry)<1) then begin
         rx:=FieldLength/2-0.8;
      end;

      //Get Robot trajectory (A*)
      RobotBestPath(num,rx,ry,traj,ActionPars[num].avoid);

      //Set target teta for each point as the final teta and the rotation power (w)
      for i:=0 to traj.count - 1 do begin
          traj.pts[i].teta := ActionPars[num].teta;
          traj.pts[i].teta_power := 1;
      end;

      //Run controller
      FormMPC.MPCcontrollerFollowTraj(RobotState[num],traj,ActionPars[num].speed,v,vn,w);
      RobotCalcData[num].trajectory_length := traj.distance;

      FormMPC.showDebug2;

      //Set speed references
      SetTacticCommandsInRobotRef(TacticCommands[num],v,vn,w);

      //Update RobotState State
      RobotState[num].v := V;
      RobotState[num].vn := Vn;
      RobotState[num].w := w;

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



