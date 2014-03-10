unit Actions;

{$mode objfpc}{$H+}

interface

uses Graphics,Coach,Param,Math,DecConsts,ObsAvoid,SysUtils;

const
  IDCTRL_OMNI_FORWARD=1;
  IDCTRL_OMNI_POSITION=2;
  IDCTRL_OMNI_XYTETA=3;

type
  TAction=(acStop, acGoToXYTeta,acAdvisedSpeed);

const
  CActionString: array[low(TAction)..High(TAction)] of string =
        ('acStop', 'acGoToXYTeta','acAdvisedSpeed');

const
  GoToXYTetaPrecision=0.025;

procedure ActionStop(num: integer);
procedure ActionGoToXYTeta(num: integer);
procedure ActionAdvisedSpeed(num: integer);
procedure VVnToVxy(teta,v,vn: double; var Vx,Vy: double);

type
  TActionFunc = procedure(num: integer);

const
  CActionFunc: array[low(TAction)..High(TAction)] of TActionFunc =
      ( @ActionStop, @ActionGoToXYTeta,@ActionAdvisedSpeed);

type
  TActionPars=record
    x,y,w,teta: double;
    sx,sy: double;    // start position for acFollowVector
    anyway: boolean;
    speed: double;
    speed_on_target: double;
    target_precision: double;
    chipkick_dist: double;
    avoid: TAvoidSet;
  end;

var
  ActionPars: array [0..MaxRobots-1] of TActionPars;
  traj: TTrajectory;



function ActionString(a: TAction): string;

implementation

uses Tactic, Utils, Robots, Roles;

function ActionString(a: TAction): string;
begin
  result:=CActionString[a];
end;

//----------------------------------------------------------------------
//  Controller

function SatVal(v, vmax: double): double;
begin
  if v > vmax then v := vmax;
  if v < -vmax then v := -vmax;
  result := v;
end;

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

procedure VxyToVVn(teta,vx,vy: double; var V,Vn: double);
var ct,st: double;
begin
  ct:=cos(teta);
  st:=sin(teta);
  v:=vx*ct+vy*st;
  vn:=-vx*st+vy*ct;
end;

procedure ActionAdvisedSpeed(num: integer);
var advV,advVn,advW:double;
begin
// n se usa no coach (ver mais tard para eliminar action)
  //advV:=RobotState[num].LocAdvV;
  //advVn:=RobotState[num].LocAdvVn;
  //advW:=RobotState[num].LocAdvW;
  //
  //SetTacticCommandsInRobotRef(TacticCommands[num],advV,advVn,advW);
end;

procedure VVnToVxy(teta,v,vn: double; var Vx,Vy: double);
var ct,st: double;
begin
  ct:=cos(teta);
  st:=sin(teta);
  vx:=v*ct-vn*st;
  vy:=v*st+vn*ct;
end;

procedure TrajectoryController(speed, speed_on_target: double; var state: TRobotState; var com: TTacticCommand; var traj: TTrajectory);
var statev, statevn, v, vn, dteta, teta_power: double;
    vmax, pw, tx, ty, segteta: double;
    idx: integer;
begin

  idx := Min(2, traj.count-1);
  tx := traj.pts[idx].x;
  ty := traj.pts[idx].y;
  dteta := DiffAngle(traj.pts[idx].teta, state.teta);
  teta_power := traj.pts[idx].teta_power;

  segteta := ATan2(ty-state.y,tx-state.x);

  VxyToVVn(segteta, state.vx, state.vy, statev, statevn);

  // calculate maximum speed for a constant desaccelaration to target
  if traj.distance < deltadist then begin
    vmax := 0;
  end else begin
    vmax := sqr(speed_on_target) +  0.7*0.7*(traj.distance - deltadist);
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

  if abs(dteta) < deltateta * Pi / 180 then begin
    pw := 0;
  end else begin
    pw := 4 * dteta - 0.5 * state.w;
    pw := pw * teta_power;
  end;

  VVnToVxy(segteta, v, vn, statev, statevn);
  VxyToVVn(state.teta, statev, statevn, v, vn);

  SetTacticCommandsInRobotRef(com, v, vn, pw);
end;

//----------------------------------------------------------------------
//  Actions

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

procedure ActionGoToXYTeta(num: integer);
var rx,ry,d: double;
    i: integer;
begin
  rx:=ActionPars[num].x;
  ry:=ActionPars[num].y;

  RobotBestPath(num,rx,ry,traj,ActionPars[num].avoid);


  for i:=0 to traj.count - 1 do begin
    traj.pts[i].teta := ActionPars[num].teta;
    traj.pts[i].teta_power := 1;
  end;
  TrajectoryController(ActionPars[num].speed, ActionPars[num].speed_on_target, RobotState[num], TacticCommands[num], traj);
  RobotCalcData[num].trajectory_length := traj.distance;
end;

end.



