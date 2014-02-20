unit Tactic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DecConsts, Roles, Coach, Math, Graphics,Field;

type
  TRefereeState=(
    rsHalt, rsFormation
    );
const
  CRefereeStateString: array[low(TRefereeState)..High(TRefereeState)] of string =
    (
    'Halt', 'Formation'
    );

type
  TRoundObstacle=record
    x,y,r: double;
    used: boolean;
  end;

type
  TRobotInfo=record
    last_role,role: TRole;
    roleTime: integer;
    //last_task,task: TTask;
    taskTime: integer;
    //last_action,action: TAction;
    actionTime: integer;
    action_complete: boolean;

    TimeToComplete: integer;  // estimated
  end;

type
  TRobotCalcData=record
    ball_dist: double;
    ball_teta: double;
    ball_in_front: boolean;

    pred_ball_x, pred_ball_y: double;

    trajectory_length: double;
  end;

function WhoIs(role: TRole): integer;
procedure CalcRobotCalcData(num: integer);
procedure DoRules;
procedure DoTactic;
procedure DoRobotRules(num: integer);


var
  oldPlay: TPlay;
  iterChangeRoles:integer=0;
  RobotInfo: array [0..MaxRobots-1] of TRobotInfo;
  RobotCalcData: array[0..MaxRobots-1] of TRobotCalcData;
  Play, last_Play: TPlay;
  PlayChangeTimeStamp: LongWord;

implementation

uses Robots, Param2, Utils;
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//    rule sequencing code

function WhoIs(role: TRole): integer;
var i: integer;
begin
   for i:=0 to MaxRobots-1 do begin
    if (robotInfo[i].role=role)then begin
      result:=i;
      exit;
    end;
  end;
 result:=-1;
end;

procedure DoRobotRules(num: integer);
var //old_task: TTask;
    //old_action: TAction;
    i: integer;
begin
  with RobotInfo[num] do begin
    // particular role rules
    RoleDefs[role].func(num);
  end;
end;

// ----------------------------------------------------------------------
//     Role decision

// calculate role priority for current game situation
procedure CalcRolePriority(out pri_role: array of TRole);
var i:integer;
    DefLineDef: double;
begin

  // default roles

   case Play of
    playHalt: begin
      for i:=0 to 4 do pri_role[i]:=roleIdle;
    end;

    playNormal: begin
      // use default roles
      //
      //
    end;

    playFormation: begin
      //Por condição pra mudar o role
      if (BallState.quality<=100)or(BallState.coachquality<=100) then begin
        pri_role[0]:=roleGoSearch;
      end else begin
        pri_role[0]:=roleDoFormation;
      end;
      pri_role[1]:=roleDoFormation;
      pri_role[2]:=roleDoFormation;
      pri_role[3]:=roleDoFormation;
      pri_role[4]:=roleDoFormation;
    end;

  end;
end;

procedure DoRules;
var newRoles,RolePri: array[0..MaxRobots-1] of TRole;
    RobotAvailable: array[0..MaxRobots-1] of boolean;
    i,j,best,act_role, cur_robot, RobotAvailableCount: integer;
    val,best_value, cur_cost: double;
    id:integer;
begin

  // set available robots
  RobotAvailableCount := 0;
  for i:=0 to MaxRobots-1 do begin
    if not RobotStatus[i].active then begin
      RobotAvailable[i]:=false;
      newRoles[i]:=RobotInfo[i].role;
    end else begin
      RobotAvailable[i]:=true;
      Inc(RobotAvailableCount);
    end;
  end;
  
  FCoach.EditRobotAvailableCount.Text:=IntToStr(RobotAvailableCount);

  // calculate role priority for current game situation
  CalcRolePriority(RolePri);

end;


//----------------------------------------------------------------------
//  Main processing

procedure InvertWorldState;
var i: integer;
begin
  with BallState do begin
    x:=-x;
    y:=-y;
    vx:=-vx;
    vy:=-vy;
  end;

  for i:=0 to MaxRobots-1 do begin
    with RobotState[i] do begin
      x:=-x;
      y:=-y;
      teta:=teta+Pi;
    end;
  end;
end;

procedure CalcRobotCalcData(num: integer);
var d, bestdist: double;
begin
  with RobotCalcData[num] do begin
    ball_dist:=Dist(BallState.x-RobotState[num].x,BallState.y-RobotState[num].y);
    ball_teta:=NormalizeAngle(ATan2(BallState.y-RobotState[num].y,BallState.x-RobotState[num].x));
    pred_ball_x:=BallState.x;
    pred_ball_y:=BallState.y;
  end;
end;

procedure DoTactic;
var rob: integer;
    old_role: array[0..MaxRobots-1] of TRole;
begin
   Fcoach.MergeBallState;
  // keep old state to see state changes
  for rob:=0 to MaxRobots-1 do begin
    CalcRobotCalcData(rob);
    old_role[rob]:=RobotInfo[rob].role;
  end;

  FCoach.EditPlayState.Text:=CPlayString[Play];

  // select the right robots for each role
  DoRules;
end;


initialization
begin
  RobotInfo[0].role:=roleIdle;
  RobotInfo[1].role:=roleIdle;
  RobotInfo[2].role:=roleIdle;
  RobotInfo[3].role:=roleIdle;
  RobotInfo[4].role:=roleIdle;

  Play := playHalt;
end;

end.
