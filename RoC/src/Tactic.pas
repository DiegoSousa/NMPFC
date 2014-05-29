unit Tactic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DecConsts, Roles, Actions, Main,
  Tasks, Math, Graphics, Field;

type
  TRefereeState=(
    rsHalt, rbStop, rbStart,
    rsOurStart, rsWaitOtherStart,
    rsOurFreeKick, rsWaitOtherFreeKick,
    rsOurGoalKick, rsWaitOtherGoalKick,
    rsOurThrowIn, rsWaitOtherThrowIn,
    rsOurCornerKick, rsWaitOtherCornerKick,
    rsDefendPenalty, rsWaitToScorePenalty, rsScorePenalty,
    rsDroppedBall,rsStartPos,rsStopPos,rsFormation
    );

const
  CRefereeStateString: array[low(TRefereeState)..High(TRefereeState)] of string =
    (
    'Halt', 'Stop', 'Start',
    'Our Start', 'Other Start',
    'Our Free Kick', 'Other Free Kick',
    'Our Goal Kick', 'Other Goal Kick',
    'Our Throw In', 'Other Throw In',
    'Our Corner Kick', 'Other Corner Kick',
    'Defend Penalty', 'Wait To Score Penalty', 'Score Penalty',
    'DroppedBall','StartPos','StopPos','Formation'
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
    last_task,task: TTask;
    taskTime: integer;
    last_action,action: TAction;
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
procedure TacticProcessRefereeComand(command: char);
procedure CalcRobotCalcData(num: integer);
procedure DoRules;
procedure PlayStateMachine;

// called by Coach
procedure DoTactic;

// called by Dec (and by DoTactic)
procedure DoRobotRules(num: integer);


var
  oldPlay: TPlay;
  iterChangeRoles:integer=0;
  RobotInfo: array [0..MaxRobots-1] of TRobotInfo;
  RobotCalcData: array[0..MaxRobots-1] of TRobotCalcData;
  Play, last_Play: TPlay;
  PlayChangeTimeStamp: LongWord;

  RefereeState: TRefereeState;
  RefereeStateTime: integer;
  RefereeStateWhenReady: TRefereeState=rbStop;

  RefereeStateEnterBallState: TBallState;
  RefereeStateBallFilter: TBallState;
  RefereeStateWantBallMoveInfo: boolean;

implementation

uses Robots, Param, Utils, obsavoid;

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

procedure DoPlayRules;
var
    mamao: integer;
begin
  oldPlay := Play;
  case RefereeState of
    rsStartPos:
      Play := playStartPos;
    rsStopPos:
      Play := playStopPos;
    rsFormation:
      Play := playFormation;

    rsWaitOtherStart: begin
      if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
         Play := playWaitOtherStart;
      end;
    end;

    rsWaitOtherFreeKick, rsWaitOtherGoalKick, rsWaitOtherThrowIn, rsWaitOtherCornerKick:begin
      if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
      if ((Play <> playBarrierDefenseSide) and (Play <> playBarrierAtackSide) and (BallState.x > -FieldDims.FieldDepth/4)) then
        Play := playBarrierAtackSide;
      if ((Play <> playBarrierDefenseSide) and (Play <> playBarrierAtackSide) and (BallState.x <= -FieldDims.FieldDepth/4)) then
        Play := playBarrierDefenseSide;
      //histerese
      if ((Play = playBarrierAtackSide) and (BallState.x > (-FieldDims.FieldDepth/4 - 0.5))) then
        Play := playBarrierAtackSide
      else
        Play := playBarrierDefenseSide;
      if ((Play = playBarrierDefenseSide) and (BallState.x <= (-FieldDims.FieldDepth/4 + 0.5))) then
        Play := playBarrierDefenseSide
      else
        Play := playBarrierAtackSide;
      //
      end;
    end;

    rsOurStart: begin
      if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
         Play := playWaitOurStart;
      end;
    end;

    rsOurFreeKick:begin
     if (BallState.quality<0) then begin
         Play := playSearch;
     end else begin
      if ((Play <> playOurFreeKickAtackSide) and (Play <> playOurFreeKickDefenseSide) and (BallState.x > FieldDims.FieldDepth/4)) then
        Play := playOurFreeKickAtackSide;
      if ((Play <> playOurFreeKickAtackSide) and (Play <> playOurFreeKickDefenseSide) and (BallState.x <= FieldDims.FieldDepth/4)) then
        Play := playOurFreeKickDefenseSide;
      //histerese
      if ((Play = playOurFreeKickAtackSide) and (BallState.x > (FieldDims.FieldDepth/4 - 0.5))) then
        Play := playOurFreeKickAtackSide
      else
        Play :=  playOurFreeKickDefenseSide;
      if ((Play = playOurFreeKickDefenseSide) and (BallState.x <= (FieldDims.FieldDepth/4 + 0.5))) then
        Play := playOurFreeKickDefenseSide
      else
        Play := playOurFreeKickAtackSide;
     end;
    end;

    rsOurGoalKick:begin
     if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
         Play:=playWaitOurGoalKick;
      end;
    end;

    rsOurThrowIn:begin
     if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
         Play:=playWaitOurThrowIn;
      end;
    end;

    rsOurCornerKick:begin
     if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
         Play:=playWaitOurCornerKick;
      end;
    end;

    rsDefendPenalty:begin
     if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
         Play := playDefendPenalty;
      end;
    end;

    rsWaitToScorePenalty:begin
     if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
         Play := playWaitToScorePenalty;
      end;
    end;

    rsScorePenalty:begin
     if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
         Play := playScorePenalty;
      end;
    end;

    rsDroppedBall:begin
     if (BallState.quality<0) then begin
         Play := playSearch;
      end else begin
         Play:= playWaitDroppedBall;
      end;
    end;

    rbStop:
      Play := playHalt;
  end;
  if Play=oldPlay then
    PlayStateMachine;

  if Play <> oldPlay then begin
    PlayChangeTimeStamp := ControlTimeStamp;
    last_Play := oldPlay;
  end;

end;

procedure PlayStateMachine;
var
  distMovedBall:double;
begin
  if RefereeState=rbStop then
    Play:=playHalt;
  if RefereeState=rbStart then begin
    case Play of
      playBarrierAtackSide: begin
         distMovedBall:=Dist(BallState.x-BallStateRstart.x,BallState.y-BallStateRstart.y);
      if distMovedBall > 0.5 then
          Play:=playNormal;
      if (GetTickCount-ControlTimeStamp) > 10000 then
          Play:=playNormal;
      end;
      playBarrierDefenseSide: begin
         distMovedBall:=Dist(BallState.x-BallStateRstart.x,BallState.y-BallStateRstart.y);
      if distMovedBall > 0.5 then
          Play:=playNormal;
      if (GetTickCount-ControlTimeStamp) > 10000 then
          Play:=playNormal;
      end;
      playWaitOtherStart: begin
         distMovedBall:=Dist(BallState.x-BallStateRstart.x,BallState.y-BallStateRstart.y);
      if distMovedBall > 0.5 then
          Play:=playNormal;
      if (GetTickCount-ControlTimeStamp) > 10000 then
          Play:=playNormal;
      end;
      playWaitOurStart:
        Play:=playOurStart;
      playOurFreeKickAtackSide, playOurFreeKickDefenseSide:
        Play:=playOurFreeKick;
      playWaitOurGoalKick:
        Play:=playOurGoalKick;
      playWaitOurThrowIn:
        Play:=playOurThrowIn;
      playWaitOurCornerKick:
        Play:=playOurCornerKick;
      playWaitToScorePenalty:
        Play := playScorePenalty;

      playOurStart: begin
        distMovedBall:=Dist(BallState.x-BallStateRstart.x,BallState.y-BallStateRstart.y);
        if distMovedBall > 0.5 then
          Play:=playNormal;
      end;
      playOurFreeKick:begin
        distMovedBall:=Dist(BallState.x-BallStateRstart.x,BallState.y-BallStateRstart.y);
        if distMovedBall > 0.5 then
          Play:=playNormal;
      end;
      playOurGoalKick:begin
        distMovedBall:=Dist(BallState.x-BallStateRstart.x,BallState.y-BallStateRstart.y);
        if distMovedBall > 0.5 then
          Play:=playNormal;
      end;
      playOurThrowIn:begin
        distMovedBall:=Dist(BallState.x-BallStateRstart.x,BallState.y-BallStateRstart.y);
        if distMovedBall > 0.5 then
          Play:=playNormal;
      end;
      playOurCornerKick:begin
        distMovedBall:=Dist(BallState.x-BallStateRstart.x,BallState.y-BallStateRstart.y);
        if distMovedBall > 0.5 then
          Play:=playNormal;
      end;
      playScorePenalty:begin
        distMovedBall:=Dist(BallState.x-BallStateRstart.x,BallState.y-BallStateRstart.y);
        if distMovedBall > 0.5 then
          Play:=playNormal;
      end;
      playWaitDroppedBall: begin
        Play := playDroppedBall;
      end;
    end;
  if (GetTickCount-ControlTimeStamp) > 10000 then
      Play:=playNormal;
  end;
  if ((abs(BallState.x)>FieldDims.BoundaryDepth/2) or (abs(BallState.y)>FieldDims.BoundaryWidth/2) and (Play<>playStartPos) and (Play<>playStopPos)) then begin
     Play:=playHalt;
  end

end;

function GetBestChipKickPulse(num: integer; d: double): integer;
var vr,v,v1,v2,k,a,b,c,delta: double;
begin
  result:=0;

  if d<0.1 then exit;

  // compensate for robot speed
  vr:=Dist(RobotState[num].vx,RobotState[num].vy);

  k:=2*sin(KickAngle)/9.8;  // 9.8 -> G
  a:=k*cos(KickAngle);
  b:=k*vr;
  c:=-d;

  delta:=b*b-4*a*c;

  if delta>0 then begin
    delta:=sqrt(delta);
    v1:=(-b-delta)/(2*a);
    v2:=(-b+delta)/(2*a);

    if v1<=0 then v1:=1e6;
    if v2<=0 then v2:=1e6;
    v:=min(v1,v2);

    if v<1e5 then begin
      d:=(v*v*2*sin(KickAngle)*cos(KickAngle))/9.8;
    end;
  end;

  if d<1.7 then
    result:=round((d+1.12)/0.047)
  else
    result:=round((d-0.19)/0.025);

  // TODO: calibrate chip kicker
  result := round(result * 1.45) + 15;

  if result>MaxChipKickPulse then result:=MaxChipKickPulse;
end;

procedure DoRobotRules(num: integer);
var old_task: TTask;
    old_action: TAction;
    i: integer;
begin
  with RobotInfo[num] do begin

    old_task:=task;

    // particular role rules
    RoleDefs[role].func(num);

    if old_task<>task then begin
      action_complete:=false;
      taskTime:=ControlTimeStamp;
      last_task:=old_task;
      ZeroMemory(@ActionPars[num],sizeof(ActionPars[num]));
    end;

    old_action:=action;

    // particular task rules
    CTaskFuncs[task](num);

    if old_action<>action then begin
      actionTime:=ControlTimeStamp;
      action_complete:=false;
      last_action:=old_action;
    end;

    // perform actions
    CActionFunc[action](num);
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

    end;

    playFormation: begin
      //Por condição pra mudar o role
      pri_role[0]:=roleDoFormation;
      if (BallState.quality<=100) then begin
        pri_role[0]:=roleTest;
      end;
      pri_role[1]:=roleDoFormation;
      pri_role[2]:=roleDoFormation;
      pri_role[3]:=roleDoFormation;
      pri_role[4]:=roleDoFormation;
    end;
  end;
end;


function EvaluateRobotRole(num: integer; role: TRole): double;
var dcost,turncost,tcost,cpr: double;
    backRInfo: TRobotInfo;
    backTPars: TTaskPars;
    backAPars: TActionPars;
    backRState: TRobotState;
begin
  result := 1e6;

  // the keeper can not be selected at will
  if RoleDefs[role].is_keeper_role then begin
    if RoleDefs[RobotStatus[num].default_role].is_keeper_role then begin
      result:=0;
    end else begin
      result:=1000;
    end;
    exit;
  end;

  // backup current state
  backRInfo:=RobotInfo[num];
  backTPars:=TaskPars[num];
  backAPars:=ActionPars[num];
  backRState:=RobotState[num];

  // give the Robot the role to test
  RobotInfo[num].role:=role;

  // act as if the robot had that role
  DoRobotRules(num);

  // distance cost
  dcost:=RobotCalcData[num].trajectory_length;
  // "cost per radian": one radian costs as much as 5 cm
  cpr:=0.05;  // parameter
  turncost:=Abs(DiffAngle(RobotState[num].teta,ActionPars[num].teta))*cpr;
  // total cost is the cost of going + turning
  tcost:=dcost+turncost;
  result:=tcost;

  // restore the original state
  RobotInfo[num]:=backRInfo;
  TaskPars[num]:=backTPars;
  ActionPars[num]:=backAPars;
  RobotState[num]:=backRState;

  // give a bonus to keep the current roles
  if RobotInfo[num].role=role then begin
    result:=result-0.12;
  end;
end;

procedure DoRules;
var newRoles,RolePri: array[0..MaxRobots-1] of TRole;
    RobotAvailable: array[0..MaxRobots-1] of boolean;
    i,j,best,act_role, cur_robot, RobotAvailableCount: integer;
    val,best_value, cur_cost: double;
    id:integer;
begin
  // clear avoid settings to draw alternative trajectories
  for i:=0 to MaxRobots-1 do begin
    avoided[i]:=false;
  end;

  // no role change if configured for no change
  if Params[paramNoRoleChange] then exit;

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
  
//===> FCoachMain.EditRobotAvailableCount.Text:=IntToStr(RobotAvailableCount);

  // calculate role priority for current game situation
  CalcRolePriority(RolePri);

  // the Keeper have the same task
  if RobotAvailableCount>=1 then begin
    RobotInfo[0].role:=RolePri[4];
  end;

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

// compensate for ball movement
//  x,y: original destination to be corrected
//  sx,sy: start (x,y) position of robot
//  avg_robot_speed: average speed at which the robot will make the manouver

procedure AdjustForBallMovement(var x,y: double; sx,sy: double);
var t,dx,dy,a,b,c,delta,t1,t2: double;
begin
  if Dist(BallState.vx,BallState.vy)<0.03 then exit;

  dx:=x-sx;
  dy:=y-sy;

  a:=BallState.vx*BallState.vx + BallState.vy*BallState.vy - AvgSpeed*AvgSpeed;
  b:=2*(dx*BallState.vx+dy*BallState.vy);
  c:=dx*dx+dy*dy;

  delta:=b*b-4*a*c;
  if delta<0 then exit;

  delta:=sqrt(delta);
  t1:=(-b-delta)/(2*a);
  t2:=(-b+delta)/(2*a);

  if t1<0 then t1:=1e6;
  if t2<0 then t2:=1e6;

  t:=min(t1,t2);
  if t>1e5 then exit;

  // don't make predictions to far into the future
  if t>MaxIntersectTime then t:=MaxIntersectTime;

  x:=x+BallState.vx*t;
  y:=y+BallState.vy*t;
end;

procedure CalcRobotCalcData(num: integer);
var d, bestdist: double;
begin
  with RobotCalcData[num] do begin
    ball_dist:=Dist(BallState.x-RobotState[num].x,BallState.y-RobotState[num].y);
    ball_teta:=NormalizeAngle(ATan2(BallState.y-RobotState[num].y,BallState.x-RobotState[num].x));
    ball_in_front:=BallInFrontOfRobot(num,0);
    pred_ball_x:=BallState.x;
    pred_ball_y:=BallState.y;

    AdjustForBallMovement(pred_ball_x,pred_ball_y,RobotState[num].x,RobotState[num].y);
  end;
end;

procedure CheckRefereeBallMovement;
var alpha: double;
begin
  // TODO get this from the robots
{  // do ball low-pass filter to increase position precision
  alpha:=0.7;
  RefereeStateBallFilter.x:=RefereeStateBallFilter.x*alpha+BallState.x*(1-alpha);
  RefereeStateBallFilter.y:=RefereeStateBallFilter.y*alpha+BallState.y*(1-alpha);
  RefereeStateBallFilter.vx:=BallState.vx;
  RefereeStateBallFilter.vy:=BallState.vy;
  RefereeStateBallFilter.count:=BallState.count;

  // send a "fake" referee box command when the ball moves
  if RefereeStateWantBallMoveInfo then begin
    if Dist(BallState.x-RefereeStateEnterBallState.x,BallState.y-RefereeStateEnterBallState.y)>0.05 then begin
      RefereeStateWantBallMoveInfo:=false;
      TacticProcessRefereeComand('@');
    end;
  end;}
end;


procedure DoTactic;
var rob: integer;
    old_role: array[0..MaxRobots-1] of TRole;
begin
  // TODO get this from the robots
  CheckRefereeBallMovement;

  // keep old state to see state changes
  for rob:=0 to MaxRobots-1 do begin
    CalcRobotCalcData(rob);
    old_role[rob]:=RobotInfo[rob].role;
  end;

  // set the "play" according to the game state and referee box state
  DoPlayRules;
  
  //===>  FCoachMain.EditPlayState.Text:=CPlayString[Play];

  // select the right robots for each role
  DoRules;

  for rob:=0 to MaxRobots-1 do begin
    with RobotInfo[rob] do begin
      if old_role[rob]<>role then begin
        roleTime:=ControlTimeStamp;
        last_role:=old_role[rob];
        ZeroMemory(@TaskPars[rob], sizeof(TaskPars[0]));
      end;
    end;
  end;

end;

procedure TacticProcessRefereeComand(command: char);
var old_team_role: TRefereeState;
begin
  old_team_role:=RefereeState;

  if FParam.RGTeamColor.Items[FParam.RGTeamColor.ItemIndex]='Cyan' then begin
  // state machine
    case command of
      'S': RefereeState:=rbStop;
      's': RefereeState:=rbStart;
      'K': RefereeState:=rsOurStart;
      'k': RefereeState:=rsWaitOtherStart;
      'F': RefereeState:=rsOurFreeKick;
      'f': RefereeState:=rsWaitOtherFreeKick;
      'G': RefereeState:=rsOurGoalKick;
      'g': RefereeState:=rsWaitOtherGoalKick;
      'T': RefereeState:=rsOurThrowIn;
      't': RefereeState:=rsWaitOtherThrowIn;
      'C': RefereeState:=rsOurCornerKick;
      'c': RefereeState:=rsWaitOtherCornerKick;
      'P': RefereeState:=rsWaitToScorePenalty;
      'p': RefereeState:=rsDefendPenalty;
      'N': RefereeState:=rsDroppedBall;
      //'U': RefereeState:=rsStopPos;
      'L': RefereeState:=rsStopPos;
    end;
  end else if FParam.RGTeamColor.Items[FParam.RGTeamColor.ItemIndex]='Magenta'then begin
    case command of
      'S': RefereeState:=rbStop;
      's': RefereeState:=rbStart;
      'k': RefereeState:=rsOurStart;
      'K': RefereeState:=rsWaitOtherStart;
      'f': RefereeState:=rsOurFreeKick;
      'F': RefereeState:=rsWaitOtherFreeKick;
      'g': RefereeState:=rsOurGoalKick;
      'G': RefereeState:=rsWaitOtherGoalKick;
      't': RefereeState:=rsOurThrowIn;
      'T': RefereeState:=rsWaitOtherThrowIn;
      'c': RefereeState:=rsOurCornerKick;
      'C': RefereeState:=rsWaitOtherCornerKick;
      'p': RefereeState:=rsWaitToScorePenalty;
      'P': RefereeState:=rsDefendPenalty;
      'N': RefereeState:=rsDroppedBall;
      //'U': RefereeState:=rsStopPos;
      'L': RefereeState:=rsStopPos;
    end;
  end;


  if old_team_role<>RefereeState then begin
    RefereeStateTime:=ControlTimeStamp;
    //===>    FCoachMain.EditRefState.Text:=CRefereeStateString[RefereeState];
    RefereeStateEnterBallState:=RefereeStateBallFilter;
    RefereeStateWantBallMoveInfo:=true;
  end;

  if command='s' then begin
    ControlTimeStamp:=GetTickCount;
    BallStateRstart.x:=BallState.x;
    BallStateRstart.y:=BallState.y;
  end;

  if command='p' then
    RefereeStateWantBallMoveInfo:=false;
end;

initialization
begin
  RobotInfo[0].role:=roleIdle;
  RobotInfo[1].role:=roleIdle;
  RobotInfo[2].role:=roleIdle;
  RobotInfo[3].role:=roleIdle;
  RobotInfo[4].role:=roleIdle;

  RefereeState := rsHalt;
  Play := playHalt;
end;

end.
