unit Tasks;

{$mode objfpc}{$H+}

interface

uses Math, Controls, Main, Graphics, DecConsts, Roles, obsavoid, SysUtils,Field,unit_localizationAux;

type
  task_func=procedure(num:integer);

type
  TTask=(taskIdle, taskKeepGoalSafe, taskKeeperDefend, taskGetRidOfBall, taskDefendPenalty,
         taskDefendAngle, taskDefendAndAvoid, taskDefendLineAngle, taskDefendArea,
         taskGoToGoodPosition, taskDefendPass, taskStayBack,
         taskAtackerWaitKickoff, taskCatchBall,
         taskBehindBall, taskGoForGoal, taskdribleForGoal, taskGoToBall,taskGoToBallKeeper,
         taskLeaveGoalieAlone, taskInterceptBall,
         taskFreeKick, taskUnderStress, taskScoreFreeKick,
         taskWingman,
         taskTest,taskAdvisedSpeed,taskGoToPnt,taskRotateToTargWithBall,
         //Tese Tiago
         taskFollowTrajectory,             //Rodrigo
         taskGoToStartTrajectory,
         taskDoFormation,
         taskDoFormationFollower
         );

const
  CTaskString: array[low(TTask)..High(TTask)] of string =
        ('taskIdle','taskKeepGoalSafe','taskKeeperDefend', 'taskGetRidOfBall', 'taskDefendPenalty',
         'taskDefendAngle','taskDefendAndAvoid','taskDefendLineAngle','taskDefendArea',
         'taskGoToGoodPosition','taskDefendPass', 'taskStayBack',
         'taskAtackerWaitKickoff', 'taskCatchBall',
         'taskBehindBall','taskGoForGoal','taskdribleForGoal','taskGoToBall','taskGoToBallKeeper',
         'taskLeaveGoalieAlone','taskInterceptBall',
         'taskFreeKick','taskUnderStress','taskScoreFreeKick',
         'taskWingman',
         'taskTest','taskAdvisedSpeed','taskGoToPnt','taskRotateToTargWithBall',
         //Tese Tiago
         'taskFollowTrajectory',  //ROdrigo
         'taskGoToStartTrajectoty',
         'taskDoFormation',
         'taskDoFormationFollower'
         );


procedure taskIdleRules(num: integer);
procedure taskKeepGoalSafeRules(num: integer);
procedure taskKeeperDefendRules(num: integer);
procedure taskDefendPenaltyRules(num: integer);

procedure taskDefendAngleRules(num: integer);
procedure taskDefendLineAngleRules(num: integer);
procedure taskDefendAreaRules(num: integer);
procedure taskDefendAndAvoidRules(num: integer);
procedure taskDefendPassRules(num: integer);
procedure taskGetRidOfBallRules(num: integer);
procedure taskRotateToTargWithBallRules(num:integer);
procedure taskGoToGoodPositionRules(num: integer);
procedure taskAdvisedSpeedRules(num: integer);
procedure taskGoToPntRules(num:integer);

procedure taskAtackerWaitKickoffRules(num: integer);
procedure taskGoToBallRules(num: integer);
procedure taskGoToBallKeeperRules(num: integer);

procedure taskBehindBallRules(num: integer);
procedure taskGoForGoalRules(num: integer);
procedure taskdribleForGoalRules(num: integer);
procedure taskCatchBallRules(num: integer);
procedure taskLeaveGoalieAloneRules(num: integer);
procedure taskWingmanRules(num: integer);
procedure taskInterceptBallRules(num: integer);

procedure taskFreeKickRules(num: integer);
procedure taskUnderStressRules(num: integer);
procedure taskScoreFreeKickRules(num: integer);

procedure taskStayBackRules(num: integer);
procedure taskTestRules(num: integer);

//Tese Tiago
procedure taskDoFormationRules(num: integer);
procedure taskDoFormationFollowerRules(num: integer);
//Rodrigo
procedure taskFollowTrajectoryRules(num: integer);
procedure taskGoToStartTrajectoryRules(num: integer);

function kickPulsFunc(num:integer):integer;

const
  CTaskFuncs: array[Low(TTask)..High(TTask)] of task_func =
         (@taskIdleRules, @taskKeepGoalSafeRules, @taskKeeperDefendRules, @taskGetRidOfBallRules, @taskDefendPenaltyRules,
         @taskDefendAngleRules, @taskDefendAndAvoidRules, @taskDefendLineAngleRules, @taskDefendAreaRules,
         @taskGoToGoodPositionRules, @taskDefendPassRules, @taskStayBackRules,
         @taskAtackerWaitKickoffRules, @taskCatchBallRules,
         @taskBehindBallRules, @taskGoForGoalRules, @taskdribleForGoalRules, @taskGoToBallRules,@taskGoToBallKeeperRules,
         @taskLeaveGoalieAloneRules, @taskInterceptBallRules,
         @taskFreeKickRules, @taskUnderStressRules, @taskScoreFreeKickRules,
         @taskWingmanRules,
         @taskTestRules,@taskAdvisedSpeedRules,@taskGoToPntRules,@taskRotateToTargWithBallRules,
         //Rodrigo
         @taskFollowTrajectoryRules,
         @taskGoToStartTrajectoryRules,
         //Tese Tiago
         @taskDoFormationRules,
         @taskDoFormationFollowerRules
         );

type
  TTaskPars=record
    x1, y1: double;
    xold,yold: double;
    teta, speed,speed_on_target: double;
    sumError:double;
    speedV,speedVn:double;
    avoid: TAvoidSet;
    taskIter:integer;
    avoid_is_set: boolean;
  end;

var
  TaskPars: array[0..MaxRobots-1] of TTaskPars;
  speedontgt: double;


function TaskString(t: TTask): string;


procedure PredictIncomingBallToGoal(var time_to_goal,target_y: double; target_x,half_width: double);

//function DistNearestOponent(px,py: double): double;
procedure GetTargetForGoal(var tx,ty: double; goalie_radius: double);
procedure GetBestPointBehindBall(var tx,ty: double);
function BallInFrontOfRobot(num: integer; dist: double): boolean;
function BallIsInOponentCorner: boolean;
//function NearestOponent(px,py: double): integer;
function BestKeeperAnglePosition(intercept_line,limit,target_y: double): double;
function VectorCircleIntersection(px,py,vx,vy,cx,cy,radius: double; var rx, ry: double): boolean;
procedure InterceptBall(var tgx,tgy:double);

implementation

uses Tactic, Actions, Param, Utils, analyser, MPC;

procedure DefenderAvoidAtacker(x_target: double; var y_target,y_cur: double);
var at_num: integer;
    dist: double;
begin

end;


function TaskString(t: TTask): string;
begin
  result:=CTaskString[t];
end;

function BallInFrontOfRobot(num: integer; dist: double): boolean;
var rx,ry,otherTeta: double;
begin
  result:=false;
  TranslateAndRotate(rx,ry,BallState.x,BallState.y,-RobotState[num].x,-RobotState[num].y,-RobotState[num].teta);
  otherTeta:=ATan2(ry,rx);
  if ((rx>0.4+dist) or (abs(ry)>0.4+dist)) then exit;
  result:=true;
end;

//----------------------------------------------------------------------
//  Task Rules

procedure taskIdleRules(num: integer);
begin
  RobotInfo[num].action:=acStop;
  ActionPars[num].w:=0;
  ActionPars[num].chipkick_dist:=0.0;
end;

function BestKeeperAnglePosition(intercept_line,limit,target_y: double): double;
begin
  if BallState.x<intercept_line then begin
    result:=BallState.y;
  end else begin
    result:=((BallState.y-target_y)*(intercept_line-(OurGoalX + 0.1)))/(BallState.x-(OurGoalX))+target_y;
  end;
  if result>limit then result:=limit;
  if result<-limit then result:=-limit;
end;

function KeeperCircle(y:double): double;
var teta,maxp: double;
begin
  result:=KeeperLine;
  if(abs(y)>=FieldDims.GoalWidth/2)then begin
     teta:=ATan2(BallState.y,BallState.x+FieldDims.FieldDepth/2);
     result:=OurGoalX+KepperCloseAreaDistance;
  end;
end;

procedure taskKeepGoalSafeRules(num: integer);
var
   ang,angVball,vball,d:double;
begin
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    speed:=SpeedMax;
    speed_on_target := 0.1;
    ang:=ATan2(BallState.y,BallState.x+FieldDims.FieldDepth/2);
    //Distancia entre bola e o Robô
    d:=dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y);
    x:=KeeperLine-FieldDims.FieldDepth/2;
    if ((BallState.quality>=600) and (d<5)) then begin

     //módulo da velocidade da bola
     vball:=Dist(BallState.vx,BallState.vy);
     //Ângulo do vetor da bola em relação ao mundo
     angVball:=Atan2(BallState.vy,BallState.vx);
     //ver com atencao
     y:=KeeperLine*tan(ang);
     if (abs(y)>=KeeperWidthLine) then begin // se ultrapassar keeper width Line colocado na with line
      y:=sign(BallState.y)*KeeperWidthLine;
     end;
     teta:=ang;
    end else begin
     y:=0;
     teta:=0;
    end;

    if (BallInFrontOfRobot(num,0)and(BallState.quality>500)) then
      chipkick_dist:=MaxChipDist
    else
    chipkick_dist:=0.0;
    avoid := [];
  end;
end;

procedure PredictIncomingBallToGoal(var time_to_goal,target_y: double; target_x,half_width: double);
begin
  if BallState.x<target_x then begin
    time_to_goal:=100;
  end else begin
    if BallState.vx>0 then begin
      time_to_goal:=100;
    end else begin
      if abs(BallState.vx)<0.05 then begin
        time_to_goal:=100;
      end else begin
        time_to_goal:=(BallState.x-target_x)/BallState.vx;
        target_y:=BallState.y+BallState.vy*(time_to_goal);
        if abs(target_y)>half_width then begin
          time_to_goal:=100;
        end;
      end;
    end;
  end;
  if time_to_goal=100 then begin
    target_y:=BestKeeperAnglePosition(KeeperLine+0.1,GoalWidth/2-0.06,0);
  end;
end;

function VectorCircleIntersection(px,py,vx,vy,cx,cy,radius: double; var rx, ry: double): boolean;
var a, b, c, delta: double;
    k1, k2, bx, by: double;
begin
  result := false;

  bx := px - cx;
  by := py - cy;

  a := vx * vx + vy * vy;
  b := 2 * (bx * vx + by * vy);
  c := by * by + bx * bx - radius * radius;

  delta := b * b - 4 * a * c;
  if delta < 0 then exit;

  delta := sqrt(delta);
  k1 := (- b - delta) / (2 * a);
  k2 := (- b + delta) / (2 * a);
  k1 := min(k1, k2);

  rx := px + k1 * vx;
  ry := py + k1 * vy;

  result := true;
end;

procedure InterceptBall(var tgx, tgy: double);
var vx,vy,v:double;
begin
   vx:=BallState.vx;
   vy:=BallState.vy;
   v:=sqrt(vx*vx+vy*vy);
   NormalizeVector(vx,vy);
   tgx:=3*v*vx+BallState.x;
   tgy:=3*v*vy+BallState.y;
end;

function PredictBallAreaIntersection(var rx, ry: double): boolean;
begin
  result := VectorCircleIntersection(BallState.x, BallState.y,
                    BallState.vx, BallState.vy, OurGoalX, 0, AreaWidth, rx, ry);
end;


procedure taskKeeperDefendRules(num: integer);
var time_to_goal,target_y: double;
    target_x,half_width: double;
    otherTeta:double;
begin
  target_x:=KeeperLine-FieldDims.FieldDepth/2;
   if((BallState.vy<=0.01)or(BallState.vx<=0.01))then begin
   target_y:=BallState.y+BallState.vy*0.04;
  end else begin
   otherTeta:=ATan2(BallState.vy,BallState.vx);
   target_y:=tan(otherTeta)*(target_x-BallState.x)+BallState.y;
  end;
  if abs(target_y)>KeeperWidthLine then
   target_y:=sign(target_y);

  half_width:=KeeperWidthLine/2;
  PredictIncomingBallToGoal(time_to_goal,target_y,target_x,half_width);
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    x:=target_x;
    y:=target_y;
    speed := SpeedMax;
    speed_on_target := SpeedMax;
    teta:=ATan2(BallState.y,BallState.x+FieldDims.FieldDepth/2);
    if BallInFrontOfRobot(num,0) then
     chipkick_dist:=MaxChipDist
    else
     chipkick_dist:=0.0;
      taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidBall];
    Otherteta:=DiffAngle(RobotState[num].teta,ATan2(RobotState[num].y-BallState.y,RobotState[num].x-BallState.x));
    if ((Otherteta>35*pi/180) and (BallState.x>RobotState[num].x)) then begin
     avoid:=[avoidRobot,avoidOponent];
    end;
  end;
end;

procedure taskDefendPenaltyRules(num: integer);
var op: integer;
begin
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    x:=OurGoalX+0.05;
    y:=0;

    speed:=SpeedMax;
    teta:=0;
    chipkick_dist:=0.0;
    avoid:=[];
  end;
end;

procedure taskGetRidOfBallRules(num: integer);
var d,cx,cy: double;
begin
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    speed := SpeedMax;
    speed_on_target := 0.35;

    cx:=BallState.x-RobotState[num].x;
    cy:=BallState.y-RobotState[num].y;

    d:=RobotCalcData[num].ball_dist;

    teta := RobotCalcData[num].ball_teta;
    //teta :=0;

    x:=BallState.x;
    y:=BallState.y;

    with TacticCommands[num] do begin
      chipkick_pulse:=0;
      if (BallInFrontOfRobot(num,0)) then begin
        chipkick_pulse:=MaxChipKickPulse;
        Low_kick:=false;
      end;
    end;

    avoid:=[];
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//    mainly defender rules

procedure taskDefendAngleRules(num: integer);
var
 line:double;
begin
  line:=LastDefenderLine -FieldDims.FieldDepth/2;

  with ActionPars[num] do begin
    teta:=RobotCalcData[num].ball_teta;
    RobotInfo[num].action:=acGoToXYTeta;

    speed:=SpeedMax;
    x:=(BallState.x-OurGoalX)/2+OurGoalX;
    if (x>line) then begin
      y:=-x*tan(teta);
      if abs(y)>KeeperWidthLine*2 then begin
        y:=sign(y)*KeeperWidthLine;
      end;
    end else begin
      x:=line;
      y:=0;
    end;

    if (Dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y)<BallDistancekick) then
     chipkick_dist:=MaxChipDist
    else
     chipkick_dist:=0;

    target_precision:=GoToXYTetaPrecision;
    anyway:=true;

    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
    if Dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y)<AvoidMinDistToBall then begin
     avoid:=[];
    end;
  end;
end;

procedure taskDefendAreaRules(num: integer);
var line: double;
begin
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    speed:=SpeedMax;
    speed_on_target:=0;

    if BallState.y>0 then begin
      y:=BestKeeperAnglePosition(LastDefenderLine+0.08,MaxFieldY,0.1);
    end else begin
      y:=BestKeeperAnglePosition(LastDefenderLine+0.08,MaxFieldY,-0.1);
    end;

    teta:=RobotCalcData[num].ball_teta;
    if teta>Pi/2 then teta:=pi/2
    else if teta<-Pi/2 then teta:=-pi/2;

    // se deve defender a frente da area, usa o comportamento normal
    if (Abs(y)<AreaWidth+LastDefenderDistToArea) and (BallState.y>LastDefenderLine) then begin
      x:=LastDefenderLine;
    end else begin
      // se deve defender o lado da area
      y:=AreaWidth+LastDefenderDistToArea;

      if BallState.y<0 then begin
        y:=-y;
      end;

      x:=OurGoalX-(((OurGoalX-BallState.X)*y)/BallState.y);
      line:=-MaxFieldX-0.04;
      if x<line then x:=line;

      // se deve ficar no canto da area
      if x>LastDefenderLine then begin
        x:=LastDefenderLine;
//        teta:=ATan2(BallState.y-y, BallState.x-x)+Pi/2;
      end;
    end;

    anyway:=true;
    target_precision:=GoToXYTetaPrecision;
    chipkick_dist:=0.0;

    avoid:=[avoidOurArea, avoidRobot];
    //avoid:=[];
  end;
end;

procedure taskDefendPassRules(num: integer);
var bx,by,vx,vy: double;
begin
  RobotInfo[num].action:=acGoToXYTeta;

  with ActionPars[num] do begin
    speed:=SpeedMax;

    bx:=RobotCalcData[num].pred_ball_x;
    by:=RobotCalcData[num].pred_ball_y;

    vx:=OurGoalX-bx;
    vy:=0-by;

    NormalizeVector(vx,vy);

    x:=bx+vx*0.5;
    y:=by+vy*0.5;

    chipkick_dist:=0.0;

    teta:=RobotCalcData[num].ball_teta;

    anyway:=true;
    target_precision:=GoToXYTetaPrecision;

    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
    if Dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y)<AvoidMinDistToBall then begin
     avoid:=[];
    end;
  end;
end;


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//    mainly defender rules

procedure taskDefendAndAvoidRules(num: integer);
var line,vx,vy,ang: double;
begin
  line:=LastDefenderLine;

  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    speed:=SpeedMax*0.5;
    speed_on_target:=SpeedMax*0.5;
    x:=line-FieldDims.FieldDepth/2;
    y:=BallState.y;
    if abs(y)>KeeperWidthLine then
      y:=sign(y)*KeeperWidthLine ;
    teta:=0;
    chipkick_dist:=0.1;
    target_precision:=GoToXYTetaPrecision;
    anyway:=true;

    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
    if Dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y)<AvoidMinDistToBall then begin
     avoid:=[];
    end;

  end;
end;

procedure taskDefendLineAngleRules(num: integer);
var
 ang,line:double;
begin
  line:=LastDefenderLine-FieldDims.FieldDepth/2;

  with ActionPars[num] do begin
    teta:=RobotCalcData[num].ball_teta;
    RobotInfo[num].action:=acGoToXYTeta;

    speed:=SpeedMax;
    x:=line;
    if ((BallState.x-OurGoalX)/2+OurGoalX)>line then begin
      y:=-x*tan(teta);
      if abs(y)>KeeperWidthLine*2 then begin
        y:=sign(y)*KeeperWidthLine;
      end;
    end else
      y:=0;

    if (Dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y)<BallDistancekick) then
     chipkick_dist:=MaxChipDist
    else
     chipkick_dist:=0;

    target_precision:=GoToXYTetaPrecision;
    anyway:=true;

    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOurArea];
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// mainly second attacker rules

procedure taskRotateToTargWithBallRules(num: integer);
var targetdir:double;
    vAngular,v,vn:double;
begin
  TacticCommands[num].chipkick_pulse:=0;
  TacticCommands[num].Low_kick:=false;
  targetdir := DiffAngle(ATan2(0-RobotState[num].y,FieldDims.FieldDepth/2-RobotState[num].x),RobotState[num].teta);
  if(abs(targetDir) > degtorad(90))  then targetDir := Sign(targetdir)*degtorad(90);

  vAngular:=sign(targetdir)*0.2+targetdir;
  vn:=-vAngular*0.3;
  v:=0.3;

  RobotInfo[num].action:=acDesiredSpeed;
  with ActionPars[num] do begin
   desiredV:=v;
   desiredVn:=vn;
   desiredW:=vAngular;
  end;
end;

procedure taskAdvisedSpeedRules(num: integer);
begin
   RobotInfo[num].action:=acAdvisedSpeed;
end;

procedure taskGoToPntRules(num: integer);
var
 Otherteta:double;
begin
  RobotInfo[num].action:=acGoToXYTeta;
  TacticCommands[num].chipkick_pulse:=0;
  TacticCommands[num].Low_kick:=false;
  with ActionPars[num] do begin
      x := taskPars[num].x1;
      y := taskPars[num].y1;
      teta := taskPars[num].teta;

      speed := taskPars[num].speed;
      speed_on_target:=taskPars[num].speed_on_target;

      chipkick_dist:=0.0;

      // nao sai do campo
      if abs(x)>FieldDims.FieldDepth/2 then begin
         x:=sign(x)*FieldDims.FieldDepth/2;
      end;

      if abs(y)>FieldDims.FieldWidth/2 then begin
         y:=sign(y)*FieldDims.FieldWidth/2;
      end;

      taskPars[num].avoid_is_set:=true;
      avoid:=[avoidRobot,avoidOponent,avoidOurArea];
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// mainly first attacker rules

procedure IsKickAGoal(num: integer; var chip_dist: double);
var d,ytarget,teta,precision,p: double;
    atacker2: integer;
begin
  chip_dist:=0.0;

  if not BallInFrontOfRobot(num,0.04) then exit;

  // TODO :TAC low_pri
  if Params[paramAtackerPassesOnTopLine] and BallIsInOponentCorner then begin
    //atacker2:=WhoIs(roleAtackerReceiver);
    if atacker2<>-1 then begin
      teta:=ATan2(RobotState[atacker2].y-RobotState[num].y,RobotState[atacker2].x-RobotState[num].x);
      d:=Dist(RobotState[atacker2].x-RobotState[num].x,RobotState[atacker2].y-RobotState[num].y);
      if Abs(DiffAngle(RobotState[num].teta,teta))*d<0.60 then
        chip_dist:=d-0.1;
      exit;
    end;
  end;

  ytarget:=BallState.y+tan(RobotState[num].teta)*(TheirGoalX-BallState.x);

  if Abs(RobotState[num].teta)>Pi/2 then exit;

  if Params[paramKickConfidence] then begin
    precision:=0.80;
    // TODO : oponent
  end else begin
    precision:=0.080;
    // TODO : oponent
  end;

  if abs(ytarget)>precision then exit;
  // TODO : oponent

    chip_dist := MaxChipDist;
end;

function BallIsInOponentCorner: boolean;
var ang,by,bx: double;
begin
  result:=false;
  if Abs(BallState.y)<GoalWidth/2 then exit;

  bx:=TheirGoalX-BallState.x;
  by:=BallState.y-Sign(BallState.y)*GoalWidth/2;
  ang:=Atan2(by,bx)*180/Pi;
  if Abs(ang)>70 then result:=true;
end;

procedure GetTargetForGoal(var tx,ty: double; goalie_radius: double);
begin
  tx := TheirGoalX;
  ty := 0;
end;

procedure GetBestPointBehindBall(var tx,ty: double);
var vy,vx,d,targetx,targety: double;
begin
  GetTargetForGoal(targetx,targety,0.08);  //PARAM: keeper radius

  vx:=BallState.x-targetx;
  vy:=BallState.y-targety;

  d:=dist(vx,vy);
  if d>1e-6 then begin
    vx:=vx/d;
    vy:=vy/d;
  end else begin
    vx:=1;
    vy:=0;
  end;
  ty:=BallState.y+vy*0.17;
  tx:=BallState.x+vx*0.17;
end;


procedure taskAtackerWaitKickoffRules(num: integer);
begin
  // TODOREF ver a questao do passe
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    speed:=0.4;

    x:=BallState.x-0.16;
    y:=BallState.y;
    teta:=0;

    chipkick_dist:=0;

    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent];
  end;

end;

procedure taskGoToBallRules(num: integer);
var cx,cy,tgx,tgy, d,Otherteta,angball,BSx,vball,BSy: double;
    LowPassFilter: TLowPassFilter;
begin

  RobotInfo[num].action:=acGoToXYTeta;

  tgx := TaskPars[num].x1;
  tgy := TaskPars[num].y1;

  with ActionPars[num] do begin
    speed := SpeedMax;
    speed_on_target:=speedontgt;

    BSx:=BallState.x+BallState.vx*0.04;
    BSy:=BallState.y+BallState.vy*0.04;

    cx := tgx-BSx;
    cy := tgy-BSy;
    NormalizeVector(cx,cy);

    if ((abs(BallState.x)>(FieldDims.FieldDepth/2)) or (abs(BallState.y)>(FieldDims.FieldWidth/2))or
        ((BallState.x>(FieldDims.FieldDepth/2)-(FieldDims.KeeperAreaDepth)) and
         (abs(BallState.y)<(FieldDims.KeeperAreaWidth/2)))) then begin
       speed :=0;
       speed_on_target:=0;
    end else begin

       x:=BSx-cx*tkToBallDist;
       y:=BSy-cy*tkToBallDist;
       teta:=ATan2(cy,cx);
       deltadist:=0.04;
       deltateta:=1.0;
    end;

    chipkick_dist:=0;
    anyway:=false;
    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall,avoidObstacles];
    Otherteta:=DiffAngle(RobotState[num].teta,ATan2(RobotState[num].y-BallState.y,RobotState[num].x-BallState.x));
    if ((Otherteta>25*pi/180) and (BallState.x>RobotState[num].x)) then begin
     avoid:=[];
    end;
  end;
end;

procedure taskGoToBallKeeperRules(num: integer);
var cx,cy,tgx,tgy, d: double;
begin

  RobotInfo[num].action:=acGoToXYTeta;

  tgx := TaskPars[num].x1;
  tgy := TaskPars[num].y1;

  with ActionPars[num] do begin
    speed := SpeedMax*1.5;
    speed_on_target:=speedontgt;

    cx := tgx-BallState.x;
    cy := tgy-BallState.y;
    NormalizeVector(cx,cy);


    if ((abs(BallState.x)>(FieldDims.FieldDepth/2)) or (abs(BallState.y)>(FieldDims.FieldWidth/2))) then begin
       speed :=0;
       speed_on_target:=0;
    end else begin
       x:=BallState.x-cx*tkToBallDist;
       y:=BallState.y-cy*tkToBallDist;
       teta:=ATan2(cy,cx);
         deltadist:=0.02;
         deltateta:=3;
    end;

    chipkick_dist:=0;

    anyway:=false;

    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
    if Dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y)<AvoidMinDistToBall then begin
     avoid:=[];
    end;

  end;
end;

procedure taskBehindBallRules(num: integer);
var cx,cy,tgx,tgy, d,Otherteta,vx,vy: double;
begin
  //Inclusão da Informação do engatilhamento da bola pra forçar a não mudança de estado!
  if (RobotState[num].withball=false) then RobotInfo[num].action:=acGoToXYTeta;

  tgx := TaskPars[num].x1;
  tgy := TaskPars[num].y1;

  with ActionPars[num] do begin
    speed := tkSpeed;
    speed_on_target:=speedontgt;

    cx := tgx-BallState.x;
    cy := tgy-BallState.y;
    NormalizeVector(cx,cy);

    x:=BallState.x-cx*tkToBallDist;
    y:=BallState.y-cy*tkToBallDist;

    teta:=ATan2(cy,cx);
    deltadist:=0.02;
    deltateta:=2;
    chipkick_dist:=0;


    if ((abs(BallState.x)>(FieldDims.BoundaryDepth/2)) or (abs(BallState.y)>(FieldDims.BoundaryWidth/2))) then begin
       speed :=0;
       speed_on_target:=0;
    end;

    anyway:=false;

    TaskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
  end;
end;

function DribleBallToGoal(num: integer): double;
var ang,gx,gy,bx,by,targetx,targety: double;
begin
  result:=0;

  GetTargetForGoal(targetx,targety,0.08);

  ang:=ATan2(targety-RobotState[num].y,targetx-RobotState[num].x);

  gx:=BallState.x-robotstate[num].x;
  gy:=BallState.y-robotstate[num].y;
  bx:=gx*cos(ang)-gy*sin(ang);
  by:=gx*sin(ang)+gy*cos(ang);

  if abs(by)<0.05 then exit;
  if Dist(bx,by)<0.13 then exit;

  if abs(by)<0.15 then begin
    result:=1.0;
  end else begin
    result:=(by-0.05)/0.1;
  end;
end;

procedure taskDribleForGoalRules(num: integer);
var cx,cy,tgx,tgy,dgoal: double;
    dummydist: double;
    randKick:integer;
begin
  tgx := TaskPars[num].x1;
  tgy := TaskPars[num].y1;
  with ActionPars[num] do begin
    RobotInfo[num].action:=acGoToXYTeta;
    speed:=SpeedMax;
    speed_on_target:=0.1;
    cx:=tgx-BallState.x;
    cy:=tgy-BallState.y;
    dgoal:=Dist(cx,cy);
    NormalizeVector(cx,cy);
    x:=RobotState[num].x+cx*0.5;
    y:=RobotState[num].y+cy*0.5;
    teta:=ATan2(cy,cx);


    with TacticCommands[num] do begin
      chipkick_pulse:=0;
      Low_kick:=false;
      //Inclusão da Informação do engatilhamento da bola pra forçar a não mudança de estado!
      if (((BallInFrontOfRobot(num,0)) and (BallState.quality>0))
            and (((not FMain.CheckBoxKickBehindMidField.Checked)and(BallState.x>=0))
            and(RobotState[num].withball=true)
            or (FMain.CheckBoxKickBehindMidField.Checked))) then begin
       if(Dist(TheirGoalX-RobotState[num].x,-RobotState[num].y)<=4)then begin
         randKick:=Random(2);
         if randKick=1 then begin
          chipkick_pulse:=kickPulsFunc(num);
          Low_kick:=false;
         end else begin
          chipkick_pulse:=MaxKickPulse;
          Low_kick:=true;
         end;
       end else begin
          chipkick_pulse:=kickPulsFunc(num);
          Low_kick:=false;
       end;
      end;
    end;

    avoid:=[];
  end;
end;

procedure taskCatchBallRules(num: integer);
var cx,cy,tgx,tgy, d,Otherteta,angball,vball,angRB,BSx,BSy,BSxn,BSyn: double;
    LowPassFilter: TLowPassFilter;
begin

  RobotInfo[num].action:=acGoToXYTeta;

  tgx := TaskPars[num].x1;
  tgy := TaskPars[num].y1;

  with ActionPars[num] do begin
    speed := SpeedMax;
    speed_on_target:=speedontgt;

    BSx:=BallState.x+BallState.vx*0.04;
    BSy:=BallState.y+BallState.vy*0.04;

    BSxn:=BallState.x_next+BallState.vx*0.04;
    BSyn:=BallState.y_next+BallState.vy*0.04;

    cx := tgx-BSx;
    cy := tgy-BSy;
    NormalizeVector(cx,cy);

    if ((abs(BallState.x)>(FieldDims.FieldDepth/2)) or (abs(BallState.y)>(FieldDims.FieldWidth/2))or
        ((BallState.x>(FieldDims.FieldDepth/2)-(FieldDims.KeeperAreaDepth)) and
         (abs(BallState.y)<(FieldDims.KeeperAreaWidth/2)))) then begin
       speed :=0;
       speed_on_target:=0;
    end else begin

       //módulo da velocidade da bola
       vball:=Dist(BallState.vx,BallState.vy);
       //Ângulo do vetor da velocidade da bola em relação ao mundo
       angball:=Atan2(BallState.vy,BallState.vx);
       //Distancia entre bola e o robô
       d:=dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y);
       //Ângulo entre velocidade do robô e velocidade da bola
       angRB:=Atan2(vball,RobotState[num].v);

       if ((angRB>(90*pi/180)) {or fastball }) then begin
         teta:= -Atan2(BallState.vy,BallState.vx);
         x:=BSx;
         y:=BSy;
         speed_on_target:=vball*4;
         speed:=vball*4;
       end else begin
         teta:= Atan2(BallState.vy,BallState.vx);
         x:=BSxn;
         y:=BSyn;
         speed_on_target:=vball*4;
         speed:=vball*4;
       end;
       deltadist:=0.04;
       deltateta:=1.0;
    end;

    chipkick_dist:=0;
    anyway:=false;
    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
    Otherteta:=DiffAngle(RobotState[num].teta,ATan2(RobotState[num].y-BallState.y,RobotState[num].x-BallState.x));
    if ((Otherteta>25*pi/180) and (BallState.x>RobotState[num].x)) then begin
     avoid:=[];
    end;
  end;
end;

procedure taskGoForGoalRules(num: integer);
begin
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    speed_on_target:=0;
    speed:=SpeedMax;
    anyway:=false;

    GetTargetForGoal(x,y,0.08);
    teta:=ATan2(y-RobotState[num].y,x-RobotState[num].x);

    x:=BallState.x;
    y:=BallState.y;

    IsKickAGoal(num,chipkick_dist);

    avoid:=[];
  end;
end;


procedure BestXYTetaForUnStuck(var x,y,teta: double);
var oposite: boolean;
    ball_y: double;
begin
  oposite:=(BallState.y<0);

  if oposite then ball_y:=-BallState.y
  else ball_y:=BallState.y;

  if (BallState.x<-MaxFieldX) then begin
    if (ball_y>MaxFieldY) then begin
      x:=-MaxFieldX+0.02;
      y:=ball_y-0.15;
      teta:=(0.6*Pi);
    end else begin
      x:=BallState.x+0.15;
      y:=ball_y;
      teta:=Pi;
    end;
  end else if (BallState.x>MaxFieldX) then begin
    if (ball_y>MaxFieldY) then begin
      x:=BallState.x-0.15;
      y:=MaxFieldY-0.02;
      teta:=(0.15*Pi);
    end else begin
      x:=BallState.x-0.15;
      y:=ball_y+0.05;
      teta:=-0.15*Pi;
    end;
  end else begin
    x:=BallState.x-0.05;
    y:=ball_y-0.15;
    teta:=0.35*Pi;
  end;

  if oposite then begin
    y:=-y;
    teta:=-teta;
  end;

  SatFieldLimits(x,y);
end;

procedure taskUnStuckBallRules(num: integer);
begin
  // TODO
end;

procedure taskLeaveGoalieAloneRules(num: integer);
begin
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    speed:=SpeedMax*0.5;
    speed_on_target:=0;
    x:=1;
    y:=BallState.y;
    teta:=0;
    target_precision:=GoToXYTetaPrecision;
    chipkick_dist:=0.0;
    anyway:=true;
    avoid:=[avoidRobot,avoidOponent];
  end;
end;

procedure taskScoreFreeKickRules(num: integer);
var cx,cy,tgx,tgy,dgoal,rob_num: double;
    dummydist: double;
begin

end;

procedure taskFreeKickRules(num: integer);
begin

end;

procedure taskUnderStressRules(num: integer);
begin

end;

procedure taskStayBackRules(num: integer);
begin
  // TODOREF: optimizar posicoes dos robots atras
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    speed:=0.5;
    //TODO:TAC
    if ((Play=playDefendPenalty) or (Play=playWaitToScorePenalty) or (Play=playScorePenalty)) then begin
      x := -FieldLength/2 + 1.0;
      y:=num*0.5-FieldWidth/2+0.2;
    end else begin
      //x := -0.5;
      x:=1;
      y := 0;
    end;
    chipkick_dist:=0;

    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
    if Dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y)<AvoidMinDistToBall then begin
     avoid:=[];
    end;
  end;
end;

procedure taskTestRules(num: integer);
begin
  with ActionPars[num] do begin

    RobotInfo[num].action:=acDesiredSpeed;
    with ActionPars[num] do begin
       desiredV:=0;
       desiredVn:=0;
       desiredW:=1;
    end;
  end;
end;

function kickPulsFunc(num:integer):integer;
var distGoal:double;
begin
   distGoal:=Dist(TheirGoalX-RobotState[num].x,-RobotState[num].y);
   if distGoal<=3 then
    result:=minKick
   else if distGoal>=7 then
    result:=MaxChipKickPulse
   else
    result:=round(slopeKick*distGoal+originKick);
end;

procedure taskWingmanRules(num: integer);
var d,ang: double;
begin

end;

procedure taskInterceptBallRules(num: integer);
var tgx,tgy,teta_ball,teta_target,Otherteta,turn_time: double;
begin
  with ActionPars[num] do begin

    InterceptBall(tgx,tgy);
    RobotInfo[num].action:=acGoToXYTeta;
    speed:=SpeedMax;
    speed_on_target:=SpeedMax;
    x:=tgx;
    y:=tgy;
    teta:=ATan2(BallState.y-RobotState[num].y,BallState.x-RobotState[num].x);

    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
  end;
end;

Procedure taskGoToGoodPositionRules(num: integer);
var
 Otherteta:double;
begin
  //RobotInfo[num].action:=acGoToXYTeta;
  RobotInfo[num].action:=acControlTrajectory;
  with ActionPars[num] do begin
      x := taskPars[num].x1;
      y := taskPars[num].y1;
      teta := taskPars[num].teta;

      speed := SpeedMax;
      speed_on_target:=0.1;
      chipkick_dist:=0.0;

      if abs(x)>FieldDims.FieldDepth/2 then begin
         x:=sign(x)*FieldDims.FieldDepth/2;
      end;

      if abs(y)>FieldDims.FieldWidth/2 then begin
         y:=sign(y)*FieldDims.FieldWidth/2;
      end;

      taskPars[num].avoid_is_set:=true;
      avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall,avoidObstacles];
  end;
end;

//------------------------------------------------------------------------
// Do Formation Rules
//------------------------------------------------------------------------

procedure taskDoFormationRules(num: integer);
var d: double;
    obs: array[0..MaxRobots+MaxOponents+7+MaxObstacles] of TRObstacle;  //Acke provisório o mais 70
    nobs, l: integer;
begin
    //---------------------------------------------------
     //              Obstacle List
     //---------------------------------------------------

      nobs := 0;

      if avoidObstacles in ActionPars[num].avoid then begin
        for l:=0 to Obstacles.count-1 do begin
          obs[nobs].x:=Obstacles.Centers[l].x;
          obs[nobs].y:=Obstacles.Centers[l].y;
          obs[nobs].r:=Obstacles.Centers[l].r + (RobotSpace/2);
          inc(nobs);
        end;
      end;

     //---------------------------------------------------
     //           End of Obstacle List
     //---------------------------------------------------

    d:=sqrt(power((RobotState[myNumber].x - BallState.x),2)+power((RobotState[myNumber].y - BallState.y),2));

    if (FormMPC.ObstacleInSegment(RobotState[num].x,RobotState[num].y,RobotState[num].x,RobotState[num].y+3,obs,nobs)=true)and(FormMPC.ObstacleInSegment(RobotState[num].x,RobotState[num].y,BallState.x,BallState.y,obs,nobs)=true) then begin
        flagForm:=true;
    end;

    with ActionPars[num] do begin

        if ((flagForm=True))and(d>strtofloatdef(FormMPC.EditP1_badi.Text,0)+0.8) and
           (FormMPC.ObstacleInSegment(RobotState[num].x,RobotState[num].y,BallState.x,BallState.y,obs,nobs)) then begin
                RobotInfo[num].action:=acGoToXYTeta;
                speed:=1;
                speed_on_target:=0.1;
                x:=BallState.x;
                y:=BallState.y;
                teta:=ATan2(BallState.y-RobotState[num].y,BallState.x-RobotState[num].x);
                taskPars[num].avoid_is_set:=true;
                avoid:=[avoidObstacles];
                if (FormMPC.ObstacleInSegment(RobotState[num].x,RobotState[num].y,BallState.x,BallState.y,obs,nobs)=False) then begin
                    flagForm:=false;
                end;
        end else begin
           RobotInfo[num].action:=acDoFormation;
           taskPars[num].avoid_is_set:=true;
           speed := TaskPars[num].speed;
           speed_on_target := staticSpeedOnTarget;
           avoid:=[avoidObstacles];

        end;
    end;
end;

procedure taskDoFormationFollowerRules(num: integer);
var meh: double;
begin

    RobotInfo[num].action:=acDoFormationFollower;

    taskPars[num].avoid_is_set:=true;

    with ActionPars[num] do begin
        speed := TaskPars[num].speed;
        speed_on_target := staticSpeedOnTarget;
        avoid:=[avoidObstacles];
    end;
end;

//--------------------------------------------------
// Follow a static trajectory
//--------------------------------------------------
procedure taskFollowTrajectoryRules(num: integer);
var meh: double;
begin

     with ActionPars[num] do begin
          RobotInfo[num].action:=acFollowTrajectory;
          speed := TaskPars[num].speed;
          speed_on_target := staticSpeedOnTarget;
     end;

end;


//--------------------------------------------------
// Go to the start of the trajectory
//--------------------------------------------------
procedure taskGoToStartTrajectoryRules(num: integer);
var meh: double;
begin

     RobotInfo[num].action:=acGoToXYTeta;

     with ActionPars[num] do begin
          speed := SpeedMax;
          speed_on_target:=0;
          x:= TaskPars[num].x1;
          y:= TaskPars[num].y1;
          teta:=TaskPars[num].teta;
          speed := TaskPars[num].speed;
          anyway:=false;
          avoid:=[avoidRobot,avoidBall,avoidOponent,avoidOurArea];
     end;

end;

end.

