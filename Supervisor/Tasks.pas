unit Tasks;

{$mode objfpc}{$H+}

interface

uses Math, Controls, Coach, Graphics, DecConsts, Roles, obsavoid, SysUtils,Field;//,unit_localizationAux;

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
         taskTest,taskAdvisedSpeed,taskGoToPnt);

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
         'taskTest','taskAdvisedSpeed','taskGoToPnt');


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
         @taskTestRules,@taskAdvisedSpeedRules,@taskGoToPntRules );

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

uses Tactic, Actions, Param, Utils;//, analyser;

procedure DefenderAvoidAtacker(x_target: double; var y_target,y_cur: double);
var at_num: integer;
    dist: double;
begin
  dist:=0.25;

  at_num:=WhoIs(roleAtackerKicker);

  if at_num>=0 then begin
    if (Abs(RobotState[at_num].x - x_target) < 0.4) then begin
      if Abs(RobotState[at_num].y - y_target) < dist+0.05 then begin
        if RobotState[at_num].y < y_cur then begin
          y_target:=RobotState[at_num].y+dist;
          if y_target>MaxFieldY then y_target:=RobotState[at_num].y-0.25;
        end else begin
          y_target:=RobotState[at_num].y-dist;
          if y_target<-MaxFieldY then y_target:=RobotState[at_num].y+0.25;
        end;
      end;
    end;
  end;
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

// mainly keeper rules

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
  //if abs(y)>GoalWidth/2-0.02 then begin
  //  y:=abs(y)-GoalWidth/2-0.02;
  //  //result:=-(MaxFieldX+0.04)+(y*((MaxFieldX+0.04)+KeeperLine))/0.04;
  //  maxp:=FieldWidth/2-0.06;
  //  result:=-maxp+(y*(maxp+KeeperLine))/0.04;
  //end;
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

   // if BallState.quality>=600 then begin
        x:=KeeperLine-FieldDims.FieldDepth/2;

        //módulo da velocidade da bola
        vball:=Dist(BallState.vx,BallState.vy);

        //Ângulo do vetor da bola em relação ao mundo
        angVball:=Atan2(BallState.vy,BallState.vx);

        //Distancia entre bola e o Robô
        d:=dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y);

        if ((vball>=0.25)and(BallState.vx>0.2)) then begin
           y:=BallState.y+(x-BallState.x)*(BallState.vy/BallState.vx);
        end else begin
           y:=KeeperLine*tan(ang);
        end;
        if (abs(y)>=KeeperWidthLine) then begin // se ultrapassar keeper width Line colocado na with line
          y:=sign(BallState.y)*KeeperWidthLine;
        end;
        teta:=ang;

    //end else begin
    //    x:=KeeperLine-FieldDims.FieldDepth/2;
    //    y:=0;
    //    teta:=0;
    //
    //end;

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
//    x:=-MaxFieldX;
    x:=OurGoalX+0.05;

    y:=0;

    // TODO : oponent
    {op:=NearestOponent(BallState.x,BallState.y);
    if op<>-1 then begin
      if abs(OponentState[op].x-BallState.x)>0.02 then begin
        y:=-((OponentState[op].y-BallState.y)*(BallState.x-x))/(OponentState[op].x-BallState.x);
        if abs(y)>GoalWidth/2-0.1 then y:=Sign(y)*(GoalWidth/2-0.1);
      end;
    end; }

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

{    if (BallState.x>RobotState[num].x) and
       (BallState.x<RobotState[num].x+Defender3BehindBall+0.05) then exit;

    x:=BallState.x-Defender3BehindBall;
    if x<OurAreaX+0.1 then x:=OurAreaX+0.1;

    if BallState.y<0 then begin
      y:=0.35;
    end else begin
      y:=-0.35;
    end;}

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

Procedure taskGoToGoodPositionRules(num: integer);
var
 Otherteta:double;
begin
  RobotInfo[num].action:=acGoToXYTeta;
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
      if ((RobotInfo[num].role=roleGoPosStart)or(RobotInfo[num].role=roleGoPosStart))then
          avoid:=[avoidRobot,avoidOponent,avoidBall,avoidObstacles]
      else
          avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall,avoidObstacles];
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
  
  {if BallInFrontOfRobot(num,-0.14) then begin
    uncond_kick := true;
  end;}



  // TODO :TAC low_pri
  if Params[paramAtackerPassesOnTopLine] and BallIsInOponentCorner then begin
    atacker2:=WhoIs(roleAtackerReceiver);
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
    {if DistNearestOponent(BallState.x,BallState.y)<0.3 then
      precision:=0.3;}
  end else begin
   // precision:=0.60;
   //manuel
    precision:=0.080;
    // TODO : oponent
    {if DistNearestOponent(BallState.x,BallState.y)<0.3 then
      precision:=0.25;}
  end;

  if abs(ytarget)>precision then exit;

  //d:=Dist(BallState.x-TheirGoalX,BallState.y-ytarget);

  // TODO : oponent
  {if (RobotCalcData[num].robot_in_front=-1) or
       (RobotCalcData[num].robot_in_front=-2-GameCalcData.oponent_keeper) then begin
    kick_speed:=MaxKickSpeed;
  end else begin }

    chip_dist := MaxChipDist;
    {
    if RobotCalcData[num].robot_in_front_dist>0.25 then begin
      //chip_dist := d - 0.1;
      if d < 1 then begin
        chip_dist := 0.5;
      end else if d < 3 then begin
        chip_dist := 0.5 + ((2 - 0.5) * (d - 1)) / (3 - 1);
      end else begin
        chip_dist := MaxChipDist;
      end;
    end else begin
      kick_speed:=2.0;
    end;}
  //end;
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
  // if there is a goal keeper
  { TODO: Oponent goalie position }

  {if GameCalcData.oponent_keeper <> -1 then begin
    ty := OponentState[GameCalcData.oponent_keeper].y;
    if ty < 0 then begin
      ty := (GoalWidth / 2 + ty + goalie_radius) * 0.5;
    end else begin
      ty := (-GoalWidth / 2 + ty - goalie_radius) * 0.5;
    end;
  end;}
end;

{
procedure GetTargetForGoal(var tx,ty: double);
var i,c,atacker: integer;
    min_d,d,y,goalie_radius: double;
begin
}
  //TODO:TAC
  {if teamRole in [trWaitOurStart, trOurStart] then begin
    c:=-1;
    min_d:=10;
    for i:=0 to MaxOponents-1 do begin
      if OponentState[i].valid then begin
        d:=Dist(OponentState[i].x-TheirGoalX,OponentState[i].y);
        if (d<min_d) and (i<>GameCalcData.oponent_keeper) then begin
          c:=i;
          min_d:=d;
        end;
      end;
    end;
    if c<>-1 then begin
      tx:=OponentState[c].x;
      ty:=OponentState[c].y;
      exit;
    end;
  end;}

  //TODO:TAC
  {if Params[paramPassTacticFromFreeKick] and
     ((teamRole in [trWaitOurStart,trOurStart]) or
      ((teamRole=trOurFreeKick) and (BallState.x>0))) then begin
    atacker:=WhoIs(roleAtacker2);
    if atacker>=0 then begin
      if teamRole=trOurFreeKick then begin
        tx:=RobotState[atacker].x;
        ty:=RobotState[atacker].y;
        exit;
      end else begin
        tx:=0.1;
        ty:=RobotState[atacker].y;
        exit;
      end;
    end;
  end;}

{  if Params[paramAtackerPassesOnTopLine] and BallIsInOponentCorner then begin
    atacker:=WhoIs(roleAtackerReceiver);
    if atacker>=0 then begin
      tx:=RobotState[atacker].x;
      ty:=RobotState[atacker].y;
      exit;
    end;
  end;

  goalie_radius:=0.08;
  //TODO:TAC
//  if teamRole in [trWaitToScorePenalty,trScorePenalty] then
//    goalie_radius:=0.16;

  tx:=TheirGoalX;
  ty:=0;

  // if there is a goal keeper
  if GameCalcData.oponent_keeper<>-1 then begin
    y := OponentState[GameCalcData.oponent_keeper].y;
    //TODO:TAC
    //if teamRole in [trWaitOurStart, trOurStart, trOurFreeKick] then begin
      ty:=y;}
    {end else begin
      if y < 0 then begin
        ty:=(GoalWidth/2+y+goalie_radius)*0.5;
      end else begin
        ty:=(-GoalWidth/2+y-goalie_radius)*0.5;
      end;
    end;}
//  end;
//end;

//function BetweenDefender: double


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
    //LowPassFilter: TLowPassFilter;
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
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
    Otherteta:=DiffAngle(RobotState[num].teta,ATan2(RobotState[num].y-BallState.y,RobotState[num].x-BallState.x));
    if ((Otherteta>25*pi/180) and (BallState.x>RobotState[num].x)) then begin
     avoid:=[];
    end;
  end;
end;

//procedure taskGoToBallRules(num: integer);
//var cx,cy,tgx,tgy, d,Otherteta,angball,BSx,vball,BSy: double;
//    LowPassFilter: TLowPassFilter;
//begin
//
//  RobotInfo[num].action:=acGoToXYTeta;
//
//  tgx := TaskPars[num].x;
//  tgy := TaskPars[num].y;
//
//  with ActionPars[num] do begin
//    speed := SpeedMax;
//    speed_on_target:=speedontgt;
//
//    BSx:=BallState.x+BallState.vx*0.04;
//    BSy:=BallState.y+BallState.vy*0.04;
//
//    cx := tgx-BSx;
//    cy := tgy-BSy;
//    NormalizeVector(cx,cy);
//
//    if ((abs(BallState.x)>(FieldDims.FieldDepth/2)) or (abs(BallState.y)>(FieldDims.FieldWidth/2))or
//        ((BallState.x>(FieldDims.FieldDepth/2)-(FieldDims.KeeperAreaDepth)) and
//         (abs(BallState.y)<(FieldDims.KeeperAreaWidth/2)))) then begin
//       speed :=0;
//       speed_on_target:=0;
//    end else begin
//
//       //módulo da velocidade da bola
//       vball:=Dist(BallState.vx,BallState.vy);
//       //Ângulo do vetor da velocidade da bola em relação ao mundo
//       angball:=Atan2(BallState.vy,BallState.vx);
//       //Distancia entre bola e o robô
//       d:=dist(RobotState[num].x-BallState.x,RobotState[num].y-BallState.y);
//
//       if (((vball>=0.25)and(d<=1)) or fastball) then begin
//         bigball:=0.5;
//         fastball:=TRUE;
//         teta:= NormalizeAngle(angball-Pi);
//         x:=BSx+BallState.vx*0.2;
//         y:=BSy+BallState.vy*0.2;
//         speed_on_target:=vball*4;
//         speed:=vball*4;
//         if vball < 0.1 then fastball:=FALSE;
//       end else begin
//         bigball:=0;
//         x:=BSx-cx*tkToBallDist;
//         y:=BSy-cy*tkToBallDist;
//         teta:=ATan2(cy,cx);
//         speed:=vball+speedMax;
//         speed_on_target:=vball*2;
//       end;
//       deltadist:=0.04;
//       deltateta:=1.0;
//    end;
//
//    chipkick_dist:=0;
//    anyway:=false;
//    taskPars[num].avoid_is_set:=true;
//    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
//    Otherteta:=DiffAngle(RobotState[num].teta,ATan2(RobotState[num].y-BallState.y,RobotState[num].x-BallState.x));
//    if ((Otherteta>25*pi/180) and (BallState.x>RobotState[num].x)) then begin
//     avoid:=[];
//    end;
//  end;
//end;

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
{var bx,by,cx,cy,ang,d,tgx,tgy,tgang: double;
    i: integer;
    point_found: boolean;
begin
  RobotInfo[num].action:=acGoToXYTeta;

  with ActionPars[num] do begin
    speed:=SpeedMax;
    speed_on_target:=0;

    //manuel...oponent_golaie not defined, points to oponent goal
    GetTargetForGoal(tgx,tgy,0); //TODO: move goalie_radius into a proper parameter

    bx:=RobotCalcData[num].pred_ball_x;
    by:=RobotCalcData[num].pred_ball_y;

    //tgang:=ATan2(RobotCalcData[num].pred_ball_y-tgy,RobotCalcData[num].pred_ball_x-tgx);

    {x := bx + cos(tgang) * 0.80; //TODO: move into a proper parameter
    y := by + sin(tgang) * 0.80;  }

    //teta := NormalizeAngle(tgang + Pi);

    cx := tgx-bx;
    cy := tgy-by;
    //cx:=tgx-RobotCalcData[num].pred_ball_x;
    //cy:=tgy-RobotCalcData[num].pred_ball_y;
    NormalizeVector(cx,cy);

    x:=BallState.x-cx*tkToBallDist;
    y:=BallState.y-cy*tkToBallDist;
    //x:=RobotCalcData[num].pred_ball_x-cx*0.17;
    //y:=RobotCalcData[num].pred_ball_y-cy*0.17;


    teta:=ATan2(cy,cx);

    chipkick_dist:=0;

    anyway:=false;

    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
  end;
end; }


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



{procedure taskBehindBallRules(num: integer);
var bestx,besty,bestdist,cx,cy,ang,d,tgx,tgy,tgang: double;
    i: integer;
    point_found: boolean;
begin
  RobotInfo[num].action:=acGoToXYTeta;

  GetTargetForGoal(tgx,tgy);

//  bx:=RobotCalcData[num].pred_ball_x;
//  by:=RobotCalcData[num].pred_ball_y;

  with ActionPars[num] do begin
    if RobotInfo[num].role=roleFreeKick then speed:=0.7
    else speed:=SpeedMax;

    cx:=RobotCalcData[num].pred_ball_x-RobotState[num].x;
    cy:=RobotCalcData[num].pred_ball_y-RobotState[num].y;

    tgang:=ATan2(RobotCalcData[num].pred_ball_y-tgy,RobotCalcData[num].pred_ball_x-tgx);

    teta:=ATan2(cy,cx);
    d:=Dist(cx,cy);


    if (d>0.25) or (Abs(DiffAngle(teta,RobotState[num].teta))>50*Pi/180) then begin
      RobotInfo[num].action:=acGoToXYTeta;

      // TODO: escolher um ponto tangente para uma trajectoria mais suave

      point_found:=false;
      bestdist:=100;
      for i:=0 to 35 do begin
        ang:=i*(10*Pi/180);
        cx:=RobotCalcData[num].pred_ball_x+cos(ang)*0.2;
        cy:=RobotCalcData[num].pred_ball_y+sin(ang)*0.2;

        if DistNearestOponent(cx,cy)>OponentSpace then begin
          //d:=Dist(RobotState[num].x-cx,RobotState[num].y-cy);

          d:=abs(DiffAngle(ang,tgang));
          if (Abs(DiffAngle(teta,tgang))<Pi/2) and (Abs(DiffAngle(teta,ang))<Pi/2) then
            d:=d+100; // avoid trajectories over the ball

          if d<bestdist then begin
            bestx:=cx;
            besty:=cy;
            bestdist:=d;
            point_found:=true;
          end;
        end;
      end;
      if point_found then begin
        x:=bestx;
        y:=besty;
      end else begin
        x:=RobotCalcData[num].pred_ball_x;
        y:=RobotCalcData[num].pred_ball_y;
      end;
    end else begin
      RobotInfo[num].action:=acOrbit;
      x:=RobotCalcData[num].pred_ball_x;
      y:=RobotCalcData[num].pred_ball_y;

      ang:=DiffAngle(teta,Atan2(tgy-y,tgx-x));

      w:=-ang*5;
      //if w>2.5 then w:=2.5;
      if w>3.5 then w:=3.5;

      if RobotInfo[num].role=roleFreeKick then sx:=0.125
      else sx:=0.14;

      sy:=ang;

      kick_speed:=0.0;
      roller_speed:=0.0;
    end;

    if d<0.2 then begin
      roller_speed:=1;
    end else begin
      roller_speed:=0;
    end;

//    if RobotInfo[num].role=roleFreeKick then kick_speed:=0;
    kick_speed:=IsKickAGoal(num);

    anyway:=false;
//    avoid:=[avoidRobot,avoidOponent,avoidBall,avoidWalls,avoidOurArea];
    avoid:=[avoidRobot,avoidOponent,avoidWalls,avoidOurArea,avoidSlide];
  end;
end;
}
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
  {tgx := TaskPars[num].x1;
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
  end;}
end;

procedure taskCatchBallRules(num: integer);
var cx,cy,tgx,tgy, d,Otherteta,angball,vball,angRB,BSx,BSy,BSxn,BSyn: double;
    //LowPassFilter: TLowPassFilter;
begin
 {
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
         //fastball:=TRUE;
         teta:= -Atan2(BallState.vy,BallState.vx);
         x:=BSx;
         y:=BSy;
         speed_on_target:=vball*4;
         speed:=vball*4;
         //if vball < 0.1 then begin
         //   fastball:=FALSE;
         //end;
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
  end;}
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

{procedure taskIntersectForGoalRules(num: integer);
var tteta: double;
    btx,bty,bvx,bvy: double;
    rtx,rty,intx,t: double;

    pixx,pixy: integer;
begin
  RobotInfo[num].action:=acFollowVector;
  with ActionPars[num] do begin
    speed:=0;
    sx:=TaskPars[num].start_x;
    sy:=TaskPars[num].start_y;
    x:=TheirGoalX;
    y:=0;
    kick_speed:=0.0;
    roller_speed:=0.0;
    avoid:=[];

    //speed:=SpeedMax*2;
    //exit;

    // calculate speed to intersect Ball;
    tteta:=ATan2(y-sy,x-sx);

    TranslateAndRotate(btx,bty,BallState.x,BallState.y,-sx,-sy,-tteta);
    TranslateAndRotate(bvx,bvy,BallState.vx,BallState.vy,0,0,-tteta);

    TranslateAndRotate(rtx,rty,RobotState[num].x,RobotState[num].y,-sx,-sy,-tteta);

    // calc intersection point
    if bty>0 then begin
      intx:=btx+(bvx*(bty-0.04))/bvy;
    end else begin
      intx:=btx+(bvx*(bty+0.04))/bvy;
    end;

    // filter estimate
    intx:=intx*0.4+TaskPars[num].inter_est*0.6;
    TaskPars[num].inter_est:=intx;

    // calc time of intersection
    t:=-bty/bvy;

    // intersection too far away
    if intx>1.5 then exit;
    // ball moving away from trajectory
    if t<-0.5 then exit;
    // robot passed intersection point
    if RobotState[num].x>intx+0.2 then exit;

    FMain.ImageField.Canvas.Pen.Style:=psSolid;
    FMain.ImageField.Canvas.Brush.Style:=bsClear;
    WorldToField(sx+intx*cos(tteta),sy+intx*sin(tteta),pixx,pixy);
    FMain.ImageField.Canvas.Ellipse(pixx-2,pixy-2,pixx+2,pixy+2);

    if t<0.1 then begin
      speed:=SpeedMax;
    end else begin
      speed:=Dist(rtx-(intx-0.08),rty)/t;
    end;

    if speed>SpeedMax*2.5 then speed:=SpeedMax*2.5;
  end;
end;}

procedure taskScoreFreeKickRules(num: integer);
var cx,cy,tgx,tgy,dgoal,rob_num: double;
    dummydist: double;
begin
  tgx := TaskPars[num].x1;
  tgy := TaskPars[num].y1;
  with ActionPars[num] do begin
    RobotInfo[num].action:=acGoToXYTeta;

    speed:=0.4;
    //speed:=0.2;

    cx:=tgx-BallState.x;
    cy:=tgy-BallState.y;
    dgoal:=Dist(cx,cy);
    NormalizeVector(cx,cy);

    x:=RobotState[num].x+cx*0.2;
    y:=RobotState[num].y+cy*0.2;
    teta:=ATan2(cy,cx);

    with TacticCommands[num] do begin
      chipkick_pulse:=0;
      if (BallInFrontOfRobot(num,0)) then begin
       if  RobotInfo[num].role=roleScorePenaltyKick then begin
          chipkick_pulse:=MaxChipKickPulse;
          Low_kick:=true;
       end else begin
          rob_num := WhoIs(roleAtackerReceiver);
          if (rob_num<>-1) then begin
           chipkick_pulse:=6;
           Low_kick:=true;
          end else begin
           chipkick_pulse:=kickPulsFunc(num);
           Low_kick:=false;
          end;
       end;
      end;
    end;

    avoid:=[];
  end;
end;

procedure taskFreeKickRules(num: integer);
begin
  // TODO
{  with ActionPars[num] do begin
    RobotInfo[num].action:=acForward;
    speed:=-SpeedMax;
    kick_speed:=0.0;
    roller_speed:=0.0;
  end;}
end;

procedure taskUnderStressRules(num: integer);
begin
  // TODO
{  with ActionPars[num] do begin
    RobotInfo[num].action:=acForward;
    speed:=-SpeedMax;
    kick_speed:=0.0;
    roller_speed:=0.0;
  end;}
end;

procedure taskStayBackRules(num: integer);
begin
  // TODOREF: optimizar posicoes dos robots atras
  RobotInfo[num].action:=acGoToXYTeta;
  with ActionPars[num] do begin
    speed:=0.5;
    //TODO:TAC
    //manuel
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
var
  bx, by, cx,cy, dx,dy, tgx, tgy, d: double;
begin
  with ActionPars[num] do begin
    RobotInfo[num].action:=acGoToXYTeta;
    //speed:=0.5;//1.5;
    speed:=SpeedMax*2;



    GetTargetForGoal(tgx,tgy,0); //TODO: move goalie_radius into a proper parameter

    bx:=RobotCalcData[num].pred_ball_x;
    by:=RobotCalcData[num].pred_ball_y;
    cx := tgx-bx;
    cy := tgy-by;
    dx:=tgx-RobotState[num].x;
    dy:=tgy-RobotState[num].y;
    NormalizeVector(cx,cy);
    teta:=ATan2(cy,cx);

    x:=BallState.x-cx*tkToBallDist;
    y:=BallState.y-cy*tkToBallDist;
    avoid:=[avoidRobot,avoidOponent, avoidBall];
    if (Dist(x-RobotState[num].x,y-RobotState[num].y)<0.2) then begin
      x:=BallState.x;
      y:=BallState.y;
      avoid:=[avoidRobot,avoidOponent];

    end;
    //teta:=ATan2(BallState.y-RobotState[num].y,BallState.x-RobotState[num].x);
    d:=Dist(tgx-RobotState[num].x,tgy-RobotState[num].y);
    if ((d<2) and (RobotState[num].x < 8)) then
      chipkick_dist:=10.0
    else
      chipkick_dist:=0.0;

  end;
end;

function kickPulsFunc(num:integer):integer;
var distGoal:double;
begin
   distGoal:=Dist(TheirGoalX-RobotState[num].x,-RobotState[num].y);
   if distGoal<=4 then
    result:=14
   else if distGoal>=9 then
    result:=MaxChipKickPulse
   else
    result:=round(coefB*distGoal+coefC);
end;



procedure taskWingmanRules(num: integer);
var d,ang: double;
begin
  // wingmans go to a diferent place during our kickoff's
    //TODO:TAC
{  if teamRole in [trWaitOurStart, trOurStart] then begin
    with ActionPars[num] do begin
      RobotInfo[num].action:=acGoToXYTeta;
      speed:=SpeedMax;

      x:=-0.2;
      y:=1.0;
      if RobotInfo[num].role=roleWingmanL then y:=-y;

      teta:=0;
    end;
    exit;
  end;

  if RobotInfo[num].role=roleWingmanR then begin
    ang:=25*Pi/180
  end else begin
    ang:=-25*Pi/180;
  end;

  with ActionPars[num] do begin
    RobotInfo[num].action:=acGoToXYTeta;
    speed:=SpeedMax;

    if (RobotInfo[num].role=roleWingmanR) and (BallState.x<OurGoalX+1.0) then begin
      x:=0.2;
      y:=0;
      teta:=0;
    end else begin
      d:=0.5;
      x:=BallState.x-cos(ang)*d;
      y:=BallState.y-sin(ang)*d;

      d:=Dist(x-OurGoalX,y);
      if (d<AreaWidth+0.2) and (d>1e-3) then begin
        x:=((x-OurGoalX)/d)*(AreaWidth+0.2)+OurGoalX;
        y:=(y/d)*(AreaWidth+0.2);
      end;
      teta:=RobotCalcData[num].ball_teta;
    end;
  end;

  with ActionPars[num] do begin
    kick_speed:=0.0;

    avoid:=[avoidRobot,avoidOponent,avoidBall,avoidOurArea,avoidSlide];
  end;}
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
    //teta := -ATan2(y-BallState.y,x-BallState.x);

    taskPars[num].avoid_is_set:=true;
    avoid:=[avoidRobot,avoidOponent,avoidOurArea,avoidBall];
    //Otherteta:=DiffAngle(RobotState[num].teta,ATan2(RobotState[num].y-BallState.y,RobotState[num].x-BallState.x));
    //if ((Otherteta>35*pi/180) and (BallState.x>RobotState[num].x)) then begin
    // avoid:=[];
    //end;
    ////GetTargetForGoal(tgx,tgy);
    //tgx:=TheirGoalX;
    //tgy:=-Sign(RobotCalcData[num].pred_ball_y)*(GoalWidth/2-0.05);
    //
    //teta_ball:=ATan2(BallState.y-y,BallState.x-x);
    //teta_target:=ATan2(tgy-y,tgx-x);
    //teta:=MidAngle(teta_ball,teta_target);
    //
    //// compensate the center offset to try to hit the ball with the bumper
    //x:=x+cos(teta+Pi)*0.05;
    //y:=y+sin(teta+Pi)*0.05;
    //
    //IsKickAGoal(num,chipkick_dist);
    //avoid:=[avoidRobot,avoidOponent,avoidBall,avoidOurArea];
  end;
end;

end.

