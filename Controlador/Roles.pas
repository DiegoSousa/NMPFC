unit Roles;

{$mode objfpc}{$H+}

interface

uses DecConsts,Field;

type
  TRoleFunc=procedure(num: integer);

type
  TRole=(roleIdle,
         roleKeeper, roleKeeperDefendPenalty, roleKeeperPassive,
         roleAtackerKicker, roleAtackerReceiver, roleReceivePass,
         roleMamao,
         roleAtackerSupporter,
         roleLastDefender, roleOtherDefender, roleRoundLastDefender, roleRoundOtherDefender,
         roleFreeKick,
         roleKickOff,
         rolePenaltyKick, roleScorePenaltyKick,
         roleStayBack,
         roleBarrierMain, roleBarrierAux, roleBarrierInterceptor,
         roleWaitForKickOff,
         roleRobotRobotDefender, roleDefend3,
         roleWingmanL, roleWingmanR ,
         roleWaitForFreeKick,
         roleWaitToThrowIn, roleThrowIn,
         roleWaitForCornerKick, roleCornerKick,
         roleWaitOurGoalKick,
         roleWaitDroppedBall,
         roleMidfield,
         roleMidOfMidfield,roleMidOfMidfieldRight,roleMidOfMidfieldLeft,roleGoPosStart,
         roleGoPosStop,
         roleNavigateToLocalize,
         roleFurtiveAtackerKicker,
         roleTest, roleGoSearch1, roleGoSearch2, roleGoSearch3, roleGoSearch4,
         //TIAGO TESE
         roleGoSearch,
         roleGoSearchFollower,
         roleDoFormation
         );

type
  TRoleDef = record
    name: string;
    func: TRoleFunc;
    is_keeper_role: boolean;
  end;

var
  deltadist,deltateta:double;

procedure RoleIdleRules(num: integer);
procedure RoleGoPosStartRules(num: integer);
procedure RoleGoPosStopRules(num: integer);
procedure RoleKeeperRules(num: integer);
procedure RoleKeeperDefendPenaltyRules(num: integer);
procedure RoleKeeperPassiveRules(num: integer);
procedure roleAtackerKickerRules(num: integer);
procedure roleAtackerKicker2Rules(num: integer);
procedure roleAtackerReceiverRules(num: integer);
procedure roleReceivePassRules(num: integer);
procedure roleLastDefenderRules(num: integer);
procedure RoleFreeKickRules(num: integer);
procedure RoleKickOffRules(num: integer);
procedure RolePenaltyKickRules(num: integer);
procedure RoleScorePenaltyKickRules(num: integer);
procedure RoleStayBackRules(num: integer);
procedure RoleMamaoRules(num: integer);
procedure RoleGoSearchRules1(num: integer);
procedure RoleGoSearchRules2(num: integer);
procedure RoleGoSearchRules3(num: integer);
procedure RoleGoSearchRules4(num: integer);
procedure RoleBarrierMainRules(num: integer);
procedure roleWaitForKickOffRules(num: integer);
procedure RoleWaitForFreeKickRules(num: integer);
procedure RoleWaitToThrowInRules(num: integer);
procedure RoleThrowInRules(num: integer);
procedure RoleWaitForCornerKickRules(num: integer);
procedure RoleCornerKickRules(num: integer);
procedure RoleWaitOurGoalLickRules(num: integer);
procedure RoleOurGoalLickRules(num: integer);
procedure RoleWaitDroppedBallRules(num:integer);
procedure RoleMidfieldRules(num: integer);
procedure RoleMidOfMidfieldRules(num: integer);
procedure RoleMidOfMidfieldRightRules(num: integer);
procedure RoleMidOfMidfieldLeftRules(num: integer);
procedure RoleNavigateToLocalizeRules(num:integer);
procedure RoleFurtiveAtackerKickerRules(num:integer);

//////
procedure roleOtherDefenderRules(num: integer);
procedure RoleRoundLastDefenderRules(num: integer);
procedure RoleRoundOtherDefenderRules(num: integer);
procedure RoleAtackerSupporterRules(num: integer);
procedure GetBarrierPosition(offset: double; var bx, by, teta: double);
procedure roleBarrierAuxRules(num: integer);
procedure roleBarrierInterceptorRules(num: integer);
procedure roleRobotRobotDefenderRules(num: integer);
procedure RoleDefend3Rules(num: integer);
procedure RoleWingmanRRules(num: integer);
procedure RoleWingmanLRules(num: integer);

//TIAGO TESE
procedure RoleTestRules(num: integer);
procedure RoleDoFormationRules(num: integer);
procedure RoleGoSearchRules(num: integer);
procedure RoleGoSearchFollowerRules(num: integer);

procedure GetReceiverPosition(offset: double; var bx, by, teta: double);

const
  RoleDefs: array[low(TRole) .. High(TRole)] of TRoleDef = (
    ( name:'roleIdle';                func: @RoleIdleRules ),
    ( name:'roleKeeper';              func: @RoleKeeperRules;              is_keeper_role:true ),
    ( name:'roleKeeperDefendPenalty'; func: @roleKeeperDefendPenaltyRules; is_keeper_role:true ),
    ( name:'roleKeeperPassive';       func: @roleKeeperPassiveRules;       is_keeper_role:true ),
    ( name:'roleAtackerKicker';       func: @roleAtackerKickerRules ),
    ( name:'roleAtackerReceiver';     func: @roleAtackerReceiverRules ),
    ( name:'roleReceivePass';         func: @roleReceivePassRules ),
    ( name:'roleMamao';               func: @roleMamaoRules ),
    ( name:'roleAtackerSupporter';    func: @roleAtackerSupporterRules ),
    ( name:'roleLastDefender';        func: @roleLastDefenderRules ),
    ( name:'roleOtherDefender';       func: @roleOtherDefenderRules ),
    ( name:'roleRoundLastDefender';   func: @roleRoundLastDefenderRules ),
    ( name:'roleRoundOtherDefender';  func: @roleRoundOtherDefenderRules ),
    ( name:'roleFreeKick';            func: @roleFreeKickRules ),
    ( name:'roleKickOff';             func: @roleKickOffRules ),
    ( name:'rolePenaltyKick';         func: @rolePenaltyKickRules ),
    ( name:'roleScorePenaltyKick';    func: @roleScorePenaltyKickRules ),
    ( name:'roleStayBack';            func: @roleStayBackRules ),

    ( name:'roleBarrierMain';         func: @roleBarrierMainRules ),
    ( name:'roleBarrierAux';          func: @roleBarrierAuxRules ),
    ( name:'roleBarrierInterceptor';  func: @roleBarrierInterceptorRules ),
    ( name:'roleWaitForKickOff';      func: @roleWaitForKickOffRules ),
    ( name:'roleRobotRobotDefender';  func: @roleRobotRobotDefenderRules ),

    ( name:'roleDefend3';             func: @roleDefend3Rules ),
    ( name:'roleWingmanR';            func: @roleWingmanRRules ),
    ( name:'roleWingmanL';            func: @roleWingmanLRules ),

    ( name:'roleWaitForFreeKick';     func: @roleWaitForFreeKickRules ),
    ( name:'roleWaitToThrowIn';       func: @roleWaitToThrowInRules ),
    ( name:'roleThrowIn';             func: @roleThrowInRules ),
    ( name:'roleWaitForCornerKick';   func: @roleWaitForCornerKickRules ),
    ( name:'roleCornerKick';          func: @roleCornerKickRules ),
    ( name:'roleWaitOurGoalKick';     func: @roleWaitOurGoalLickRules),
    ( name:'roleWaitDroppedBall';     func: @RoleWaitDroppedBallRules),

    ( name:'RoleMidfield';     func: @RoleMidfieldRules),
    ( name:'RoleMidOfMidfield';     func: @RoleMidOfMidfieldRules),
    ( name:'roleMidOfMidfieldLeft';     func: @RoleMidOfMidfieldLeftRules),
    ( name:'roleMidOfMidfieldRight';     func: @RoleMidOfMidfieldRightRules),
    ( name:'roleGoPosStart';     func: @RoleGoPosStartRules),
    ( name:'roleGoPosStop';     func: @RoleGoPosStopRules),
    ( name:'roleNavigateToLocalize'; func: @RoleNavigateToLocalizeRules),
    ( name:'roleFurtiveAtackerKicker'; func: @RoleFurtiveAtackerKickerRules),
    ( name:'roleTest';                func: @roleTestRules ),
    ( name:'roleGoSearch1';           func: @roleGoSearchRules1 ),
    ( name:'roleGoSearch2';           func: @roleGoSearchRules2 ),
    ( name:'roleGoSearch3';           func: @roleGoSearchRules3 ),
    ( name:'roleGoSearch4';           func: @roleGoSearchRules4 ),
    //TIAGO TESE
    ( name:'roleGoSearchFollower';           func: @roleGoSearchFollowerRules ),
    ( name:'roleGoSearch';           func: @roleGoSearchRules ),
    ( name:'roleDoFormation'; func: @RoleDoFormationRules)
  );

implementation

uses Main, Tasks, Actions, Tactic, Param, Utils, Analyser, Math, ObsAvoid, {Unit_RolesAux,}MPC;

//----------------------------------------------------------------------
//  Role Rules
//----------------------------------------------------------------------

procedure RoleIdleRules(num: integer);
begin
  RobotInfo[num].task:=taskIdle;
end;


function BallNearCorner(c: double): boolean;
var ang: double;
begin
  ang:=ATan2(Abs(BallState.y)-(-0.25),BallState.x-(-FieldLength*0.5));
  result:=(Abs(BallState.y)>0.48-0.1*c) and (not (ang<0)) and (ang>(Pi/2-(30*Pi/180)*c));
end;

procedure RoleGoPosStartRules(num: integer);
begin
  with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      x1 := Xstart;
      y1:= Ystart;
      teta := 0;
      speed := 2;
    end;
    deltadist:=0.09;
    deltateta:=5;
    task:=taskGoToGoodPosition;
  end;
end;

procedure RoleGoPosStopRules(num: integer);
begin
  with RobotInfo[num], TaskPars[num] do begin
      x1 := Xstop;
      y1:= Ystop;
      teta := 0;
      speed := SpeedMax*0.5;
    deltadist:=0.09;
    deltateta:=5;
    if (Dist(x1-RobotState[num].x,y1-RobotState[num].y)<0.2) then
     task:=taskIdle
    else
     task:=taskGoToGoodPosition;
  end;
end;

procedure RoleKeeperRules(num: integer);
var time_to_goal, target_y,target_x, dist_ball_area,otherTeta: double;
begin
  if ((abs(BallState.y)>FieldDims.AreaWidth/2) or (BallState.x>-FieldDims.FieldDepth/2+FieldDims.AreaDepth) or (BallState.quality<=500)) then begin
    deltadist:=0.04;
    deltateta:=5;
    RobotInfo[num].task:=taskKeepGoalSafe;
  end;
  if ((abs(BallState.y)<=FieldDims.AreaWidth/2) and (BallState.x<=-FieldDims.FieldDepth/2+FieldDims.AreaDepth) and (BallState.x>=-FieldDims.FieldDepth/2) and (BallState.quality>=500)) then begin
    deltadist:=0.04;
    deltateta:=5;
    RobotInfo[num].task:=taskGetRidOfBall
  end;
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;

procedure RoleKeeperDefendPenaltyRules(num: integer);
begin
  RobotInfo[num].task:=taskDefendPenalty;
end;

procedure RoleKeeperPassiveRules(num: integer);
begin
  RobotInfo[num].task:=taskKeepGoalSafe;
end;

procedure roleAtackerKicker2Rules(num: integer);
var d,ang,ang_ball,tgx,tgy,rob_dist,BSv,Bx,By: double;
begin

  //
  with RobotInfo[num] do begin
    GetTargetForGoal(tgx,tgy,0);

    // state rules
    TaskPars[num].x1 := tgx;
    TaskPars[num].y1 := tgy;
    speedontgt:=0.1;
    //

    case RobotInfo[num].task of
      taskCatchBall: begin
        if RobotState[num].withball=true then begin
               task:=taskdribleForGoal;
        end;
      end;
      taskdribleForGoal: begin
        if (RobotState[num].withball=false) then begin
          task:=taskCatchBall;
        end;
      end;
      else
        task:=taskCatchBall;
    end;

    if ((abs(BallState.x)>(FieldDims.FieldDepth/2)) or (abs(BallState.y)>(FieldDims.FieldWidth/2))or
        ((BallState.x>(FieldDims.FieldDepth/2)-(FieldDims.KeeperAreaDepth)) and
         (abs(BallState.y)<(FieldDims.KeeperAreaWidth/2)))) then begin
        task:=taskIdle;
    end;
  end;
end;

procedure roleAtackerKickerRules(num: integer);
var d,ang,ang_ball,tgx,tgy,rob_dist,BSv,Bx,By: double;
begin

  with RobotInfo[num] do begin
    GetTargetForGoal(tgx,tgy,0);

    // angulo do robo em relacao ao angulo de vector bola baliza
    ang:=DiffAngle(RobotState[num].teta,Atan2(tgy - BallState.y, tgx - BallState.x)) * 180 / Pi;
    // distancia em relacao à bola
    d := Dist(RobotState[num].x - BallState.x, RobotState[num].y - BallState.y);
    // angulo do robo em relacao ao angulo de vector bola
    ang_ball := DiffAngle(Atan2(BallState.y - RobotState[num].y, BallState.x - RobotState[num].x),RobotState[num].teta) * 180 / Pi;

    // state rules
    TaskPars[num].x1 := tgx;
    TaskPars[num].y1 := tgy;
    speedontgt:=0.1;
    //

    case task of
      taskGoToBall: begin
        if ((abs(ang)<5) and (abs(ang_ball)<6)
                         and (RobotState[num].w<0.1)
                         and (d<(tkToBallDist+0.08))) then begin
          task:=taskdribleForGoal;
        end;
      end;
      taskdribleForGoal: begin
        if (((abs(ang)>10)or(d>=tkToBallDist+0.10)or(ang_ball>20))) then begin
          task:=taskGoToBall;
        end;
      end;
      else
        task:=taskGoToBall;
    end;

    if ((abs(BallState.x)>(FieldDims.FieldDepth/2)) or (abs(BallState.y)>(FieldDims.FieldWidth/2))or
        ((BallState.x>(FieldDims.FieldDepth/2)-(FieldDims.KeeperAreaDepth)) and
         (abs(BallState.y)<(FieldDims.KeeperAreaWidth/2)))) then begin
        task:=taskIdle;
    end;
  end;
end;

procedure roleAtackerReceiverRules(num: integer);
begin
  with RobotInfo[num] do begin
    with TaskPars[num] do begin
      StopDistance:=ReceiverDistance;
      x1 := BallState.x;
      //
      if BallState.y >1 then
         y1 := BallState.y - StopDistance
      else if BallState.y<=0.1 then
         y1 := BallState.y + StopDistance;
      //

      teta := -ATan2(y1-BallState.y,x1-BallState.x);
      speed := SpeedMax;
    end;
    deltadist:=0.04;
    deltateta:=2;
    task:=taskGoToGoodPosition;
    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      task:=taskIdle;
    end;
  end;
end;

procedure roleReceivePassRules(num: integer);
var d: double;
begin
  with RobotInfo[num] do begin

  // distancia em relacao à bola
    d := Dist(RobotState[num].x - BallState.x, RobotState[num].y - BallState.y);
    with TaskPars[num] do begin
      StopDistance:=ReceiverDistance;
      x1 := BallState.x;
      if BallState.y >1 then
         y1 := BallState.y - ReceiverDistance
      else if BallState.y<0.5 then
         y1 := BallState.y + ReceiverDistance;

      teta := -ATan2(y1-BallState.y,x1-BallState.x);
      speed := SpeedMax;
    end;
    deltadist:=0.05;
    deltateta:=2;

    task:=taskGoToGoodPosition;

    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      task:=taskIdle;
    end;
  end;
end;

procedure roleLastDefenderRules(num: integer);
var
  target_y, time_to_goal: double;
  bx, by,d:double;
begin

  PredictIncomingBallToGoal(time_to_goal, target_y, LastDefenderLine+0.08,2);
  deltadist:=0.09;
  deltateta:=5;
  RobotInfo[num].task:=taskDefendLineAngle;
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;

procedure roleOtherDefenderRules(num: integer);
begin
  deltadist:=0.09;
  deltateta:=5;
  RobotInfo[num].task:=taskDefendAngle;
end;

procedure RoleRoundLastDefenderRules(num: integer);
var rx, ry, target, radius: double;
begin
  with RobotInfo[num], TaskPars[num] do begin

    target := 0.5;
    radius := AreaWidth + 1.5;

    if VectorCircleIntersection(
      BallState.x, BallState.y, OurGoalX - BallState.x, target - BallState.y,
      OurGoalX, target, radius, rx, ry) then begin
      x1 := rx;
      y1 := ry;
    end;
    teta := RobotCalcData[num].ball_teta;
    speed := SpeedMax;
    avoid_is_set := true;
    avoid := [avoidRobot, avoidOurArea];

    task:=taskGoToGoodPosition;
  end;
end;

procedure RoleRoundOtherDefenderRules(num: integer);
var rx, ry, target, radius: double;
begin
  with RobotInfo[num], TaskPars[num] do begin

    target := -0.015;
    radius := AreaWidth + 0.58;

    if VectorCircleIntersection(
      BallState.x, BallState.y, OurGoalX - BallState.x, target - BallState.y,
      OurGoalX, target, radius, rx, ry) then begin
      x1 := rx;
      y1 := ry;
    end;
    teta := RobotCalcData[num].ball_teta;
    speed := SpeedMax;

    avoid_is_set := true;
    avoid := [avoidRobot, avoidOponent, avoidOurArea];

    task:=taskGoToGoodPosition;
  end;
end;

procedure CommonGoalKickRules(num: integer; wait_for_it: boolean);
var d,ang,ang_ball,tgx,tgy,rob_dist: double;
begin
   with RobotInfo[num], TaskPars[num] do begin

    tgx :=TheirGoalX;
    tgy := 0;
    speedontgt:=0;

    // diferenca entre a angulo target/robo e o angulo fo robo
    ang:=DiffAngle(RobotState[num].teta,
      Atan2(tgy - BallState.y, tgx - BallState.x)) * 180 / Pi;

    d := Dist(RobotState[num].x - BallState.x, RobotState[num].y - BallState.y);
    // diferenca entre angulo do robo e angulo com abola
    ang_ball := DiffAngle(
    Atan2(BallState.y - RobotState[num].y, BallState.x - RobotState[num].x),
    RobotState[num].teta) * 180 / Pi;

    TaskPars[num].x1 := tgx;
    TaskPars[num].y1 := tgy;

    case RobotInfo[num].task of
      taskBehindBall: begin
        if (not wait_for_it) and (abs(ang)<4) and (RobotState[num].w<0.1) and (d<tkToBallDist+0.03) and (abs(ang_ball)<10) then begin
          task:=taskScoreFreeKick;
        end;
      end;
      taskScoreFreeKick: begin
        if (abs(ang)>50) or (d>tkToBallDist+0.08) or (ang_ball>50) then begin
          task:=taskBehindBall;
        end;
      end;
      else
        task:=taskBehindBall;
    end;
  end;
end;
procedure CommonCornerKickRules(num: integer; wait_for_it: boolean);
var d,ang,ang_ball,tgx,tgy,rob_dist: double;
begin
   with RobotInfo[num], TaskPars[num] do begin

    tgx :=TheirGoalX-FieldDims.AreaDepth;
    tgy := 0;
    speedontgt:=0;

    // diferenca entre a angulo target/robo e o angulo fo robo
    ang:=DiffAngle(RobotState[num].teta,
      Atan2(tgy - BallState.y, tgx - BallState.x)) * 180 / Pi;

    d := Dist(RobotState[num].x - BallState.x, RobotState[num].y - BallState.y);
    // diferenca entre angulo do robo e angulo com abola
    ang_ball := DiffAngle(
    Atan2(BallState.y - RobotState[num].y, BallState.x - RobotState[num].x),
    RobotState[num].teta) * 180 / Pi;

    TaskPars[num].x1 := tgx;
    TaskPars[num].y1 := tgy;

    case RobotInfo[num].task of
      taskBehindBall: begin
        if (not wait_for_it) and (abs(ang)<4) and (RobotState[num].w<0.1) and (d<tkToBallDist+0.03) and (abs(ang_ball)<10) then begin
          task:=taskScoreFreeKick;
        end;
      end;
      taskScoreFreeKick: begin
        if (abs(ang)>50) or (d>tkToBallDist+0.08) or (ang_ball>50) then begin
          task:=taskBehindBall;
        end;
      end;
      else
        task:=taskBehindBall;
    end;
  end;
end;
procedure CommonPenaltyKickRules(num: integer; wait_for_it: boolean);
var d,ang,ang_ball,tgx,tgy,rob_dist: double;
    randSide:integer;
begin
   with RobotInfo[num], TaskPars[num] do begin

    randSide:=round(Random+0.5);

    tgy := FieldDims.KeeperAreaWidth/3;
    tgx := TheirGoalX;

    speedontgt:=0;

    // diferenca entre a angulo target/robo e o angulo fo robo
    ang:=DiffAngle(RobotState[num].teta,
      Atan2(tgy - BallState.y, tgx - BallState.x)) * 180 / Pi;

    d := Dist(RobotState[num].x - BallState.x, RobotState[num].y - BallState.y);
    // diferenca entre angulo do robo e angulo com abola
    ang_ball := DiffAngle(
    Atan2(BallState.y - RobotState[num].y, BallState.x - RobotState[num].x),
    RobotState[num].teta) * 180 / Pi;

    TaskPars[num].x1 := tgx;
    TaskPars[num].y1 := tgy;

    case RobotInfo[num].task of
      taskBehindBall: begin
        if (not wait_for_it) and (abs(ang)<5) and (RobotState[num].w<0.1) and (d<tkToBallDist+0.03) and (abs(ang_ball)<10) then begin
          task:=taskScoreFreeKick;
        end;
      end;
      taskScoreFreeKick: begin
        if (abs(ang)>50) or (d>tkToBallDist+0.08) or (ang_ball>50) then begin
          task:=taskBehindBall;
        end;
      end;
      else
        task:=taskBehindBall;
    end;
  end;
end;

procedure CommonFreeKickRules(num: integer; wait_for_it: boolean);
var d,ang,ang_ball,tgx,tgy,rob_dist: double;
begin
  with RobotInfo[num] do begin
    rob_num := WhoIs(roleAtackerReceiver);

    if (rob_num<>-1) then begin
      tgx := BallState.x;
      if BallState.y>1 then
       tgy := -0.5+BallState.y
      else if BallState.y<0.5 then
       tgy := +0.5+BallState.y;
    end else begin
      tgx:=BallState.x+0.5*cos(Atan2(0-BallState.y,TheirGoalX-BallState.x));
      tgy:=BallState.y+0.5*sin(Atan2(0-BallState.y,TheirGoalX-BallState.x));
    end;

    speedontgt:=0;
    // diferenca entre a angulo target/robo e o angulo do robo
    ang:=DiffAngle(RobotState[num].teta,Atan2(tgy - BallState.y, tgx - BallState.x)) * 180 / Pi;
    d := Dist(RobotState[num].x - BallState.x, RobotState[num].y - BallState.y);
    // diferenca entre angulo do robo e angulo com abola
    ang_ball := DiffAngle(Atan2(BallState.y - RobotState[num].y, BallState.x - RobotState[num].x),
        RobotState[num].teta) * 180 / Pi;

    // state rules
    TaskPars[num].x1 := tgx;
    TaskPars[num].y1 := tgy;

    case RobotInfo[num].task of
      taskBehindBall: begin
        if (not wait_for_it) and (abs(ang)<4) and (RobotState[num].w<0.1) and (abs(ang_ball)<10) then begin
          deltadist:=0.02;
          deltateta:=2;
          task:=taskScoreFreeKick;
        end;
      end;
      taskScoreFreeKick: begin
        if (abs(ang)>50) or (ang_ball>50) or (d>0.4) then begin
          deltadist:=0.09;
          deltateta:=5;
          task:=taskBehindBall;
        end;
      end;
      else
        task:=taskBehindBall;
    end;
  end;
end;

procedure RoleFreeKickRules(num: integer);
begin
  CommonFreeKickRules(num, false);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;

procedure RoleWaitForKickOffRules(num: integer);
begin
  CommonFreeKickRules(num, true);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;

procedure roleRobotRobotDefenderRules(num: integer);
var best_oponent: integer;
    zonex, zoney: double;
    d,vx,vy: double;
begin

  zonex := -1.15;
  zoney := -Sign(BallState.y) * 0.90;

  // TODO : oponent
  best_oponent := -1;
  vx := zonex;
  vy := zoney;

  with RobotInfo[num] do begin
    with TaskPars[num] do begin
      x1 := vx;
      y1 := vy;
      teta := RobotCalcData[num].ball_teta;
      speed := SpeedMax;
    end;
    task:=taskGoToGoodPosition;
  end;
end;

procedure RoleScorePenaltyKickRules(num: integer);
begin
    CommonPenaltyKickRules(num, false);
    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      RobotInfo[num].task:=taskIdle;
    end;
end;

procedure RolePenaltyKickRules(num: integer);
begin
  CommonPenaltyKickRules(num,true);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;

procedure RoleStayBackRules(num: integer);
begin
    deltadist:=0.09;
    deltateta:=5;
  RobotInfo[num].task:=taskStayBack;
end;

procedure CommonMamaoRules(num: integer; offset: double);
begin
  with RobotInfo[num] do begin
    with TaskPars[num] do begin
      x1 := TheirGoalX - FieldDims.FieldDepth/4;
      y1 :=0;
      teta := RobotCalcData[num].ball_teta;
      speed := SpeedMax;
    end;
    task:=taskGoToGoodPosition;
  end;

end;


procedure RoleMamaoRules(num: integer);
begin
  deltadist:=0.09;
  deltateta:=5;
  CommonMamaoRules(num, 0.4);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;

procedure RoleGoSearchRules1(num: integer);
var X1Loc,Y1Loc,X2Loc,Y2Loc,X3Loc,Y3Loc,X4Loc,Y4Loc,val:double;
begin
//
//   X1Loc:=FieldDims.FieldDepth/4;
//   Y1Loc:=-FieldDims.FieldWidth/4;
//   X2Loc:=-FieldDims.FieldDepth/4;
//   Y2Loc:=-FieldDims.FieldWidth/4;
//   X3Loc:=-FieldDims.FieldDepth/4;
//   Y3Loc:=FieldDims.FieldWidth/4;
//   X4Loc:=FieldDims.FieldDepth/4;
//   Y4Loc:=FieldDims.FieldWidth/4;
//   TaskPars[num].teta:=0;
//   TaskPars[num].speed:=0.7;
//   val:=0.2;
//   with RobotInfo[num], TaskPars[num] do begin
//     with TaskPars[num] do begin
//       if (((xold<>X1loc)and(yold<>Y1loc))and((xold<>X2loc)and(yold<>Y2loc))and((xold<>X3loc)and(yold<>Y3loc)))or
//          ((xold=X4loc)and(yold=Y4loc)) then begin
//         x1 := X1Loc;
//         y1 := Y1Loc;
//         if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
//             ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
//           xold:=X1loc;
//           yold:=Y1loc;
//         end;
//       end else if ((xold=X1loc)and(yold=Y1loc)) then begin
//         x1 := X2loc;
//         y1 := Y2loc;
//         if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
//             ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
//           xold:=X2loc;
//           yold:=Y2loc;
//         end;
//       end else if ((xold=X2loc)and(yold=Y2loc)) then begin
//         x1 := X3loc;
//         y1 := Y3loc;
//         if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
//             ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
//           xold:=X3loc;
//           yold:=Y3loc;
//         end;
//       end else if ((xold=X3loc)and(yold=Y3loc)) then begin
//         x1 := X4loc;
//         y1 := Y4loc;
//         if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
//             ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
//           xold:=X4loc;
//           yold:=Y4loc;
//         end;
//       end;
//     end;
//     deltateta:=5;
//     task:=taskGoToGoodPosition;
//   end;

  with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      x1 := -(FieldDims.FieldDepth/4);
      y1:= 0;
      teta := 0;
      speed := 2.5;
    end;
    deltadist:=0.04;
    deltateta:=5;
    task:=taskGoToGoodPosition;
  end;
end;


procedure RoleGoSearchRules2(num: integer);
begin
  with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      //x1 := 0;
      x1 := 2.5;
      //y1:= -(FieldDims.FieldWidth/4);
      y1:=-2.3;
      teta := 0;
      speed := 2;
    end;
    deltadist:=0.04;
    deltateta:=5;
    task:=taskGoToGoodPosition;
  end;
end;

procedure RoleGoSearchRules3(num: integer);
begin
  with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      x1 := 0;
      y1:= (FieldDims.FieldWidth/4);
      teta := 0;
      speed := 2;
    end;
    deltadist:=0.04;
    deltateta:=5;
    task:=taskGoToGoodPosition;
  end;
end;

procedure RoleGoSearchRules4(num: integer);
begin
   with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      x1 := (FieldDims.FieldDepth/4);
      y1:= 0;
      teta := 0;
      speed := 2;
    end;
    deltadist:=0.04;
    deltateta:=5;
    task:=taskGoToGoodPosition;
  end;
end;

procedure RoleMamao2Rules(num: integer);
begin
  deltadist:=0.09;
  deltateta:=5;
  CommonMamaoRules(num, 0);
end;
procedure RoleMidfieldRules(num: integer);
begin
  with RobotInfo[num] do begin
    with TaskPars[num] do begin

      x1 :=-3;
      y1 :=0;
      teta := RobotCalcData[num].ball_teta;
      speed := SpeedMax;
    end;
    deltadist:=0.09;
    deltateta:=5;
    task:=taskGoToGoodPosition;
  end;
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;
procedure RoleMidOfMidfieldRules(num: integer);
begin
  with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      x1 := -FieldDims.FieldDepth/4;
      y1:=0;
      teta := RobotCalcData[num].ball_teta;
      speed := SpeedMax;
    end;
    deltadist:=0.09;
    deltateta:=5;
    task:=taskGoToGoodPosition;
    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      task:=taskIdle;
    end;
  end;
end;

procedure RoleMidOfMidfieldRightRules(num: integer);
begin
  with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      x1 := -FieldDims.FieldDepth/4;
      y1:=FieldDims.FieldWidth/4;
      teta := RobotCalcData[num].ball_teta;
      speed := SpeedMax;
    end;
    deltadist:=0.09;
    deltateta:=5;
    task:=taskGoToGoodPosition;
    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      task:=taskIdle;
    end;
  end;
end;

procedure RoleMidOfMidfieldLeftRules(num: integer);
begin
  with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      x1 := -FieldDims.FieldDepth/4;
      y1:=-FieldDims.FieldWidth/4;
      teta := RobotCalcData[num].ball_teta;
      speed := SpeedMax;
    end;
    deltadist:=0.09;
    deltateta:=5;
    task:=taskGoToGoodPosition;
    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      task:=taskIdle;
    end;
  end;
end;

procedure RoleNavigateToLocalizeRules(num: integer);
begin
  with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      task:=taskAdvisedSpeed;
    end;
  end;
end;

procedure RoleFurtiveAtackerKickerRules(num: integer);
var v,xNext,yNext,cx,cy:double;
    targetdir,angVehGoal,distBall:double;
    error:double;
begin
  with RobotInfo[num], TaskPars[num] do begin
    x1 := BallState.x_next;
    y1 := BallState.y_next;
    teta := ATan2(BallState.y_next-RobotState[num].y,BallState.x_next-RobotState[num].x);
    distBall:=Dist(BallState.x_n,BallState.y_n);
    task:=taskGoToPnt;

    if (RobotState[num].withball) then begin
     sumError:=0;
     targetdir := DiffAngle(ATan2(0-RobotState[num].y,FieldDims.FieldDepth/2-RobotState[num].x),RobotState[num].teta);
     if abs(targetdir)<degtorad(5) then begin
      speed:=SpeedMax;
      speed_on_target:=0.3;
      x1 :=TheirGoalX;
      y1 := BallState.y_next;
      task:=taskdribleForGoal
     end else begin
      task:=taskRotateToTargWithBall;
     end;
    end else begin
      if ((abs(DiffAngle(ATan2(BallState.y_next-RobotState[num].y,BallState.x_next-RobotState[num].x),RobotState[num].teta))<30*pi/180) and (Dist(BallState.x_n,BallState.y_n)<3))then begin
       if distBall<1.5 then
         speed:=0.5*SpeedMax
       else
         speed :=SpeedMax;
       error :=(BallState.vx_n-vxnTarget);
       sumError:=sumError+error;
       speed_on_target:=0.3;

       //speed_on_target:=error*0.25+sumError*0.01;
       //limitar a velocidade ao speed maximo
       if (speed_on_target>SpeedMax) then
        speed_on_target:=0.5*SpeedMax;

      end else begin
        sumError:=0;
        speed:=SpeedMax;
        speed_on_target:=0.2;
      end;
    end;
    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      task:=taskIdle;
    end;
  end;
end;

procedure RoleAtackerSupporterRules(num: integer);
begin
  with RobotInfo[num], TaskPars[num] do begin
    with TaskPars[num] do begin
      x1 := BallState.x - ReceiverDistance;
      y1 := BallState.y;
      teta := 0;
      speed := SpeedMax;
    end;
    task:=taskGoToGoodPosition;
    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      task:=taskIdle;
    end;
  end;
end;

procedure RoleKickOffRules(num: integer);
begin
  CommonFreeKickRules(num, false);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
   RobotInfo[num].task:=taskIdle;
  end;
end;

procedure GetBarrierPosition(offset: double; var bx, by, teta: double);
var ang, vx, vy,offsetInc: double;
begin
     vx := OurGoalX - ballstate.x;
     vy := 0 - ballstate.y;
     NormalizeVector(vx, vy);
     ang := pi + offset * Pi / 180;
     TranslateAndRotate(vx,vy,0,0,-vx,-vy,ang);
     teta := ATan2(-vy, -vx);
  if ((BallState.x < (-FieldDims.FieldDepth/2+StopDistance+FieldDims.AreaDepth+0.25)) and (BallState.x > -FieldDims.FieldDepth/2+FieldDims.AreaDepth+0.25) and (offset=0))  then
  begin
       bx:=-FieldDims.FieldDepth/2+FieldDims.AreaDepth-0.25;
       by:=FieldDims.AreaDepth*tan(teta);
  end
  else if ((BallState.x < -FieldDims.FieldDepth/2+FieldDims.AreaDepth+0.25)and(offset=0)) then
  begin
       if BallState.y>0 then
       begin
            by:=FieldDims.AreaWidth/2-0.25;
            if teta*180/pi>angCornerDefence then
            begin
                 bx:=-FieldDims.FieldDepth/2+by/tan(angCornerDefence*pi/180);
            end
            else
            begin
                bx:=-FieldDims.FieldDepth/2+by/tan(teta);
            end;
       end
       else
       begin
            by:=-FieldDims.AreaWidth/2+0.25;
            if teta*180/pi<-angCornerDefence then
            begin
                 bx:=-FieldDims.FieldDepth/2+by/tan(-angCornerDefence*pi/180);
            end
            else
            begin
                bx:=-FieldDims.FieldDepth/2+by/tan(teta);
            end;
       end;
  end
  else
  begin
     offsetInc:=offset;
     bx := BallState.x + vx * StopDistance;
     by := BallState.y + vy * StopDistance;
     If (not (abs(offset)<20)) then begin
     while((abs(bx)>FieldDims.FieldDepth/2-FieldDims.AreaDepth-0.30))do begin // and (abs(by)<FieldDims.FieldWidth/2-StopDistance))do begin
              offsetInc:=offsetInc+Sign(offsetInc)*3;
              vx := OurGoalX - ballstate.x;
              vy := 0 - ballstate.y;
              NormalizeVector(vx, vy);
              ang := pi + offsetInc * Pi / 180;
              TranslateAndRotate(vx,vy,0,0,-vx,-vy,ang);
              bx := BallState.x + vx * StopDistance;
              by := BallState.y + vy * StopDistance;
    end;
    end;
  end;

end;
procedure GetReceiverPosition(offset: double; var bx, by, teta: double);
var ang, vx, vy,offsetInc: double;
begin
              bx := BallState.x - 1;
              by := BallState.y + sqrt(ReceiverDistance*ReceiverDistance-1);
end;

procedure RoleBarrierMainRules(num: integer);
var vx,vy,ang: double;
begin
  with RobotInfo[num] do begin
    with TaskPars[num] do begin
      StopDistance:= 2;
      teta := RobotCalcData[num].ball_teta;
      vx:=BallState.x-(-FieldDims.FieldDepth/2);
      vy:=BallState.y;
      //
      NormalizeVector(vx,vy);
      //
      ang:=ATan2(vy,vx);
      x1:=BallState.x-StopDistance*cos(ang);
      y1:=BallState.y-StopDistance*sin(ang);

      if(y1<-FieldDims.FieldDepth/2+FieldDims.AreaDepth+0.1) then
         y1:=-FieldDims.FieldDepth/2+FieldDims.AreaDepth+0.1;

      speed := SpeedMax;
    end;
    task:=taskGoToGoodPosition;
  end;
end;

procedure roleBarrierAuxRules(num: integer);
var offset: double;
otherbarrier:integer;
begin
  otherbarrier:=WhoIs(roleBarrierInterceptor);
  with RobotInfo[num] do begin
    with TaskPars[num] do begin

      StopDistance:=2;
      if  (otherbarrier<>-1) then begin
          offset := BarrierAngle;
      end else begin
         if BallState.y<1 then begin
           offset := -BarrierAngle;
         end else if BallState.y>1 then begin
           offset := BarrierAngle;
         end else begin
           offset := BarrierAngle;
         end;
      end;
         GetBarrierPosition(offset, x1, y1, teta);
         speed := SpeedMax;
         avoid_is_set:=false;
         deltadist:=0.09;
         deltateta:=5;
         task:=taskGoToGoodPosition;
     end;
    end;

end;

procedure roleBarrierInterceptorRules(num: integer);
var offset: double;
begin
  with RobotInfo[num] do begin
    with TaskPars[num] do begin
      StopDistance:=2;
      offset := -BarrierAngle;
      GetBarrierPosition(offset, x1, y1, teta);
      speed := SpeedMax ;
      avoid_is_set:=false;
    end;
    deltadist:=0.09;
    deltateta:=5;
    task:=taskGoToGoodPosition;
  end;
end;

procedure RoleAtacker2Rules(num: integer);
begin
  with RobotInfo[num] do begin
    task:=taskGoToGoodPosition;
  end
end;

procedure RoleDefend3Rules(num: integer);
begin
  RobotInfo[num].task:=taskDefendPass;
end;

procedure RoleUnderStressRules(num: integer);
begin
  RobotInfo[num].task:=taskUnderStress;
end;

procedure RoleWingmanRRules(num: integer);
begin
  RobotInfo[num].task:=taskWingman;
end;

procedure RoleWingmanLRules(num: integer);
begin
  RobotInfo[num].task:=taskWingman;
end;

//Tese Tiago

procedure RoleTestRules(num: integer);
var xaux,yaux,val,X1Loc,Y1Loc,X2Loc,Y2Loc,X3Loc,Y3Loc:double;
begin
   //TaskPars[num].teta:=0;
   TaskPars[num].speed:=2;
   val:=0.2;

   X1Loc:=2.5;
   Y1Loc:=-2.3;
   X2Loc:=4.3;
   Y2Loc:=-2.3;
   X3Loc:=4.3;
   Y3Loc:=-1.4;

   with RobotInfo[num], TaskPars[num] do begin
     with TaskPars[num] do begin
       if (((xold<>X1loc)and(yold<>Y1loc))and((xold<>X2loc)and(yold<>Y2loc))) then begin
         x1 := X1Loc;
         y1 := Y1Loc;
         teta:=0;
         if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
             ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
           xold:=X1loc;
           yold:=Y1loc;
         end;
       end else if ((xold=X1loc)and(yold=Y1loc)) then begin
         x1 := X2loc;
         y1 := Y2loc;
         teta:=0;
         if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
             ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
           xold:=X2loc;
           yold:=Y2loc;
         end;
       end else if ((xold=X2loc)and(yold=Y2loc)) then begin
         x1 := X3loc;
         y1 := Y3loc;
         teta:=90*pi/180;
         //teta:=0;
         if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
             ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
           xold:=X3loc;
           yold:=Y3loc;
         end;
       end;
     end;
     deltateta:=1;
     task:=taskGoToGoodPosition;
   end;
end;

procedure RoleDoFormationRules(num: integer);
begin

  with TaskPars[num] do begin
      speed := staticSpeed;
      //speed := strtofloatDef(FormMPC.EditVMax.Text,2);
  end;

  with RobotInfo[num] do begin
    if FormationSettings.active then
       task:=taskDoFormation
    else
       task := taskIdle
  end;

end;

procedure RoleGoSearchFollowerRules(num: integer);
begin

  with TaskPars[num] do begin
      speed := staticSpeed;
      //speed := strtofloatDef(FormMPC.EditVMax.Text,2);
  end;

  with RobotInfo[num] do begin
    if FormationSettings.active then
       task:=taskDoFormationFollower
    else
       task := taskIdle
  end;


end;

procedure RoleGoSearchRules(num: integer);
var X1Loc,Y1Loc,X2Loc,Y2Loc,X3Loc,Y3Loc,X4Loc,Y4Loc,val:double;
begin
   if (FMain.CBDrone.Checked=false) then begin

       X1Loc:=FieldDims.FieldDepth/4;
       Y1Loc:=-FieldDims.FieldWidth/4;
       X2Loc:=-FieldDims.FieldDepth/4;
       Y2Loc:=-FieldDims.FieldWidth/4;
       X3Loc:=-FieldDims.FieldDepth/4;
       Y3Loc:=FieldDims.FieldWidth/4;
       X4Loc:=FieldDims.FieldDepth/4;
       Y4Loc:=FieldDims.FieldWidth/4;

       TaskPars[num].speed:=1;
       val:=0.2;
       with RobotInfo[num], TaskPars[num] do begin
         with TaskPars[num] do begin
           if (((xold<>X1loc)and(yold<>Y1loc))and((xold<>X2loc)and(yold<>Y2loc))and((xold<>X3loc)and(yold<>Y3loc)))or
              ((xold=X4loc)and(yold=Y4loc)) then begin
             x1 := X1Loc;
             y1 := Y1Loc;
             teta:=270*pi/180;
             if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
                 ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
               xold:=X1loc;
               yold:=Y1loc;
             end;
           end else if ((xold=X1loc)and(yold=Y1loc)) then begin
             x1 := X2loc;
             y1 := Y2loc;
             teta:=180*pi/180;
             if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
                 ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
               xold:=X2loc;
               yold:=Y2loc;
             end;
           end else if ((xold=X2loc)and(yold=Y2loc)) then begin
             x1 := X3loc;
             y1 := Y3loc;
             teta:=90*pi/180;
             if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
                 ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
               xold:=X3loc;
               yold:=Y3loc;
             end;
           end else if ((xold=X3loc)and(yold=Y3loc)) then begin
             x1 := X4loc;
             y1 := Y4loc;
             teta:=0;
             if (((RobotState[num].x)<x1+val)and((RobotState[num].x)>x1-val)and
                 ((RobotState[num].y)<y1+val)and((RobotState[num].y)>y1-val)) then begin
               xold:=X4loc;
               yold:=Y4loc;
             end;
           end;
         end;
         deltateta:=1;
         task:=taskGoToGoodPosition;
       end;
   end else begin

       TaskPars[num].speed:=0.7;
       with RobotInfo[num], TaskPars[num] do begin
         deltateta:=1;
         task:=taskTest;
         if RobotState[num].teta>2*pi then RobotState[num].teta:=RobotState[num].teta-(2*pi);
       end;
   end;
end;

procedure RoleWaitForFreeKickRules(num: integer);
begin
   //RobotInfo[num].task:=taskBehindBall;
   CommonFreeKickRules(num, true);
    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      RobotInfo[num].task:=taskIdle;
    end;
end;

procedure RoleWaitToThrowInRules(num: integer);
begin
   //RobotInfo[num].task:=taskBehindBall;
   CommonFreeKickRules(num, true);
    if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
      RobotInfo[num].task:=taskIdle;
    end;
end;
procedure RoleThrowInRules(num: integer);
begin
  //RobotInfo[num].task := taskScoreFreeKick;
  CommonFreeKickRules(num, false);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;
procedure RoleWaitForCornerKickRules(num: integer);
begin
   CommonCornerKickRules(num, true);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;
procedure RoleCornerKickRules(num: integer);
begin
  CommonCornerKickRules(num, false);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;
procedure RoleWaitOurGoalLickRules(num: integer);
begin
  CommonGoalKickRules(num, true);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;
procedure RoleOurGoalLickRules(num: integer);
begin
  CommonGoalKickRules(num, false);
  if ((abs(BallState.x)>FieldDims.FieldDepth/2) or (abs(BallState.y)>FieldDims.FieldWidth/2)) then begin
    RobotInfo[num].task:=taskIdle;
  end;
end;

procedure RoleWaitDroppedBallRules(num: integer);
var ang,vx,vy,vnorm: double;
begin
  with RobotInfo[num] do begin
    with TaskPars[num] do begin
      StopDistance:=ReceiverDistance-1;
      teta := RobotCalcData[num].ball_teta;
      vx:=FieldDims.FieldDepth/2-BallState.x;
      vy:=0-BallState.y;
      //
      NormalizeVector(vx,vy);
      //
      ang:=ATan2(vy,vx);
      x1:=BallState.x-cos(ang);
      y1:=BallState.y-sin(ang);
      speed := SpeedMax;
    end;
    task:=taskGoToGoodPosition;
  end;
end;


end.
