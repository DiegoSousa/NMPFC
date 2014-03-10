unit Roles;

{$mode objfpc}{$H+}

interface

uses DecConsts,Field;

type
  TRoleFunc=procedure(num: integer);

//TODO:TAC create a new role for a second atacker to receive a pass at corner kicks

type
  TRole=(roleIdle,
         roleKeeper, roleKeeperDefendPenalty, roleKeeperPassive,
         roleAtackerKicker, roleAtackerReceiver, roleReceivePass,
         roleMamao, roleGoSearch1, roleGoSearch2, roleGoSearch3, roleGoSearch4,
         roleAtackerSupporter,
         roleLastDefender, roleOtherDefender, roleRoundLastDefender, roleRoundOtherDefender,
         roleFreeKick,
         roleKickOff,
         rolePenaltyKick, roleScorePenaltyKick,
         roleStayBack,
         roleBarrierMain, roleBarrierAux, roleBarrierInterceptor,
         roleWaitForKickOff,
         roleRobotRobotDefender, roleDefend3,
         roleTest, roleWingmanL, roleWingmanR ,
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
procedure roleBarrierMainRules(num: integer);
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
    ( name:'roleGoSearch1';           func: @roleGoSearchRules1 ),
    ( name:'roleGoSearch2';           func: @roleGoSearchRules2 ),
    ( name:'roleGoSearch3';           func: @roleGoSearchRules3 ),
    ( name:'roleGoSearch4';           func: @roleGoSearchRules4 ),
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
    ( name:'roleTest';                func: @roleTestRules ),
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

    //TIAGO TESE
    ( name:'roleGoSearchFollower';           func: @roleGoSearchFollowerRules ),
    ( name:'roleGoSearch';           func: @roleGoSearchRules ),
    ( name:'roleDoFormation'; func: @RoleDoFormationRules)
  );

implementation

uses Coach, Tactic, Param, Utils{, Analyser}, Math, ObsAvoid;//, Unit_RolesAux;

//----------------------------------------------------------------------
//  Role Rules
//----------------------------------------------------------------------

procedure RoleIdleRules(num: integer);
begin

end;

function BallNearCorner(c: double): boolean;
var ang: double;
begin
  ang:=ATan2(Abs(BallState.y)-(-0.25),BallState.x-(-FieldLength*0.5));
  result:=(Abs(BallState.y)>0.48-0.1*c) and (not (ang<0)) and (ang>(Pi/2-(30*Pi/180)*c));
end;

procedure RoleGoPosStartRules(num: integer);
begin

end;

procedure RoleGoPosStopRules(num: integer);
begin

end;

procedure RoleKeeperRules(num: integer);
begin

end;

procedure RoleKeeperDefendPenaltyRules(num: integer);
begin

end;

procedure RoleKeeperPassiveRules(num: integer);
begin

end;

procedure roleAtackerKicker2Rules(num: integer);
begin

end;

procedure roleAtackerKickerRules(num: integer);
begin

end;

procedure roleAtackerReceiverRules(num: integer);
begin

end;

procedure roleReceivePassRules(num: integer);
begin

end;

procedure roleLastDefenderRules(num: integer);
begin

end;

procedure roleOtherDefenderRules(num: integer);
begin

end;

procedure RoleRoundLastDefenderRules(num: integer);
begin

end;

procedure RoleRoundOtherDefenderRules(num: integer);
begin
end;

procedure RoleFreeKickRules(num: integer);
begin

end;

procedure RoleWaitForKickOffRules(num: integer);
begin

end;

procedure roleRobotRobotDefenderRules(num: integer);
begin

end;

procedure RoleScorePenaltyKickRules(num: integer);
begin

end;

procedure RolePenaltyKickRules(num: integer);
begin

end;

procedure RoleStayBackRules(num: integer);
begin

end;

procedure RoleMamaoRules(num: integer);
begin

end;

procedure RoleGoSearchRules1(num: integer);
begin

end;

procedure RoleGoSearchRules2(num: integer);
begin

end;

procedure RoleGoSearchRules3(num: integer);
begin

end;

procedure RoleGoSearchRules4(num: integer);
begin

end;

procedure RoleMamao2Rules(num: integer);
begin

end;

procedure RoleMidfieldRules(num: integer);
begin

end;

procedure RoleMidOfMidfieldRules(num: integer);
begin

end;

procedure RoleMidOfMidfieldRightRules(num: integer);
begin

end;

procedure RoleMidOfMidfieldLeftRules(num: integer);
begin

end;

procedure RoleNavigateToLocalizeRules(num: integer);
begin

end;

procedure RoleFurtiveAtackerKickerRules(num: integer);
begin

end;

procedure RoleAtackerSupporterRules(num: integer);
begin

end;

procedure RoleKickOffRules(num: integer);
begin

end;

procedure roleBarrierMainRules(num: integer);
begin

end;

procedure roleBarrierAuxRules(num: integer);
begin

end;

procedure roleBarrierInterceptorRules(num: integer);
begin

end;

procedure RoleAtacker2Rules(num: integer);
begin

end;

procedure RoleDefend3Rules(num: integer);
begin

end;

procedure RoleUnderStressRules(num: integer);
begin

end;

procedure RoleWingmanRRules(num: integer);
begin

end;

procedure RoleWingmanLRules(num: integer);
begin

end;

//Tiago Tese

procedure RoleTestRules(num: integer);
begin

end;

procedure RoleGoSearchFollowerRules(num: integer);
begin

end;

procedure RoleGoSearchRules(num: integer);
begin

end;

procedure RoleDoFormationRules(num: integer);
begin

end;
/////// End Tiago

procedure RoleWaitForFreeKickRules(num: integer);
begin

end;

procedure RoleWaitToThrowInRules(num: integer);
begin

end;

procedure RoleThrowInRules(num: integer);
begin

end;
procedure RoleWaitForCornerKickRules(num: integer);

begin

end;

procedure RoleCornerKickRules(num: integer);
begin

end;

procedure RoleWaitOurGoalLickRules(num: integer);
begin

end;

procedure RoleOurGoalLickRules(num: integer);
begin

end;

procedure RoleWaitDroppedBallRules(num: integer);
begin

end;

end.