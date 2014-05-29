unit Roles;

{$mode objfpc}{$H+}

interface

uses DecConsts,Field;

type
  TRoleFunc=procedure(num: integer);

type
  TRole=(roleIdle,
         roleTest,
         roleGoSearch,
         roleGoSearchFollower,
         roleFollowTrajectory,
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

type
 TRolePars=record
   isReady : boolean;    //Robot is at start of trajectory
   runningTraj: boolean; //Robot is running the trajectory
 end;

var
 RolePars: array[0..MaxRobots-1] of TRolePars;


procedure RoleIdleRules(num: integer);
procedure RoleTestRules(num: integer);
procedure RoleDoFormationRules(num: integer);
procedure RoleGoSearchRules(num: integer);
procedure RoleGoSearchFollowerRules(num: integer);
procedure RoleFollowTrajectoryRules(num:integer);

const
  RoleDefs: array[low(TRole) .. High(TRole)] of TRoleDef = (
    ( name:'roleIdle';                func: @RoleIdleRules ),
    ( name:'roleTest';                func: @roleTestRules ),
    ( name:'roleGoSearchFollower';           func: @roleGoSearchFollowerRules ),
    ( name:'roleGoSearch';           func: @roleGoSearchRules ),
    ( name:'roleFollowTrajectory';    func: @RoleFollowTrajectoryRules),
    ( name:'roleDoFormation'; func: @RoleDoFormationRules)
  );

implementation

uses Main, Tasks, Actions, Tactic, Param, Utils, Analyser, Math, ObsAvoid, MPC;

//----------------------------------------------------------------------
//  Role Rules
//----------------------------------------------------------------------

procedure RoleIdleRules(num: integer);
begin
  RobotInfo[num].task:=taskIdle;
end;

procedure RoleTestRules(num: integer);
var xaux,yaux,val,X1Loc,Y1Loc,X2Loc,Y2Loc,X3Loc,Y3Loc:double;
begin
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
         //task:=taskGoToGoodPosition;
         if FormationSettings.active then
             task:=taskGoToGoodPosition
         else
             task := taskIdle
       end;

end;

//-------------------------------------------------------------------------
// RoleFollowTrajectory
//
// Follows a given trajectory (staticTraj)
//-------------------------------------------------------------------------
procedure RoleFollowTrajectoryRules(num: integer);
var distFromStart: double;
begin

       with RobotInfo[num] do begin

            //Distance from start of trajectory
            distFromStart := dist((RobotState[num].x-staticTraj.pts[0].x),(RobotState[num].y-staticTraj.pts[0].y));

            //Check if it's at the start of the trajectory
            if(distFromStart < 0.1) then
                RolePars[num].isReady := true
            else
                RolePars[num].isReady := false;

            //Set tasks
            if(not RolePars[num].isReady) and (not RolePars[num].runningTraj) then begin

               //Not at the start of the trajectory, move there
               task:=taskGoToStartTrajectory;
               with TaskPars[num] do begin
                 x1 := staticTraj.pts[0].x;
                 y1 := staticTraj.pts[0].y;
                 teta := staticTraj.pts[0].teta;
                 speed := 1;
               end;

            end else if (not RolePars[num].runningTraj) then begin
                //Is at the start of trajectory, stop
                task:=taskIdle;
            end;

             //Follow Trajectory
             if (RolePars[num].runningTraj)  then begin
               task:=taskFollowTrajectory;

               with TaskPars[num] do begin
                 speed := staticSpeed;
               end;

               ActionPars[num].controllerIndex:=controllerIndex;

             end else
                 staticTraj.currentPoint := 0;    //reset current point of trajectory
            end;
end;

end.
