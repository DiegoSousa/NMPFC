//------------------------------------------------------------------------------
//
// getTraj.pas
//
//------------------------------------------------------------------------------
//
// Contains procedures for generating trajectories for the robot. Trajectories
// are saved to a TTrajectory object passed as reference.
//
//  Program Objectives:
// - Implement the following trajectories:
// 1. Hook
// 2. Gate (90Deg4Corners)
//
// - Save trajectory in a file
// - Generate trajectory from a file
// - Calc Distance
//------------------------------------------------------------------------------

unit genTraj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, obsavoid, Utils, Math;

  procedure calcDistance(var traj : TTrajectory);
  procedure saveTrajToFile(var traj:TTrajectory; fileName:string);

  //Procedures for trajectory generation

  procedure generateTrajectory90Deg4Corners(var traj: TTrajectory; x_start, y_start, L1, L2, L3, L4, L5 : double; varTeta : boolean);
  procedure generateTrajectoryHook(var traj: TTrajectory; x_i,y_i,x_f,y_f,circleRadius,circleLength,currAngle,v_ini,v_fin : double);
  procedure generateTrajectoryLoadFromFile(var traj: TTrajectory; fileName : string);

  const

       //distance between points in the trajectory
       trajStep = 0.2;

implementation


  //*************************************************************
  // generateTrajectory90Deg4Corners
  //
  // Generates a pulse trajectory
  //-------------------------------------------------------------
  procedure generateTrajectory90Deg4Corners(var traj: TTrajectory; x_start, y_start, L1, L2, L3, L4, L5 : double; varTeta : boolean);
  var
     i,cornerPoint : integer;
     cornerPoint1, cornerPoint2, cornerPoint3, cornerPoint4, cornerPoint5 : integer;
     tetaRef : array[0 .. 5] of double;
  begin

     if varTeta then begin
       //define teta for teta changes
       tetaRef[0] := 0;
       tetaRef[1] := pi/2;
       tetaRef[2] := 0;
       tetaRef[3] := -pi/2;
       tetaRef[4] := 0;
     end
     else begin
       tetaRef[0] := 0;
       tetaRef[1] := 0;
       tetaRef[2] := 0;
       tetaRef[3] := 0;
       tetaRef[4] := 0;
     end;


     traj.count:=MaxTrajectoryCount;
     traj.static := true;
     traj.index:=0;

     cornerPoint1 := round(abs(L1)/trajStep);
     cornerPoint2 := cornerPoint1 + round(abs(L2)/trajStep);
     cornerPoint3 := cornerPoint2 + round(abs(L3)/trajStep);
     cornerPoint4 := cornerPoint3 + round(abs(L4)/trajStep);
     cornerPoint5 := cornerPoint4 + round(abs(L5)/trajStep);

     traj.pts[0].x := x_start;
     traj.pts[0].y := y_start;

     for i:= 1 to cornerPoint1 do begin
         if (i <= MaxTrajectoryCount-1) then begin
            with traj.pts[i] do begin
              x := traj.pts[i-1].x + Sign(L1)*trajStep;
              y := y_start;
              teta := tetaRef[0];
              teta_power:= 1;
            end;
         end else begin
             calcDistance(traj);
             exit;
         end;
     end;


     for i:= cornerPoint1+1 to cornerPoint2 do begin
         if (i <= MaxTrajectoryCount-1) then begin
            with traj.pts[i] do begin
              x := traj.pts[i-1].x;
              y := traj.pts[i-1].y + Sign(L2)*trajStep;
              teta := tetaRef[1];
              teta_power:= 1;
            end;
         end else begin
             calcDistance(traj);
             exit;
         end;
     end;

   for i:= cornerPoint2+1 to cornerPoint3 do begin
       if (i <= MaxTrajectoryCount-1) then begin
         with traj.pts[i] do begin
              x := traj.pts[i-1].x + Sign(L3)*trajStep;
              y := traj.pts[i-1].y;
              teta := tetaRef[2];
              teta_power:= 1;
         end;
       end else begin
             calcDistance(traj);
             exit;
         end;
     end;

     for i:= cornerPoint3+1 to cornerPoint4 do begin
         if (i <= MaxTrajectoryCount-1) then begin
            with traj.pts[i] do begin
              x := traj.pts[i-1].x;
              y := traj.pts[i-1].y + Sign(L4)*trajStep;
              teta := tetaRef[3];
              teta_power:= 1;
            end;
         end else  begin
             calcDistance(traj);
             exit;
         end;
     end;

     for i:= cornerPoint4+1 to cornerPoint5 do begin
         if (i <= MaxTrajectoryCount-1) then begin
            with traj.pts[i] do begin
              x := traj.pts[i-1].x + Sign(L5)*trajStep;
              y := traj.pts[i-1].y;
              teta := tetaRef[4];
              teta_power:= 1;
            end;
         end else begin
             calcDistance(traj);
             exit;
         end;
     end;

     traj.count:=cornerPoint5 + 1;
     calcDistance(traj);


  end;


  //*************************************************************
  // generateTrajectoryHook
  //
  // Generates a trajectory in hook
  //-------------------------------------------------------------
  procedure generateTrajectoryHook(var traj: TTrajectory; x_i,y_i,x_f,y_f,circleRadius,circleLength,currAngle,v_ini,v_fin : double);
  var
     i : integer;
     i_limit, i_limit_circle : integer;
     circleStep, teta_inc, teta_init : double;
     length, angle : double;
     lineAngleStep : double;
     x_c,y_c : double;
     v_step : double;
  begin

     traj.static := true;
     traj.index:=0;

     //calc stuff
     length := sqrt(power((x_i-x_f),2)+power((y_i-y_f),2));
     i_limit := round(length/trajStep);
     angle := atan2(y_f-y_i,x_f-x_i);
     lineAngleStep :=  (angle + pi -currAngle)/i_limit;
     v_step:=(v_ini-v_fin)/i_limit;

     //fill line
     for i:= 0 to i_limit do begin
         with traj.pts[i] do begin
              x := x_i + trajStep*i*cos(angle);
              y := y_i + trajStep*i*sin(angle);
              teta := currAngle + i*lineAngleStep;
              teta_power:= 1;
              vRef:=v_ini;
         end;
     end;

     //add circle
     x_c := traj.pts[i].x + circleRadius*cos(angle + pi/2);
     y_c := traj.pts[i].y + circleRadius*sin(angle + pi/2);
     circleStep := (trajStep/10)/circleRadius;
     teta_init := angle - pi/2;
     teta_inc := teta_init;
     i+=1;


     while teta_inc <= teta_init+(2*pi*circleLength) do begin

            with traj.pts[i] do begin
                x := x_c + circleRadius*cos(teta_inc);
                y := y_c + circleRadius*sin(teta_inc);
                teta := angle + pi;
                teta_power:= 1;
                vRef:=v_fin;
            end;

            teta_inc += circleStep;
            i+=1;

     end;
     {
       //Generate circular trajectory starting in (x_center + radius, y_center)
       //and going counter-clockwise;
       for i := 0 to (MaxTrajectoryCount-1) do begin
           if (i <= MaxTrajectoryCount-1) and (teta_inc <= 2*pi*length)  then begin
              with traj.pts[i] do begin
                x := x_center + radius*cos(teta_inc);
                y := y_center + radius*sin(teta_inc);
                teta := teta_inc;
                teta_power:= 1;
                teta_inc += step;
              end;
           end else begin
               traj.count := i;
               calcDistance(traj);
               exit;
           end;
       end;
     }

     //calculate size
     traj.count:=i-1;
     calcDistance(traj);

  end;

  //*************************************************************
  // saveTrajToFile
  //
  // saves trajectory to text file
  //-------------------------------------------------------------
  procedure saveTrajToFile(var traj:TTrajectory; fileName:string);
  var
   trajFile : TextFile;
   trajText: string;
   index: integer;
  begin

  AssignFile(trajFile,fileName);
  Rewrite(trajFile);

  //WriteLn(trajFile,format('Count: %d',[traj.count]));
  for index := 0 to traj.count-1 do
  begin
       trajText := format('%.5f %.5f %.5f',[traj.pts[index].x,traj.pts[index].y,traj.pts[index].teta]);
           WriteLn(trajFile,trajText);
  end;

   CloseFile(trajFile);

end;

 //*************************************************************
  // generateTrajectoryLoadFromFile
  //
  // Loads trajectory from textfile
  //-------------------------------------------------------------
   procedure generateTrajectoryLoadFromFile(var traj: TTrajectory; fileName : string);
   var
      i: integer;
      line: string;
      trajFile : TextFile;
      sl : TStringList;
      x,y,teta:double;
   begin

        i:= 0;
        //Load file
        if FileExists(fileName) then
        begin
             Assign(trajFile,fileName);
             Reset(trajFile);
        end;

        //Load it into trajectory

        try
           while not EOF(trajFile) do BEGIN
             ReadLn(trajFile,x,y,teta);
             traj.pts[i].x:=x;
             traj.pts[i].y:=y;
             traj.pts[i].teta:=teta;
             i:=i+1;
           end;
        except
        end;

     traj.count:=i;
     traj.static:=true;
     traj.index :=0;
     traj.currentPoint:=0;
     calcDistance(traj);

   end;


  //*************************************************************
  // calcDistance
  //
  // calculates distance of currentPoint to end of trajectory
  // saves it onto traj.distance
  //-------------------------------------------------------------
  procedure calcDistance(var traj : TTrajectory);
  var
     i : integer;
  begin

       if traj.static then begin
          traj.distance := 0;
          for i := traj.currentPoint to traj.count-2 do
              traj.distance += dist(traj.pts[i+1].x-traj.pts[i].x,traj.pts[i+1].y-traj.pts[i].y);
       end;

  end;

end.

