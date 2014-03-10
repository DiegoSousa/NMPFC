unit analyser;

interface

const
  ScanRays=36;
  ScanRobotRadius=0.09;
  ScanOponentRadius=0.09;

{type
  TKickScan=record
    robot_dist: array[0..ScanRays-1] of double;
    robot_present: array[0..ScanRays-1] of boolean;
    oponent_dist: array[0..ScanRays-1] of double;
    oponent_present: array[0..ScanRays-1] of boolean;

    cost: array[0..ScanRays-1] of double;

    angle_start, angle_end: double;
  end;}

//procedure KickScan(x,y,angle_start,angle_end: double; exclude_robot_mask: integer; var scan: TKickScan);
//procedure CalcPassCost(var scan: TKickScan);

//procedure FirstRobotInVector(x,y,teta: double; var rob_num: integer; var rob_dist: double);

implementation

uses Main, DecConsts, Utils, Math;

{procedure KickScan(x,y,angle_start,angle_end: double; exclude_robot_mask: integer; var scan: TKickScan);
var i,j: integer;
    ang,d,ang_delta,ang_scan,ang_inc: double;
begin
  scan.angle_start:=angle_start;
  scan.angle_end:=angle_end;

  for i:=0 to ScanRays-1 do begin
    scan.oponent_dist[i]:=1e6;
    scan.oponent_present[i]:=false;
    scan.robot_dist[i]:=1e6;
    scan.robot_present[i]:=false;
    scan.cost[i]:=0.0;
  end;

  ang_inc:=(angle_end-angle_start)/ScanRays;
  angle_start:=angle_start+ang_inc/2;

  for i:=0 to MaxOponents-1 do begin
    if OponentState[i].valid then begin
      ang:=ATan2(OponentState[i].x-x,OponentState[i].y-y);
      d:=Dist(OponentState[i].x-x,OponentState[i].y-y);
      if d<0.0001 then ang_delta:=pi else ang_delta:=arctan(ScanOponentRadius/d);

      ang_scan:=angle_start;
      for j:=0 to ScanRays-1 do begin
        if (ang_scan>ang-ang_delta) and (ang_scan<ang+ang_delta) and
           (d<scan.oponent_dist[j]) then begin
          scan.oponent_dist[j]:=d;
          scan.oponent_present[j]:=true;
        end;
      end;
    end;
  end;

  for i:=0 to MaxRobots-1 do begin
    if (RobotState[i].count>0) and (((exclude_robot_mask shr i) and 1)=0) then begin
      ang:=ATan2(RobotState[i].x-x,RobotState[i].y-y);
      d:=Dist(RobotState[i].x-x,RobotState[i].y-y);
      if d<0.0001 then ang_delta:=pi else ang_delta:=arctan(ScanRobotRadius/d);

      ang_scan:=angle_start;
      for j:=0 to ScanRays-1 do begin
        if (ang_scan>ang-ang_delta) and (ang_scan<ang+ang_delta) and
           (d<scan.robot_dist[j]) then begin
          scan.robot_dist[j]:=d;
          scan.robot_present[j]:=true;
        end;
      end;
    end;
  end;

end;

procedure CalcPassCost(var scan: TKickScan);
var i,iteration: integer;
begin

  with scan do begin

    for i:=0 to ScanRays-1 do begin
      cost[i]:=5;
      if oponent_present[i] then cost[i]:=Min(cost[i],oponent_dist[i]);
      if robot_present[i] then cost[i]:=Min(cost[i],robot_dist[i]);
    end;

    for iteration:=1 to 8 do begin
      for i:=0 to ScanRays-1 do begin
        if (not oponent_present[i]) and (not robot_present[i]) then begin
          if i=0 then cost[0]:=(cost[0]+cost[1]*2)/3
          else if i=ScanRays-1 then cost[i]:=(cost[i]+cost[i-1]*2)/3
          else cost[i]:=(cost[i-1]+cost[i]+cost[i+1])/3;
        end;
      end;
    end;

  end;

end;


procedure FirstRobotInVector(x,y,teta: double; var rob_num: integer; var rob_dist: double);
var i: integer;
    ang,d,ang_delta: double;
begin
  rob_dist:=10000;
  rob_num:=-1;
  for i:=0 to MaxOponents-1 do begin
    if OponentState[i].valid then begin
      ang:=ATan2(OponentState[i].x-x,OponentState[i].y-y);
      d:=Dist(OponentState[i].x-x,OponentState[i].y-y);
      if d<0.0001 then ang_delta:=pi else ang_delta:=arctan(ScanOponentRadius/d);

      if (teta>ang-ang_delta) and (teta<ang+ang_delta) and
         (d<rob_dist) then begin
        rob_dist:=d; rob_num:=-2-i;
      end;
    end;
  end;
  for i:=0 to MaxRobots-1 do begin
    if (RobotState[i].count>0) then begin
      ang:=ATan2(RobotState[i].x-x,RobotState[i].y-y);
      d:=Dist(RobotState[i].x-x,RobotState[i].y-y);
      if d>0.05 then begin
        ang_delta:=arctan(ScanRobotRadius/d);
        if (teta>ang-ang_delta) and (teta<ang+ang_delta) and
           (d<rob_dist) then begin
          rob_dist:=d; rob_num:=i;
        end;
      end;
    end;
  end;
end; }

end.
