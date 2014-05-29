unit Robots;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Tactic,
     Math, Types, dynmatrix, Main, DecConsts, Tasks, Roles, obsavoid, LogCompat;


const
   RobotDrawingVertex: array[0..13] of TPos=(
      (x:-0.21; y:0.065), (x:0.1; y:0.24), (x:0.21; y:0.175),
      (x:0.21; y:0.125),  (x:0.26; y:0.125), (x:0.26; y:0.1), (x:0.21; y:0.1),
      (x:0.21; y:-0.1),  (x:0.26; y:-0.1), (x:0.26; y:-0.125), (x:0.21; y:-0.125),
      (x:0.21; y:-0.175), (x:0.1; y:-0.24), (x:-0.21; y:-0.065));


type
  TRobotStatus = record
    active: boolean;
    kicker_operational: boolean;
    roller_operational: boolean;
    degree_offset: double;

    default_role: TRole;
    start_role: integer;

    manual_control: boolean;
    v1: integer;
    v2: integer;
    v3: integer;
    roller_speed: integer;
    kick_pulse: integer;
    kick_type: integer;

    // this field is filled with "kick_pulse" when the button "kick now" is pressed
    // and cleared on SendRadioPacket
    kick_command: integer;
  end;


var
  RobotStatus: array[0..MaxRobots-1] of TRobotStatus;


procedure PropagateXYTetaBOA(var RS: TRobotstate);
procedure PropagateXYTeta(var RS: TRobotstate);
procedure PropagateXYTetaOthers(var RS: TRobotstate);
procedure RobotStateDoublesToMatrix(var Robot: TRobotState);
procedure RobotStateMatrixToDoubles(var Robot: TRobotState);

procedure SaturateRobotXY(var RS: TRobotstate);

procedure DrawRobotText(var RS: TRobotState; extraColor: Tcolor; CNV: TCanvas);
procedure DrawRobot(var RS: TRobotState; extraColor: Tcolor; CNV: TCanvas);
procedure DrawRobotInfo(var RS: TRobotState; var RI: TRobotInfo; CNV: TCanvas);
procedure DrawTraj(var Traj: TTrajectory; CNV: TCanvas);


implementation

uses Utils, Param, Field, Actions, LocMap;


procedure RobotStateMatrixToDoubles(var Robot: TRobotState);
begin
  with Robot do begin
    x:=Xk.getv(0,0);
    y:=Xk.getv(1,0);
    teta:=Xk.getv(2,0);

    cov_x:=Pk.getv(0,0);
    cov_y:=Pk.getv(1,1);
    cov_xy:=0.5*(Pk.getv(0,1)+Pk.getv(1,0));
    cov_teta:=Pk.getv(2,2);
  end;
end;

procedure RobotStateDoublesToMatrix(var Robot: TRobotState);
begin
  with Robot do begin
    Xk.setv(0,0,x);
    Xk.setv(1,0,y);
    Xk.setv(2,0,teta);

    Pk.setv(0,0,cov_x);
    Pk.setv(1,1,cov_y);
    Pk.setv(0,1,cov_xy);
    Pk.setv(1,0,cov_xy);
    Pk.setv(2,2,cov_teta);
  end;
end;

procedure PropagateXYTetaBOA(var RS: TRobotstate);
var i: integer;
    act_v,act_vn,act_w: double;
    dv, dvn,dteta, dt:double;
    cov_pos_noise,cov_teta_noise: double;
    ce,se, cde, sde:double;
    dx,dy: double;
begin
  with RS do begin
    v:=0.5774*View.Odos[0].speedw[0]*(Wheel1Diameter/2)-0.5774*View.Odos[0].speedw[1]*(Wheel1Diameter/2);
    vn:=0.3333*View.Odos[0].speedw[0]*(Wheel1Diameter/2)+0.3333*View.Odos[0].speedw[1]*(Wheel1Diameter/2)-0.6667*View.Odos[0].speedw[2]*(Wheel1Diameter/2);
    w:=(0.3333/wheeltoCenterdist)*View.Odos[0].speedw[0]*(Wheel1Diameter/2)+(0.3333/wheeltoCenterdist)*View.Odos[0].speedw[1]*(Wheel1Diameter/2)+(0.3333/wheeltoCenterdist)*View.Odos[0].speedw[2]*(Wheel1Diameter/2);

    VVnToVxy(teta, v,vn, vx, vy);
  end;
end;


procedure PropagateXYTeta(var RS: TRobotstate);
var i: integer;
    act_v,act_vn,act_w: double;
    dv, dvn,dteta, dt:double;
    cov_pos_noise,cov_teta_noise: double;
    ce,se, cde, sde:double;
    dx,dy: double;
begin
(* inverse matrix
 [ 0.5774 -0.5774 0
  0.3333  0.3333 -0.6667
  0.3333/wheeltoCenterdist  0.3333/wheeltoCenterdist 0.3333/wheeltoCenterdist ]
*)

  act_v:=0;
  act_vn:=0;
  act_w:=0;
  dt:=0;
  cov_pos_noise:=1e-3;
  cov_teta_noise:=1e-6;

  with RS do begin
    Phik:=MEye(3);
    Qk.SetSize(3,3);

    dv:=0.5774*View.Odos[0].dwheel[0] -0.5774*View.Odos[0].dwheel[1];
    dvn:= 0.3333*View.Odos[0].dwheel[0] + 0.3333*View.Odos[0].dwheel[1] -0.6667*View.Odos[0].dwheel[2];
    dteta:= (0.3333/wheeltoCenterdist)*View.Odos[0].dwheel[0] + (0.3333/wheeltoCenterdist)*View.Odos[0].dwheel[1] + (0.3333/wheeltoCenterdist)*View.Odos[0].dwheel[2];


    NormalizeAngle(dteta);
    ce:=cos(teta);
    se:=sin(teta);
    cde:=cos(dteta);
    sde:=sin(dteta);


    if abs(dteta)=0 then begin
      x:=x+(dv*ce-dvn*se);
      y:=y+(dv*se+dvn*ce);
    end else begin
      x:=x+(dv*sde+dvn*(cde-1))*cos(teta+dteta/2)/dteta-(dv*(1-cde)+dvn*sde)*sin(teta+dteta/2)/dteta;
      y:=y+(dv*sde+dvn*(cde-1))*sin(teta+dteta/2)/dteta+(dv*(1-cde)+dvn*sde)*cos(teta+dteta/2)/dteta;
      teta:=NormalizeAngle(teta+dteta);
    end;

    View.dOdos.x:=x-dx;
    View.DOdos.y:=y-dy;
    View.DOdos.teta:=dteta;

    act_v:=act_v+dv;
    act_vn:=act_vn+dvn;
    act_w:=act_w+dteta;
    dt:=dt+View.Odos[0].count/10000;
    RobotStateDoublesToMatrix(RS);

    SaturateRobotXY(RS);
    // TODO (ainda não entra em conta com vnt)

    Phik.setv(0,2,-act_v*se*0.04);
    Phik.setv(1,2,act_v*ce*0.04);
    Qk.setv(0,0,cov_pos_noise);
    Qk.setv(1,1,cov_pos_noise);
    Qk.setv(2,2,cov_teta_noise);
    Pk:=Qk + (Phik* Pk * MTran(Phik));
    RobotStateMatrixToDoubles(RS);

    v:=act_v;
    vn:=act_vn;
    w:=act_w;
    //temp code!!!!!!!!! Remendo!!!!!
    v:=0.5774*View.Odos[0].speedw[0]*(Wheel1Diameter/2)-0.5774*View.Odos[0].speedw[1]*(Wheel1Diameter/2);
    vn:=0.3333*View.Odos[0].speedw[0]*(Wheel1Diameter/2)+0.3333*View.Odos[0].speedw[1]*(Wheel1Diameter/2)-0.6667*View.Odos[0].speedw[2]*(Wheel1Diameter/2);
    w:=(0.3333/wheeltoCenterdist)*View.Odos[0].speedw[0]*(Wheel1Diameter/2)+(0.3333/wheeltoCenterdist)*View.Odos[0].speedw[1]*(Wheel1Diameter/2)+(0.3333/wheeltoCenterdist)*View.Odos[0].speedw[2]*(Wheel1Diameter/2);


    VVnToVxy(teta, v,vn, vx, vy);

    RobotStateDoublesToMatrix(RS);
  end;
end;


procedure PropagateXYTetaOthers(var RS: TRobotstate);
var cov_xy_noise,cov_teta_noise: double;
begin
  cov_xy_noise:=0.01;
  cov_teta_noise:=0.001;
  // TODO
  with RS do begin
    x:=x+0.04*v*cos(teta);
    y:=y+0.04*v*sin(teta);
    teta:=teta+0.04*w*pi/180;
    cov_x:=cov_x+cov_xy_noise;
    cov_y:=cov_y+cov_xy_noise;
    cov_teta:=cov_teta+cov_teta_noise;
  end;
end;

procedure SaturateRobotXY(var RS: TRobotstate);
begin
  with RS do begin
    if x>FieldDims.BoundaryDepth/2 then x:=FieldDims.BoundaryDepth/2;
    if x<-FieldDims.BoundaryDepth/2 then x:=-FieldDims.BoundaryDepth/2;
    if y>FieldDims.BoundaryWidth/2 then y:=FieldDims.BoundaryWidth/2;
    if y<-FieldDims.BoundaryWidth/2 then y:=-FieldDims.BoundaryWidth/2;
  end;
  RobotStateDoublesToMatrix(RS);
end;


procedure DrawRobot(var RS: TRobotState; extraColor: Tcolor; CNV: TCanvas);
var i,x1,y1: integer;
    xr,yr: double;
    Pts: array[Low(RobotDrawingVertex)..High(RobotDrawingVertex)] of TPoint;
begin
  with CNV do begin //tcanvas
    pen.color:=clblack;
    brush.color:=clBlack;
    brush.style:=bsSolid;

    for i:=Low(RobotDrawingVertex) to High(RobotDrawingVertex) do begin
      RotateAndTranslate(xr,yr,RobotDrawingVertex[i].x*1.25,RobotDrawingVertex[i].y*1.25,RS.x,RS.y,RS.teta);
      WorldToMap(xr,yr,Pts[i].x,Pts[i].y);
    end;
    Polygon(Pts);

    brush.color:=clgreen;
    brush.style:=bsClear;

    RotateAndTranslate(xr,yr,-0.08*1.25,0,RS.x,RS.y,RS.teta);
    WorldToMap(xr,yr,x1,y1);
    font.Color:=CTeamColorColor24[TeamColor];
    TextOut(x1-2,y1-5,inttostr(RS.num+1));

    if RS.num=myNumber then begin
      DrawCovElipse(RS.x,RS.y,RS.cov_x,RS.cov_y,RS.cov_xy,10,CNV);
    end;
  end;
end;

procedure DrawRobotText(var RS: TRobotState; extraColor: Tcolor; CNV: TCanvas);
var i,x1,y1: integer;
    xr,yr: double;
    Pts: array[Low(RobotDrawingVertex)..High(RobotDrawingVertex)] of TPoint;
begin
  with CNV do begin //tcanvas
    pen.color:=clblack;
    brush.color:=clBlack;
    brush.style:=bsSolid;

    for i:=Low(RobotDrawingVertex) to High(RobotDrawingVertex) do begin
      RotateAndTranslate(xr,yr,RobotDrawingVertex[i].x*1.25,RobotDrawingVertex[i].y*1.25,RS.x,RS.y,RS.teta);
      WorldToMap(xr,yr,Pts[i].x,Pts[i].y);
    end;
    Polygon(Pts);

    brush.color:=clgreen;
    brush.style:=bsClear;

    RotateAndTranslate(xr,yr,-0.08*1.25,0,RS.x,RS.y,RS.teta);
    WorldToMap(xr,yr,x1,y1);
    font.Color:=CTeamColorColor24[TeamColor];
    TextOut(x1-2,y1-5,inttostr(RS.num+1));

    if RS.num=myNumber then begin
      font.Color:=clWhite;
      TextOut(x1-20,y1-20,format('%.1f,%.1f,%d',[RS.x,RS.Y,round(RS.teta/(2*pi)*360)]));
      DrawCovElipse(RS.x,RS.y,RS.cov_x,RS.cov_y,RS.cov_xy,10,CNV);
    end;
  end;
end;



procedure DrawRobotInfo(var RS: TRobotState; var RI: TRobotInfo; CNV: TCanvas);
var i,x1,y1: integer;
    xr,yr: double;
    Pts: array[Low(RobotDrawingVertex)..High(RobotDrawingVertex)] of TPoint;
begin
  with CNV do begin //tcanvas

    font.Color:=clWhite;

    RotateAndTranslate(xr,yr,0,0,RS.x,RS.y,RS.teta);
    WorldToMap(xr,yr,x1,y1);

    TextOut(x1-14,y1-23,RoleDefs[RI.role].name);
  end;
end;

procedure DrawTraj(var Traj: TTrajectory; CNV: TCanvas);
var i,x1,y1: integer;
begin
  with CNV do begin //tcanvas
    pen.color:=clyellow;
    brush.color:=clBlack;
    brush.style:=bsSolid;
    
    WorldToMap(RobotState[myNumber].x,RobotState[myNumber].y,x1,y1);
    moveto(x1,y1);
    for i := 0 to traj.count-1 do begin
      with traj.pts[i] do begin
        WorldToMap(x,y,x1,y1);
        lineto(x1,y1);
      end;
    end;
  end;
end;


end.
