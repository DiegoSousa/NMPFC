unit houghTransform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,math,unit_localizationAux;
const
HoughRho=125;
HoughTheta=90;
rsRho=0.04;
rsTheta=1;

type
  TXY = record
  x,y:double;
  rho,theta:double;
  idTheta,idRho:integer;
  val:integer;
  end;

  { THough }
  THough = class
  private
   procedure FillAtheta;
   procedure FillRho;
   procedure CreateHoughMatrices;
   procedure ClearH;
   function GetOccuranceGrid(r, theta:integer): integer;
   procedure incOccurance(r, theta: integer);
   procedure SetOccuranceGrid(r, theta,val: integer);
   procedure decOccurance(r, theta: integer);
  public
   lowPassHoughRho:TLowPassFilter;
   lowPassHoughTheta:TLowPassCompass;
   H:array [1..2*HoughRho,1..2*HoughTheta] of integer;
   Mtheta:array [1..2*HoughTheta] of double;
   MRho:array [1..2*HoughRho] of double;
   thetaMax,RhoMax:integer;
   resolTheta,resolRho:double;
   constructor Create;
   procedure HoughTransform(points:TPointList);
   function GetHoughMax:TXY;
  end;

implementation
uses unit_Localization;
{ THough }

procedure THough.FillAtheta;
var i:integer;
begin
   for i:=1 to thetaMax do
    Mtheta[i]:=degtorad(((i-1)-(thetaMax div 2))*resolTheta);
end;

procedure THough.FillRho;
var i:integer;
begin
 for i:=1 to RhoMax do
  MRho[i]:=((i-1)-(RhoMax div 2))*resolRho;
end;

procedure THough.ClearH;
begin
   FillChar(H,SizeOf(H),0);
end;

procedure THough.CreateHoughMatrices;
begin
  thetaMax:=round(2/resolTheta*90);
  RhoMax:=round(2/resolRho*5);
  FillAtheta;
  FillRho;
  ClearH;
end;

constructor THough.Create;
begin
 lowPassHoughRho:=TLowPassFilter.Create(0.5,0.04,0);
 lowPassHoughTheta:=TLowPassCompass.Create(0.5,0.04,0);
 resolTheta:=rsTheta;
 resolRho:=rsRho;
 CreateHoughMatrices;
end;

function THough.GetOccuranceGrid(r, theta: integer): integer;
begin
 result:=H[r,theta];
end;

procedure THough.incOccurance(r, theta: integer);
begin
 H[r,theta]:=H[r,theta]+1;
end;

procedure THough.SetOccuranceGrid(r, theta, val: integer);
begin
 H[r,theta]:=val;
end;

procedure THough.decOccurance(r, theta: integer);
begin
   H[r,theta]:=H[r,theta]+1;
end;

procedure THough.HoughTransform(points: TPointList);
var x,y,ang,r:double;
    angTheta,i:integer;
begin
  ClearH;
  for i:=0 to points.PCount do begin
    x:=points.PList[i].x;
    y:=points.PList[i].y;
    for angTheta:=1 to thetaMax do begin
      ang:=Mtheta[angTheta];
      r:=(x*cos(ang)+y*sin(ang))/resolRho;
      if ((round(r+0.5+(RhoMax div 2))>=1) and (round(r+(RhoMax div 2))<=RhoMax))then
       incOccurance(round(r+0.5+(RhoMax div 2)),angTheta);
    end;
  end;
end;

function THough.GetHoughMax: TXY;
var r,angTheta:integer;
    xy:TXY;
begin
   xy.val:=-1;
   for r:=1 to RhoMax do begin
    for angTheta:=1 to thetaMax do begin
      if H[r,angTheta]>xy.val then begin
         xy.val:=H[r,angTheta];
         xy.idRho:=r;
         xy.idTheta:=angTheta;
         xy.rho:=MRho[r];
         xy.theta:=Mtheta[angTheta];
      end;
     end;
   end;
   Result:=xy;
end;


end.

