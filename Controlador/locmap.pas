unit LocMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, localization, math, Spin;

type

  { TFLocMap }

  TFLocMap = class(TForm)
    CBMShowRobot: TCheckBox;
    CBMShow: TCheckBox;
    CBMShowSensorPoints: TCheckBox;
    CBMousePosition: TCheckBox;
    CBGlobalLocalization: TCheckBox;
    EditMapLoadName: TEdit;
    EditPesoLocalization: TEdit;
    ImageLocMap: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LVar: TLabel;
    LPos: TLabel;
    procedure CBMShowChange(Sender: TObject);
    procedure EditPesoLocalizationEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageLocMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageLocMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure ImageLocMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

  procedure DrawLocalizationData(var rp:TRLocState;var VList:TPointList);
  procedure LDrawPoint(rp:TRLocState; cor:TColor; size:integer);
  procedure LDrawListOfPoints(var rp:TRLocState; var PL: TPointList);
  procedure LDrawRobot(rp:TRLocState);
  procedure DrawCovariancia(rpos, c:localization.TPos; cor:TColor);

var
  FLocMap: TFLocMap;
  s:string;
  ltimebegin,timebegin: integer;
  MRobotPos:TPos;

implementation

uses Main, omni3;

{ TFLocMap }

procedure TFLocMap.ImageLocMapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 { if CBMousePosition.Checked then begin
    LPos.Visible:=true;
    LPos.Left:=round(x-LPos.Width*0.5);
    LPos.Top:=y+25;
    LPos.Caption:=Format('x=%2f; y=%2f',[(x-MapAreaW/2)*cell_size*imfactor,(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor]);
  end else begin
    LPos.Visible:=false;
  end;
  CBMousePosition.Caption:=Format('%2f; %2f',[(x-MapAreaW/2)*cell_size*imfactor,(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor]);
  CBMousePosition.Caption:=CBMousePosition.Caption+Format('-->Dist= %2f',[MapDist[round((abs(MapAreaH-y))*imfactor),round(X*imfactor)]]);
  }
end;



procedure TFLocMap.CBMShowChange(Sender: TObject);
begin
  if CBMShow.Checked then begin
    if not MapLoaded then
    LoadMap(ExtractFilePath(Application.ExeName)+FMain.DataDir+'/maps/'+EditMapLoadName.Text+'.dst');
    ImageLocMap.Width:=MapAreaW;
    ImageLocMap.Height:=MapAreaH;
    CBMShow.Left:=MapAreaW-CBMShow.Width;
    CBMShow.Top:=MapAreaH;
    CBMShowRobot.Left:=MapAreaW-CBMShow.Width-CBMShowRobot.Width;
    CBMShowRobot.Top:=MapAreaH;
    CBMShowSensorPoints.Left:=MapAreaW-CBMShow.Width-CBMShowRobot.Width-CBMShowSensorPoints.Width;
    CBMShowSensorPoints.Top:=MapAreaH;
    CBMousePosition.Left:=ImageLocMap.Left;
    CBMousePosition.Top:=MapAreaH;
    FLocMap.Width:=MapAreaW+100;
    FLocMap.Height:=MapAreaH+100;
   // Label1.Left:= ImageLocMap.Left+20;
    //Label1.Top:= MapAreaH+25;
    //LVar.Left:=ImageLocMap.Left+20;
    //LVar.Top:=MapAreaH+60;
    ImageLocMap.Visible:=false;
    DrawArrayDist(MapDist, MapAreaW, MapAreaH, FLocMap.ImageLocMap);
  end;
end;

procedure TFLocMap.EditPesoLocalizationEditingDone(Sender: TObject);
begin
  StrToFloatDef(EditPesoLocalization.Text, 1e-9);
end;

procedure TFLocMap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Fmain.AuxFormClosed('LocMap');
end;

procedure TFLocMap.FormCreate(Sender: TObject);
begin

  Fmain.InsertAuxForms(FLocMap,'LocMap');
  CBMShowChange(Self);
end;

procedure TFLocMap.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ((x<MapAreaW) and (y<MapAreaH) )then begin
    //RobotState[myNumber].x:=(x-MapAreaW/2)*cell_size*imfactor;
    //RobotState[myNumber].y:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
    MRobotPos.x:=(x-MapAreaW/2)*cell_size*imfactor;
    MRobotPos.y:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
  end;

end;

procedure TFLocMap.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ((x<MapAreaW) and (y<MapAreaH) )then begin
    if CBMousePosition.Checked then begin
      LPos.Visible:=true;
      LPos.Left:=round(x-LPos.Width*0.5);
      LPos.Top:=y+25;
      LPos.Caption:=Format('x=%2f; y=%2f',[(x-MapAreaW/2)*cell_size*imfactor,(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor]);
    end else begin
      LPos.Visible:=false;
    end;
    CBMousePosition.Caption:=Format('%2f; %2f',[(x-MapAreaW/2)*cell_size*imfactor,(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor]);
    CBMousePosition.Caption:=CBMousePosition.Caption+Format('-->Dist= %2f',[MapDist[round((abs(MapAreaH-y))*imfactor),round(X*imfactor)]]);
  end;
end;

procedure TFLocMap.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var
    xtemp, ytemp: double;
begin
  if ((x<MapAreaW) and (y<MapAreaH) )then begin
    xtemp:=(x-MapAreaW/2)*cell_size*imfactor;
    ytemp:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
    RobotState[myNumber].x:=MRobotPos.x;
    RobotState[myNumber].y:=MRobotPos.y;
    RobotState[myNumber].teta:=arctan2(ytemp-RobotState[myNumber].y, xtemp- RobotState[myNumber].x);
    Label1.Caption:=format('%.2f;%.2f;%.2f',[RobotState[myNumber].x,RobotState[myNumber].y,radtodeg(RobotState[myNumber].teta)]);
  end;
end;

procedure TFLocMap.ImageLocMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  RobotState[myNumber].x:=(x-MapAreaW/2)*cell_size*imfactor;
  RobotState[myNumber].y:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
end;

procedure TFLocMap.ImageLocMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  xtemp,ytemp:double;
begin
    xtemp:=(x-MapAreaW/2)*cell_size*imfactor;
    ytemp:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
    RobotState[myNumber].teta:=arctan2(ytemp-RobotState[myNumber].y, xtemp- RobotState[myNumber].x);
    Label1.Caption:=format('%.2f;%.2f;%.2f',[RobotState[myNumber].x,RobotState[myNumber].y,radtodeg(RobotState[myNumber].teta)]);
end;

procedure LDrawRobot(RP:TRLocState);
begin
  RP.rpos.x:=round(MapAreaW/2+RP.rpos.x/cell_size/imfactor);
  RP.rpos.y:=round(-RP.rpos.y/cell_size/imfactor+MapAreaH/2);

  //with FLocMap.ImageLocMap do begin
  with FLocMap do begin
    //Canvas.Pen.Color:=clRed;
    //DrawPoint(RPos,clRed,4);
    Canvas.Pen.Color:=clBlue;
    //Canvas.Arc(50,50,200,200,0,360*16);
    Canvas.Arc(round(RP.rpos.x-5),round(RP.rpos.y-5),round(RP.rpos.x+5),round(RP.rpos.y+5), 0, 360*16);
    Canvas.Pen.Color:=clRed;
    Canvas.Line(round(RP.rpos.x),round(RP.rpos.y), round(RP.rpos.x + 10*cos(rp.rpos.teta)), round(RP.rpos.y-10*sin(RP.rpos.teta)));
  end;
end;

procedure LDrawPoint(rp:TRLocState; cor:TColor; size:integer);
var
  p:Tpos;
begin
  p.x:=round(rp.rpos.x/cell_size/imfactor+MapAreaW/2);
  p.y:=round(-rp.rpos.y/cell_size/imfactor + MapAreaH/2);
  //with FLocMap.ImageLocMap do begin
  with FLocMap do begin
    Canvas.Pen.Color:=cor;
    Canvas.Brush.Color:=cor;
    if (p.x >= 0) and (p.y >=0) then
      Canvas.Rectangle(round(p.x-size),round(p.y-size),round(p.x+size),round(p.y+size));
  end;
end;

procedure LDrawListOfPoints(var rp:TRLocState; var PL: TPointList);
var
  pp:TRLocState;
  p:TPos;
  t:Tpos;
  i:integer;
  s:string;
  c:integer;
begin
  //robot
  //DrawPoint(rp, clRed, 2);
  //points
  for i := 0  to PL.PCount-1  do begin
    pp.rpos.x:=rp.rpos.x+cos(rp.rpos.teta)*(PL.PList[i].x) - sin(rp.rpos.teta)*(PL.PList[i].y);
    pp.rpos.y:=rp.rpos.y+sin(rp.rpos.teta)*(PL.PList[i].x) + cos(rp.rpos.teta)*(PL.PList[i].y);
   // if ((abs(pp.rpos.x) > 0) and (abs(pp.rpos.y) > 0)) then begin
      LDrawPoint(pp, clBlue, 1);
    //end;
  end;
end;

procedure DrawCovariancia(rpos, c:localization.Tpos; cor: TColor);
begin
 { with rp do begin
    if not (cov.x=0) then
      c.x:= (1/cov.x)*0.001;
    if not (cov.y = 0) then
      c.y:= (1/cov.y)*0.001;
    if not (cov.teta  = 0) then
      c.teta:= (1/cov.teta)*0.05;

    rpos.x:=round(MapAreaW/2+rpos.x/cell_size/imfactor);
    rpos.y:=round(-rpos.y/cell_size/imfactor+MapAreaH/2);

    FLocMap.Canvas.Pen.Color:=clRed;
    FLocMap.Canvas.Arc(round(rpos.x-c.x),round(rpos.y-c.y),round(rpos.x+c.x),round(rpos.y+c.y), 0, 360*16);
    FLocMap.Canvas.Pen.Color:=clYellow;
    FLocMap.Canvas.Line(round(rpos.x-c.teta*cos(rpos.teta)),round(rpos.y+c.teta*sin(rpos.teta)), round(rpos.x + c.teta*cos(rpos.teta)), round(rpos.y-c.teta*sin(rpos.teta)));
  end; }
  //with rp do begin
   { if not (cov.x=0) then
      //c.x:= (1/varpos.x)*0.001;
      c.x:=cov.x;
    if not (cov.y = 0) then
      //c.y:= (1/varpos.y)*0.001;
      c.y:=cov.y;
    if not (cov.teta  = 0) then
      //c.teta:= (1/varpos.teta)*0.001;
      c.teta:=cov.teta;  }

    //FLocMap.LVar.Caption:=format('%.5f; %.5f; %.5f', [c.x, c.y, c.teta]);

    rpos.x:=round(MapAreaW/2+rpos.x/cell_size/imfactor);
    rpos.y:=round(-rpos.y/cell_size/imfactor+MapAreaH/2);

    FLocMap.Canvas.Pen.Color:=cor;
    FLocMap.Canvas.Arc(round(rpos.x-c.x),round(rpos.y-c.y),round(rpos.x+c.x),round(rpos.y+c.y), 0, 360*16);
    FLocMap.Canvas.Pen.Color:=clYellow;
    FLocMap.Canvas.Line(round(rpos.x-c.teta*cos(rpos.teta)),round(rpos.y+c.teta*sin(rpos.teta)), round(rpos.x + c.teta*cos(rpos.teta)), round(rpos.y-c.teta*sin(rpos.teta)));



 // end;
end;

procedure DrawLocalizationData(var rp:TRLocState;var VList:TPointList);
var
  r: localization.TPos;
begin
  if FMain.CBLocalization.Checked then begin
    if FLocMap.CBMShow.Checked then begin
      //FLocMap.Canvas.Clear;
      FLocMap.Canvas.Draw(0,0,TempBitmap);

      if FLocMap.CBMShowRobot.Checked then begin
        LDrawRobot(rp);
       { r.x:=rp.cov.x*10000;
        r.y:=rp.cov.y*10000;
        r.teta:=rp.cov.teta*10000;
        DrawCovariancia(rp.rpos,r, clRed);
        r.x:=rp.varpos.x*10000;
        r.y:=rp.varpos.y*10000;
        r.teta:=rp.varpos.teta*100000;
        DrawCovariancia(rp.rpos,r, clRed);  }
      end;
      if FLocMap.CBMShowSensorPoints.Checked then begin
        LDrawListOfPoints(rp,VList);
      end;
      if DStates[0].RobotActive then
        FLocMap.Label1.Color:=clGreen
      else
        FLocMap.Label1.Color:=clRed;
     end;
  end;

end;



initialization
  {$I locmap.lrs}

end.

