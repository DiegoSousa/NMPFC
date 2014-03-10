unit KalmanBall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, IniPropStorage, ExtCtrls, StdCtrls,KalmanBall_Aux,math;

const
  CDataConfigDir='data';
  CDataConfigFile='ConfigKalmanBall.ini';
  PB_factor=50;

type

  { TFBall }

  TFBall = class(TForm)
    Button1: TButton;
    B_Up: TButton;
    B_Reset: TButton;
    B_Play: TButton;
    B_Load: TButton;
    B_Set: TButton;
    CB_Show: TCheckBox;
    CB_ShowObsVel: TCheckBox;
    CB_ShowObsBall: TCheckBox;
    E_qualDecayLin: TEdit;
    E_qualDecay: TEdit;
    E_MinIter: TEdit;
    E_ModPred: TEdit;
    E_VelPredQ: TEdit;
    E_ModeObs: TEdit;
    E_MinCovXY: TEdit;
    E_MinCovVxy: TEdit;
    E_estX: TEdit;
    E_estY: TEdit;
    E_estVx: TEdit;
    E_estVy: TEdit;
    E_Sample: TEdit;
    E_LoadBallLog: TEdit;
    E_InitMode: TEdit;
    E_Cov0XY: TEdit;
    E_Cov0VXY: TEdit;
    E_powerRo: TEdit;
    E_powerTeta: TEdit;
    E_errorRo: TEdit;
    E_errorTeta: TEdit;
    E_OdoXY: TEdit;
    E_OdoAng: TEdit;
    E_SRate: TEdit;
    GB_Prediction: TGroupBox;
    GB_Update: TGroupBox;
    GroupBox1: TGroupBox;
    IniPropStorage: TIniPropStorage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    M_Debug: TMemo;
    PB_BallMap: TPaintBox;
    PC_KalmanBall: TPageControl;
    TB_BallParameters: TTabSheet;
    Timer: TTimer;
    TS_BallMap: TTabSheet;
    procedure BallMapClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure B_UpClick(Sender: TObject);
    procedure B_LoadClick(Sender: TObject);
    procedure B_PlayClick(Sender: TObject);
    procedure B_ResetClick(Sender: TObject);
    procedure B_SetClick(Sender: TObject);
    procedure E_MinCovVxyChange(Sender: TObject);
    procedure E_SampleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure drawPose(PBToDraw:TPaintBox;DrawColor:TColor);
    procedure drawBallPose(x,y,vx,vy:double;PBToDraw:TPaintBox;DrawColor:TColor);
    procedure drawBallEllipse(x,y,covX,Covy,Covxy:double;PBToDraw:TPaintBox;DrawColor:TColor);
    procedure extractData(var v,vn,w,deltTeta,deltU,deltUn,ro,teta:double;var quality:integer;data:string);
    procedure estimateBallPosition(var BallStateQual:double;deltV,deltVn,deltTeta:double;obsRo,obsTeta:double;quality:integer;var x,y,Vx,Vy:double;var filterIter:boolean);
  end;

 TestBallParam= record
  errorOdoXY:double;
  errorOdoAng:double;
  sRate:double;
  modPred:integer;
  errorRo:double;
  errorTeta:double;
  errorPowerRo:integer;
  errorPowerTeta:integer;
  InitCovXY:double;
  InitCovVXY:double;
  InitMode:integer;
  Zcamera:double;
  Zball:double;
  minCovXY:double;
  minCovVxY:double;
  modObs:integer;
  velPredQ:double;
  minIter:integer;
  qualDecay:double;
  qualDecayLin:double;
 end;

var
  FBall: TFBall;
  estBallParameters:TestBallParam;
  estBall:TestBall;
  logBall:TStringList;
  timerIter:integer;
implementation
uses Camera, Main, dynmatrix, MPC;
{ TFBall }

procedure TFBall.FormCreate(Sender: TObject);
var FDataDir:string;
    i:integer;
begin
 FDataDir:=CDataConfigDir;
  for i := 1 to paramcount do begin
    if not (paramstr(i) = '-coach') then begin
      if directoryExists(extractfilepath(application.ExeName)+FDataDir) then begin
        FDataDir:=paramstr(i);
      end
      else begin
         mkdir(ExtractFilePath(Application.ExeName)+FDataDir);
      end;
    end;
  end;

  if not DirectoryExists(ExtractFilePath(Application.ExeName)+FDataDir) then begin
    mkdir(ExtractFilePath(Application.ExeName)+FDataDir);
  end;

  IniPropStorage.IniFileName:=ExtractFilePath(Application.ExeName)+FDataDir+'/'+CDataConfigFile;
  IniPropStorage.Restore;

  estBall:=TestBall.Create;
  B_SetClick(Self);
end;

procedure TFBall.FormDestroy(Sender: TObject);
begin
  estBall.Free;
end;

procedure TFBall.B_SetClick(Sender: TObject);
begin
  with estBallParameters do begin
   errorOdoXY:=StrToFloatDef(E_OdoXY.Text,0);
   modPred:=StrToIntDef(E_ModPred.Text,0);
   errorOdoAng:=StrToFloatDef(E_OdoAng.Text,0)*pi/180;
   sRate:=StrToFloatDef(E_SRate.Text,0.025);
   errorRo:=StrToFloatDef(E_errorRo.Text,0)*pi/180;
   errorTeta:=StrToFloatDef(E_errorTeta.Text,0)*pi/180;
   errorPowerRo:=StrToIntDef(E_powerRo.Text,2);
   errorPowerTeta:=StrToIntDef(E_powerTeta.Text,2);
   InitCovXY:=StrToFloatDef(E_Cov0XY.Text,0);
   InitCovVXY:=StrToFloatDef(E_Cov0VXY.Text,0);
   InitMode:=StrToIntDef(E_InitMode.Text,0);
   Zcamera:=StrToFloatDef(FCamera.EditLCamHeight.Text,0.9);
   Zball:=StrToFloatDef(FCamera.EditCamZBall.Text,0.16);
   minCovXY:=StrToFloatDef(E_MinCovXY.Text,1e-3);
   minCovVxY:=StrToFloatDef(E_MinCovVxy.Text,1e-3);
   modObs:=StrToIntDef(E_ModeObs.Text,0);
   velPredQ:=StrToFloatDef(E_VelPredQ.Text,0.1);
   minIter:=StrToIntDef(E_MinIter.Text,2);
   qualDecay:=StrToFloatDef(E_qualDecay.Text,0.9);
   qualDecayLin:=StrToFloatDef(E_qualDecayLin.Text,10);
   estBall.setParameters(errorOdoXY,
                          errorOdoAng,
                          modPred,
                          sRate,
                          errorRo,
                          errorTeta,
                          errorPowerRo,
                          errorPowerTeta,
                          InitCovXY,
                          InitCovVXY,
                          InitMode,
                          Zcamera,
                          Zball,
                          minCovXY,
                          minCovVxY,
                          modObs,
                          velPredQ,
                          minIter,
                          qualDecay,
                          qualDecayLin);
  end;
end;

procedure TFBall.E_MinCovVxyChange(Sender: TObject);
begin

end;

procedure TFBall.E_SampleChange(Sender: TObject);
begin

end;

procedure TFBall.B_LoadClick(Sender: TObject);
var
 path,dir_file:string;
begin
 logBall:=TStringList.Create;
 try
  path:=extractfilepath(application.ExeName)+E_LoadBallLog.Text;
  logBall.LoadFromFile(path);
  B_Play.Enabled:=true;
 except
  on E: Exception do begin;
    showmessage(E.Message);
  end;
 end;
end;

procedure TFBall.BallMapClick(Sender: TObject);
begin

end;

procedure TFBall.Button1Click(Sender: TObject);
begin
  Timer.Enabled:=false;
end;

procedure TFBall.B_UpClick(Sender: TObject);
var v,vn,w,deltTeta,deltU,deltUn,ro,teta:double;
    quality:integer;
    estx,esty,estVx,estVy:double;
    filterIter:boolean;
    qualDec:double;
begin
  //Extract data
  extractData(v,vn,w,deltTeta,deltU,deltUn,ro,teta,quality,logBall[timerIter]);
  E_Sample.Text:=format('%d/%d',[timerIter+1,logBall.Count]);
  timerIter:=timerIter+1;
  if timerIter=logBall.Count then begin
   timerIter:=0;
  end;
  qualDec:=1000;
  estimateBallPosition(qualDec,deltU,deltUn,deltTeta,ro,teta,quality,estx,esty,estVx,estVy,filterIter);
end;

procedure TFBall.B_PlayClick(Sender: TObject);
begin
  Timer.Enabled:=true;
  timerIter:=0;
end;

procedure TFBall.B_ResetClick(Sender: TObject);
begin
  timerIter:=0;
  estBall.stateFilter:=0;
end;

procedure TFBall.Label2Click(Sender: TObject);
begin

end;

procedure TFBall.TimerTimer(Sender: TObject);
var v,vn,w,deltTeta,deltU,deltUn,ro,teta:double;
    quality:integer;
    estx,esty,estVx,estVy:double;
    filterIter:boolean;
    qualDec:double;
begin
  //Extract data
  extractData(v,vn,w,deltTeta,deltU,deltUn,ro,teta,quality,logBall[timerIter]);
  E_Sample.Text:=format('%d/%d',[timerIter+1,logBall.Count]);
  timerIter:=timerIter+1;
  if timerIter=logBall.Count then begin
   timerIter:=0;
  end;
  qualDec:=1000;
  estimateBallPosition(qualDec,deltU,deltUn,deltTeta,ro,teta,quality,estx,esty,estVx,estVy,filterIter);
end;

procedure TFBall.drawPose(PBToDraw:TPaintBox;DrawColor:TColor);
const
  LenghtTeta=10;
var
  RobotPositionPixelX, RobotPositionPixelY: double;
begin
  RobotPositionPixelX:=PBToDraw.Width div 2;
  RobotPositionPixelY:=PBToDraw.Height div 2;

  PBToDraw.Canvas.Pen.Color:=DrawColor;
  PBToDraw.Canvas.Pen.Width:=1;

  PBToDraw.Canvas.Arc(round(RobotPositionPixelX-5), round(RobotPositionPixelY-5), round(RobotPositionPixelX+5), round(RobotPositionPixelY+5), 0, 360*16);
  PBToDraw.Canvas.Line(round(RobotPositionPixelX),round(RobotPositionPixelY),round(RobotPositionPixelX + LenghtTeta),round(RobotPositionPixelY));
end;

procedure TFBall.drawBallPose(x,y,vx,vy: double; PBToDraw: TPaintBox;DrawColor: TColor);
var
  BallPositionPixelX, BallPositionPixelY: double;
  theta:double;
  modV:double;
begin
  BallPositionPixelX:=PBToDraw.Width  div 2 + x*PB_factor;
  BallPositionPixelY:=PBToDraw.Height div 2 - y*PB_factor;

  PBToDraw.Canvas.Pen.Color:=DrawColor;
  PBToDraw.Canvas.Pen.Width:=1;

  PBToDraw.Canvas.Arc(round(BallPositionPixelX-5), round(BallPositionPixelY-5), round(BallPositionPixelX+5), round(BallPositionPixelY+5), 0, 360*16);
  theta:=arctan2(vy,vx);
  modV:=sqrt(power(vx,2)+power(vy,2))*PB_factor;
  PBToDraw.Canvas.Line(round(BallPositionPixelX),round(BallPositionPixelY),round(BallPositionPixelX + modV*cos(theta)),round(BallPositionPixelY - modV*sin(theta)));
end;

procedure TFBall.drawBallEllipse(x,y,covX,Covy,Covxy: double; PBToDraw: TPaintBox;DrawColor: TColor);
const
  LenghtTeta=10;
var
  BallPositionPixelX, BallPositionPixelY: double;
  SDX,SDY,SDTeta: double;
  SDXPixel,SDYPixel: double;
begin
  PBToDraw.Canvas.Pen.Color:=DrawColor;
  PBToDraw.Canvas.Pen.Width:=1;

  BallPositionPixelX:=PBToDraw.Width  div 2 + x*PB_factor;
  BallPositionPixelY:=PBToDraw.Height div 2 - y*PB_factor;

  SDX:=sqrt(CovX)*2;//86%
  SDY:=sqrt(CovY)*2;
  SDXPixel:=SDX*PB_factor;
  SDYPixel:=SDY*PB_factor;

  PBToDraw.Canvas.Ellipse(round(BallPositionPixelX-SDXPixel), round(BallPositionPixelY-SDYPixel),
                          round(BallPositionPixelX+SDXPixel), round(BallPositionPixelY+SDYPixel));
end;

procedure TFBall.extractData(var v, vn, w, deltTeta, deltU, deltUn, ro,
  teta: double; var quality: integer;data:string);
var
 id:array [1..18] of integer;
 valArray:array [1..9] of double;
 i,k:integer;
begin
 k:=1;
 for i:=1 to Length(data)-1 do begin
  if ((data[i]<>' ')and((data[i-1]=' ')or(data[i+1]=' '))) then begin
   id[k]:=i;
   k:=k+1;
  end;
 end;
 for i:=1 to 9 do begin
  valArray[i]:=StrToFloat(Copy(data,id[(i-1)*2+1],id[(i-1)*2+2]+1-id[(i-1)*2+1]));
 end;
 v:=valArray[1];
 vn:=valArray[2];
 w:=valArray[3];
 deltTeta:=valArray[4];
 deltU:=valArray[5];
 deltUn:=valArray[6];
 ro:=valArray[7];
 teta:=valArray[8];
 quality:=round(valArray[9]);
end;

procedure TFBall.estimateBallPosition(var BallStateQual:double;deltV,deltVn,deltTeta:double;obsRo,obsTeta:double;quality:integer;var x,y,Vx,Vy:double;var filterIter:boolean);
var d,r,tetao,k1,sigr,sigphi: double;
begin
  estBall.calcQualityDecay(BallStateQual);
  estBall.KalmanFilterEstBall(BallStateQual,deltV,deltVn,deltTeta,obsRo,obsTeta,quality,x,y,Vx,Vy);
  filterIter:=estBall.filterStarted;
  if CB_Show.Checked then begin
    //Draw Things
    PB_BallMap.Canvas.Clear;
    drawBallEllipse(x,y,estBall.ballCovX,estBall.ballCovY,estBall.ballCovXY,PB_BallMap,clGreen);


    //if FMain.CBISTScenario.Checked=true then begin
    //  d:=(sqrt(power((RobotState[myNumber].x-BallState.x),2)+power((RobotState[myNumber].y-BallState.y),2)));
    //  r:=(sqrt(power((RobotState[myNumber].x-BallState.x),2)+power((RobotState[myNumber].y-BallState.y),2)+0.4225));
    //  tetao:=arctan(0.65/d);
    //  k1:=(0.02);
    //  sigr:=k1*(power(d,2)/0.02)+(0.4225/(2*(power(r,2)-0.01)))+(0.4225/(0.04*(power(r,2)-0.01)));
    //  sigphi:=1/(2*(power(r,2)-(0.01*power((sin(tetao)),2))));
    //  sigtemp.setv(0,0,sigr);
    //  sigtemp.setv(0,1,0);
    //  sigtemp.setv(1,0,0);
    //  sigtemp.setv(1,1,sigphi);
    //  sigmaT:=sigtemp;
    //end else
    //if FormMPC.FEUPCB.Checked=true then begin
      d:=(sqrt(power((RobotState[myNumber].x-BallState.x),2)+power((RobotState[myNumber].y-BallState.y),2)));
      sigtemp.setv(0,0,power((d),2));
      sigtemp.setv(0,1,0);
      sigtemp.setv(1,0,0);
      sigtemp.setv(1,1,d);
      sigmaT:=sigtemp;
    //end;
    if FMain.CBISTScenario.Checked=true then begin
      sigr:=Sr;
      sigphi:=Sphi;
      sigtemp.setv(0,0,sigr);
      sigtemp.setv(0,1,0);
      sigtemp.setv(1,0,0);
      sigtemp.setv(1,1,sigphi);
      sigmaT:=sigtemp;
    end;

    drawBallPose(x,y,Vx,Vy,PB_BallMap,clGreen);
    drawPose(PB_BallMap,clRed);
    E_estX.Text:=FloatToStr(x);
    E_estY.Text:=FloatToStr(y);
    E_estVx.Text:=FloatToStr(Vx);
    E_estVy.Text:=FloatToStr(Vy);
    //Observed Ball
    if CB_ShowObsVel.Checked then begin
     drawBallPose(estBall.ballObsX,estBall.ballObsY,estBall.ballObsVx,estBall.ballObsVy,PB_BallMap,clBlue);
    end else if CB_ShowObsBall.Checked then begin
     drawBallPose(estBall.ballObsX,estBall.ballObsY,0,0,PB_BallMap,clBlue);
    end;
    PB_BallMap.Canvas.TextOut(round(PB_BallMap.Width div 2 + estBall.ballObsX*PB_factor),round(PB_BallMap.Height div 2 - estBall.ballObsY*PB_factor),format('%d',[quality]));
  end;
end;


initialization
  {$I kalmanball.lrs}

end.

