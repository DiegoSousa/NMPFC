unit Coach;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Menus, IniPropStorage,lNet,lNetComponents, dynmatrix,
  DecConsts, Roles, DOM, XMLRead, IniFiles, Math, Fuzzy;

//------- Types e Consts do WLan---------------//
type
  TPlayerRobotState = packed record
    x, y, teta: single;
    vx, vy, w: single;
    conf: single;
    Active: boolean;
    WithBall: boolean;
  end;

  TPlayerBallState = packed record
    x,y,vx,vy: single;
    x_n,y_n,vx_n,vy_n: single;
    x_next,y_next: single;
    quality: single;
  end;

  TPlayerObsState = packed record
    x, y: single;
    conf: single;
  end;

  TPlayerInfo = packed record
    Magic: LongWord;
    RobotState: TPlayerRobotState;
    ObsState: array [0..MaxRobots-1] of TPlayerObsState;
    BallState: TPlayerBallState;
    num: byte;
    Batery:double;
  end;


  TCoachRobotState = packed record
    x, y, teta: single;
    vx, vy, w: single;
    conf: single;
    role: TRole;
    active: boolean;
  end;

  TCoachBallState = packed record
    x,y,vx,vy: single;
    votes:integer;
    x_n,y_n,vx_n,vy_n: single;
    x_next,y_next: single;
    coachQuality: single;
  end;

  TCoachObsState = packed record
    x, y: single;
    conf: single;
  end;

  TCoachInfo = packed record
    Magic: LongWord;
    RobotState: array [0..MaxRobots-1] of TCoachRobotState;
    BallState: TCoachBallState;
    ObsStates:  array [0..MaxRobots-1] of TCoachObsState;
    Play: TPlay;
  end;
//-------Fim dos Types e Consts do WLan---------------//

//------- Types e Consts do Omni3---------------//
type
  TDriverStateMode=(dsmPWM,dsmPID,dsmPID_PWM);

  TDriverState=record
    pwm, out_pwm: integer;
    ref: integer;
    filt_ev, filt_ref, K, invTi, TD, alpha: single;
    Vact, Iact: double;
    enc: Word;

    PID_period: integer;
    HardMotIMax: integer;
    PWM_MaxSlope: integer;

    Mode: TDriverStateMode;
    AlphaPos,AlphaNeg,BetaPos,BetaNeg: double;

    RobotActive: boolean;
    ticks_act: boolean;
  end;
//-------Fim dos Types e Consts do Omni3---------------//

//------- Types e Consts do Main---------------//

const
  NumMotors= 3;

  Idcoach=6;
  NSim=30;
  Nstress=3;
  Stress_w_tr=20;

  MaxAuxForms=16;
  MaxV=1.0;
  MaxW=0.1;
  ImgWidth=384;
  ImgHeight=288;
  MaxColors=8;
  CTrackColors=6;
  MaxCenters=16;
  MaxSegLines=64;
  MaxEdges=96;
  MaxEdges2=64;
  MaxRadarRays=64;
  MaxRegions=16;
  MaxOdos=4;
  MaxGoals=6;
  MaxMessRobots=8;
  MaxVSensors=MaxRegions;

  MaxObstacles=32;
  MaxWorldStripes=16;

  MaxVertices=10;
  Maxpoints=50;
  MaxOpt=2000;

  iBallColor=0;
  iYellowGoal=1;
  iBlueGoal=2;
  iPurpleTeam=3;
  iCyanTeam=4;
  iBlackColor=5;
  iWhiteColor=6;
  iGreenColor=7;

  acStop=0;
  acGotoBall=1;
  acfindBall=2;
  acGotoGoal=3;
  acRotateTeta=4;
  acContorn=5;
  acDefend=6;

  colorcolor: array[0..MaxColors-1] of word=($7E00,$7FE0,$001F,$5C1F,$03FF,$4210,$7FFF,$03E0);
  colorcolor24: array[0..MaxColors-1] of Tcolor=(clred,clyellow,clblue,clpurple,clnavy,clgray,clwhite,$00FF00);

  MechAlpha = 30 * Pi / 180;

  MechCosAlpha = 0.86602540378443864676372317075294; //cos(MechAlpha);
  MechSinAlpha = 0.5; //sin(MechAlpha);

  M_E=2.71828182845904523536028747135266;

  MechB = 0.07;
  MechD = 0.07;

  PACKET_COACH_MAGIC = $DEADBEEF;
  PACKET_PLAYER_MAGIC = $CAFEBABE;

type
  TGoalColor=(gcYellow,gcBlue);
  TTeamColor=(tcMagenta,tcCyan);

  iColorSet=set of iBallColor..iGreenColor;

const
  CGoalColorStr: array[low(TGoalColor)..High(TGoalColor)] of string = ('Yellow','Blue');
  CTeamColorStr: array[low(TTeamColor)..High(TTeamColor)] of string = ('Magenta','Cyan');
  CGoalColorColor24: array[low(TGoalColor)..High(TGoalColor)] of Tcolor = (clYellow,clBlue);
  CTeamColorColor24: array[low(TTeamColor)..High(TTeamColor)] of Tcolor = ($00FF00FF,$00FFFF00);

type
  TPos=record
    x,y, teta: double;
  end;

type
  TTacticCommand=record
    v,vn,w: double;
    v1,v2,v3: double;
    chipkick_pulse: integer;
    Low_kick: boolean;
  end;

  Tcenter=record
    ro,teta,d: double;
    xw,yw: double;
    roMin,roMax,tetaMin,tetaMax: double;
    area: integer;
  end;

  TCenters= record
    data: array[0..MaxCenters-1] of Tcenter;
    count: integer;
  end;

  TEdge= record
    lineNum: integer;
    xi,yi: integer;
    ro,teta: double;
    d,xw,yw: double;
    color1,color2: integer;
    quality: integer;
  end;

  TRegion= record
    BestColor: integer;
    BestColorPer: integer;
    x1,y1,x2,y2: integer;
    phi,teta: double;
    xw,yw: double;
  end;

  TRadar= record
    lineNum: integer;
    color: integer;
    size: integer;
    teta,ro: double;
    xw,yw,d: double;
  end;

  TOdo= record
    speedw: array[0..2] of double;
    dwheel: array[0..2] of double;
    count: integer;
  end;

  //values in meters and rads
  TDOdos=record
    x:double;
    y:double;
    teta:double;
  end;


  TAbsOdo=record
    pos: array[0..3] of integer;
    count: integer;
  end;

  TView=record
    FrameTime: integer;
    SendTime: integer;
    Centers: array[0..cTrackColors-1] of Tcenters;
    Edges: array[0..MaxEdges-1] of TEdge;
    EdgesCount: integer;
    Regions: array[0..MaxRegions-1] of TRegion;
    RegionsCount: integer;
    Odos,FreeOdos: array[0..MaxOdos-1] of TOdo;
    OdosCount,OdosTotalCount: integer;
    DOdos:TDOdos;
    Radar: array[0..MaxRadarRays-1] of TRadar;
    RadarRaysCount: integer;
  end;


type
  TBallState=record
    x,y,vx,vy,estx,esty: double;
    xl,yl,vxl,vyl: double;
    x_n,y_n,vx_n,vy_n: double;
    x_next,y_next: double;
    cov_x,cov_pos_noise,cov_y:double;
    cov_Vx,cov_Vy:double;
    quality: double;
    timestamp: integer;
    votes:integer;
    xcoach,ycoach: double;
    coachquality: double;
    touched: boolean;
  end;

  TRobotState=record
    valid:boolean;
    x,y,teta: double;
    cov_x,cov_y,cov_xy,cov_teta: double;
    v,vn,w: double;
    sv,svn,sw: double;
    vx,vy: double;
    v1,v2,v3: double;
    timestamp: integer;
    num: integer;
    Xk,Pk: TDMatrix;
    Phik,Hk,Qk: TDMatrix;
    withball: boolean;
    count: integer;
    compass, compass_filtered: double;
    compass_active: boolean;
    Vbatery:double;
  end;

  TObstacle=record
    xw,yw: double;
    color: integer;
    quality: double;
  end;

  TObstacles=record
    Centers:array[0..MaxObstacles-1] of TObstacle;
    count: integer;
  end;

  TEdgeLine=record
    iEdges: array [0..2] of integer;
    conf: double;
    teta,odist: double;
  end;
//-------Fim dos Types e Consts do Main---------------//

//------------Types e Consts do Param----------------//
Const
  UDPBufSize=1024;

type
  TUDPBuffer= record
    data: array[0..UDPBufSize-1] of byte;
    MessSize, ReadDisp: integer;
  end;

//-------- Fim dos Types e Consts do Param----------//

type
  TRobotStateForm = record
    CBRobotActive, CBForceRobotActive: TCheckBox;
    EditRobotState: TEdit;
  end;

  TObsStates = record
    Obs: array[0..MaxRobots-1] of TObstacle;
  end;

  { TFCoach }

  TFCoach = class(TForm)
    BRefBoxConnect: TButton;
    BRefBoxDisconnect: TButton;
    ButtonStart: TButton;
    ButtonStop: TButton;
    CBForceRobotActive1: TCheckBox;
    CBForceRobotActive2: TCheckBox;
    CBForceRobotActive3: TCheckBox;
    CBForceRobotActive4: TCheckBox;
    CBForceRobotActive5: TCheckBox;
    CBForceRobotActive6: TCheckBox;
    CBKeeperActive: TCheckBox;
    CBRobotActive1: TCheckBox;
    CBRobotActive2: TCheckBox;
    CBRobotActive3: TCheckBox;
    CBRobotActive4: TCheckBox;
    CBRobotActive5: TCheckBox;
    CBRobotActive6: TCheckBox;
    CBOn: TCheckBox;
    CheckBoxOtherTactic: TCheckBox;
    CheckBoxOtherTactic2: TCheckBox;
    CBFuzzy: TCheckBox;
    EditMessIP: TEdit;
    EditPlayState: TEdit;
    EditRefBoxIP: TEdit;
    EditRefState: TEdit;
    EditRobotAvailableCount: TEdit;
    EditRobotState1: TEdit;
    EditRobotState2: TEdit;
    EditRobotState3: TEdit;
    EditRobotState4: TEdit;
    EditRobotState5: TEdit;
    EditRobotState6: TEdit;
    EditVbatRobot1: TEdit;
    EditVbatRobot2: TEdit;
    EditVbatRobot3: TEdit;
    EditVbatRobot4: TEdit;
    EditVbatRobot5: TEdit;
    EditVbatRobot6: TEdit;
    FormStorage: TIniPropStorage;
    GBRefBox: TGroupBox;
    GBRobotInfo1: TGroupBox;
    GBRobotInfo2: TGroupBox;
    GBRobotInfo3: TGroupBox;
    GBRobotInfo4: TGroupBox;
    GBRobotInfo5: TGroupBox;
    GBRobotInfo6: TGroupBox;
    ImageMap: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu: TMainMenu;
    MemoRefBox: TMemo;
    MemoRefBoxBad: TMemo;
    MenuAbout: TMenuItem;
    MenuExit: TMenuItem;
    MenuFile: TMenuItem;
    MenuWindows: TMenuItem;
    N1: TMenuItem;
    RGDecision: TRadioGroup;
    RGRobotSel: TRadioGroup;
    RGBallSel: TRadioGroup;
    SdpoUDPSuper: TLUDPComponent;
    TCPRefBox: TLTCPComponent;
    TimerDoTactic: TTimer;
    UDPRefBox: TLUDPComponent;
    procedure BRefBoxConnectClick(Sender: TObject);
    procedure BRefBoxDisconnectClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure CBForceRobotActive1Change(Sender: TObject);
    procedure CBForceRobotActive2Change(Sender: TObject);
    procedure CBForceRobotActive3Change(Sender: TObject);
    procedure CBForceRobotActive4Change(Sender: TObject);
    procedure CBForceRobotActive5Change(Sender: TObject);
    procedure CBForceRobotActive6Change(Sender: TObject);
    procedure CBKeeperActiveChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure ImageMapClick(Sender: TObject);
    procedure ImageMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuWinDefaultClick(Sender: TObject);

    procedure SdpoUDPCoachReceive(aSocket: TLSocket);
    procedure SdpoUDPSuperError(const msg: string; aSocket: TLSocket);
    procedure SdpoUDPSuperReceive(aSocket: TLSocket);
    procedure ShowAll;
    procedure MainLoop;
    procedure TCPRefBoxReceive(aSocket: TLSocket);
    procedure TimerDoTacticTimer(Sender: TObject);

    procedure ParseRefBoxData(data: String);
    procedure ParseRefBoxUDPData(data: String);
    procedure ParseRefBoxUDPData(data, attr: String);
    procedure ParseXmlData(xmldata: string);
    procedure ShowRobotTimes(var RS: TRobotstate; var RSF: TRobotStateForm; robnum:integer);
    procedure MergeBallState;
    procedure MergeBallState2;

    procedure UDPRefBoxReceive(aSocket: TLSocket);
  private
    { private declarations }
  public
    { public declarations }
    NetTime: DWORD;
    deb_txt: string;
    AuxForms: array[0..MaxAuxForms-1] of TForm;
    NumAuxForms: integer;
    DataDir: string;
    down_x,down_y: integer;

    procedure ProcessPlayerPacket(packet_str: string);
    procedure SendCoachInfo;

    procedure InsertAuxForms(Fm: TForm; cap: string);
    procedure AuxFormClosed(cap: string);
    procedure SaveAuxForms;
    procedure CloseAuxForms;
    procedure RestoreAuxForms;

    procedure SetRobotStateForm;

//-------Funções do Main---------//
procedure ResetView;
procedure WorldToMapDim(dw: double; var dm: integer);
procedure WorldToMap(xw,yw: double; var xm: integer; var ym: integer);
function  WorldToMapP(xw,yw: double):TPoint;
procedure MapToWorld( xm, ym: integer; var xw,yw: double);
function Get24Color(cor: integer): Tcolor;
procedure DrawFieldMap(field_canvas: TCanvas);
procedure DrawBall(field_canvas: TCanvas; var BS: TBallState; var RS: TRobotState; i: integer);
procedure DrawBallM(field_canvas: TCanvas; var BS: TBallState; BallColor: Tcolor);
procedure DrawObstacles(var Obs: TObstacles);
//----Fim das Funções do Main----//

//-------Funções do Actions---------//
procedure VVnToVxy(teta,v,vn: double; var Vx,Vy: double);
//----Fim das Funções do Actions----//

  end; 

var
  FCoach: TFCoach;
  BallStates,LastBallStates: array[0..MaxRobots-1] of TBallState;

  InputVals : array[0..1] of real;

  BallsFiltered: array[0..MaxRobots-1] of double;
  BallsFilteredId: array[0..MaxRobots-1] of integer;
  ObsStates: array[0..MaxRobots-1] of TObsStates;
  RobotStateForm: array[0..MaxRobots-1] of TRobotStateForm;

  DataPath: string = '';

//----------Variáveis do Omni3---------------//
  DStates: array[0..NumMotors-1] of TDriverState;
//----------Fim das Variáveis do Omni3-------//

//----------Variáveis do Main---------------//

ControlTimeStamp:LongWord;
  TacticCommands: array[0..MaxRobots-1] of TTacticCommand;
  RobotState: array[0..MaxRobots-1] of TRobotState;
  OponentState: array[0..MaxOponents-1] of TRobotState;

  Tres_interc: boolean=false;
  min_ger_SL: double;
  EstRobotState_GLOBAL:TRobotState;
  Count_Loc_GP: Integer = 0;
  Count_Loc_GOALS: Integer = 0;

  LastData: string;

  Flogs: file;
  CountUdp: integer;
  Buff_erro:array[0..Nsim-1,0..1] of double;
  Vsim, Wsim, Sum_err_stress_v,Sum_err_stress_w: double;
  Idx_erro: integer;
  Flag_stress,Flag_stress_ant: boolean;
  ref_v_ant, ref_w_ant : double;
  v_on_stress, vn_on_stress, w_on_stress: double;
  int_ev,int_ew: double;
  walk_counter: integer;

  myNumber: integer;
  AttackGoal: TGoalColor;
  TeamColor: TTeamColor;
  Dfieldmag: double;

  ticksToMess: integer;
  walkLevel: integer;

  YellowTetaCenters, BlueTetaCenters: TCenters;

  YellowGoal,BlueGoal: TCenter;
  YellowGoalAge,BlueGoalAge: integer;

  BallState,LastBallState,BallStateRstart: TBallState;
  ObsBallState, LastObsBallState: TBallState;
  NetBallState: TBallState;

  Obstacles: TObstacles;
  WGLine,GWLine: TEdgeLine;
  View, LastView: TView;

  MouseControlX,MouseControlY,MouseControlTeta: double;
  MouseControlValid: boolean=false;

  FieldImageWidth, FieldImageHeight: integer;
  LogFile: text;
  LogString: string;

//----------Fim das Variáveis do Main-------//

implementation

uses Field, Robots, Utils, Tactic, Param, Log;

{ TFCoach }

procedure TFCoach.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveAuxForms;
  CloseAuxForms;
  TCPRefBox.Disconnect;
end;

procedure TFCoach.CBKeeperActiveChange(Sender: TObject);
begin
  if CBKeeperActive.Checked then begin
     KeeperWay:=roleKeeper;
  end else begin
     KeeperWay:=roleKeeperPassive;
  end;
end;

procedure TFCoach.ButtonStartClick(Sender: TObject);
begin
  //RefereeState:=rsStartPos;
  //if (RGDecision.ItemIndex=2) then begin
    RefereeState:=rsFormation;
  //end;
end;

procedure TFCoach.BRefBoxDisconnectClick(Sender: TObject);
begin
  TCPRefBox.Disconnect;
  UDPRefBox.Disconnect;
end;

procedure TFCoach.BRefBoxConnectClick(Sender: TObject);
begin
  TCPRefBox.Connect(EditRefBoxIP.Text, 28097);
  UDPRefBox.Listen(30000);
end;

procedure TFCoach.ButtonStopClick(Sender: TObject);
begin
  RefereeState:=rsStopPos;
end;

procedure TFCoach.CBForceRobotActive1Change(Sender: TObject);
begin
   //RobotStatus[0].Active:=RobotStateForm[0].CBRobotActive.Checked;
end;

procedure TFCoach.CBForceRobotActive2Change(Sender: TObject);
begin
   //RobotStatus[1].Active:=RobotStateForm[1].CBRobotActive.Checked;
end;

procedure TFCoach.CBForceRobotActive3Change(Sender: TObject);
begin
   //RobotStatus[2].Active:=RobotStateForm[2].CBRobotActive.Checked;
end;

procedure TFCoach.CBForceRobotActive4Change(Sender: TObject);
begin
   //RobotStatus[3].Active:=RobotStateForm[3].CBRobotActive.Checked;
end;

procedure TFCoach.CBForceRobotActive5Change(Sender: TObject);
begin
   //RobotStatus[4].Active:=RobotStateForm[4].CBRobotActive.Checked;
end;

procedure TFCoach.CBForceRobotActive6Change(Sender: TObject);
begin
   //RobotStatus[5].Active:=RobotStateForm[5].CBRobotActive.Checked;
end;

procedure TFCoach.FormCreate(Sender: TObject);
var SessionPropsList: TStringList;
    SessionPropsFileName: string;
begin
  NumAuxForms:=0;
  if paramcount>=1 then begin
    DataDir:=paramstr(1);
    if not directoryExists(extractfilepath(application.ExeName)+DataDir) then DataDir:=DataPath+'data';
  end else begin
    DataDir:=DataPath+'data'
  end;
//  FormStorage.IniFileName:=extractfilepath(application.ExeName)+'\'+dataDir+'\Config.ini';
  if not DirectoryExists(ExtractFilePath(Application.ExeName)+DataDir) then
    mkdir(ExtractFilePath(Application.ExeName)+DataDir);

  SessionPropsFileName := ExtractFilePath(Application.ExeName)+DataDir+'/SessionPropsMain.txt';
  if FileExists(SessionPropsFileName) then begin
    SessionPropsList := TStringList.Create;
    try
      SessionPropsList.LoadFromFile(SessionPropsFileName);
      SessionPropsList.Delimiter:=';';
      SessionProperties := SessionPropsList.DelimitedText;
    finally
      SessionPropsList.Free;
    end;
  end;
  FormStorage.IniFileName:=ExtractFilePath(Application.ExeName)+DataDir+'/Config.ini';
  FormStorage.Restore;
  SetRobotStateForm;

  loadfuzzysets;


end;

procedure TFCoach.FormShow(Sender: TObject);
begin
  // TODO Log crash
  FLog.FillTreeView(FLog.TreeView);
  FLog.LoadTree(FLog.TreeView);
  FLog.RefreshGrid(FLog.TreeView);

  //SdpoUDPSuper.Listen(7373);
  FieldImageWidth := ImageMap.Width;
  FieldImageHeight := ImageMap.Height;

  RestoreAuxForms;
  UpdateFieldDims;
  TimerDoTactic.Enabled := true;
end;

procedure TFCoach.ImageMapClick(Sender: TObject);
begin

end;

procedure TFCoach.ShowAll;
var i,tx,ty: integer;
    cl: Tcolor;
begin


    DrawFieldMap(ImageMap.canvas);
    for i:=0 to MaxRobots-1 do begin
      RobotState[i].num:=i;
      if i<>5 then begin
        if (RobotStatus[i].active=true) then begin
           DrawRobot(RobotState[i],cl,ImageMap.canvas);
           DrawRobotInfo(RobotState[i],RobotInfo[i],ImageMap.canvas);
           DrawBall(ImageMap.canvas,BallStates[i],RobotState[i],i);
        end else begin
           DrawRobot(RobotState[i],clRed,ImageMap.canvas);
           DrawRobotInfo(RobotState[i],RobotInfo[i],ImageMap.canvas);
        end;
        ShowRobotTimes(RobotState[i],RobotStateForm[i],i);
      end;
    end;
    if BallState.quality>=0 then begin
       DrawBallM(ImageMap.canvas,BallState,clred);
    end;
end;

procedure TFCoach.ImageMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  down_x:=x;
  down_y:=y;
end;

procedure TFCoach.ImageMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Xw,Yw,V: double;
    new_teta: double;
begin
  new_teta:=atan2(-Y+down_y,X-down_x);
  MapToWorld(down_x,down_y,xw,yw);
  V:=sqrt(sqr(-Y+down_y)+sqr(X-down_x));

  if ssShift in Shift then begin
      with BallStates[RGBallSel.ItemIndex] do begin
        x:=xw;
        y:=yw;
      end;
  end else begin
    with RobotState[RGRobotSel.ItemIndex] do begin
      x:=xw;
      y:=yw;
      teta:=new_teta;
    end;
    if RGRobotSel.ItemIndex=6 then begin
    with BallStates[RGBallSel.ItemIndex] do begin
        x:=xw;
        y:=yw;
    end;
    end;
  end;
end;

procedure TFCoach.MenuAboutClick(Sender: TObject);
begin
  Showmessage('5DPO Coach'+#$0d+'2011');
end;

procedure TFCoach.MenuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFCoach.MenuWinDefaultClick(Sender: TObject);
var MenuItem: TMenuItem;
begin
    MenuItem:=Sender as TMenuItem;
    MenuItem.Checked:=true;
    AuxForms[MenuItem.Tag].Show;
    AuxForms[MenuItem.Tag].BringToFront;
end;

procedure TFCoach.SdpoUDPCoachReceive(aSocket: TLSocket);
var PlayerInfo: TPlayerInfo;
    packet_str: string;
    i, RobotNumber: integer;
begin
  try
    aSocket.GetMessage(packet_str);

    // only accpet valid length packets
    if length(packet_str) <> sizeof(PlayerInfo) then exit;

    copymemory(@PlayerInfo, @(packet_str[1]), sizeof(PlayerInfo));

    if PlayerInfo.Magic <> PACKET_PLAYER_MAGIC then exit;
    EditMessIP.Text:= aSocket.PeerAddress;
  except
  end;
end;

procedure TFCoach.SdpoUDPSuperError(const msg: string; aSocket: TLSocket);
begin
  try
    SdpoUDPSuper.Disconnect(true);
    SdpoUDPSuper.Listen(7272+RGRobotSel.ItemIndex);
  except
  end;
end;

procedure TFCoach.SdpoUDPSuperReceive(aSocket: TLSocket);
var packet_str: string;
begin
  try
    aSocket.GetMessage(packet_str);
    ProcessPlayerPacket(packet_str);
  except
  end;
end;

procedure TFCoach.MainLoop;
var  i,j: integer;
begin
  NetTime:=getTickcount();
  deb_txt:='';

  ShowAll;
  deb_txt:=deb_txt+format(',%3d',[getTickcount()-NetTime]);
end;

procedure TFCoach.TCPRefBoxReceive(aSocket: TLSocket);
var data: string;
begin
  TCPRefBox.GetMessage(data);
  ParseRefBoxData(data);
end;

procedure TFCoach.TimerDoTacticTimer(Sender: TObject);
begin
  SdpoUDPSuper.Listen(7373);
  //MergeBallState;
  //MergeBallState2;
  DoTactic;
  SendCoachInfo;
  ShowAll;
  if (CBOn.Checked=true) then begin
      FLog.LogFrame(GetTickCount,0,1);
  end else begin
      FLog.LogFrame(GetTickCount,0,0);
  end;
end;

procedure TFCoach.ParseRefBoxData(data: String);
var i: integer;
    command,s: string;
    c: char;
begin
  command := '';
  while MemoRefBoxBad.Lines.Count > 4 do MemoRefBoxBad.Lines.Delete(0);

  for i:=1 to length(data) do begin
    if data[i]=#0 then continue;
    command:=command+data[i];
  end;

  if (length(command) <> 1) and (command<>'1s') and (command<>'2s')
     and (command<>'Hh') and (command<>'He')  and (command<>'N') then
  begin
    MemoRefBoxBad.Lines.Add(command);
    exit;
  end;

   //if ( (command <> 'K' ) ) then exit;
  //MemoRefBoxBad.Lines.Add(command);
  //MemoRefBox.Lines.Add(command);
  //FCoachMain.MemoRefBox.Clear;

  //FCoachMain.MemoRefBox.Append(command[1]);

  c := command[1];
  s := copy(command,1,1);
  MemoRefBox.Append(c);
  MemoRefBox.Append(s);

  if command = '1s' then  c := 's';
  if command = '2s' then  c := 's';

  if c='*' then ;  // nothing

  TacticProcessRefereeComand(c);
end;

procedure TFCoach.ParseRefBoxUDPData(data: String);
var i: integer;
    command: string;
    c: char;
begin
  c:=' ';
  if data='GameStart' then c:='s'
  else if data='GameStop' then c:='S'
  else if data='Cancel' then c:='H' //?
  else if data='DroppedBall' then c:='N' //?
  else if data='StageChange' then c:='@'   //????
  else if data='Parking' then c:='U';
  TacticProcessRefereeComand(c);
end;

procedure TFCoach.ParseRefBoxUDPData(data, attr: String);
var i: integer;
    command: string;
    c: char;
begin
  c:=' ';
  //assumes that we are cyan, need to review this
  if data='KickOff' then begin
    if attr='Cyan' then
      c:='k'
    else
      c:='K';
  end else  if data='FreeKick' then begin
    if attr='Cyan' then
      c:='f'
    else
      c:='F';
  end else  if data='GoalKick' then begin
  end else if data='ThrowIn' then begin
  end else if data='Corner' then begin
  end else if data='Penalty' then begin
    if attr='Cyan' then
      c:='p'
    else
      c:='P';
  end else if data='Substitution' then begin
  end else if data='CardAwarded' then begin
  end else if data='GoalAwarded' then begin
  end;
  TacticProcessRefereeComand(c);
end;

procedure TFCoach.ParseXmlData(xmldata: string);
var
  Child: TDOMNode;
  doc: TXMLDocument;
  s: TStringStream;
  i: integer;
begin
  s:=TStringStream.Create(xmldata);
  try
    s.Position:=0;
    ReadXMLFile(doc,s);
    Child:=doc.DocumentElement.FirstChild;
    while Assigned(Child) do begin
      EditMessIP.Text:=Child.Attributes.Item[2].NodeValue+':  '+Child.Attributes.Item[1].NodeValue;
      with Child.ChildNodes do
      try
        for i := 0 to (Count-1) do  begin
          if ((Item[i].NodeName = 'GameStart') or (Item[i].NodeName = 'GameStop') or (Item[i].NodeName = 'Cancel') or (Item[i].NodeName = 'DroppedBall') or (Item[i].NodeName = 'StageChange') or (Item[i].NodeName = 'Parking')) then
            ParseRefBoxUDPData(Item[i].NodeName)
          else
            ParseRefBoxUDPData(Item[i].NodeName, Item[i].Attributes.Item[0].NodeValue);
        end;
      finally
        free;
      end;
      Child:=Child.NextSibling;
    end;
  finally
    s.Free;
  end;
end;

procedure TFCoach.ShowRobotTimes(var RS: TRobotstate; var RSF: TRobotStateForm;
  robnum: integer);
begin
  RSF.EditRobotState.Text:=format('%.2f',[(getTickcount-RS.timestamp)/1000]);
  if  (getTickcount-RS.timestamp)/1000 > 10 then begin
    RobotStatus[robnum].Active:=false;
  end;

  if RobotStatus[robnum].Active then begin
   RSF.EditRobotState.Color:=clGreen;
  end else begin
    RSF.EditRobotState.Color:=clRed;
  end;
end;

procedure TFCoach.MergeBallState;
var ball_filt, BestBall: double;
    i,BestBallIdx,quality: integer;
begin
  ball_filt := 0.7;
  BestBall:=0;
  BestBallIdx:=-1;
  quality:=0;
  for i:=0 to MaxRobots-1 do begin
    // filtro
//    BallsFiltered[i] := ball_filt*BallsFiltered[i] + (1-ball_filt)*BallStates[i].quality;
    //Só considera bolas de robôs ativos e qualidade de cada um positiva
    if (RobotStatus[i].active=true) and (BallStates[i].quality>=0) then begin
      BallsFiltered[i] := ball_filt*BallsFiltered[i] + (1-ball_filt)*BallStates[i].quality;
      if (BallsFiltered[i]>BestBall) then begin
         BestBall:=BallsFiltered[i];
         BestBallIdx:=i;
      end;
    end;
  end;

  // the best robot seeing the ball is used to update the new ball position
  if (BestBallIdx>=0) then begin
    BallState.votes:=0;
    BallState:=BallStates[BestBallIdx];
    BallState.timestamp:=GetTickCount;
  end else begin
    BallState.votes:=0;
    BallState.quality:=-1;
  end;
end;


procedure TFCoach.MergeBallState2;
var ball_filt, BestBall: double;
    i,j,k,BestBallIdx,quality,votes: integer;
    countDist:integer;
    distBetwBall:double;
    BallStatesMerged: array[0..MaxRobots-1] of TBallState;
begin
  for i:=0 to MaxRobots-2 do begin
    countDist:=1;
    for j:=i+1 to MaxRobots-1 do begin
      if ((RobotStatus[j].active=true) and (BallStates[j].quality>0)and(RobotStatus[i].active=true) and (BallStates[i].quality>0)) then begin
       distBetwBall:=sqrt(power(BallStates[j].x-BallStates[i].x,2)+power(BallStates[j].y-BallStates[i].y,2));
       if(distBetwBall<1.5)then begin
        BallsFiltered[countDist-1]:=j;
        countDist:=countDist+1;
       end;
      end;
    end;
    BallStatesMerged[i].votes:=countDist;
    BallStatesMerged[i].x:=BallStates[i].x/(countDist+1);
    BallStatesMerged[i].y:=BallStates[i].y/(countDist+1);
    BallStatesMerged[i].x_next:=BallStates[i].x_next/(countDist+1);
    BallStatesMerged[i].y_next:=BallStates[i].y_next/(countDist+1);
    BallStatesMerged[i].quality:=BallStates[i].quality/(countDist+1);
    for j:=0 to countDist-2 do begin
      BallStatesMerged[i].x:=BallStatesMerged[i].x+BallStates[BallsFilteredId[j]].x/(countDist+1);
      BallStatesMerged[i].y:=BallStatesMerged[i].y+BallStates[BallsFilteredId[j]].y/(countDist+1);
      BallStatesMerged[i].x_next:=BallStatesMerged[i].x_next+BallStates[BallsFilteredId[j]].x_next/(countDist+1);
      BallStatesMerged[i].y_next:=BallStatesMerged[i].y_next+BallStates[BallsFilteredId[j]].y_next/(countDist+1);
      BallStatesMerged[i].quality:=BallStatesMerged[i].quality+BallStates[BallsFilteredId[j]].quality/(countDist+1);
    end;
  end;


  votes:=0;
  for i:=0 to MaxRobots-1 do begin
   if(BallStatesMerged[i].votes>votes)then begin
    BallState:=BallStatesMerged[i];
    votes:=BallStatesMerged[i].votes;
   end;
  end;
  if votes<=0 then begin
   BallState.votes:=0;
   BallState.quality:=-1;
  end;
end;

procedure TFCoach.UDPRefBoxReceive(aSocket: TLSocket);
var
  xmldata: string;
begin
  UDPRefBox.GetMessage(xmldata);
  ParseXmlData(xmldata);
end;

procedure TFCoach.ProcessPlayerPacket(packet_str: string);
var PlayerInfo: TPlayerInfo;
    i: integer;
begin
  if length(packet_str) <> sizeof(PlayerInfo) then exit;

  copymemory(@PlayerInfo, @(packet_str[1]), sizeof(PlayerInfo));

  if PlayerInfo.Magic <> PACKET_PLAYER_MAGIC then exit;

  // update ONE robot state
  with RobotState[PlayerInfo.num] do begin
    case PlayerInfo.num of
    0:EditVbatRobot1.Text:=format('%2f',[Vbatery]);
    1:EditVbatRobot2.Text:=format('%2f',[Vbatery]);
    2:EditVbatRobot3.Text:=format('%2f',[Vbatery]);
    3:EditVbatRobot4.Text:=format('%2f',[Vbatery]);
    4:EditVbatRobot5.Text:=format('%2f',[Vbatery]);
    end;
    x:=PlayerInfo.RobotState.x;
    y:=PlayerInfo.RobotState.y;
    teta:=PlayerInfo.RobotState.teta;
    vx:=PlayerInfo.RobotState.vx;
    vy:=PlayerInfo.RobotState.vy;
    W:=PlayerInfo.RobotState.w;

    count:=Round(PlayerInfo.RobotState.conf);

    Vbatery:=PlayerInfo.Batery;
    withball:=PlayerInfo.RobotState.WithBall;
    timestamp:=GetTickCount;
  end;

  with RobotStatus[PlayerInfo.num] do begin
    if RobotStateForm[PlayerInfo.num].CBForceRobotActive.Checked then begin
      Active:=RobotStateForm[PlayerInfo.num].CBRobotActive.Checked
    end else
    if (RobotStateForm[PlayerInfo.num].CBRobotActive.Checked) and (PlayerInfo.RobotState.Active)  then begin
      active:=true;
    end else begin
      active:=false;
    end;
  end;

  // update this robot's ball state for future merging
  with BallStates[PlayerInfo.num] do begin
    x:=PlayerInfo.BallState.x;
    y:=PlayerInfo.BallState.y;
    vx:=PlayerInfo.BallState.vx;
    vy:=PlayerInfo.BallState.vy;
    x_next:=PlayerInfo.BallState.x_next;
    y_next:=PlayerInfo.BallState.y_next;
    quality:=PlayerInfo.BallState.quality;
    touched := PlayerInfo.RobotState.WithBall;
  end;

  for i:=0 to MaxRobots-1 do begin
    with ObsStates[PlayerInfo.num] do begin
      Obs[i].xw:=PlayerInfo.ObsState[i].x;
      Obs[i].yw:=PlayerInfo.ObsState[i].y;
      Obs[i].quality:=PlayerInfo.ObsState[i].conf;
    end;
  end;
end;

procedure TFCoach.SendCoachInfo;
var CoachInfo: TCoachInfo;
    packet_str: string;
    output, i: integer;
    outputVal,d: double;
begin
  try
    CoachInfo.Magic := PACKET_COACH_MAGIC;

    for i := 0 to MaxRobots-1 do begin
      with RobotState[i] do begin
        CoachInfo.RobotState[i].x := x;
        CoachInfo.RobotState[i].y := y;
        CoachInfo.RobotState[i].teta := teta;
        CoachInfo.RobotState[i].vx := vx;
        CoachInfo.RobotState[i].vy := vy;
        CoachInfo.RobotState[i].w := w;
        CoachInfo.RobotState[i].active := RobotStatus[i].active;
      end;

      if (CBOn.Checked=true) then begin
          CoachInfo.RobotState[i].role := RobotInfo[i].role;

          //A SELEÇÃO DAS ROLES DEVE SER FEITA PELO ALGORITMO FUZZY A SER FEITO NA ITALIA!!!
          //if (FCoach.RGDecision.ItemIndex=2) then begin
          if (CBFuzzy.Checked=true) then begin
              d:=sqrt(power((RobotState[i].x - BallState.x),2)+power((RobotState[i].y - BallState.y),2));

              //O ALGORITMO FUZZY DEVERÁ SER POSTO AQUI!!!
              InputVals[BallQuality]:=BallState.quality;
              InputVals[ConfInDistance]:=d;
              outputVal:=ProcessAllSugeno(InputVals);

              if outputVal>0.5 then begin
                 output:=Formation;
              end else begin
                 output:=Search;
              end;

              EditRobotState6.Text:=floattostr(outputVal);
              EditVbatRobot6.Text:=inttostr(output);

              if (output=0) then begin
                  if i<>0 then begin
                     CoachInfo.RobotState[i].role := roleGoSearch;
                     RobotInfo[i].role:=roleGoSearch;
                  end else begin
                     CoachInfo.RobotState[i].role := roleGoSearchFollower;
                     RobotInfo[i].role:=roleGoSearchFollower;
                  end;
              end else begin
                  CoachInfo.RobotState[i].role := roleDoFormation;
                  RobotInfo[i].role:=roleDoFormation;
              end;

          end else begin
              if BallState.quality<100 then begin
                  if i<>0 then begin
                     CoachInfo.RobotState[i].role := roleGoSearch;
                     RobotInfo[i].role:=roleGoSearch;
                  end else begin
                     CoachInfo.RobotState[i].role := roleGoSearchFollower;
                     RobotInfo[i].role:=roleGoSearchFollower;
                  end;
              end else begin
                 CoachInfo.RobotState[i].role := roleDoFormation;
                 RobotInfo[i].role:=roleDoFormation;
              end;
          end;
      end else begin
          CoachInfo.RobotState[i].role := roleIdle;
          RobotInfo[i].role:=roleIdle;
      end;
//      DrawRobotInfo(RobotState[i],RobotInfo[i],ImageMap.canvas);
    end;

    with BallState do begin
      CoachInfo.BallState.x:=x;
      CoachInfo.BallState.y:=y;
      CoachInfo.BallState.vx:=vx;
      CoachInfo.BallState.vy:=vy;
      CoachInfo.BallState.x_next:=x_next;
      CoachInfo.BallState.y_next:=y_next;
      CoachInfo.BallState.coachQuality:=quality;
      CoachInfo.BallState.votes:=votes;
    end;
    CoachInfo.Play:=Play;

    packet_str := StringOfChar(#0, sizeof(CoachInfo));
    copymemory(@(packet_str[1]), @CoachInfo, sizeof(CoachInfo));

    for i := 0 to MaxRobots-1 do begin
      // robots ip+port must be 'IPBase'.'101-106':'7271-7276'
      if RGDecision.ItemIndex=0 then  //Local Host
        SdpoUDPSuper.SendMessage(packet_str, '127.0.0.1:'+inttostr(7271 + i))
      else   //Net
        SdpoUDPSuper.SendMessage(packet_str, FParam.EditIPBase.Text +'.'+IntToStr(101 + i)+ ':'+inttostr(7272 + i));
    end;
  except
  end;
end;

procedure TFCoach.InsertAuxForms(Fm: TForm; cap: string);
var NewItem: TMenuItem;
begin
  NewItem := TMenuItem.Create(Self); //first create the separator
  NewItem.Caption := cap;
  NewItem.tag := NumAuxForms;
  NewItem.OnClick := @MenuWinDefaultClick;
  MenuWindows.Add(NewItem); //add the new item to the Windows menu
  if NumAuxForms<MaxAuxForms-1 then begin
    AuxForms[NumAuxForms]:=Fm;
    inc(NumAuxForms);
  end;
end;

procedure TFCoach.AuxFormClosed(cap: string);
var i: integer;
begin
  for i := 0 to NumAuxForms - 1 do begin
    if MenuWindows.Items[i].Caption = cap then begin
      MenuWindows.Items[i].Checked := false;
      exit;
    end;
  end;
end;

procedure TFCoach.SaveAuxForms;
var i: integer;
    Ini: TIniFile;
begin
  Ini := TIniFile.Create(FormStorage.IniFileName);
  try
    for i:=0 to MenuWindows.Count-1 do begin
      Ini.WriteInteger('WINDOWS',MenuWindows.items[i].Caption+'_Visible',ord(MenuWindows.items[i].Checked));
    end;
  finally
    Ini.Free;
  end;
end;

procedure TFCoach.CloseAuxForms;
var
  i: integer;
begin
  for i:=0 to MenuWindows.Count-1 do begin
    AuxForms[MenuWindows.items[i].tag].Close;
  end;
end;

procedure TFCoach.RestoreAuxForms;
var i: integer;
    Ini: TIniFile;
begin
  Ini := TIniFile.Create(FormStorage.IniFileName);
  try
    for i:=0 to MenuWindows.Count-1 do begin
      if Ini.readInteger('WINDOWS',MenuWindows.items[i].Caption+'_Visible',0)<>0 then begin
        MenuWindows.items[i].Checked:=true;
        AuxForms[MenuWindows.items[i].tag].Show;
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TFCoach.SetRobotStateForm;
begin
  with RobotStateForm[0] do begin
    CBForceRobotActive:=CBForceRobotActive1;
    CBRobotActive:=CBRobotActive1;
    EditRobotState:=EditRobotState1;
  end;
  with RobotStateForm[1] do begin
    CBForceRobotActive:=CBForceRobotActive2;
    CBRobotActive:=CBRobotActive2;
    EditRobotState:=EditRobotState2;
  end;
  with RobotStateForm[2] do begin
    CBForceRobotActive:=CBForceRobotActive3;
    CBRobotActive:=CBRobotActive3;
    EditRobotState:=EditRobotState3;
  end;
  with RobotStateForm[3] do begin
    CBForceRobotActive:=CBForceRobotActive4;
    CBRobotActive:=CBRobotActive4;
    EditRobotState:=EditRobotState4;
  end;
  with RobotStateForm[4] do begin
    CBForceRobotActive:=CBForceRobotActive5;
    CBRobotActive:=CBRobotActive5;
    EditRobotState:=EditRobotState5;
  end;
  //with RobotStateForm[5] do begin
  //  CBForceRobotActive:=CBForceRobotActive6;
  //  CBRobotActive:=CBRobotActive6;
  //  EditRobotState:=EditRobotState6;
  //end;
end;

procedure TFCoach.WorldToMapDim(dw: double; var dm: integer);
begin
  dm:=round(dw*Dfieldmag);
end;

procedure TFCoach.WorldToMap(xw, yw: double; var xm: integer; var ym: integer);
begin
  xm:=round(FieldImageWidth/2+xw*Dfieldmag);
  ym:=round(FieldImageHeight/2-yw*Dfieldmag);
end;

function TFCoach.WorldToMapP(xw, yw: double): TPoint;
begin
  result.x:=round(FieldImageWidth/2+xw*Dfieldmag);
  result.y:=round(FieldImageHeight/2-yw*Dfieldmag)
end;

procedure TFCoach.MapToWorld(xm, ym: integer; var xw, yw: double);
begin
  xw:=(xm-FieldImageWidth/2)/Dfieldmag;
  yw:=(FieldImageHeight/2-ym)/Dfieldmag;
end;

function TFCoach.Get24Color(cor: integer): Tcolor;
var altColor: integer;
begin
  altColor:=cor;
  if AttackGoal<>gcYellow then begin
    if cor=iYellowGoal then altColor:=iBlueGoal
    else if cor=iBlueGoal then altColor:=iYellowGoal;
  end;
  result:=colorcolor24[altColor];

end;

procedure TFCoach.VVnToVxy(teta,v,vn: double; var Vx,Vy: double);
var ct,st: double;
begin
  ct:=cos(teta);
  st:=sin(teta);
  vx:=v*ct-vn*st;
  vy:=v*st+vn*ct;
end;

procedure TFCoach.ResetView;
var i: integer;
begin
  with View do begin
    FrameTime:=0;
    SendTime:=0;
    EdgesCount:=0;
    RegionsCount:=0;
    OdosCount:=0;

    for i:=0 to CTrackColors-1 do begin
      Centers[i].Count:=0;
    end;
  end;
end;

procedure TFCoach.DrawFieldMap(field_canvas: TCanvas);
var x1,y1,x2,y2,r,d: integer;
begin
  with field_canvas, FieldDims do begin //tcanvas
    //Retângulo do campo
    brush.Style:=bsSolid;
    brush.color:=clgreen;
    pen.color:=clgreen;
    rectangle(0,0,FieldImageWidth,FieldImageHeight);
    //Linha do meio do campo
    WorldToMap(-FieldDepth/2,Fieldwidth/2,x1,y1);
    WorldToMap(FieldDepth/2,-Fieldwidth/2,x2,y2);
    brush.style:=bsClear;
    pen.color:=clLtGray;
    pen.Width:=1;
    Polyline([point((x1+x2) div 2,y1),point((x1+x2) div 2,y2)]);
    //Ângulos do Escanteio
    WorldtoMapdim(CornerDist,d);
    arc(x1-d,y1-d,x1+d,y1+d,x1,y2,x2,y1); //|¨
    arc(x1-d,y2-d,x1+d,y2+d,x2,y2,x1,y1); //¨|
    arc(x2-d,y1-d,x2+d,y1+d,x1,y1,x2,y2); //|_
    arc(x2-d,y2-d,x2+d,y2+d,x2,y1,x1,y2); //_|
    //Limites do campo em preto
    pen.Width:=2;
    pen.color:=clBlack;
    Polyline([WorldToMapP(BoundaryDepth/2,BoundaryWidth/2), WorldToMapP(-BoundaryDepth/2,BoundaryWidth/2),
              WorldToMapP(-BoundaryDepth/2,-BoundaryWidth/2), WorldToMapP(BoundaryDepth/2,-BoundaryWidth/2),
              WorldToMapP(BoundaryDepth/2,BoundaryWidth/2)]);

    //Gol
    pen.color:=clLtGray;
    Polyline([point(x1,y1),point(x2,y1),point(x2,y2),point(x1,y2),point(x1,y1)]);
    //Circulo maior e menor do meio do campo
    WorldtoMapdim(CircleRadius,r);
    ellipse((x1+x2) div 2-r,(y1+y2) div 2-r,(x1+x2) div 2+r,(y1+y2) div 2+r);  // maior
    ellipse((x1+x2) div 2-3,(y1+y2) div 2-3,(x1+x2) div 2+3,(y1+y2) div 2+3);  // menor
    //Pequena e Grande Área de cada lado
    Polyline([WorldToMapP(FieldDepth/2,AreaWidth/2), WorldToMapP(FieldDepth/2-AreaDepth,AreaWidth/2),
             WorldToMapP(FieldDepth/2-AreaDepth,-AreaWidth/2), WorldToMapP(FieldDepth/2,-AreaWidth/2)]);
    Polyline([WorldToMapP(-FieldDepth/2,AreaWidth/2), WorldToMapP(-FieldDepth/2+AreaDepth,AreaWidth/2),
             WorldToMapP(-FieldDepth/2+AreaDepth,-AreaWidth/2), WorldToMapP(-FieldDepth/2,-AreaWidth/2)]);
    Polyline([WorldToMapP(FieldDepth/2,KeeperAreaWidth/2), WorldToMapP(FieldDepth/2-KeeperAreaDepth,KeeperAreaWidth/2),
             WorldToMapP(FieldDepth/2-KeeperAreaDepth,-KeeperAreaWidth/2), WorldToMapP(FieldDepth/2,-KeeperAreaWidth/2)]);
    Polyline([WorldToMapP(-FieldDepth/2,KeeperAreaWidth/2), WorldToMapP(-FieldDepth/2+KeeperAreaDepth,KeeperAreaWidth/2),
             WorldToMapP(-FieldDepth/2+KeeperAreaDepth,-KeeperAreaWidth/2), WorldToMapP(-FieldDepth/2,-KeeperAreaWidth/2)]);
    //Gol Amarelo (Direita)
    pen.color:=Get24Color(iYellowGoal);
    Polyline([WorldToMapP(FieldDepth/2,GoalWidth/2),
              WorldToMapP(FieldDepth/2+GoalDepth,GoalWidth/2),
              WorldToMapP(FieldDepth/2+GoalDepth,-GoalWidth/2),
              WorldToMapP(FieldDepth/2,-GoalWidth/2)]);
    //Gol Azul (Esquerda)
    pen.color:=Get24Color(iBlueGoal);
    Polyline([WorldToMapP(-FieldDepth/2,GoalWidth/2),
              WorldToMapP(-FieldDepth/2-GoalDepth,GoalWidth/2),
              WorldToMapP(-FieldDepth/2-GoalDepth,-GoalWidth/2),
              WorldToMapP(-FieldDepth/2,-GoalWidth/2)]);
    pen.Width:=1;

    //Linha de Direção do Ataque
    pen.color:=clLtGray;
    pen.Width:=3;
    WorldToMap((-FieldDepth)/10,(Fieldwidth+3)/2,x1,y1);
    WorldToMap((FieldDepth)/10,(Fieldwidth+3)/2,x2,y2);
    Polyline([point(x1,y1),point(x2,y2)]);
    //Seta de Direção do Ataque
    pen.color:=clLtGray;
    pen.Width:=3;
    WorldToMap((FieldDepth)/10,(Fieldwidth+3)/2,x2,y2);
    Polyline([point((x2-7),(y2-7)),point(x2,y2)]);
    Polyline([point((x2-7),(y2+7)),point(x2,y2)]);
  end;
end;

procedure TFCoach.DrawBall(field_canvas: TCanvas; var BS: TBallState; var RS: TRobotState; i: integer);
var r,rx,ry: double;
    BallColor: TColor;
    touched: boolean;
    x1,y1,x2,y2,x3,y3,x4,y4: integer;
begin
  r:=0.15;
  with field_canvas do begin //tcanvas
    case i of
    0:brush.color:=clAqua;
    1:brush.color:=clGray;
    2:brush.color:=clYellow;
    3:brush.color:=clSilver;
    4:brush.color:=clblue;
    end;

    if (i<>5) then begin
    BallColor:=brush.color;
    brush.style:=bsSolid;
    pen.color:=BallColor;

    WorldToMap(BS.x-r,BS.y+r,x1,y1);
    WorldToMap(BS.x+r,BS.y-r,x2,y2);
    ellipse(rect(x1,y1,x2,y2));

    WorldToMap(BS.x,BS.y,x1,y1);
    WorldToMap(BS.x+BS.vx*0.5,BS.y+BS.vy*0.5,x2,y2);
    moveto(x1,y1);
    lineto(x2,y2);
    font.Color:=clWhite;
    brush.style:=bsClear;
    textout(x2,y2,inttostr(round(BS.quality)));

    rx:=sqrt(BS.cov_x);
    if rx>5 then rx:=5;
    ry:=sqrt(BS.cov_y);
    if ry>5 then ry:=5;
    WorldToMap(BS.x-rx,BS.y+ry,x1,y1);
    WorldToMap(BS.x+rx,BS.y-ry,x2,y2);
    ellipse(rect(x1,y1,x2,y2));


    touched:=RS.withball;

    if touched=true then begin
       BallState.touched:=true;
    end else if (touched=false) then begin
       BallState.touched:=false;
    end;
    end;
  end;
end;

procedure TFCoach.DrawBallM(field_canvas: TCanvas; var BS: TBallState; BallColor: Tcolor);
var r,rx,ry: double;
    touched: boolean;
    x1,y1,x2,y2,x3,y3,x4,y4: integer;
begin
  r:=0.15;

  touched:=BallState.touched;

  with field_canvas do begin //tcanvas
    if (touched=false) then begin

       brush.color:=clred;
       BallColor:=brush.color;
       brush.style:=bsSolid;
       pen.color:=BallColor;

       WorldToMap(BS.x-r,BS.y+r,x1,y1);
       WorldToMap(BS.x+r,BS.y-r,x2,y2);
       ellipse(rect(x1,y1,x2,y2));

    end else if (touched=true) then begin
       brush.color:=clred;
       BallColor:=brush.color;
       brush.style:=bsSolid;
       pen.color:=BallColor;

       WorldToMap(BS.x-r,BS.y+r,x1,y1);
       WorldToMap(BS.x+r,BS.y-r,x2,y2);
       ellipse(rect(x1,y1,x2,y2));

       brush.style:=bsClear;
       pen.color:=clblack;
       pen.Width:=3;
       ellipse(rect(x1,y1,x2,y2));

       brush.color:=clred;
       BallColor:=brush.color;
       brush.style:=bsSolid;
       pen.color:=BallColor;

       WorldToMap(BS.x,BS.y,x1,y1);
       WorldToMap(BS.x,BS.y,x2,y2);
       ellipse(rect(x1,y1,x2,y2));
    end;

    WorldToMap(BS.x,BS.y,x1,y1);
    WorldToMap(BS.x+BS.vx*0.5,BS.y+BS.vy*0.5,x2,y2);
    moveto(x1,y1);
    lineto(x2,y2);
    font.Color:=clWhite;
    brush.style:=bsClear;
    textout(x2,y2,inttostr(round(BS.quality)));

    rx:=sqrt(BS.cov_x);
    if rx>5 then rx:=5;
    ry:=sqrt(BS.cov_y);
    if ry>5 then ry:=5;
    WorldToMap(BS.x-rx,BS.y+ry,x1,y1);
    WorldToMap(BS.x+rx,BS.y-ry,x2,y2);
    ellipse(rect(x1,y1,x2,y2));
  end;
end;

procedure TFCoach.DrawObstacles(var Obs: TObstacles);
var r: double;
    x1,y1,x2,y2: integer;
    i: integer;
begin
  r:=0.10;
  with ImageMap.canvas do begin //tcanvas
    for i:=0 to Obs.count-1 do begin
      brush.color:=colorcolor24[Obs.centers[i].color];
      brush.style:=bsSolid;
      pen.color:=clblack;

      WorldToMap(Obs.centers[i].xw-r,Obs.centers[i].yw+r,x1,y1);
      WorldToMap(Obs.centers[i].xw+r,Obs.centers[i].yw-r,x2,y2);
      rectangle(rect(x1,y1,x2,y2));
      font.Color:=clWhite;
      brush.style:=bsClear;
      textout(x2,y2,inttostr(round(1000*Obs.centers[i].quality)));
    end;
  end;
end;


initialization
  {$I coach.lrs}
   RobotState[0].Vbatery:=0;
   RobotState[1].Vbatery:=0;
   RobotState[2].Vbatery:=0;
   RobotState[3].Vbatery:=0;
   RobotState[4].Vbatery:=0;

end.

