unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, LResources,
  StdCtrls, math, RLan, Menus, dynmatrix, Types, IniFiles,
  CheckLst, ExtCtrls, ComCtrls, Grids, IniPropStorage,
  lNetComponents, lNet, Roles, DecConsts, WLan,KalmanBall,KalmanBall_Aux;

const
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

  //MaxObstacles=32;
  MaxObstacles=61; //numero de raios do Ovis!!!! Heber
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
    quality:integer;
    ro,teta,d: double;
    xw,yw: double;
    Vxw,Vyw:double;
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
    //x,y: integer;
    color: integer;
    size: integer;
    teta,ro: double;
    xw,yw,d: double;
  end;

  TOdo= record
//    speed: array[0..2] of integer;
    speedw: array[0..2] of double;
    dwheel: array[0..2] of double;
    RobotDelU, RobotDelUn, RobotDelTeta:double;
    RobotSpeedV, RobotSpeedVn, RobotSpeedW: double;
    Avaible: boolean;
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

    Compass: double;
    CompassAvaible: boolean;

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
    vote:integer;
    xcoach,ycoach: double;
    coachquality: double;
  end;
  
  TRobotState=record
    valid:boolean;
    x,y,teta: double;
    LocalizationAvailable: boolean; //indica se o robô está perdido
    LocAdvV, LocAdvVn, LocAdvW: double; //indica para onde a localizacao quer mandar o robô
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
    x,y,r: double;
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

  TStressState=record
    BufErr:array[0..Nsim-1] of double;
    ValOn,treshold: double;
    rand1: double;
    Sum_err: double;
    idx: integer;
    LastFlagStress, FlagStress: boolean;
  end;

  { TFMain }

  TFMain = class(TForm)
    BSetRole: TButton;
    BStopReference: TButton;
    Button_ShowLocalization: TButton;
    CBDefRole: TComboBox;
    CBISTScenario: TCheckBox;
    CBDrone: TCheckBox;
    CBStartRole: TComboBox;
    CBKick: TCheckBox;
    CBLocalization: TCheckBox;
    CBUseCompass: TCheckBox;
    CBSimTwo: TCheckBox;
    CBShowLogGraph: TCheckBox;
    CBUisGK: TCheckBox;
    CheckBoxKickBehindMidField: TCheckBox;
    CheckBox_NoCamera: TCheckBox;
    Edit1: TEdit;
    EditPlayName: TEdit;
    EditRoleName: TEdit;
    EditDebugPosition: TEdit;
    EditTaskName: TEdit;
    EditTimes: TEdit;
    FormStorage: TIniPropStorage;
    GroupBox3: TGroupBox;
    ImageShow: TPaintBox;
    ImageMap: TPaintBox;
    Label9: TLabel;
    MI_BallEst: TMenuItem;
    MenuItem_Joystick: TMenuItem;
    MenuItem_Hardware: TMenuItem;
    RG_BallSelection: TRadioGroup;
    RadioGroup_temp: TRadioGroup;
    SdpoUDPSimTwo: TLUDPComponent;
    MemoActionPars: TMemo;
    SdpoUDP: TLUDPComponent;
    SdpoUDPSuper: TLUDPComponent;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuWindows: TMenuItem;
    RGController: TRadioGroup;
    MenuExit: TMenuItem;
    N1: TMenuItem;
    MenuAbout: TMenuItem;
    TabDebug: TTabSheet;
    Timer_NoCamera: TTimer;
    TimerSimulator: TTimer;
    GBMainControl: TGroupBox;
    CBSimulator: TCheckBox;
    CBstress: TCheckBox;
    RGRobotSel: TRadioGroup;
    GBReferences: TGroupBox;
    Label24: TLabel;
    Label23: TLabel;
    Label22: TLabel;
    BSetXYTeta: TButton;
    Label27: TLabel;
    Label28: TLabel;
    Label1: TLabel;
    EditV: TEdit;
    EditVn: TEdit;
    EditW: TEdit;
    EditRobotX: TEdit;
    EditRobotY: TEdit;
    EditRobotTeta: TEdit;
    PPGMain: TPageControl;
    TSShow: TTabSheet;
    CBShow: TCheckBox;
    CBShowHeader: TCheckBox;
    CBShowCenters: TCheckBox;
    CBShowEdges: TCheckBox;
    CBShowRegions: TCheckBox;
    CBShowField: TCheckBox;
    CBShowRadar: TCheckBox;
    CBShowPoles: TCheckBox;
    CBShowLog: TCheckBox;
    CBGlobHist: TCheckBox;
    CBTraj: TCheckBox;
    CBGoalLines: TCheckBox;
    CLBShowValues: TCheckListBox;
    Info: TTabSheet;
    MemoRegions: TMemo;
    EditStresses: TEdit;
    Label7: TLabel;
    EditGameState: TEdit;
    LblStress: TLabel;
    EditInfo: TEdit;
    Editinfo2: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label13: TLabel;
    Editdrivereset: TEdit;
    Memo1: TMemo;
    Btnradar: TButton;
    Tablogs: TTabSheet;
    GroupBox1: TGroupBox;
    EditFileudplog: TEdit;
    Label2: TLabel;
    CBsavelogs: TCheckBox;
    Memoudp: TMemo;
    TimerUdplog: TTimer;
    CBudp: TCheckBox;
    Editinfo3: TEdit;
    Label15: TLabel;
    lblpack: TLabel;
    Editpwm1: TEdit;
    Editpwm2: TEdit;
    Editpwm3: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    RGmode: TRadioGroup;
    EditDebugShow: TEdit;
    CBShowGoodStripes: TCheckBox;
    procedure BSetRoleClick(Sender: TObject);
    procedure BStopReferenceClick(Sender: TObject);
    procedure Button_ShowLocalizationClick(Sender: TObject);
    procedure CBDefRoleClick(Sender: TObject);
    procedure CBSimTwoClick(Sender: TObject);
    procedure CheckBox_NoCameraChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem_HardwareClick(Sender: TObject);
    procedure MenuItem_JoystickClick(Sender: TObject);
    procedure MI_BallEstClick(Sender: TObject);
    procedure RGRobotSelClick(Sender: TObject);
    procedure SdpoUDPReceive(aSocket: TLSocket);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BSetXYTetaClick(Sender: TObject);
    procedure MenuWinDefaultClick(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure ImageMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageShowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CBSimulatorClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure SdpoUDPSimTwoError(const msg: string; aSocket: TLSocket);
    procedure SdpoUDPSimTwoReceive(aSocket: TLSocket);
    procedure SdpoUDPSuperError(const msg: string; aSocket: TLSocket);
    procedure SdpoUDPSuperReceive(aSocket: TLSocket);
    procedure TimerSimulatorTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure btnrunudplogsClick(Sender: TObject);
    procedure TimerUdplogTimer(Sender: TObject);
    procedure CBudpClick(Sender: TObject);
    procedure Timer_NoCameraTimer(Sender: TObject);

  private
//// por Scolari   16/11/04
    procedure Omin_pwm(pwm1,pwm2,pwm3: integer);
    procedure Omin_Ticks(v,vn,w: double; pwm1,pwm2,pwm3: integer);
    procedure Omin_Ticks2(v,vn,w: double; out w1,w2,w3: double);//Heber função provisoria de adaptação da nova estrutura
    procedure ParseLanEdges;
////////////////
    procedure FillShowValuesItem(GT: Tstrings; itemName, formatString: string; value: double);
    procedure ResetView;

    procedure OminSim(v,vn,w: double; var R: TRobotState);

    procedure ShowEdges;
    procedure UpdateCompass(var RState: TRobotState);
    procedure UpdateCompass2;
    procedure UpdateEdges;

    procedure NewMainControl;
    procedure SendPlayerInfo;
    procedure SimMainLoop;
    procedure SimTwoMainLoop;
    procedure ISTMainLoop;
    procedure udpmainloop;
    procedure ManualControl(var V, Vn, W: double; var Kick: boolean);
    procedure saveudplogs(filename,s: string);

  public

    NetTime: DWORD;
    check : integer;
    countResetDrive: array[0..3] of integer;
    ResetDrive: array[0..3] of boolean;

    m_speed,mpos :TDMatrix;

    Ser1_Data, Ser2_Data: string;
    Ser1_Out, Ser2_Out: string;
    NetBuffer,NetOutBuffer: TUDPBuffer;

    SLShowValues: TStrings;
    TempStream : TMemoryStream;
    Good_image: boolean;
    StrTream: TStringStream;
    Gtext: TStringList;
    deb_txt: string;
    cycleCount: integer;
    DataDir: string;
    AuxForms: array[0..MaxAuxForms-1] of TForm;
    NumAuxForms: integer;

    ActOdo, LastOdo:TAbsOdo;
    ActFreeOdo, LastFreeOdo:TAbsOdo;

    VBatCount,VBatAge: integer;

    Act_image_num, Last_image_num, Last_image_block: integer;

    ref_v,ref_w: double;
    filt_ev,filt_ew: double;

    stFindDir, stInThisAction: integer;
    stReqTeta: double;
    down_x,down_y: integer;
    draw_x,draw_y: integer;
    Dbl_x,Dbl_y: integer;

    function LowPassFilterV(meassure,oldmeassure: double; Falpha: double): double;

    procedure ProcessCoachPacket(packet_str: string);
    procedure ProcessSimTwoMsg(Data: string);
    procedure ProcessMermaidMsg(Data: string);
    procedure InsertAuxForms(Fm: TForm; cap: string);
    procedure AuxFormClosed(cap: string);
    procedure SaveAuxForms;
    procedure CloseAuxForms;
    procedure RestoreAuxForms;
    procedure NetSendAll;
    procedure ShowLanHeader(var mess: string; St: Tstringlist);
    procedure ShowLanCentersText(var mess: string; St: Tstrings);
    procedure ShowLanRegionsText(var mess: string; St: Tstringlist);

    procedure ShowAll;
    procedure ShowCenters;
    procedure ShowRadar;
    procedure ShowRegions;
    procedure FillShowValues(GT: TStrings; var RS: TRobotState; var BS: TBallState; var View: TView);
    procedure MemoRegionsToArray;
    procedure LoadRegions;
    procedure DrawFieldMap(field_canvas: TCanvas);
    procedure DrawBall(field_canvas: TCanvas; var BS: TBallState; BallColor: Tcolor);
    procedure DrawObstacles(var Obs: TObstacles);

    procedure MainLoop;
    procedure ParseLanMess;
    procedure ParseLanClusters;
    procedure ParseLanSerials;
    procedure ParseLanRadar;

    procedure UpdateOdos;
    procedure UpdateOdos2;
    procedure UpdateCenters(icenters: iColorSet);
    procedure UpdateRadar;
    procedure UpdateObstacles;
    procedure PropagateObstacles;
    procedure AddObstacle(xw, yw, teta: double; ObstacleColor: integer; var Obstacles: TObstacles);
    procedure UpdateWorldState(var OldRobotState,NewRobotState: TRobotState);
    procedure CheckLines;
    procedure CheckLine(var ELine:TEdgeLine);
    procedure ShowLine(var ELine:TEdgeLine);

    procedure JoyControl(var v,vn,w: double; var kick:boolean);
    procedure ExtractBallFromCenters(var ObsBallState: TBallState);
    procedure ExtractCenterBallFromCenters(var ObsBallCenter: Tcenter);

    procedure MergeBallState(var BS,OBS: TBallState);
    procedure Localize(var RS: TRobotState);
    procedure UpdateLocalizationData(NewView: TView; var RS: TRobotState);
    procedure getTacticComand(var v,w,vn:double);

    procedure ControlAccelerationNLTMD(out V:double;out Vn:double;out W:double);
    procedure PropagateXYBall(var BS: TBallState);
    procedure PropagateBallQualityDecay(var BS: TBallState);
  end;

var
  FMain: TFMain;

  sigtemp:TDMatrix;
  sigver:TDMatrix;
  sigtemp0:TDMatrix;
  sigtemp1:TDMatrix;
  sigtemp2:TDMatrix;
  sigtemp3:TDMatrix;
  sigtemp4:TDMatrix;
  sigmaT:TDMatrix;
  siga,sigb,sigc,sigd: double;
  Sr,Sphi: double;

  fastball: boolean=FALSE;
  bigball: double = 0;
  flagForm: boolean=false;

  // true if the program is running in "Coach Mode"
  CoachMode: boolean;
  
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
  StressState: array[0..2] of TStressState;
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
  ObsBallState,ObsBallStateEstimated, LastObsBallState: TBallState;
  ObsBallCenter:Tcenter;
  NetBallState: TBallState;
  Obstacles: TObstacles;
  WGLine,GWLine: TEdgeLine;
  View, LastView: TView;
  MouseControlX,MouseControlY,MouseControlTeta: double;
  MouseControlValid: boolean=false;
  FieldImageWidth, FieldImageHeight: integer;
  LogFile: text;
  LogString: string;
  LRState: array[0..MaxRobots-1] of TRobotState;
  max_linear_acceleration:double;
  max_angular_acceleration:double;

procedure WorldToMapDim(dw: double; var dm: integer);
procedure WorldToMap(xw,yw: double; var xm: integer; var ym: integer);
function  WorldToMapP(xw,yw: double):TPoint;
procedure MapToWorld( xm, ym: integer; var xw,yw: double);
function Get24Color(color: integer): Tcolor;
function CLBGetChecks(CLB: TCheckListBox): string;
procedure CLBSetChecks(CLB: TCheckListBox; checks: string);
procedure StringsDraw(Cnv: TCanvas; SL: TStrings; var X,Y : integer);
procedure CalcBallSpeed(var B,LB: TBallState);
procedure ShowWorldClusters(Cnv: TCanvas);

implementation

uses Utils, Camera, Param, Log, Robots, kicker,
     Joy,Field, LCLType, omni3, Tactic, Actions, Tasks, CoachMain, localization,
     LocMap, Unit_Hardware, Unit_joystick, unit_Localization;

//--------------------------------------------------------------------------------------------
// LAN

procedure TFMain.ParseLanMess;
begin
  ParseLanClusters;
  ParseLanEdges;
  ParseLanRadar;
end;

procedure TFMain.ParseLanSerials;
var id: char;
    str: string;
begin
  id:=chr(NetPeekByte(NetBuffer));
  if id<>'S' then exit;

  NetGetByte(NetBuffer);
  str:=NetGetString(NetBuffer);
  Daisy.ProcessRawData(str);
end;


procedure TFMain.ParseLanClusters;
var
  ncolor,tempcolor,ccolor: integer;
  icluster: integer;
  id:char;
begin
  id:=chr(NetPeekByte(NetBuffer));
  if id<>'C' then exit;
  NetGetByte(NetBuffer);

  ncolor:=NetGetByte(NetBuffer);

  for tempcolor:=0 to ncolor-1 do begin
    ccolor:=tempcolor;
    if AttackGoal<>gcYellow then begin
      if tempcolor=iYellowGoal then ccolor:=iBlueGoal
      else if tempcolor=iBlueGoal then ccolor:=iYellowGoal;
    end;
    
    view.centers[ccolor].count:=NetGetWord(NetBuffer);
    
    for icluster:=0 to view.centers[ccolor].count-1 do begin
      with view.centers[ccolor].data[icluster] do begin
        area:=round(NetGetFloat(NetBuffer));

        tetaMax:=NetGetAngle(NetBuffer);
        tetaMin:=NetGetAngle(NetBuffer);
        roMax:=NetGetAngle(NetBuffer);
        roMin:=NetGetAngle(NetBuffer);
        teta:=NetGetAngle(NetBuffer);
        ro:=NetGetAngle(NetBuffer);
      end;
    end;
  end;
end;

procedure TFMain.ParseLanEdges;
var id: char;
    i,nEdges,color1_2: integer;
begin
  id:=chr(NetPeekByte(NetBuffer));
  if id<>'e' then exit;

  NetGetByte(NetBuffer);

  nEdges:=NetGetWord(NetBuffer);

  for i:=0 to nEdges-1 do begin

    with View.Edges[i] do begin
      lineNum:=NetGetByte(NetBuffer);
      color1_2:=NetGetByte(NetBuffer);
      color1:=(color1_2 shr 4) and $0F;
      color2:=color1_2 and $0F;
      teta:=NetGetAngle(NetBuffer);
      ro:=NetGetAngle(NetBuffer);
    end;
  end;
  View.EdgesCount:=nEdges;
  EditDebugShow.text:=format(' numEdges2: %d',[nEdges]);
end;

procedure TFMain.ParseLanRadar;
var id: char;
    i,nRays: integer;
begin
  id:=chr(NetPeekByte(NetBuffer));
  if id<>'R' then exit;

  NetGetByte(NetBuffer);


  nRays:=NetGetWord(NetBuffer);

  for i:=0 to nRays-1 do begin

    with View.Radar[i] do begin
      lineNum:=NetGetByte(NetBuffer);
      color:=NetGetByte(NetBuffer);
      size:=NetGetByte(NetBuffer);
      teta:=NetGetAngle(NetBuffer);
      ro:=NetGetAngle(NetBuffer);
    end;
  end;
  View.RadarRaysCount:=nRays;
end;


procedure TFmain.NetSendAll;
begin
  ClearUDPBuffer(NetOutBuffer);
  NetPutByte(NetOutBuffer,ord('1'));
  NetPutString(NetOutBuffer,ser1_out);
  ser1_out:='';

  NetPutByte(NetOutBuffer,ord('2'));
  NetPutString(NetOutBuffer,ser2_out);
  ser2_out:='';
  SdpoUDP.Send(NetOutBuffer.data,NetOutBuffer.MessSize,FParam.EditVisionIP.text+':6969');
end;

//------------------------------------------------------------------
// Main Loop

procedure TFMain.SdpoUDPReceive(aSocket: TLSocket);
var
  data: string;
  NumberBytes,i: integer;
begin
  try
    SdpoUDP.GetMessage(data);

    if CBSimulator.Checked then exit;

    zeromemory(@(NetBuffer.data[0]),UDPBufSize);
    NumberBytes:=length(data);

    if NumberBytes<UDPBufSize then begin
      NetBuffer.MessSize:=NumberBytes;
      NetBuffer.ReadDisp:=0;
      copymemory(@(NetBuffer.data[0]),@(data[1]),NumberBytes);
      MainLoop;
    end;

//////// UDP Logs //////
   if CBsavelogs.Checked = true then
   begin
     saveudplogs(EditFileudplog.text,data);
   end;
     except
      on E: Exception do Editinfo3.text:=E.message;
  end;

end;

procedure TFMain.ResetView;
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

procedure TFMain.UpdateCompass(var RState: TRobotState);
var teta_filt: double;
begin
  with RState do begin

   exit;
   compass_active:=KickerState.CompassActive;
    if not compass_active then begin
      FLocMap.Label2.Caption:='Stalled';
      exit;
    end;
    Form_Hardware.GetCompass(compass);
     compass:=NormalizeAngle(compass);

    FLocMap.Label2.Caption:=format('Teta %.2f ; Compass %.2f;',[radtodeg(teta), radtodeg(compass)]) ;
    if abs(DiffAngle(teta,compass))>pi/2 then begin
      FLocMap.Label2.Caption:=format('Jump: Teta %.2f ; Compass %.2f;',[radtodeg(teta), radtodeg(compass)]) ;
      teta:=NormalizeAngle(teta+pi);
    end else begin
      FLocMap.Label2.Caption:=format('Teta %.2f ; Compass %.2f;',[radtodeg(teta), radtodeg(compass)]) ;
    end;
  end;
end;

procedure TFMain.UpdateCompass2;
begin
  View.CompassAvaible:=Form_Hardware.GetCompass(View.Compass);
  View.Compass:=NormalizeAngle(View.Compass);
end;

procedure TFMain.Localize(var RS: TRobotState);
var  i,j: integer;
    CurLocalization: TRLocState;
    FiltLocalization: double;
    VPointList: TPointList;
    VarPos: TPos;
    Accept: boolean;
begin
    //compass

  //Last Known Position
  with CurLocalization do begin
    rpos.x:=RS.x;
    rpos.y:=RS.y;
    rpos.teta:=RS.teta;
    dodos.x:=View.DOdos.x;
    dodos.y:=View.DOdos.y;
    dodos.teta:=View.DOdos.teta;
    VarPos.x:=RS.cov_x;
    VarPos.y:=RS.cov_y;
    VarPos.teta:=RS.cov_teta;
  end;

  //List of seen points
  if CBUisGK.Checked then begin
    VPointList.PCount:=0;
    if View.EdgesCount>0 then begin
      for i:=0 to View.EdgesCount-1 do begin
        if Dist(View.Edges[i].xw,View.Edges[i].yw)<2 then begin
          VPointList.PList[VPointList.PCount].x:=View.Edges[i].xw;
          VPointList.PList[VPointList.PCount].y:=View.Edges[i].yw;
          VPointList.PCount:=VPointList.PCount+1;
        end;
      end;
    end;
  end
  else begin
    VPointList.PCount:=0;
    if View.EdgesCount>0 then begin
      VPointList.PCount:=View.EdgesCount;
      for i:=0 to VPointList.PCount-1 do begin
        VPointList.PList[i].x:=View.Edges[i].xw;
        VPointList.PList[i].y:=View.Edges[i].yw;
      end;
    end;
  end;

  //localization
  if VPointList.PCount > 5 then
    locRobot(CurLocalization, VPointList);

  //fusion
  if not JumpFlag then begin
    EstimateFusion(CurLocalization.rpos, CurLocalization.dodos, CurLocalization.loc, CurLocalization.VarPos, CurLocalization.cov);
    FLocMap.Label3.Caption:='true '+IntToStr(jumps);
  end else begin
    JumpFlag:=false;
    FLocMap.Label3.Caption:='false';
  end;

  //actualization
  RS.cov_x:=CurLocalization.VarPos.x;
  RS.cov_y:=CurLocalization.VarPos.y;
  RS.cov_teta:=CurLocalization.VarPos.teta;

  if VPointList.PCount>10 then begin
    with RS do begin
      x := CurLocalization.rpos.x;
      y := CurLocalization.rpos.y;
      teta := CurLocalization.rpos.teta;
    end;
  end;

  //Drawing
  if CBShow.Checked then
    DrawLocalizationData(CurLocalization,VPointList);
end;

procedure TFMain.UpdateLocalizationData(NewView: TView; var RS: TRobotState);
var
  NewInputData: TLocInputData;
  NewOutputData: TLocOutputData;
  i: integer;
begin
  NewInputData.VPointList.PCount:=0;
  for i:=0 to NewView.EdgesCount-1 do begin
    if NewInputData.VPointList.PCount<(Length(NewInputData.VPointList.PList)-1) then begin
      NewInputData.VPointList.PList[i].x:=NewView.Edges[i].xw;
      NewInputData.VPointList.PList[i].y:=NewView.Edges[i].yw;
      inc(NewInputData.VPointList.PCount);
    end
    else begin
      break;
    end;
  end;
  NewInputData.OdosData.Avaible:=NewView.Odos[0].Avaible;
  NewInputData.OdosData.dv:=NewView.Odos[0].RobotDelU;
  NewInputData.OdosData.dvn:=NewView.Odos[0].RobotDelUn;
  NewInputData.OdosData.dteta:=NewView.Odos[0].RobotDelTeta;
  NewInputData.OdosData.v:=NewView.Odos[0].RobotSpeedV;
  NewInputData.OdosData.vn:=NewView.Odos[0].RobotSpeedVn;
  NewInputData.OdosData.w:=NewView.Odos[0].RobotSpeedW;
  NewInputData.Compass.Angle:=NewView.Compass;
  NewInputData.Compass.Avaible:=NewView.CompassAvaible;
  Form_Localization.SetInputData(NewInputData);
  NewOutputData:=Form_Localization.GetOutputData;
  RS.x:=NewOutputData.x;
  RS.y:=NewOutputData.y;
  RS.teta:=NewOutputData.teta;
  RS.LocalizationAvailable:=NewOutputData.Avaible;
  Rs.LocAdvV:=NewOutputData.LocAdvV;
  Rs.LocAdvVn:=NewOutputData.LocAdvVn;
  Rs.LocAdvW:=NewOutputData.LocAdvW;
end;

procedure TFMain.getTacticComand(var v, w, vn: double);
begin

end;

function calcvprop(v:double):double;
begin
    if abs(v)>2 then Result:=0.4
    else if (abs(v)>1.5) and (abs(v)<=2) then Result:=0.1
    else if (abs(v)>1) and (abs(v)<=1.5) then Result:=0.1
    else if (abs(v)>0.5) and (abs(v)<=1) then Result:=0.1
    else if abs(v)<=0.5 then Result:=0.1;
end;

procedure TFMain.MainLoop;
var  i,j: integer;
    CurLocalization: TRLocState;
    FiltLocalization,vxltemp,vyltemp,vpropx,vpropy: double;
    VPointList: TPointList;
    filterIter:boolean;
    qualTemp:double;
begin
  if RadioGroup_temp.ItemIndex=0 then begin
    NetTime:=getTickcount();
    deb_txt:='';

    ResetView;
    ParseLanMess;
    UpdateEdges;
    UpdateOdos;

    UpdateCenters([iBallColor,iPurpleTeam,iCyanTeam]);
    UpdateObstacles;
    PropagateObstacles;

    for i:=0 to MaxRobots-1 do begin
      if i=mynumber then begin
        PropagateXYTeta(RobotState[i]);
        //PropagateXYTetaBoa(RobotState[i]);
      end else begin
        PropagateXYTetaOthers(RobotState[i]);
      end;
    end;

    ltimebegin:=getTickcount();
    if CBLocalization.Checked then
      Localize(RobotState[myNumber]);

    UpdateRadar;

    if (CBUseCompass.Checked) then
      UpdateCompass(RobotState[myNumber]);

    FLocMap.CBMShow.Caption:=format('Show %3d',[getTickcount()-ltimebegin]);

    // ultimo estado da bola
    LastBallState:=BallState;
    // ultima bola observada
    LastObsBallState:=ObsBallState;
    // Quality decay
    PropagateXYBall(BallState);
    ExtractBallFromCenters(ObsBallState);
    //Depois de extraída só é boa se          //MUDAR CONFORME ABAIXO!!!


    if ObsBallState.quality>=400 then begin
      BallState.vx:=(BallState.x-LastBallState.x)/0.04;
      BallState.vy:=(BallState.y-LastBallState.y)/0.04;

      vpropx:=calcvprop(BallState.vxl);
      vpropy:=calcvprop(BallState.vyl);

      vxltemp:=((ObsBallState.xl-LastObsBallState.xl)/0.04);//*vpropx;
      vyltemp:=((ObsBallState.yl-LastObsBallState.yl)/0.04);//*vpropy;

      RotateAndTranslate(BallState.vxl,BallState.vyl,vxltemp,vyltemp,0,0,RobotState[myNumber].teta);

      BallState.vxl:=BallState.vxl-RobotState[mynumber].vx;
      BallState.vyl:=BallState.vyl-RobotState[mynumber].vy;

      BallState.estx:=LowPassFilterV(BallState.x,LastBallState.estx, 0.1);
      BallState.esty:=LowPassFilterV(BallState.y,LastBallState.esty, 0.1);
      BallState.vxl:=LowPassFilterV(BallState.vxl,LastBallState.vxl, 0.05);
      BallState.vyl:=LowPassFilterV(BallState.vyl,LastBallState.vyl, 0.05);

      MergeBallState(BallState,ObsBallState);
    end else begin
      BallState.vxl:=0;
      BallState.vyl:=0;
    end;

    timebegin:=getTickcount();

    NewMainControl;
    deb_txt:=deb_txt+format('%3d',[getTickcount()-NetTime]);

  end else begin
    NetTime:=getTickcount();
    deb_txt:='';

    ResetView;
    ParseLanMess;
    UpdateEdges;
    UpdateOdos2;
    UpdateCompass2;

    UpdateCenters([iBallColor,iPurpleTeam,iCyanTeam]);
    UpdateObstacles;
    PropagateObstacles;
    // preciso ver isto (isto já está feito)

    PropagateXYTetaboa(RobotState[myNumber]);

    UpdateLocalizationData(View, RobotState[myNumber]);

    UpdateRadar;

    // BOLA
    if RG_BallSelection.ItemIndex=0 then begin
      // ultimo estado da bola
      LastBallState:=BallState;
      // ultima bola observada
      LastObsBallState:=ObsBallState;
      // Quality decay
      PropagateXYBall(BallState);
      ExtractBallFromCenters(ObsBallState);
      //Depois de extraída só é boa se
      if ObsBallState.quality>=400 then begin

        BallState.vx:=(BallState.x-LastBallState.x)/0.04;
        BallState.vy:=(BallState.y-LastBallState.y)/0.04;

        //Martelada para correcao da escala do vx e vy local
        vpropx:=calcvprop(BallState.vxl);
        vpropy:=calcvprop(BallState.vyl);

        vxltemp:=((ObsBallState.xl-LastObsBallState.xl)/0.04);//*vpropx;
        vyltemp:=((ObsBallState.yl-LastObsBallState.yl)/0.04);//*vpropy;

        RotateAndTranslate(BallState.vxl,BallState.vyl,vxltemp,vyltemp,0,0,RobotState[myNumber].teta);

        BallState.vxl:=BallState.vxl+RobotState[mynumber].vx;
        BallState.vyl:=BallState.vyl+RobotState[mynumber].vy;

        BallState.estx:=LowPassFilterV(BallState.x,LastBallState.estx, 0.1);
        BallState.esty:=LowPassFilterV(BallState.y,LastBallState.esty, 0.1);

        BallState.vxl:=LowPassFilterV(BallState.vxl,LastBallState.vxl, 0.01);
        BallState.vyl:=LowPassFilterV(BallState.vyl,LastBallState.vyl, 0.01);

        MergeBallState(BallState,ObsBallState);
      end else begin
        BallState.vxl:=0;
        BallState.vyl:=0;
      end;
    end else if RG_BallSelection.ItemIndex=1 then begin
      // ultimo estado da bola
      LastBallState:=BallState;
      // ultima bola observada
      LastObsBallState:=ObsBallStateEstimated;
      // Quality decay
      PropagateBallQualityDecay(BallState);
      //Extract Ball from Centers
      ExtractCenterBallFromCenters(ObsBallCenter);
      //Runs the Ball estimation_ Kalman Filter
      ObsBallStateEstimated.quality:=ObsBallCenter.quality;
      // as variáveis estimadas estão em ObsBallEstimated x_n,y_n,vx_n,vy_n. Tudo relativo ao robot
      FBall.estimateBallPosition(BallState.quality,View.Odos[0].RobotDelU,View.Odos[0].RobotDelUn,View.Odos[0].RobotDelTeta,
                                 ObsBallCenter.ro,ObsBallCenter.teta,ObsBallCenter.quality,ObsBallStateEstimated.x_n,
                                 ObsBallStateEstimated.y_n,ObsBallStateEstimated.vx_n,ObsBallStateEstimated.vy_n,filterIter);
      //Faz a rotação e translação
      if ObsBallStateEstimated.quality>0 then begin
        RotateAndTranslate1(ObsBallStateEstimated.x,ObsBallStateEstimated.y,ObsBallStateEstimated.vx,ObsBallStateEstimated.vy,
                            ObsBallStateEstimated.x_n,ObsBallStateEstimated.y_n,ObsBallStateEstimated.vx_n,ObsBallStateEstimated.vy_n,
                            RobotState[myNumber].x,RobotState[myNumber].y,RobotState[myNumber].teta,
                            RobotState[myNumber].v,RobotState[myNumber].vn);
        // calcula o proximo valor de x e y
        calcNextBallXY(ObsBallStateEstimated.x_next,ObsBallStateEstimated.y_next,ObsBallStateEstimated.x_n,ObsBallStateEstimated.y_n,
                       ObsBallStateEstimated.vx_n,ObsBallStateEstimated.vy_n,RobotState[myNumber].x,RobotState[myNumber].y,
                       RobotState[myNumber].teta,1);
      end;
      //é extraída apenas se houve observacao
      if((ObsBallStateEstimated.quality>0)and(filterIter))then begin
        BallState:=ObsBallStateEstimated;
        if(abs(ObsBallStateEstimated.x)>FieldLength/2+0.5) or (abs(ObsBallStateEstimated.y)>FieldWidth/2+0.5)then begin
         BallState.quality:=-1;
        end;
      end;
    end;

    timebegin:=getTickcount();

    NewMainControl;
    deb_txt:=deb_txt+format('%3d',[getTickcount()-NetTime]);
  end;

  sleep(1);

  SendPlayerInfo;
  deb_txt:=deb_txt+format(',%3d',[getTickcount()-NetTime]);

  if RGController.ItemIndex=0 then  // freeze
    FLog.LogFrame(GetTickCount,0,0)
  else
    FLog.LogFrame(GetTickCount,0,1);
  deb_txt:=deb_txt+format(',%3d',[getTickcount()-NetTime]);

  ShowAll;
  deb_txt:=deb_txt+format(',%3d',[getTickcount()-NetTime]);

  LastView:=View;

  EditTimes.text:=deb_txt;
  inc(cycleCount);
end;

procedure TFMain.SimMainLoop;
var  i: integer;
     brdist, minBRdist, vbo,vbox,vboy: double;
begin
  deb_txt:='';

  ResetView;

//  UpdateOdos;
  view.OdosCount:=1;
  view.Odos[0].speedw[0]:=RobotState[mynumber].v1;
  view.Odos[0].speedw[1]:=RobotState[mynumber].v2;
  view.Odos[0].speedw[2]:=RobotState[mynumber].v3;

  if RGController.ItemIndex<>0 then begin
    for i:=0 to MaxRobots-1 do begin
      if i=mynumber then PropagateXYTeta(RobotState[i])
      else PropagateXYTetaOthers(RobotState[i]);
    end;
  end;

  PropagateXYBall(BallState);

  minBRdist:=0.25+0.1;
  brdist:=sqrt(sqr(RobotState[mynumber].x-BallState.x)+sqr(RobotState[mynumber].y-BallState.y));
  if brdist<minBRdist then begin //TODO
    vbox:=BallState.x-RobotState[mynumber].x;
    vboy:=BallState.y-RobotState[mynumber].y;
    vbo:=sqrt(sqr(vbox)+sqr(vboy));
    if vbo<1e-6 then vbo:=1e-6;
    vbox:=vbox/vbo;
    vboy:=vboy/vbo;
    BallState.x:=BallState.x+vbox*(minBRdist-brdist);
    BallState.y:=BallState.y+vboy*(minBRdist-brdist);
    
    if TacticCommands[myNumber].chipkick_pulse > 0 then begin
      BallState.vx := BallState.vx + cos(RobotState[myNumber].teta) * 10;
      BallState.vy := BallState.vy + sin(RobotState[myNumber].teta) * 10;
    end;
  end;

  LastBallState:=BallState;
  LastObsBallState:=ObsBallState;
  PropagateXYBall(BallState);

  if Dist(BallState.x-RobotState[mynumber].x,BallState.y-RobotState[mynumber].y)<6 then begin

    BallState.quality:=900;
    ObsBallState:=BallState;
    RelocateAbsCoordItem(ObsBallState.x,ObsBallState.y,
                         RobotState[mynumber].x,RobotState[mynumber].y,RobotState[mynumber].teta,
                         RobotState[mynumber].x,RobotState[mynumber].y,RobotState[mynumber].teta);
  end else begin
    ObsBallState.quality:=-1000;
  end;

  if ObsBallState.quality>=0 then begin
    MergeBallState(LastObsBallState,ObsBallState);
    BallState:=LastObsBallState;
  end;
  //Aqui entra o newmaincontrol
  NewMainControl;
  sleep(1);
  SendPlayerInfo;
  if RGController.ItemIndex=0 then  // freeze
    FLog.LogFrame(GetTickCount,0,0)
  else
    FLog.LogFrame(GetTickCount,0,1);

  ShowAll;
  LastView:=View;
  inc(cycleCount);
end;

procedure TFMain.SimTwoMainLoop;
var  i: integer;
     brdist, minBRdist, vbo,vbox,vboy: double;
begin
  deb_txt:='';

  ResetView;
  //Tiago Teste
  ParseLanMess;
  UpdateEdges;
  //UpdateRadar;
  UpdateOdos;
  UpdateCenters([iBallColor,iPurpleTeam,iCyanTeam]);
  //UpdateObstacles;
  PropagateObstacles;
//

  if RGController.ItemIndex<>0 then begin
    for i:=0 to MaxRobots-1 do begin
      if i=mynumber then PropagateXYTeta(RobotState[i])
      else PropagateXYTetaOthers(RobotState[i]);
    end;
  end;

  LastBallState:=BallState;
  //Quality Decay
  PropagateXYBall(BallState);
  CalcBallSpeed(ObsBallState,LastObsBallState);
  if ObsBallState.quality>=0 then begin
    MergeBallState(LastObsBallState,ObsBallState);
    MergeBallState(BallState,ObsBallState);
  end;

  RobotState[mynumber].vx:=(RobotState[mynumber].x-LRState[myNumber].x)/0.04;
  RobotState[mynumber].vy:=(RobotState[mynumber].y-LRState[myNumber].y)/0.04;
  LRState:=RobotState;

  minBRdist:=0.25+0.1;
  brdist:=sqrt(sqr(RobotState[mynumber].x-BallState.x)+sqr(RobotState[mynumber].y-BallState.y));
  if brdist<minBRdist then begin //TODO
    vbox:=BallState.x-RobotState[mynumber].x;
    vboy:=BallState.y-RobotState[mynumber].y;
    vbo:=sqrt(sqr(vbox)+sqr(vboy));
    if vbo<1e-6 then vbo:=1e-6;
    vbox:=vbox/vbo;
    vboy:=vboy/vbo;
    BallState.x:=BallState.x+vbox*(minBRdist-brdist);
    BallState.y:=BallState.y+vboy*(minBRdist-brdist);

    if TacticCommands[myNumber].chipkick_pulse > 0 then begin
      BallState.vx := BallState.vx + cos(RobotState[myNumber].teta) * 10;
      BallState.vy := BallState.vy + sin(RobotState[myNumber].teta) * 10;
    end;
  end;

  //propagacao da bola
  LastBallState:=BallState;
  LastObsBallState:=ObsBallState;
  PropagateXYBall(BallState);

  if Dist(BallState.x-RobotState[mynumber].x,BallState.y-RobotState[mynumber].y)<4 then begin
    ObsBallState:=BallState;
    ObsBallState.quality:=1000;
    RelocateAbsCoordItem(ObsBallState.x,ObsBallState.y,
                         RobotState[mynumber].x,RobotState[mynumber].y,RobotState[mynumber].teta,
                         RobotState[mynumber].x,RobotState[mynumber].y,RobotState[mynumber].teta);
  end else begin
    ObsBallState.quality:=-1000;
  end;

  if ObsBallState.quality>=0 then begin
    MergeBallState(LastObsBallState,ObsBallState);
    BallState:=LastObsBallState;
  end;
    //Aqui entra o newmaincontrol
  NewMainControl;
  sleep(1);
  SendPlayerInfo;
  if RGController.ItemIndex=0 then  // freeze
    FLog.LogFrame(GetTickCount,0,0)
  else
    FLog.LogFrame(GetTickCount,0,1);

  ShowAll;
  LastView:=View;
  inc(cycleCount);
end;

procedure TFMain.ISTMainLoop;
var  i: integer;
     brdist, minBRdist, vbo,vbox,vboy: double;
begin
  deb_txt:='';

  PropagateObstacles;

  if RGController.ItemIndex<>0 then begin
    for i:=0 to MaxRobots-1 do begin
      if i=mynumber then PropagateXYTeta(RobotState[i])
      else PropagateXYTetaOthers(RobotState[i]);
    end;
  end;

  LastBallState:=BallState;
  //Quality Decay
  PropagateXYBall(BallState);
  CalcBallSpeed(ObsBallState,LastObsBallState);
  if ObsBallState.quality>=0 then begin
    MergeBallState(LastObsBallState,ObsBallState);
    MergeBallState(BallState,ObsBallState);
  end;

  RobotState[mynumber].vx:=(RobotState[mynumber].x-LRState[myNumber].x)/0.04;
  RobotState[mynumber].vy:=(RobotState[mynumber].y-LRState[myNumber].y)/0.04;
  LRState:=RobotState;

  minBRdist:=0.25+0.1;
  brdist:=sqrt(sqr(RobotState[mynumber].x-BallState.x)+sqr(RobotState[mynumber].y-BallState.y));
  if brdist<minBRdist then begin //TODO
    vbox:=BallState.x-RobotState[mynumber].x;
    vboy:=BallState.y-RobotState[mynumber].y;
    vbo:=sqrt(sqr(vbox)+sqr(vboy));
    if vbo<1e-6 then vbo:=1e-6;
    vbox:=vbox/vbo;
    vboy:=vboy/vbo;
    BallState.x:=BallState.x+vbox*(minBRdist-brdist);
    BallState.y:=BallState.y+vboy*(minBRdist-brdist);

    if TacticCommands[myNumber].chipkick_pulse > 0 then begin
      BallState.vx := BallState.vx + cos(RobotState[myNumber].teta) * 10;
      BallState.vy := BallState.vy + sin(RobotState[myNumber].teta) * 10;
    end;
  end;

  //propagacao da bola
  LastBallState:=BallState;
  LastObsBallState:=ObsBallState;
  PropagateXYBall(BallState);

  //if Dist(BallState.x-RobotState[mynumber].x,BallState.y-RobotState[mynumber].y)<4 then begin
    ObsBallState:=BallState;
    ObsBallState.quality:=1000;
    RelocateAbsCoordItem(ObsBallState.x,ObsBallState.y,
                         RobotState[mynumber].x,RobotState[mynumber].y,RobotState[mynumber].teta,
                         RobotState[mynumber].x,RobotState[mynumber].y,RobotState[mynumber].teta);
  //end else begin
  //  ObsBallState.quality:=-1000;
  //end;

  if ObsBallState.quality>=0 then begin
    MergeBallState(LastObsBallState,ObsBallState);
    BallState:=LastObsBallState;
  end;
    //Aqui entra o newmaincontrol
  NewMainControl;
  sleep(1);
  //SendPlayerInfo;
  //if RGController.ItemIndex=0 then  // freeze
  //  FLog.LogFrame(GetTickCount,0,0)
  //else
  //  FLog.LogFrame(GetTickCount,0,1);

  ShowAll;
  LastView:=View;
  inc(cycleCount);
end;


procedure TFMain.ControlAccelerationNLTMD(out V:double;out Vn:double;out W:double);
var
  Derivada_V:double;
  Derivada_Vn:double;
  Derivada_W:double;
  Vaux, Vnaux, Waux,k_acel:double;
begin

  derivada_V:=(V-RobotState[myNumber].sV)/0.04;
  derivada_Vn:=(Vn-RobotState[myNumber].sVn)/0.04;
  derivada_W:=(W-RobotState[myNumber].sW)/0.04;


  if abs(v)>0.01 then begin
    if derivada_V > max_linear_acceleration then begin
      if v>0 then
        k_acel:=1
      else
        k_acel:=1;
      Vaux:=k_acel*max_linear_acceleration*0.04+RobotState[myNumber].sV;
      if Vaux<V then V:=Vaux;
    end else if derivada_V <(-max_linear_acceleration) then begin
      if v<0 then
         k_acel:=1
      else
         k_acel:=1;
      Vaux:=k_acel*(-max_linear_acceleration)*0.04+RobotState[myNumber].sV;
      if Vaux>V then V:=Vaux;
    end;
  end;

  if abs(vn)>0.01 then begin
    if derivada_Vn >max_linear_acceleration then begin
      k_acel:=1;
      Vnaux:=k_acel*max_linear_acceleration*0.04+RobotState[myNumber].sVn;
      if Vnaux<Vn then Vn:=Vnaux;
    end else if derivada_Vn <(-max_linear_acceleration) then begin
      k_acel:=1;
      Vnaux:=k_acel*(-max_linear_acceleration)*0.04+RobotState[myNumber].sVn;
      if Vnaux>Vn then Vn:=Vnaux;
    end;
  end;

  if abs(w)>0.1 then begin
    if derivada_W >max_angular_acceleration then begin
      k_acel:=1;
      Waux:=k_acel*max_angular_acceleration*0.04+RobotState[myNumber].sW;
      if Waux<W then W:=Waux;
    end else if derivada_W <(-max_angular_acceleration) then begin
      k_acel:=1;
      Waux:=k_acel*(-max_angular_acceleration)*0.04+RobotState[myNumber].sW;
      if Waux>W then W:=Waux;
    end;
  end;
end;

procedure TFMain.NewMainControl;//heber
var
  V,Vn,W,k: double;
  w1,w2,w3: double;
  kickPulse: integer;
  kickPulseValue,pwm11,pwm22,pwm33: integer;
  LowKick,valsensor: boolean;
  Kick: boolean;
  pwm1,pwm2,pwm3: double;
  RollerEnable: boolean;
  i,j,num:integer;
begin
  //Nova construção do MainControl!
  V:=0;
  Vn:=0;
  W:=0;
  kickPulse:=0;
  LowKick:=false;
  RollerEnable:=false;

  case RGController.ItemIndex of
    0: begin //freeze
      V:=0;
      Vn:=0;
      W:=0;
      kickPulse:=0;
      LowKick:=false;
      RollerEnable:=false;

      Form_Hardware.SetAngVelRefWheels(V,Vn,W);
      Form_Hardware.SetKicker(kickPulse);
      Form_Hardware.SetRoller(0);
    end;
    1: begin //Manual
      ManualControl(v,vn,w,Kick);
      kickPulse:=0;
      LowKick:=false;
      RollerEnable:=false;

      ControlAccelerationNLTMD(v,vn,w);
      Omin_Ticks2(v,vn,w,w1,w2,w3);
      Form_Hardware.SetAngVelRefWheels(w1,w2,w3);
      Form_Hardware.SetKicker(kickPulse);
      Form_Hardware.SetRoller(0);

      with MemoActionPars.Lines do begin
        Clear;
        Add(format('Robô - x=%2f y=%2f teta=%2f',[RobotState[myNumber].x,RobotState[myNumber].y,RobotState[myNumber].teta]));
        Add(format('Robô - v=%2f vn=%2f w=%2f',[RobotState[myNumber].v,RobotState[myNumber].vn,RobotState[myNumber].w]));
        Add(format('Bola - x=%2f y=%2f',[BallState.x,BallState.y]));
        Add(format('Bola - Vx=%2f Vy=%2f',[BallState.vx,BallState.vy]));
      end;

    end;
    2: begin // PWM
      pwm1:=double(strtointdef(editpwm1.Text,0));
      pwm2:=double(strtointdef(editpwm2.Text,0));
      pwm3:=double(strtointdef(editpwm3.Text,0));
      V:=0;
      Vn:=0;
      W:=0;
      kickPulse:=0;
      LowKick:=false;
      RollerEnable:=false;

      Form_Hardware.SetPWMWheels(pwm1,pwm2,pwm3);
      Form_Hardware.SetKicker(kickPulse);
      Form_Hardware.SetRoller(0);
    end;
    3: begin // Play
      Edit1.Text:='';
      Play:=playNormal;
      CalcRobotCalcData(myNumber);
      DoRobotRules(myNumber);

      EditRoleName.Text:=RoleDefs[RobotInfo[myNumber].role].name;
      EditTaskName.Text:=CTaskString[RobotInfo[myNumber].task] + inttostr(TacticCommands[myNumber].chipkick_pulse);

      with MemoActionPars.Lines do begin
        Clear;
        Add(format('Robô - x=%2f y=%2f teta=%2f',[RobotState[myNumber].x,RobotState[myNumber].y,RobotState[myNumber].teta]));
        Add(format('Robô - v=%2f vn=%2f w=%2f',[RobotState[myNumber].sv,RobotState[myNumber].svn,RobotState[myNumber].sw]));
        Add(format('Robô - vx=%2f vy=%2f',[RobotState[myNumber].vx,RobotState[myNumber].vy]));
        Add(format('Bola - x=%2f y=%2f',[BallState.x,BallState.y]));
        Add(format('Bola - Vx=%2f Vy=%2f',[BallState.vx,BallState.vy]));
      end;

      v:=TacticCommands[myNumber].v;
      vn:=TacticCommands[myNumber].vn;
      w:=TacticCommands[myNumber].w;


      kickPulse:=TacticCommands[myNumber].chipkick_pulse;
      LowKick:=TacticCommands[myNumber].Low_kick;
      Edit1.Text:=Edit1.Text+format(', %2f %2f %2f',[v,vn,w]);

      ControlAccelerationNLTMD(v,vn,w);
      Omin_Ticks2(v,vn,w,w1,w2,w3);
      Form_Hardware.SetAngVelRefWheels(w1,w2,w3);
      Form_Hardware.SetSpecialKicker(kickPulse,LowKick);

      if (CBSimTwo.Checked=false)and(CBSimulator.Checked=false) then begin
          valsensor:=Form_Hardware.GetBallSensor();

          if RobotInfo[myNumber].task=taskdribleForGoal then begin
            if valsensor=true then begin
               if RobotState[myNumber].v<=0.2 then begin
                  Form_Hardware.SetRoller(100);
               end else begin
                  k:=0.025;
                  Form_Hardware.SetRoller(-((RobotState[myNumber].v*10)*k));
               end;
            end;
          end else begin
             Form_Hardware.SetRoller(100);
          end;
      end;
    end;
    4: begin //Joy
      V:=Form_joystick.V;
      Vn:=Form_joystick.Vn;
      W:=Form_joystick.W;
      kickPulse:=Form_joystick.kickPulse;
      LowKick:=false;
      RollerEnable:=false;

      ControlAccelerationNLTMD(v,vn,w);
      Omin_Ticks2(v,vn,w,w1,w2,w3);
      Form_Hardware.SetAngVelRefWheels(w1,w2,w3);
      Form_Hardware.SetKicker(kickPulse);
      Form_Hardware.SetRoller(RollerEnable);
    end;
  end;

  if CBSimulator.Checked=true then begin
    OminSim(v,vn,w,RobotState[myNumber]);
    if CBISTScenario.Checked=true then begin
      SdpoUDPSimTwo.SendMessage(IntToStr(myNumber)+' '+
                floatToStr(RobotState[myNumber].v)+' '+
                floatToStr(RobotState[myNumber].vn)+' '+
                floatToStr(RobotState[myNumber].w)+' '+
                floatToStr(siga)+' '+
                floatToStr(sigb)+' '+
                floatToStr(sigc)+' '+
                floatToStr(sigd),
                FParam.EditSimTwoIP.Text+':'+FParam.EditSimTwoPort.Text);
      if RGController.ItemIndex=0 then begin
        SdpoUDPSimTwo.SendMessage(IntToStr(myNumber)+' '+
                  floatToStr(0)+' '+
                  floatToStr(0)+' '+
                  floatToStr(0)+' '+
                  floatToStr(0)+' '+
                  floatToStr(0)+' '+
                  floatToStr(0)+' '+
                  floatToStr(0),
                  FParam.EditSimTwoIP.Text+':'+FParam.EditSimTwoPort.Text);
      end;
    end;
  end else if CBSimTwo.Checked=true then begin
    Omin_Ticks(v,vn,w,pwm11,pwm22,pwm33);
    SendPID_PWM;
    if FParam.RGSide.ItemIndex=0 then begin
     SdpoUDPSimTwo.SendMessage(chr(35)+chr(13)+IntToStr(myNumber)+chr(13)+
                 IntToStr(round(100*DStates[0].ref*Ktic_rad_out))+chr(13)+
                 IntToStr(100*round(DStates[1].ref*Ktic_rad_out))+chr(13)+
                 IntToStr(100*round(DStates[2].ref*Ktic_rad_out))+chr(13),//+
                 //IntToStr(kickPulse)+chr(13),
                 FParam.EditSimTwoIP.Text+':'+FParam.EditSimTwoPort.Text);
    end else begin
     SdpoUDPSimTwo.SendMessage(chr(35)+chr(13)+IntToStr(myNumber+5)+chr(13)+
                   IntToStr(round(100*DStates[0].ref*Ktic_rad_out))+chr(13)+
                   IntToStr(round(100*DStates[1].ref*Ktic_rad_out))+chr(13)+
                   IntToStr(round(100*DStates[2].ref*Ktic_rad_out))+chr(13),//+
                 //IntToStr(kickPulse)+chr(13),
                 FParam.EditSimTwoIP.Text+':'+FParam.EditSimTwoPort.Text);
    end;
  end;

  if (CBSimulator.Checked=false)and(CBSimTwo.Checked=false) then begin
    Form_Hardware.Send_outputs;
    Form_Hardware.Request_inputs;
  end;

  RobotState[myNumber].sv:=v;
  RobotState[myNumber].svn:=vn;
  RobotState[myNumber].sw:=w;
end;

procedure TFMain.SendPlayerInfo;
var PlayerInfo: TPlayerInfo;
    packet_str: string;
    i: integer;
    val: boolean;
begin
  try
    PlayerInfo.Magic := PACKET_PLAYER_MAGIC;

    val:=Form_Hardware.GetBallSensor;

    if (CBSimTwo.Checked=false) then begin
      RobotState[MyNumber].withball:=val;
    end;

    // send my robot state
    with RobotState[MyNumber] do begin
      PlayerInfo.RobotState.x := x;
      PlayerInfo.RobotState.y := y;
      PlayerInfo.RobotState.teta := teta;
      PlayerInfo.RobotState.vx := vx;
      PlayerInfo.RobotState.vy := vy;
      PlayerInfo.RobotState.w := sw;

      PlayerInfo.RobotState.conf := count;
      PlayerInfo.RobotState.Active:=((not Form_Hardware.Failure)and(Form_Localization.ok));
      PlayerInfo.Batery:=KickerState.Vbat;
      PlayerInfo.RobotState.WithBall:=withball;
    end;

    // send the ball state
    PlayerInfo.BallState.x := BallState.x;
    PlayerInfo.BallState.y := BallState.y;
    PlayerInfo.BallState.vx:= BallState.vx;
    PlayerInfo.BallState.vy:= BallState.vy;
    PlayerInfo.BallState.x_next := BallState.x_next;
    PlayerInfo.BallState.y_next := BallState.y_next;
    PlayerInfo.BallState.quality := BallState.quality;

    // send the obstacles
    // TODO: actually send the obstacles

    PlayerInfo.num := myNumber;

    zeromemory(@(PlayerInfo.ObsState), sizeof(PlayerInfo.ObsState));

    packet_str := StringOfChar(#0, sizeof(PlayerInfo));
    copymemory(@(packet_str[1]), @PlayerInfo, sizeof(PlayerInfo));

    SdpoUDPSuper.SendMessage(packet_str, FParam.EditSuperIP.Text + ':'+IntToStr(7373));
  except
  end;
end;

procedure TFMain.ShowAll;
var i,tx,ty: integer;
    cl: Tcolor;
begin
  if CBShow.Checked then begin
    if CBShowField.Checked then begin
      DrawFieldMap(ImageMap.canvas);
      for i:=0 to MaxRobots-1 do begin
        if i=myNumber then begin
          cl:=clwhite;
          DrawRobotText(RobotState[i],cl,ImageMap.canvas);
        end else begin
          cl:=CTeamColorColor24[TeamColor];
          DrawRobot(RobotState[i],cl,ImageMap.canvas);
        end;
      end;
      DrawBall(ImageMap.canvas, BallState,clred);
      DrawTRaj(Traj,ImageMap.canvas);
    end;

    with ImageShow.Canvas do begin
      brush.Style:=bsSolid;
      brush.Color:=clBlack;
      pen.color:=clBlack;
      Rectangle(0,0,width,height);
    end;
    if CBShowCenters.Checked then ShowCenters;
    if CBShowEdges.Checked then ShowEdges;
    if CBShowRadar.Checked then begin
      ShowRadar;
      DrawObstacles(Obstacles);
    end;
    DrawObstacles(Obstacles);//add by tiago
    if CBShowRegions.Checked then ShowRegions;

    ShowLine(GWLine);
    ShowLine(WGLine);

    Gtext.Clear;
    FillShowValues(Gtext,RobotState[myNumber], BallState, View);
    if CBShowLog.Checked then FLog.LogToStrings(GText,FLog.TreeView,LogBufferIn);

    tx:=draw_x;
    ty:=draw_y;
    ImageShow.canvas.Font.Color:=clgray;
    ImageShow.canvas.brush.Style:=bsClear;
    StringsDraw(ImageShow.canvas,Gtext,tx,ty);
  end;
end;

procedure TFMain.FillShowValuesItem(GT: Tstrings; itemName, formatString: string; value: double);
var i: integer;
begin
  i:=CLBShowValues.items.IndexOf(itemName);
  if (i>=0) then begin
    if CLBShowValues.Checked[i] then
      GT.add(format(itemName+formatString,[value]));
  end else begin
    CLBShowValues.items.add(itemName);
  end;
end;

procedure TFMain.FillShowValues(GT: Tstrings; var RS: TRobotState; var BS: TBallState; var View: TView);
begin
  FillShowValuesItem(GT,'Ball Dist',': %.2f',sqrt(sqr(RS.x-BS.x)+sqr(RS.y-BS.y)));
  FillShowValuesItem(GT,'Ball Angle',': %.2f',diffangle(atan2(BS.y-RS.y,BS.x-RS.x),RS.teta)/pi*180);
end;

procedure TFMain.OminSim(v,vn,w: double; var R: TRobotState);
var
    mte,ce,se: double;
    i:integer;
begin

// Ideal Model, based on reference velocities

  ce:=cos(R.teta);
  se:=sin(R.teta);

  R.v1:= 0.886*v+0.5*vn+WheeltoCenterdist*w;
  R.v2:= -0.886*v+0.5*vn+WheeltoCenterdist*w;
  R.v3:= -1*vn+WheeltoCenterdist*w;

  R.x:=R.x+0.04*(v*ce-vn*se);
  R.y:=R.y+0.04*(v*se+vn*ce);
  R.teta:=R.teta+0.04*w;

end;

procedure TFMain.Omin_Ticks(v,vn,w: double; pwm1,pwm2,pwm3: integer);
var speed1w,speed2w,speed3w,sp1t,sp2t,sp3t: double;

begin


  speed1w:= 0.886*v+0.5*vn+WheeltoCenterdist*w;
  speed2w:= -0.886*v+0.5*vn+WheeltoCenterdist*w;
  speed3w:= -1*vn+WheeltoCenterdist*w;

  sp1t:= (speed1w/(Wheel1Diameter/2))/Ktic_rad_out;
  sp2t:= (speed2w/(Wheel2Diameter/2))/Ktic_rad_out;
  sp3t:= (speed3w/(Wheel3Diameter/2))/Ktic_rad_out;

  DStates[0].ref:= round(sp1t);   // speed in ticks
  DStates[1].ref:= round(sp2t);   // speed in ticks
  DStates[2].ref:= round(sp3t);   // speed in ticks

  // ticks -> pwm
  pwm1 :=  round(sp1t/Ktic_pwm);
  pwm2 :=  round(sp2t/Ktic_pwm);
  pwm3 :=  round(sp3t/Ktic_pwm);

  if pwm1 > 250 then  pwm1 := 250;
  if pwm2 > 250 then  pwm2 := 250;
  if pwm3 > 250 then  pwm3 := 250;

  if pwm1 < -250 then  pwm1 := -250;
  if pwm2 < -250 then  pwm2 := -250;
  if pwm3 < -250 then  pwm3 := -250;


  DStates[0].pwm:= pwm1;
  DStates[1].pwm:= pwm2;
  DStates[2].pwm:= pwm3;
end;

procedure TFMain.Omin_Ticks2(v,vn,w: double; out w1,w2,w3: double);
var
  speed1w,speed2w,speed3w: double;
begin
  speed1w:= 0.886*v+0.5*vn+WheeltoCenterdist*w;
  speed2w:= -0.886*v+0.5*vn+WheeltoCenterdist*w;
  speed3w:= -1*vn+WheeltoCenterdist*w;

  w1:=speed1w/(Wheel1Diameter/2);
  w2:=speed2w/(Wheel2Diameter/2);
  w3:=speed3w/(Wheel3Diameter/2);
end;

procedure TFMain.Omin_pwm(pwm1,pwm2,pwm3: integer);
var
  speed1w,speed2w,speed3w,sp1t,sp2t,sp3t: double;
begin
  DStates[0].pwm:=pwm1;
  DStates[1].pwm:=pwm2;
  DStates[2].pwm:=pwm3;
end;

procedure TFmain.UpdateCenters(icenters: iColorSet);
var tempcolor,i:integer;
begin
  with view do begin
    for tempcolor:=0 to cTrackColors-1 do begin // todas as cores

      if not (tempcolor in icenters) then continue;

      for i:=0 to Centers[tempcolor].Count-1 do begin
        with centers[tempcolor].data[i] do begin
          if tempcolor in [iBallColor, iPurpleTeam, iCyanTeam] then begin
            FCamera.CamRoTetaZToXYD(ro,teta,colorZ[tempcolor],d,xw,yw);
          end else begin
            xw:=0;
            yw:=0;
            d:=0;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFmain.UpdateOdos;
var i: integer;
    K1: double;
    w1,w2,w3: double;
begin
  with view do begin
    with Odos[0] do begin
      Form_Hardware.GetDeltaAngWheels(w1,w2,w3);
      dwheel[0]:=w1*(Wheel1Diameter/2);
      dwheel[1]:=w2*(Wheel2Diameter/2);
      dwheel[2]:=w3*(Wheel3Diameter/2);
      Form_Hardware.GetSpeedAngWheels(w1,w2,w3);
      speedw[0]:=w1;
      speedw[1]:=w2;
      speedw[2]:=w3;
    end;
  end;
end;

procedure TFmain.UpdateOdos2;
var i: integer;
    K1: double;
    w1,w2,w3: double;
begin
  with view do begin
    with Odos[0] do begin
      Avaible:=Form_Hardware.GetDeltaAngWheels(w1,w2,w3);
      dwheel[0]:=w1*(Wheel1Diameter/2);
      dwheel[1]:=w2*(Wheel2Diameter/2);
      dwheel[2]:=w3*(Wheel3Diameter/2);
      Form_Hardware.GetSpeedAngWheels(w1,w2,w3);
      speedw[0]:=w1;
      speedw[1]:=w2;
      speedw[2]:=w3;
      RobotDelU:=0.5774*View.Odos[0].dwheel[0]-0.5774*View.Odos[0].dwheel[1];
      RobotDelUn:=0.3333*View.Odos[0].dwheel[0]+0.3333*View.Odos[0].dwheel[1]-0.6667*View.Odos[0].dwheel[2];
      RobotDelTeta:=(0.3333/wheeltoCenterdist)*View.Odos[0].dwheel[0]+(0.3333/wheeltoCenterdist)*View.Odos[0].dwheel[1]+(0.3333/wheeltoCenterdist)*View.Odos[0].dwheel[2];
      RobotSpeedV:=0.5774*View.Odos[0].speedw[0]*(Wheel1Diameter/2)-0.5774*View.Odos[0].speedw[1]*(Wheel1Diameter/2);
      RobotSpeedVn:=0.3333*View.Odos[0].speedw[0]*(Wheel1Diameter/2)+0.3333*View.Odos[0].speedw[1]*(Wheel1Diameter/2)-0.6667*View.Odos[0].speedw[2]*(Wheel1Diameter/2);
      RobotSpeedW:=(0.3333/wheeltoCenterdist)*View.Odos[0].speedw[0]*(Wheel1Diameter/2)+(0.3333/wheeltoCenterdist)*View.Odos[0].speedw[1]*(Wheel1Diameter/2)+(0.3333/wheeltoCenterdist)*View.Odos[0].speedw[2]*(Wheel1Diameter/2);
    end;
  end;
end;

procedure TFmain.UpdateEdges;
var i:integer;
begin
  with view do begin
    for i:=0 to EdgesCount-1 do begin
      with edges[i] do begin
        FCamera.CamRoTetaZToXYD(ro,teta,0,d,xw,yw);
      end;
    end;
  end;
end;

procedure TFmain.UpdateRadar;
const
  MaxDist=2;
  ObsRadius=0.25;
var
  i:integer;
begin
  with view do begin
    Obstacles.count:=0;
    for i:=0 to RadarRaysCount-5 do begin
      with Radar[i] do begin
        if ro>rad(80) then ro:=rad(80);
        FCamera.CamRoTetaZToXYD(ro,teta,0,d,xw,yw);
        if (i<=MaxObstacles-1) and (d<MaxDist) then begin
          Obstacles.Centers[Obstacles.count].color:=iBlackColor;
          Obstacles.Centers[Obstacles.count].r:=ObsRadius;
          RotateAndTranslate(Obstacles.Centers[Obstacles.count].x,Obstacles.Centers[Obstacles.count].y,xw,yw,
                             RobotState[myNumber].x,RobotState[myNumber].y,
                             RobotState[myNumber].teta);
          inc(Obstacles.count);
        end;
      end;
    end;
  end;
end;

procedure TFmain.UpdateObstacles;
var i:integer;
    ydisps: array [0..1] of double;
begin
  with view do begin
    for i:=0 to RegionsCount-1 do begin
      with Regions[i] do begin
        if BestColor in [iYellowGoal,iBlueGoal,iPurpleTeam,iCyanTeam,iBlackColor] then begin
          AddObstacle(Regions[i].xw,Regions[i].yw,Regions[i].teta,Regions[i].BestColor,Obstacles);
        end;
      end;
    end;

    ydisps[0]:=-0.2;
    ydisps[1]:=0.2;
  end;
end;

procedure TFmain.AddObstacle(xw,yw,teta: double; ObstacleColor: integer ; var Obstacles: TObstacles);
var j,idx:integer;
    xt,yt,delta,d: double;
begin
  delta:=0.2;
  d:=Dist(yw,xw);            // parametro
  if (d>1.5) or (d<1e-3) or (abs(teta)>30/180*pi) then exit;// só interessam obstáculos à frente e perto
  idx:=-1;
  for j:=0 to Obstacles.count-1 do begin   //procura um obstáculo existente no qual encaixe
    RotateAndTranslate(xt,yt,
                       xw,yw,
                       RobotState[myNumber].x,RobotState[myNumber].y,RobotState[myNumber].teta);
    if (abs(xt-Obstacles.Centers[j].xw)<delta) and
       (abs(yt-Obstacles.Centers[j].yw)<delta) then begin
      Obstacles.Centers[j].color:=ObstacleColor;
      Obstacles.Centers[j].quality:=1;
      Obstacles.Centers[j].quality:=Obstacles.Centers[j].quality*1.2;
      if Obstacles.Centers[j].quality >1 then Obstacles.Centers[j].quality:=1;
      idx:=j;
      break;
    end;
  end; // senão vai criar um novo
  if idx<>-1 then exit;
  if Obstacles.count<MaxObstacles-1 then begin
    RotateAndTranslate(Obstacles.Centers[Obstacles.count].xw,
                       Obstacles.Centers[Obstacles.count].yw,
                       xw,yw,
                       RobotState[myNumber].x,RobotState[myNumber].y,RobotState[myNumber].teta);
    Obstacles.Centers[Obstacles.count].color:=ObstacleColor;
    Obstacles.Centers[Obstacles.count].quality:=1;
    inc(Obstacles.count);
  end;

end;

procedure TFmain.PropagateObstacles;
var i:integer;
begin
  with Obstacles do begin
    i:=0;
    while i<Obstacles.count do begin
      Centers[i].quality:=Centers[i].quality*0.9; //parametro
      if Centers[i].quality<0.2 then begin
        Centers[i].xw:=Centers[count-1].xw;
        Centers[i].yw:=Centers[count-1].yw;
        Centers[i].color:=Centers[count-1].color;
        Centers[i].quality:=0;
        dec(count);
      end;
      inc(i);
    end;
  end;
end;

function TFMain.LowPassFilterV(meassure,oldmeassure: double; Falpha: double): double;
begin
  Result:=Falpha*meassure+(1-Falpha)*oldmeassure;
end;

procedure TFMain.PropagateXYBall(var BS: TBallState);
var cov_pos_noise,ang: double;
begin
  cov_pos_noise:=0.01;
  with BS do begin
    cov_x:=cov_x+cov_pos_noise;
    cov_y:=cov_y+cov_pos_noise;
    quality:=quality*BallQualityDecay-BallLinearQualityDecay;
  end;
end;

procedure TFMain.PropagateBallQualityDecay(var BS: TBallState);
begin
  with BS do begin
    quality:=quality*BallQualityDecay-BallLinearQualityDecay;
  end;
end;

//--------------------------------------------------------------------------------------------
// Form events

procedure TFMain.FormCreate(Sender: TObject);
var
  i,j: integer;
  SessionPropsList: TStringList;
  SessionPropsFileName: string;
  k: TRole;
begin
  // parse command line parameters
  DataDir := 'data';
  CoachMode := false;

  for i := 1 to paramcount do begin
    if paramstr(i) = '-coach' then begin
      CoachMode := true;
    end else begin
      DataDir:=paramstr(i);
      if not directoryExists(extractfilepath(application.ExeName)+dataDir) then dataDir:='data';
    end;
  end;

  WindowState := wsNormal;
  decimalSeparator:='.';
  walk_counter:=0;

  for i:=0 to Nsim-1 do begin
    for j:=0 to 1 do begin
      Buff_erro[i,j]:=0;
    end;
  end;
  Vsim:=0;
  Wsim:=0;
  Sum_err_stress_v:=0;
  Sum_err_stress_w:=0;
  Idx_erro:=0;

  ref_v_ant:=0;
  ref_w_ant:=0;
  ref_v:=0;
  Flag_stress:=false;
  Flag_stress_ant:=false;
  v_on_stress:=0;
  w_on_stress:=0;

  Ser1_Data:='';
  Ser2_Data:='';
  Ser1_Out:='';
  Ser2_Out:='';
  LastData:='';

  CBDefRole.Items.Clear;
  CBStartRole.Items.Clear;
  for k:=Low(TRole) to High(TRole) do begin
    CBDefRole.Items.Add(RoleDefs[k].name);
    CBStartRole.Items.Add(RoleDefs[k].name);
  end;
  CBDefRole.DropDownCount:=15;
  CBStartRole.DropDownCount:=15;

  NumAuxForms:=0;
  
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

  SLShowValues:= TStringList.Create;
  TempStream := TMemoryStream.Create;
  Gtext:= TStringList.Create;

  ImageShow.Canvas.Font.Name:='Terminal';

  Good_image:=false;
  StrTream:=TStringStream.Create('');

  cycleCount:=0;
  with View do begin
    for i:=0 to cTrackColors-1 do begin   //  CentersCount:array[0..cTrackColors-1] of integer;
     Centers[i].Count:=0;
    end;
    EdgesCount:=0;
    RegionsCount:=0;
    LoadRegions;
    OdosCount:=0;
    OdosTotalCount:=0;
  end;
  LastView:=View;

  for i:=0 to MaxRobots-1 do begin
    with RobotState[i] do begin
      x:=4*-random(100)/100;
      y:=2*(100-random(200))/100;
      teta:=10*random;
      cov_x:=1e-10;
      cov_y:=1e-10;
      cov_xy:=0;
      cov_teta:=1e-9;
      num:=i;
      Xk.SetSize(3,1);
      Pk.SetSize(3,3);
      RobotStateDoublesToMatrix(RobotState[i]);
    end;
  end;
  with RobotState[myNumber] do begin
    x:=-2;
    y:=0.5;
    teta:=0;
  end;
  ref_v:=0;
  ref_w:=0;
  with BallState do begin
    x:=1;
    y:=0;
    estx:=0;
    esty:=0;
    vxl:=0;
    vyl:=0;
    xcoach:=1;
    ycoach:=0;
    cov_x:=100;
    cov_y:=100;
    quality:=0;
    vx:=0;
    vy:=0;
    cov_Vx:=100;
    cov_Vy:=100;
  end;

  sigtemp:=Mzeros(2,2);
  sigver:=Mzeros(2,2);
  sigtemp0:=Mzeros(2,2);
  sigtemp1:=Mzeros(2,2);
  sigtemp2:=Mzeros(2,2);
  sigtemp3:=Mzeros(2,2);
  sigtemp4:=Mzeros(2,2);
  sigmaT:=Mzeros(2,2);

end;

procedure TFMain.BStopReferenceClick(Sender: TObject);
begin
  EditV.Text:='0';
  EditVn.Text:='0';
  EditW.Text:='0';
end;

procedure TFMain.Button_ShowLocalizationClick(Sender: TObject);
begin
  Form_Localization.Show;
end;

procedure TFMain.BSetRoleClick(Sender: TObject);
begin
  RobotInfo[myNumber].role:=TRole(CBStartRole.ItemIndex);
end;

procedure TFMain.CBDefRoleClick(Sender: TObject);
begin
  RobotStatus[myNumber].default_role:=TRole(CBDefRole.ItemIndex);
end;

procedure TFMain.CBSimTwoClick(Sender: TObject);
var i: integer;
begin
  if CBSimTwo.Checked then begin
    for i := 0 to MaxRobots - 1 do begin
      RobotStatus[i].active := true;
      RobotState[i].count := 1;
    end;
  end;
end;

procedure TFMain.CheckBox_NoCameraChange(Sender: TObject);
begin
  if CheckBox_NoCamera.Checked then begin
    Timer_NoCamera.Enabled:=true;
  end
  else begin
    Timer_NoCamera.Enabled:=false;
  end;
end;

procedure TFMain.FormShow(Sender: TObject);
var
  i:integer;
begin
  CLBShowValues.Items.clear;
  FillShowValues(Gtext,RobotState[0],BallState,View); // just to fill the box

  // TODO Log crash
  FLog.FillTreeView(FLog.TreeView);
  FLog.LoadTree(FLog.TreeView);
  FLog.RefreshGrid(FLog.TreeView);
  draw_x:= 120;
  draw_y:=2;

  RestoreAuxForms;
  UpdateFieldDims;

  FCamera.CamsRefresh;
  RobotState[myNumber].x:=strtofloatdef(EditRobotX.text,0);
  RobotState[myNumber].y:=strtofloatdef(EditRobotY.text,0);
  RobotState[myNumber].teta:=strtofloatdef(EditRobotTeta.text,0);
  
  RobotStatus[myNumber].default_role:=TRole(CBDefRole.ItemIndex);
  RobotInfo[myNumber].role:=RobotStatus[myNumber].default_role;

  if CoachMode then begin
    SdpoUDPSuper.Listen(7373);
  end else begin
    SdpoUDP.Listen(7171);
    SdpoUDPSuper.Listen(7272+myNumber);
    SdpoUDPSimTwo.Listen(StrToInt(FParam.EditSimTwoListenPort.Text));
  end;

  if CoachMode then begin
    FieldImageWidth := FCoachMain.ImageMap.Width;
    FieldImageHeight := FCoachMain.ImageMap.Height;
    Hide;
    FCoachMain.Show;
  end else begin
    FieldImageWidth := FMain.ImageMap.Width;
    FieldImageHeight := FMain.ImageMap.Height;
  end;

  LoadMap(ExtractFilePath(Application.ExeName)+DataDir+'/maps/'+FLocMap.EditMapLoadName.Text+'.dst');
  DrawArrayDist(MapDist, MapAreaW, MapAreaH, FLocMap.ImageLocMap);
  MapLoaded:=true;

  Xstart:=strtofloatDef(FParam.EditXstart.text,0);
  Ystart:=strtofloatDef(FParam.EditYstart.text,0);
  Xstop:=strtofloatDef(FParam.EditXstop.text,0);
  Ystop:=strtofloatDef(FParam.EditYstop.text,0);
end;

procedure TFMain.MenuItem_HardwareClick(Sender: TObject);
begin
  Form_Hardware.Show;
end;

procedure TFMain.MenuItem_JoystickClick(Sender: TObject);
begin
  Form_joystick.Show;
end;

procedure TFMain.MI_BallEstClick(Sender: TObject);
begin
  FBall.Show;
end;

procedure TFMain.RGRobotSelClick(Sender: TObject);
begin
  SdpoUDPSimTwo.Disconnect;
  SdpoUDPSimTwo.Listen(StrToInt(FParam.EditSimTwoListenPort.text)+RGRobotSel.ItemIndex+1);
  SdpoUDPSuper.Disconnect;
  SdpoUDPSuper.Listen(7272+RGRobotSel.ItemIndex);
end;

procedure TFMain.FormDestroy(Sender: TObject);
var i: integer;
begin
  SLShowValues.Free;
  Gtext.Free;
  StrTream.Free;
  TempStream.Free;
end;

procedure TFMain.FormClose(Sender: TObject);
var
  checks: string;
  i:integer;
begin
  SdpoUDPSuper.Disconnect;
  SdpoUDP.Disconnect;
  SdpoUDPSimTwo.Disconnect;
  checks:=CLBGetChecks(CLBShowValues);
  SaveAuxForms;
  CloseAuxForms;
end;

procedure TFMain.InsertAuxForms(Fm: TForm; cap: string);
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

procedure TFMain.AuxFormClosed(cap: string);
var i: integer;
begin
  for i := 0 to NumAuxForms - 1 do begin
    if MenuWindows.Items[i].Caption = cap then begin
      MenuWindows.Items[i].Checked := false;
      exit;
    end;
  end;
end;

procedure TFMain.MenuWinDefaultClick(Sender: TObject);
var MenuItem: TMenuItem;
begin
  MenuItem:=Sender as TMenuItem;
  MenuItem.Checked:=true;
  AuxForms[MenuItem.Tag].Show;
  AuxForms[MenuItem.Tag].BringToFront;
end;

procedure TFMain.SaveAuxForms;
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

procedure TFMain.CloseAuxForms;
var
  i: integer;
begin
  for i:=0 to MenuWindows.Count-1 do begin
    AuxForms[MenuWindows.items[i].tag].Close;
  end;
end;

procedure TFMain.RestoreAuxForms;
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

//------------------------------------------------------------------------------
// Interface

//------------------------------------------------------------------------------
// Text Display

procedure TFMain.ShowLanHeader(var mess: string; St: Tstringlist);
begin

end;

procedure TFMain.ShowLanCentersText(var mess:string; St: Tstrings);
var tempcolor, icenter, ActCenter: integer;
begin
  ActCenter:=0;
  for tempcolor:=0 to NumLanColors-1 do begin // todas as cores NA LAN
    View.Centers[tempcolor].Count:=0;
  end;
end;

procedure TFMain.ShowLanRegionsText(var mess:string; St: Tstringlist);
var i: integer;
begin

end;

//--------------------------------------------------------------------------------------------
// Graphical Display

function Get24Color(color: integer): Tcolor;
var altColor: integer;
begin
  altColor:=color;
  if AttackGoal<>gcYellow then begin
    if color=iYellowGoal then altColor:=iBlueGoal
    else if color=iBlueGoal then altColor:=iYellowGoal;
  end;
  result:=colorcolor24[altColor];
end;

procedure TFmain.ShowCenters;
var tempcolor,icenter:integer;
    xp,yp,ra,xd,yd,xr,yr: double;
    x1,y1,rai: integer;
    robot: TRobotState;
begin
  ShowWorldClusters(ImageShow.canvas);
end;

procedure ShowWorldClusters(Cnv: TCanvas);
var c,i,xi,yi,xt,yt: integer;
    dMin,dMax: double;
    cu,cv:integer;
begin
  cu:=196;
  cv:=144;
  for c:=0 to cTrackColors-1 do begin
    //pix:=colorcolor[c];
    with View.Centers[c] do begin
      for i:=0 to Count-1 do begin
        with Cnv, data[i] do begin
          Pen.Color:=colorcolor24[c];

          //FCamera.CamRoTetaZToXYD(roMin,teta,zw: double; var d,xw,yw: double);
          dMin:=1.5/pi*180*roMin;
          dMax:=1.5/pi*180*romax;
          xi:=round(cu+dMin*cos(tetaMin));
          yi:=round(cv+dMin*sin(tetaMin));
          MoveTo(xi,yi);
          xt:=round(cu+dMax*cos(tetaMin));
          yt:=round(cv+dMax*sin(tetaMin));
          LineTo(xt,yt);
          xt:=round(cu+dMax*cos(tetaMax));
          yt:=round(cv+dMax*sin(tetaMax));
          LineTo(xt,yt);
          xt:=round(cu+dMin*cos(tetaMax));
          yt:=round(cv+dMin*sin(tetaMax));
          LineTo(xt,yt);
          LineTo(xi,yi);

        end;
      end;
    end;
  end;
end;

procedure TFmain.ShowRadar;
var i,x2,y2: integer;
    RS:TRobotState;
    ct,st,dx,dy: double;
begin
  with View do begin
    for i:=0 to RadarRaysCount-1 do begin
      if CBShowField.Checked then with ImageMap.canvas,Radar[i] do begin
        RS:=RobotState[myNumber];
        pen.color:=colorcolor24[color];
        ct:=cos(RS.teta);
        st:=sin(RS.teta);
        WorldToMap(RS.x+xw*ct+yw*-st,RS.y+xw*st+yw*ct,x2,y2);
        //        moveto(x1,y1);
        if i=0 then moveto(x2,y2)
        else lineto(x2,y2);
      end;
    end;
  end;
end;

procedure TFmain.ShowEdges;
var i, x1, y1: integer;
    RS: TRobotState;
    cu, cv, ra: integer;
begin
  cu := 196;
  cv := 144;
  ra := 2;
  with View do begin
    for i:=0 to EdgesCount-1 do begin
      with ImageShow.canvas, Edges[i] do begin
        RS:=RobotState[myNumber];
        pen.color:=colorcolor24[Edges[i].color1];
        
        x1:=round(cu+1.5/pi*180*ro*cos(teta));
        y1:=round(cv+1.5/pi*180*ro*sin(teta));
        moveto(x1-ra,y1);
        lineto(x1+ra+1,y1);
        pen.color:=colorcolor24[Edges[i].color2];
        moveto(x1,y1-ra);
        lineto(x1,y1+ra+1);
      end;
      if CBShowField.Checked then with ImageMap.canvas,Edges[i] do begin
        RS:=RobotState[myNumber];
        pen.color:=colorcolor24[Edges[i].color1];
        WorldToMap(RS.x+Edges[i].xw*cos(RS.teta)+Edges[i].yw*sin(-(RS.teta)), RS.y+Edges[i].xw*sin(RS.teta)+Edges[i].yw*cos(RS.teta),x1,y1);
        moveto(x1-ra,y1);
        lineto(x1+ra+1,y1);
        pen.color:=colorcolor24[Edges[i].color2];
        moveto(x1,y1-ra);
        lineto(x1,y1+ra+1);
      end;
    end;
  end;
end;

procedure TFmain.MemoRegionsToArray;
var i,tmp: integer;
    txt: string;
    ERegion: TRegion;
begin
  gtext.Clear;
  View.RegionsCount:=0;
  for i:=0 to MemoRegions.lines.count-1 do begin
    txt:=MemoRegions.lines[i];
    ParseString(txt,', ',gtext);
    if gtext.count<>4 then continue;
    tmp:=strtointdef(gtext[0],-1);
    if tmp=-1 then continue;
    ERegion.x1:=tmp;
    tmp:=strtointdef(gtext[1],-1);
    if tmp=-1 then continue;
    ERegion.y1:=tmp;
    tmp:=strtointdef(gtext[2],-1);
    if tmp=-1 then continue;
    ERegion.x2:=tmp;
    tmp:=strtointdef(gtext[3],-1);
    if tmp=-1 then continue;
    ERegion.y2:=tmp;

    ERegion.BestColor:=255;
    ERegion.BestColorPer:=0;
    View.Regions[View.RegionsCount]:=ERegion;
    inc(View.regionsCount);
  end;
end;

procedure TFmain.LoadRegions;
begin
  MemoRegions.Lines.LoadFromFile(extractfilepath(application.ExeName)+DataDir+'/regions.lst');
  MemoRegionsToArray;
end;

procedure TFmain.ShowRegions;
var i: integer;
begin
  with View, ImageShow.Canvas do begin
    for i:=0 to regionsCount-1 do begin
      if Regions[i].BestColor<>255 then begin
        pen.color:=colorcolor24[Regions[i].BestColor];
      end else begin
        pen.color:=clBlack;
      end;
      // TODO
      rectangle(rect(Regions[i].x1,Regions[i].y1,Regions[i].x2,Regions[i].y2));
    end;
  end;
end;

procedure WorldToMapDim(dw: double; var dm: integer);
begin
  dm:=round(dw*Dfieldmag);
end;

procedure WorldToMap(xw,yw: double; var xm: integer; var ym: integer);
begin
  xm:=round(FieldImageWidth/2+xw*Dfieldmag);
  ym:=round(FieldImageHeight/2-yw*Dfieldmag);
end;

function WorldToMapP(xw,yw: double):TPoint;
begin
  result.x:=round(FieldImageWidth/2+xw*Dfieldmag);
  result.y:=round(FieldImageHeight/2-yw*Dfieldmag);
end;

procedure MapToWorld( xm, ym: integer; var xw,yw: double);
begin
  xw:=(xm-FieldImageWidth/2)/Dfieldmag;
  yw:=(FieldImageHeight/2-ym)/Dfieldmag;
end;


procedure TFMain.DrawFieldMap(field_canvas: TCanvas);
var x1,y1,x2,y2,r,d: integer;
begin
  with field_canvas, FieldDims do begin //tcanvas
    brush.Style:=bsSolid;
    brush.color:=clgreen;
    pen.color:=clgreen;
;
    rectangle(0,0,FieldImageWidth,FieldImageHeight);
    WorldToMap(-FieldDepth/2,Fieldwidth/2,x1,y1);
    WorldToMap(FieldDepth/2,-Fieldwidth/2,x2,y2);
    brush.style:=bsClear;

    pen.color:=clLtGray;
    pen.Width:=1;
    Polyline([point((x1+x2) div 2,y1),point((x1+x2) div 2,y2)]);  // middle line
    WorldtoMapdim(CornerDist,d);
    arc(x1-d,y1-d,x1+d,y1+d,x1,y2,x2,y1); //|¨
    arc(x1-d,y2-d,x1+d,y2+d,x2,y2,x1,y1); //¨|
    arc(x2-d,y1-d,x2+d,y1+d,x1,y1,x2,y2); //|_
    arc(x2-d,y2-d,x2+d,y2+d,x2,y1,x1,y2); //_|
    pen.Width:=2;
    pen.color:=clBlack;
    Polyline([WorldToMapP(BoundaryDepth/2,BoundaryWidth/2), WorldToMapP(-BoundaryDepth/2,BoundaryWidth/2),
              WorldToMapP(-BoundaryDepth/2,-BoundaryWidth/2), WorldToMapP(BoundaryDepth/2,-BoundaryWidth/2),
              WorldToMapP(BoundaryDepth/2,BoundaryWidth/2)]);

    pen.color:=clLtGray;
    Polyline([point(x1,y1),point(x2,y1),point(x2,y2),point(x1,y2),point(x1,y1)]);  // walls
    WorldtoMapdim(CircleRadius,r);
    ellipse((x1+x2) div 2-r,(y1+y2) div 2-r,(x1+x2) div 2+r,(y1+y2) div 2+r);  // circle
    ellipse((x1+x2) div 2-3,(y1+y2) div 2-3,(x1+x2) div 2+3,(y1+y2) div 2+3);  // Small circle
    Polyline([WorldToMapP(FieldDepth/2,AreaWidth/2), WorldToMapP(FieldDepth/2-AreaDepth,AreaWidth/2),
             WorldToMapP(FieldDepth/2-AreaDepth,-AreaWidth/2), WorldToMapP(FieldDepth/2,-AreaWidth/2)]);
    Polyline([WorldToMapP(-FieldDepth/2,AreaWidth/2), WorldToMapP(-FieldDepth/2+AreaDepth,AreaWidth/2),
             WorldToMapP(-FieldDepth/2+AreaDepth,-AreaWidth/2), WorldToMapP(-FieldDepth/2,-AreaWidth/2)]);
    Polyline([WorldToMapP(FieldDepth/2,KeeperAreaWidth/2), WorldToMapP(FieldDepth/2-KeeperAreaDepth,KeeperAreaWidth/2),
             WorldToMapP(FieldDepth/2-KeeperAreaDepth,-KeeperAreaWidth/2), WorldToMapP(FieldDepth/2,-KeeperAreaWidth/2)]);
    Polyline([WorldToMapP(-FieldDepth/2,KeeperAreaWidth/2), WorldToMapP(-FieldDepth/2+KeeperAreaDepth,KeeperAreaWidth/2),
             WorldToMapP(-FieldDepth/2+KeeperAreaDepth,-KeeperAreaWidth/2), WorldToMapP(-FieldDepth/2,-KeeperAreaWidth/2)]);
    pen.color:=Get24Color(iYellowGoal);
    Polyline([WorldToMapP(FieldDepth/2,GoalWidth/2), WorldToMapP(FieldDepth/2+GoalDepth,GoalWidth/2),
             WorldToMapP(FieldDepth/2+GoalDepth,-GoalWidth/2), WorldToMapP(FieldDepth/2,-GoalWidth/2)]);
    pen.color:=Get24Color(iBlueGoal);
    Polyline([WorldToMapP(-FieldDepth/2,GoalWidth/2), WorldToMapP(-FieldDepth/2-GoalDepth,GoalWidth/2),
             WorldToMapP(-FieldDepth/2-GoalDepth,-GoalWidth/2), WorldToMapP(-FieldDepth/2,-GoalWidth/2)]);
    pen.Width:=1;
  end;
end;

procedure TFMain.DrawBall(field_canvas: TCanvas; var BS: TBallState; BallColor: Tcolor);
var r,rx,ry: double;
    x1,y1,x2,y2: integer;
begin
  r:=0.15;
  with field_canvas do begin //tcanvas
    brush.color:=clred;
    brush.color:=BallColor;
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
  end;
end;

procedure TFMain.DrawObstacles(var Obs: TObstacles);
var r: double;
    x1,y1,x2,y2: integer;
    i: integer;
begin
  with ImageMap.canvas do begin //tcanvas
    for i:=0 to Obs.count-1 do begin
      brush.color:=colorcolor24[Obs.centers[i].color];
      brush.style:=bsSolid;
      pen.color:=clblack;

      r:=Obs.Centers[i].r;
      WorldToMap(Obs.centers[i].x-r,Obs.centers[i].y+r,x1,y1);
      WorldToMap(Obs.centers[i].x+r,Obs.centers[i].y-r,x2,y2);
      Ellipse(x1,y1,x2,y2);
    end;
  end;
end;

procedure TFMain.ImageMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  down_x:=x;
  down_y:=y;
end;

procedure TFMain.ImageMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Xw,Yw,V: double;
    new_teta: double;
begin
  new_teta:=atan2(-Y+down_y,X-down_x);
  MapToWorld(down_x,down_y,xw,yw);
  V:=sqrt(sqr(-Y+down_y)+sqr(X-down_x));

  if ssCtrl in Shift then begin
    MouseControlX:=xw;
    MouseControlY:=yw;
    MouseControlTeta:=new_teta;
    MouseControlValid:=true;
  end else if ssShift in Shift then begin
    if CBSimulator.Checked then begin
      with BallState do begin
        x:=xw;
        y:=yw;
        Vx:=V*cos(new_teta)*0.1;
        Vy:=V*sin(new_teta)*0.1;
      end;
    end;
  end else begin
    if RGRobotSel.ItemIndex<>6 then begin
      with RobotState[RGRobotSel.ItemIndex] do begin
        x:=xw;
        y:=yw;
        teta:=new_teta;
      end;
    end;
  end;
end;

procedure TFMain.BSetXYTetaClick(Sender: TObject);
var nx,ny,nteta: double;
begin
  try
    RobotState[myNumber].x:=strtofloat(EditRobotX.text);
    RobotState[myNumber].y:=strtofloat(EditRobotY.text);
    RobotState[myNumber].teta:=strtofloat(EditRobotTeta.text);
    nx:=strtofloat(EditRobotX.text);
    ny:=strtofloat(EditRobotY.text);
    nteta:=strtofloat(EditRobotTeta.text);
  except
    EditRobotX.text:='0';
    EditRobotY.text:='0';
    RobotState[myNumber].x:=0;
    RobotState[myNumber].y:=0;
    RobotState[myNumber].teta:=0;
  end;
end;


procedure TFMain.MergeBallState(var BS,OBS: TBallState);
var lbd,lbdv: double;
begin
  if CBSimulator.Checked then begin
    lbd:=0.5;
    lbdv:=0.5;
  end
  else begin
    lbd:=0.9;
    lbdv:=0.96;
  end;
  BS.x:=BS.x*lbd+OBS.x*(1-lbd);
  BS.y:=BS.y*lbd+OBS.y*(1-lbd);

  BS.vx:=BS.vx*lbdv+OBS.vx*(1-lbdv);
  BS.vy:=BS.vy*lbdv+OBS.vy*(1-lbdv);

  BS.vxl:=BS.vxl*lbdv+OBS.vxl*(1-lbdv);
  BS.vyl:=BS.vyl*lbdv+OBS.vyl*(1-lbdv);

  BS.quality:=BS.quality*lbd+OBS.quality*(1-lbd);
end;

procedure TFMain.ExtractBallFromCenters(var ObsBallState: TBallState);
var i: integer;
    q,dist_area,rec_area: double;
    txt: string;
    ang1,ang2,xaux,yaux: double;
begin
  ObsBallState.quality:=-1;
  ObsBallCenter.quality:=-1;
  if View.Centers[iBallcolor].Count>0 then begin
    txt:=inttostr(View.Centers[iBallcolor].Count);
    for i:=0 to View.Centers[iBallcolor].Count-1 do begin
      with View.Centers[iBallcolor].data[i] do begin
        txt:=txt+': '+format('%.2f',[d]);
        if d<0.1
          then continue; // Filtra bolas más
        if d>7
          then continue; // Filtra bolas longe
        if ro > degtorad(86)
          then continue;  // Filtra bolas altas
        if area<12
          then continue;
        //
        ObsBallState.quality:=1000;
        ObsBallCenter:=View.Centers[iBallcolor].data[i];
        ObsBallCenter.quality:=-1;
        //Faz a rotação e translação da posição local da boa do referencial do robô para o mundo
        RotateAndTranslate(ObsBallState.x,ObsBallState.y,xw,yw,RobotState[myNumber].x,RobotState[myNumber].y,
          RobotState[myNumber].teta);
        //Salvei globalmente os valores de x e y pelo referencial do robô
        ObsBallState.xl:=xw;
        ObsBallState.yl:=yw;

        if (abs(ObsBallState.x)>FieldDims.BoundaryDepth/2) or (abs(ObsBallState.y)>FieldDims.BoundaryWidth/2) then begin
          ObsBallState.quality:=-1;
          continue;
        end;

        CalcBallSpeed(ObsBallState,LastObsBallState);
        break;
      end;
    end;
  end;
end;

procedure TFMain.ExtractCenterBallFromCenters(var ObsBallCenter: Tcenter);
var i: integer;
begin
  ObsBallCenter.quality:=-1;
  if View.Centers[iBallcolor].Count>0 then begin
    for i:=0 to View.Centers[iBallcolor].Count-1 do begin
      with View.Centers[iBallcolor].data[i] do begin
        if d<0.1 //distance in meters
          then continue; // Filtra bolas más
        if d>7
          then continue; // Filtra bolas longe
        if ro > degtorad(86)
          then continue;  // Filtra bolas altas
        if area<12
          then continue;
        ObsBallCenter:=View.Centers[iBallcolor].data[i];
        ObsBallCenter.quality:=round(50*area/d);
        if ObsBallCenter.quality>1000 then begin
         ObsBallCenter.quality:=1000;
        end;
        break;
      end;
    end;
  end;
end;

procedure CalcBallSpeed(var B,LB: TBallState);
var dt,flt:double;
begin
  dt:=40e-3;
  flt:=0.85; // Bug conceptual
  B.vx:=LB.vx*flt + (1-flt)*(B.x-LB.x)/dt;
  B.vy:=LB.vy*flt + (1-flt)*(B.y-LB.y)/dt;
end;

procedure TFMain.JoyControl( var v,vn,w: double; var kick:boolean);
var ret,i: integer;
    txt: string;
begin

  if joyfd=0 then  exit;

  while true do begin
    ret:=fileread(joyfd, joyEvent, sizeof(joyEvent));
    if ret<0 then break;

    for i:=0 to 3 do begin
      if (joyEvent.etype=JS_EVENT_AXIS) and (joyEvent.number=i) then begin
        axis[i]:=joyEvent.value;
      end;
      if (joyEvent.etype=JS_EVENT_BUTTON) and (joyEvent.number=i) then begin
        buts[i]:=joyEvent.value;
      end;
    end;
  end;

    // limitar velocidades
    if axis[1] > 8000 then v:= (axis[1]-8000) * (-1/32767)
    else if axis[1] < -8000 then v:= (axis[1]+8000) * (-1/32767)
    else v:=0;

    if axis[0] > 8000 then vn:= (axis[0]-8000) * (-1/32767)
    else if axis[0] < -8000 then vn:= (axis[0]+8000) * (-1/32767)
    else vn:=0;

    if axis[2] > 8000 then w:= (axis[2]-8000) * (-1.5/32767)
    else if axis[2] < -8000 then w:= (axis[2]+8000) * (-1.5/32767)
    else w:=0;


    txt:='';
    txt:=txt+'v'+format(':%.1f ',[v]);
    txt:=txt+'vn'+format(':%.1f ',[vn]);
    txt:=txt+'w'+format(':%.1f ',[w]);

    if buts[0] = 1 then
    begin
      kick:=true;
      txt:=txt+'kick: true';
    end
    else
    begin
      kick:=false;
      txt:=txt+'kick: false';
    end;

    Fjoy.EditJoyEvent.Text:=txt;
end;

procedure TFMain.MenuExitClick(Sender: TObject);
begin
  close;
end;


procedure TFMain.UpdateWorldState(var OldRobotState,NewRobotState: TRobotState);
var pdist,pteta: double;
    i: integer;
begin
  // update do estado "observado" da bola
  with ObsBallState do begin
    pdist:=Dist(x-OldRobotState.x,y-OldRobotState.y);
    pteta:=ATan2(y-OldRobotState.y,x-OldRobotState.x)-OldRobotState.teta;
    x:=NewRobotState.x+cos(pteta+NewRobotState.teta)*pdist;
    y:=NewRobotState.y+sin(pteta+NewRobotState.teta)*pdist;
  end;

  with BallState do begin
    pdist:=Dist(x-OldRobotState.x,y-OldRobotState.y);
    pteta:=ATan2(y-OldRobotState.y,x-OldRobotState.x)-OldRobotState.teta;
    x:=NewRobotState.x+cos(pteta+NewRobotState.teta)*pdist;
    y:=NewRobotState.y+sin(pteta+NewRobotState.teta)*pdist;
  end;

  with LastObsBallState do begin
    pdist:=Dist(x-OldRobotState.x,y-OldRobotState.y);
    pteta:=ATan2(y-OldRobotState.y,x-OldRobotState.x)-OldRobotState.teta;
    x:=NewRobotState.x+cos(pteta+NewRobotState.teta)*pdist;
    y:=NewRobotState.y+sin(pteta+NewRobotState.teta)*pdist;
  end;

  // update do estado "observado" dos obstáculos
  for i:=0 to Obstacles.count-1 do begin
    with Obstacles.centers[i] do begin
      pdist:=Dist(xw-OldRobotState.x,yw-OldRobotState.y);
      pteta:=ATan2(yw-OldRobotState.y,xw-OldRobotState.x)-OldRobotState.teta;
      xw:=NewRobotState.x+cos(pteta+NewRobotState.teta)*pdist;
      yw:=NewRobotState.y+sin(pteta+NewRobotState.teta)*pdist;
    end;
  end;
end;

procedure TFMain.CheckLines;
var i: integer;
begin
  for i:=low(GWline.iEdges) to high(GWline.iEdges) do begin
    GWLine.iEdges[i]:=-1;
    WGLine.iEdges[i]:=-1;
  end;
  WGLine.conf:=0;
  GWline.conf:=0;

  // Procura s edges de verde para branco e/ou de branco para verde
  for i:=0 to View.EdgesCount-1 do begin
    if (View.Edges[i].color1=iGreenColor) and
       (View.Edges[i].color2=iWhiteColor) and
       (GWLine.iEdges[min(View.Edges[i].lineNum,high(GWLine.iEdges))]=-1) then begin
      GWLine.iEdges[min(View.Edges[i].lineNum,high(GWLine.iEdges))]:=i;
    end else if (View.Edges[i].color1=iWhiteColor) and
                (View.Edges[i].color2=iGreenColor) and
                (WGLine.iEdges[min(View.Edges[i].lineNum,high(WGLine.iEdges))]=-1) then begin
      WGLine.iEdges[min(View.Edges[i].lineNum,high(WGLine.iEdges))]:=i;
    end;
  end;

  CheckLine(GWLine);
  CheckLine(WGLine);    // parametro
end;

procedure TFMain.CheckLine(var ELine:TEdgeLine);
var x1,y1,x2,y2,x3,y3,s,ac,tetauv,kapa,xr,yr: double;
begin
  with Eline do begin
    conf:=0;
    if (iEdges[0]<>-1) and (iEdges[1]<>-1) and (iEdges[2]<>-1) then begin
      x1:=View.Edges[iEdges[0]].xw;
      y1:=View.Edges[iEdges[0]].yw;
      x2:=View.Edges[iEdges[1]].xw;
      y2:=View.Edges[iEdges[1]].yw;
      x3:=View.Edges[iEdges[2]].xw;
      y3:=View.Edges[iEdges[2]].yw;
      s:= ((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)) * ((x3-x1)*(x3-x1)+(y3-y1)*(y3-y1));
      if s>0 then begin
        ac:=((x2-x1)*(x3-x1)+(y2-y1)*(y3-y1))/sqrt(s);
        if (ac<1) and (ac>-1) then
          tetauv:=arccos(ac)
        else
          tetauv:=pi;
      end else begin
        tetauv:=pi;
      end;
      if tetauv<10*pi/180 then begin  // Parametro
        conf:=1;
        teta:=(atan2(y3-y1,x3-x1)+pi/2);
        kapa:=(-x1*(x1-x3)-y1*(y1-y3))/((x1-x3)*(x1-x3)+(y1-y3)*(y1-y3));
        xr:=kapa*(x1-x3)+x1;
        yr:=kapa*(y1-y3)+y1;
        odist:=sqrt(xr*xr+yr*yr);//+0.05;
      end;
    end;
  end;
end;

procedure TFMain.ShowLine(var ELine:TEdgeLine);
begin
  if ELine.conf>0 then begin
    with ImageShow.canvas,ELine do begin
      pen.color:=clOlive;
      moveto(View.Edges[iEdges[0]].xi,View.Edges[iEdges[0]].yi);
      lineto(View.Edges[iEdges[2]].xi,View.Edges[iEdges[2]].yi);
    end;
  end;
end;

//-----------------------------------------------------------------------------------------
// CLB Utils

function CLBGetChecks(CLB: TCheckListBox): string;
var i: integer;
    checks: string;
begin
  checks:='';
  for i:=0 to CLB.Items.Count-1 do begin
    if CLB.Checked[i] then checks:=checks +'1' else checks:=checks +'0';
  end;
  result:=checks;
end;

procedure CLBSetChecks(CLB: TCheckListBox; checks: string);
var i: integer;
begin
  for i:=0 to min(CLB.Items.Count,length(checks))-1 do begin
    if checks[i+1]='1' then CLB.Checked[i]:=true else CLB.Checked[i]:=false;
  end;
end;

procedure StringsDraw(Cnv: TCanvas; SL: TStrings; var X,Y : integer);
var i,th: integer;
begin
  with Cnv do begin
    th:=TextHeight('1');
    for i:=0 to SL.Count-1 do begin
      TextOut(X,Y,SL.Strings[i]);
      Y:=Y+th;
    end;
  end;
end;

procedure TFMain.ImageShowMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then begin
    draw_x:=x;
    draw_y:=y;
  end;
end;

procedure TFMain.CBSimulatorClick(Sender: TObject);
var i: integer;
begin
  if CBSimulator.Checked then begin
    TimerSimulator.Enabled:=True;
    for i := 0 to MaxRobots - 1 do begin
      RobotStatus[i].active := true;
      RobotState[i].count := 1;
    end;
  end else begin
    TimerSimulator.Enabled:=False;
  end;
end;

procedure TFMain.MenuAboutClick(Sender: TObject);
begin
  Showmessage('5dpo2000 Decision'+#$0d+'(c)2000-2008 5dpo');
end;

procedure TFMain.SdpoUDPSimTwoError(const msg: string; aSocket: TLSocket);
begin

end;

procedure TFMain.SdpoUDPSimTwoReceive(aSocket: TLSocket);
var
  msg: string;
begin
  SdpoUDPSimTwo.GetMessage(msg);
  if CBSimTwo.Checked=true then begin
    ProcessSimTwoMsg(msg);
    SimTwoMainLoop;
  end else if CBISTScenario.Checked=true then begin
    ProcessMermaidMsg(msg);
    ISTMainLoop;
    //SimMainLoop;
  end
end;

procedure TFMain.SdpoUDPSuperError(const msg: string; aSocket: TLSocket);
begin
  SdpoUDPSuper.Disconnect(true);
  SdpoUDPSuper.Listen(7272+RGRobotSel.ItemIndex);
end;

procedure TFMain.SdpoUDPSuperReceive(aSocket: TLSocket);
var packet_str: string;
begin
  try
    aSocket.GetMessage(packet_str);
    if CoachMode then begin  // se for treinador recebendo um packet de um jogador
      FCoachMain.ProcessPlayerPacket(packet_str);
    end else begin   // se o packet for do coach
      ProcessCoachPacket(packet_str);
    end;
  except
  end;
end;

procedure TFMain.TimerSimulatorTimer(Sender: TObject);
begin
  SimMainLoop;
end;

procedure TFMain.ManualControl(var V, Vn, W: double; var Kick: boolean);
begin
  v:=strtofloatDef(EditV.text,0);
  vn:=strtofloatDef(EditVn.text,0);
  w:=strtofloatDef(EditW.text,0);
  kick:=false;
end;


procedure TFMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  v_key, vn_key, w_key, kicktest: string;
begin

  if key=VK_F1 then FMain.Close;

  if RGController.ItemIndex=5 then begin

    if key=VK_UP then begin
      if EditV.Text='0' then begin
        EditV.Text:=v_key;
      end else if EditV.Text<>v_key then begin
        EditV.Text:='0';
      end;
      key:=0;
    end;

    if key=VK_DOWN then begin
      if EditV.Text='0' then begin
        EditV.Text:='-'+v_key;
      end else if EditV.Text=v_key then begin
        EditV.Text:='0';
      end;
      key:=0;
    end;

    if key=VK_LEFT then begin
      if EditVn.Text='0' then begin
        EditVn.Text:=vn_key;
      end else if EditVn.Text<>vn_key then begin
        EditVn.Text:='0';
      end;
      key:=0;
    end;

    if key=VK_RIGHT then begin
      if EditVn.Text='0' then begin
        EditVn.Text:='-'+vn_key;
      end else if EditVn.Text=vn_key then begin
        EditVn.Text:='0';
      end;
      key:=0;
    end;

    if key=VK_N then begin
      if EditW.Text='0' then begin
        EditW.Text:=w_key;
      end else if EditW.Text<>w_key then begin
        EditW.Text:='0';
      end;
      key:=0;
    end;

    if key=VK_M then begin
      if EditW.Text='0' then begin
        EditW.Text:='-'+w_key;
      end else if EditW.Text=w_key then begin
        EditW.Text:='0';
      end;
      key:=0;
    end;

    if key=VK_SPACE then begin
      EditV.Text:='0';
      EditVn.Text:='0';
      EditW.Text:='0';
      key:=0;
    end;

    if key=VK_SHIFT then begin
      Ser2_Out:=kicktest;
      key:=0;
    end;

  end;
end;

procedure TFMain.saveudplogs(filename,s: string);
var
F: file;
ToWrite,NumWritten: integer;
begin
  AssignFile(F, filename);
  if not FileExists(filename)
    then Rewrite(F,1)
    else Reset(F,1);
  Seek(F,filesize(F));
  ToWrite:=length(s);

  Blockwrite(F,ToWrite,sizeof(ToWrite),NumWritten);
  if sizeof(ToWrite)<>NumWritten then Memoudp.lines.add('Error -> Save UDP logs');

  Blockwrite(F,s[1],towrite,NumWritten);
  Memoudp.lines.add(inttostr(NumWritten));
  if ToWrite<>NumWritten then Memoudp.lines.add('Error -> Save UDP logs');

  CloseFile(F);
end;

procedure TFMain.ProcessCoachPacket(packet_str: string);
var CoachInfo: TCoachInfo;
    i: integer;
begin
// é o que o jogador recebe
  if length(packet_str) <> sizeof(CoachInfo) then exit;

  copymemory(@CoachInfo, @(packet_str[1]), sizeof(CoachInfo));
  
  if CoachInfo.Magic <> PACKET_COACH_MAGIC then exit;

  for i := 0 to MaxRobots - 1 do begin
    // update robot role
    if RobotInfo[i].role <> CoachInfo.RobotState[i].role then begin
      with RobotInfo[i] do begin
        roleTime := GetTickCount;
        last_role := role;
        role := CoachInfo.RobotState[i].role;

        ZeroMemory(@TaskPars[i], sizeof(TaskPars[0]));
      end;
    end;

    // don't update my own state
    if (i = MyNumber) and not (CBSimulator.Checked) then continue;

    // update all others
    with CoachInfo.RobotState[i] do begin
      RobotState[i].count := Ord(active);
      RobotState[i].x := x;
      RobotState[i].y := y;
      RobotState[i].teta := teta;
      RobotState[i].vx := vx;
      RobotState[i].vy := vy;
      RobotState[i].w := w;

//      VxyToVVn(teta, vx, vy, RobotState[i].v, RobotState[i].vn);


      RobotStatus[i].active := active;
      EditRoleName.Text:=EditRoleName.Text;

    end;
  end;

  //Era isso q tava fudendo o merging da bola na formação!!!
//  if ((BallState.quality < CoachInfo.BallState.coachQuality)and(BallState.quality < 500)and(Dist(BallState.x-RobotState[i].x,BallState.y-RobotState[i].y)>1.5)) then begin
     BallState.x:=CoachInfo.BallState.x;
     BallState.y:=CoachInfo.BallState.y;
     BallState.vx:=CoachInfo.BallState.vx;
     BallState.vy:=CoachInfo.BallState.vy;
     BallState.x_next:=CoachInfo.BallState.x_next;
     BallState.y_next:=CoachInfo.BallState.y_next;
     BallState.quality:=CoachInfo.BallState.coachQuality;
//  end;

  Play:=CoachInfo.Play;
  EditPlayName.Text:= CPlayString[Play];
end;

procedure TFMain.btnrunudplogsClick(Sender: TObject);
var
F:file;
NumRead,i: integer;
filename,data: string;
NumberBytes,Tam,count: integer;
Buf: array[1..1024] of Char;

begin
  //SdpoUDP.Active:=false;
  SdpoUDP.Disconnect;

  filename:=EditFileudplog.Text;
  AssignFile(F, filename);
  if not FileExists(filename)
    then Memoudp.lines.add('Error -> File not found')
    else Reset(F,1);

  Count:=0;
  while not Eof(F) do
  begin
    Inc(Count);
    BlockRead(F,Tam,sizeof(Tam),NumRead);  // le tamanho bloco
    BlockRead(F,Buf,Tam,NumRead);   // le bloco
    memoudp.lines.Add(inttostr(Count));
    data:=Buf;
    zeromemory(@(NetBuffer.data[0]),UDPBufSize);
    NumberBytes:= NumRead;
    if NumberBytes<UDPBufSize then begin
      NetBuffer.MessSize:=NumberBytes;
      NetBuffer.ReadDisp:=0;
      copymemory(@(NetBuffer.data[0]),@(data[1]),NumberBytes);
      MainLoop;
    end;

    sleep(20);
  end;

  Closefile(F);
  SdpoUDP.Listen(7171);
end;

procedure TFMain.TimerUdplogTimer(Sender: TObject);
begin
  UdpMainLoop;
end;

procedure TFMain.UdpMainLoop;
var
F:file;
NumRead,i: integer;
filename,data: string;
NumberBytes,Tam,count: integer;
Buf: array[1..1024] of Char;

begin

  if Eof(Flogs) then begin
   TimerUdplog.Enabled:=false;
   CBudpClick(CBudp);
   exit;
  end;

  Inc(CountUdp);
  memoudp.lines.Add(inttostr(CountUdp));

  BlockRead(Flogs,Tam,sizeof(Tam),NumRead);  // le tamanho bloco
  BlockRead(Flogs,Buf,Tam,NumRead);   // le bloco

  data:=Buf;
  zeromemory(@(NetBuffer.data[0]),UDPBufSize);
  NumberBytes:= NumRead;
  if NumberBytes<UDPBufSize then begin
    NetBuffer.MessSize:=NumberBytes;
    NetBuffer.ReadDisp:=0;
    copymemory(@(NetBuffer.data[0]),@(data[1]),NumberBytes);
    MainLoop;
  end;
  sleep(1);
 
end;

procedure TFMain.CBudpClick(Sender: TObject);
begin
  if CBudp.Checked then begin
    SdpoUDP.disconnect;
    CountUdp:=0;
    AssignFile(Flogs, EditFileudplog.Text);
    if not FileExists(EditFileudplog.Text) then
    begin
      Memoudp.lines.add('Error -> File not found');
      exit;
    end else
    begin
      Reset(Flogs,1);
    end;
    TimerUdplog.Enabled:=True;
  end else begin
    Closefile(Flogs);
    SdpoUDP.listen(6000);
    TimerUdplog.Enabled:=False;
  end;
end;

procedure TFMain.Timer_NoCameraTimer(Sender: TObject);
begin
  MainLoop;
end;

procedure TFMain.ProcessSimTwoMsg(Data: string);
var
  gData: TStringList;
  i,WB:integer;

begin
  Randomize;
  gData:=TStringList.Create;
  gData.Text:=Data;

  myNumber:=StrToIntDef(gData[0], 0);

  RobotState[myNumber].x:=strtofloatDef(gData[1], 0);
  RobotState[myNumber].y:=strtofloatDef(gData[2], 0);
  RobotState[myNumber].teta:=strtofloatDef(gData[3], 0);
  if FParam.RGSide.ItemIndex=1 then begin
    RotateAndTranslate(RobotState[myNumber].x, RobotState[myNumber].y,RobotState[myNumber].x, RobotState[myNumber].y,0,0,degtorad(180));
    RobotState[myNumber].teta:=NormalizeAngle(RobotState[myNumber].teta+degtorad(180));
  end;

  for i:=0 to NumMotors-1 do begin
    DStates[i].Vact:=StrToInt(gData[i+4]);
  end;
  LastObsBallState:=ObsBallState;
  ObsBallState.x:=strtofloatDef(gData[7], 0);// + Random(10)*0.005;
  ObsBallState.y:=strtofloatDef(gData[8], 0);// + Random(10)*0.005;

  WB:=strtointDef(gData[9], 0);

  //Obstacles.Centers[0].x:=strtofloatDef(gData[10], 0);
  //Obstacles.Centers[1].x:=strtofloatDef(gData[11], 0);
  //Obstacles.Centers[2].x:=strtofloatDef(gData[12], 0);
  //Obstacles.Centers[3].x:=strtofloatDef(gData[13], 0);
  //Obstacles.Centers[4].x:=strtofloatDef(gData[14], 0);    //Obstacles X
  //
  //Obstacles.Centers[0].y:=strtofloatDef(gData[15], 0);
  //Obstacles.Centers[1].y:=strtofloatDef(gData[16], 0);
  //Obstacles.Centers[2].y:=strtofloatDef(gData[17], 0);
  //Obstacles.Centers[3].y:=strtofloatDef(gData[18], 0);
  //Obstacles.Centers[4].y:=strtofloatDef(gData[19], 0);
  //
  //Obstacles.Centers[0].r:=strtofloatDef(gData[20], 0);
  //Obstacles.Centers[1].r:=strtofloatDef(gData[21], 0);
  //Obstacles.Centers[2].r:=strtofloatDef(gData[22], 0);
  //Obstacles.Centers[3].r:=strtofloatDef(gData[23], 0);
  //Obstacles.Centers[4].r:=strtofloatDef(gData[24], 0);
  //
  //Obstacles.Centers[5].x:=strtofloatDef(gData[25], 0);
  //Obstacles.Centers[5].y:=strtofloatDef(gData[26], 0);
  //Obstacles.Centers[5].r:=0.5;
  //Obstacles.Centers[6].x:=strtofloatDef(gData[27], 0);
  //Obstacles.Centers[6].y:=strtofloatDef(gData[28], 0);
  //Obstacles.Centers[6].r:=0.5;
  //Obstacles.Centers[7].x:=strtofloatDef(gData[29], 0);
  //Obstacles.Centers[7].y:=strtofloatDef(gData[30], 0);
  //Obstacles.Centers[7].r:=0.5;
  //Obstacles.Centers[8].x:=strtofloatDef(gData[31], 0);
  //Obstacles.Centers[8].y:=strtofloatDef(gData[32], 0);
  //Obstacles.Centers[8].r:=0.5;
  //Obstacles.Centers[9].x:=strtofloatDef(gData[33], 0);
  //Obstacles.Centers[9].y:=strtofloatDef(gData[34], 0);
  //Obstacles.Centers[9].r:=0.5;
  //Obstacles.Centers[10].x:=strtofloatDef(gData[35], 0);
  //Obstacles.Centers[10].y:=strtofloatDef(gData[36], 0);
  //Obstacles.Centers[10].r:=0.5;
  //Obstacles.Centers[11].x:=strtofloatDef(gData[37], 0);
  //Obstacles.Centers[11].y:=strtofloatDef(gData[38], 0);
  //Obstacles.Centers[11].r:=0.5;
  //Obstacles.Centers[12].x:=strtofloatDef(gData[39], 0);
  //Obstacles.Centers[12].y:=strtofloatDef(gData[40], 0);
  //Obstacles.Centers[12].r:=0.5;
  //Obstacles.Centers[13].x:=strtofloatDef(gData[41], 0);
  //Obstacles.Centers[13].y:=strtofloatDef(gData[42], 0);
  //Obstacles.Centers[13].r:=0.5;
  //Obstacles.Centers[14].x:=strtofloatDef(gData[43], 0);
  //Obstacles.Centers[14].y:=strtofloatDef(gData[44], 0);
  //Obstacles.Centers[14].r:=0.5;
  //Obstacles.Centers[15].x:=strtofloatDef(gData[45], 0);
  //Obstacles.Centers[15].y:=strtofloatDef(gData[46], 0);
  //Obstacles.Centers[15].r:=0.5;
  //Obstacles.Centers[16].x:=strtofloatDef(gData[47], 0);
  //Obstacles.Centers[16].y:=strtofloatDef(gData[48], 0);
  //Obstacles.Centers[16].r:=0.5;
  //Obstacles.Centers[17].x:=strtofloatDef(gData[49], 0);
  //Obstacles.Centers[17].y:=strtofloatDef(gData[50], 0);
  //Obstacles.Centers[17].r:=0.5;
  //Obstacles.Centers[18].x:=strtofloatDef(gData[51], 0);
  //Obstacles.Centers[18].y:=strtofloatDef(gData[52], 0);
  //Obstacles.Centers[18].r:=0.5;

  //Obstacles.Centers[5].x:=strtofloatDef(gData[15], 0);
  //Obstacles.Centers[6].x:=strtofloatDef(gData[16], 0);
  //Obstacles.Centers[7].x:=strtofloatDef(gData[17], 0);
  //Obstacles.Centers[8].x:=strtofloatDef(gData[18], 0);
  //Obstacles.Centers[9].x:=strtofloatDef(gData[19], 0);
  //Obstacles.Centers[10].x:=strtofloatDef(gData[20], 0);
  //Obstacles.Centers[11].x:=strtofloatDef(gData[21], 0);
  //
  //Obstacles.Centers[0].y:=strtofloatDef(gData[22], 0);
  //Obstacles.Centers[1].y:=strtofloatDef(gData[23], 0);
  //Obstacles.Centers[2].y:=strtofloatDef(gData[24], 0);
  //Obstacles.Centers[3].y:=strtofloatDef(gData[25], 0);
  //Obstacles.Centers[4].y:=strtofloatDef(gData[26], 0);
  //Obstacles.Centers[5].y:=strtofloatDef(gData[27], 0);     //Obstacles Y
  //Obstacles.Centers[6].y:=strtofloatDef(gData[28], 0);
  //Obstacles.Centers[7].y:=strtofloatDef(gData[29], 0);
  //Obstacles.Centers[8].y:=strtofloatDef(gData[30], 0);
  //Obstacles.Centers[9].y:=strtofloatDef(gData[31], 0);
  //Obstacles.Centers[10].y:=strtofloatDef(gData[32], 0);
  //Obstacles.Centers[11].y:=strtofloatDef(gData[33], 0);
  //
  //Obstacles.Centers[0].r:=strtofloatDef(gData[34], 0);
  //Obstacles.Centers[1].r:=strtofloatDef(gData[35], 0);
  //Obstacles.Centers[2].r:=strtofloatDef(gData[36], 0);
  //Obstacles.Centers[3].r:=strtofloatDef(gData[37], 0);
  //Obstacles.Centers[4].r:=strtofloatDef(gData[38], 0);
  //Obstacles.Centers[5].r:=strtofloatDef(gData[39], 0);       //Obstacles R
  //Obstacles.Centers[6].r:=strtofloatDef(gData[40], 0);
  //Obstacles.Centers[7].r:=strtofloatDef(gData[41], 0);
  //Obstacles.Centers[8].r:=strtofloatDef(gData[42], 0);
  //Obstacles.Centers[9].r:=strtofloatDef(gData[43], 0);
  //Obstacles.Centers[10].r:=strtofloatDef(gData[44], 0);
  //Obstacles.Centers[11].r:=strtofloatDef(gData[45], 0);

  //Obstacles.Centers[0].x:=strtofloatDef(gData[10], 0);
  //Obstacles.Centers[0].y:=strtofloatDef(gData[11], 0);
  //Obstacles.Centers[0].r:=0.5;
  //Obstacles.Centers[1].x:=strtofloatDef(gData[12], 0);
  //Obstacles.Centers[1].y:=strtofloatDef(gData[13], 0);
  //Obstacles.Centers[1].r:=0.5;
  //Obstacles.Centers[2].x:=strtofloatDef(gData[14], 0);
  //Obstacles.Centers[2].y:=strtofloatDef(gData[15], 0);
  //Obstacles.Centers[2].r:=0.5;
  //Obstacles.Centers[3].x:=strtofloatDef(gData[16], 0);
  //Obstacles.Centers[3].y:=strtofloatDef(gData[17], 0);
  //Obstacles.Centers[3].r:=0.5;
  //Obstacles.Centers[4].x:=strtofloatDef(gData[18], 0);
  //Obstacles.Centers[4].y:=strtofloatDef(gData[19], 0);
  //Obstacles.Centers[4].r:=0.5;
  //Obstacles.Centers[5].x:=strtofloatDef(gData[20], 0);
  //Obstacles.Centers[5].y:=strtofloatDef(gData[21], 0);
  //Obstacles.Centers[5].r:=0.5;
  //Obstacles.Centers[6].x:=strtofloatDef(gData[22], 0);
  //Obstacles.Centers[6].y:=strtofloatDef(gData[23], 0);
  //Obstacles.Centers[6].r:=0.5;
  //Obstacles.Centers[7].x:=strtofloatDef(gData[24], 0);
  //Obstacles.Centers[7].y:=strtofloatDef(gData[25], 0);
  //Obstacles.Centers[7].r:=0.5;
  //Obstacles.Centers[8].x:=strtofloatDef(gData[26], 0);
  //Obstacles.Centers[8].y:=strtofloatDef(gData[27], 0);
  //Obstacles.Centers[8].r:=0.5;

  //Obstacles.count:=4;
  //Obstacles.count:=38;
  //Obstacles.count:=24;
  //Obstacles.count:=18;
  //Obstacles.count:=12;



  if WB=1 then begin
    RobotState[myNumber].withball:=true;
  end else begin
    RobotState[myNumber].withball:=false;
  end;

  if FParam.RGSide.ItemIndex=1 then begin
    RotateAndTranslate(ObsBallState.x,ObsBallState.y,ObsBallState.x,ObsBallState.y,0,0,degtorad(180));
  end;
  if Dist(ObsBallState.x-RobotState[myNumber].x,ObsBallState.y-RobotState[myNumber].y)<4 then begin
    ObsBallState.quality:=1000;
  end else begin
    ObsBallState.quality:=-1000;
  end;

  gData.Free;

end;

procedure TFMain.ProcessMermaidMsg(Data: string);
var
  gData: TStringList;
  i:integer;
  myNumb: array[0..4] of integer;
  receivedX,receivedY,receivedTeta: array[0..4] of double;
  mn,mx,my,mteta: array[0..4] of string;
  vxb,vyb,xb,yb,sigr,sigphi: double;
  mvxb,mvyb,mxb,myb,msigrb,msigphib: string;

begin
  Randomize;
  gData:=TStringList.Create;
  gData.Text:=Data;

  SScanf(Data,'%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s',
           [@mn[0], @mx[0], @my[0], @mteta[0],
            @mn[1], @mx[1], @my[1], @mteta[1],
            @mn[2], @mx[2], @my[2], @mteta[2],
            @mn[3], @mx[3], @my[3], @mteta[3],
            @mn[4], @mx[4], @my[4], @mteta[4],
            @mvxb, @mvyb, @mxb, @myb,
            @msigrb, @msigphib]);

  for i:=0 to 4 do begin
    myNumb[i]:=strtointdef(mn[i],2);
    receivedX[i]:=strtofloatdef(mx[i],2);
    receivedY[i]:=strtofloatdef(my[i],2);
    receivedTeta[i]:=strtofloatdef(mteta[i],2);
    if myNumber=i then begin
        RobotState[myNumber].x:=receivedX[i];
        RobotState[myNumber].y:=receivedY[i];
        RobotState[myNumber].teta:=receivedTeta[i];
    end else begin
        RobotState[i].x:=receivedX[i];
        RobotState[i].y:=receivedY[i];
        RobotState[i].teta:=receivedTeta[i];
    end;
  end;
  vxb:=strtofloatdef(mvxb,2);
  vyb:=strtofloatdef(mvyb,2);
  xb:=strtofloatdef(mxb,2);
  yb:=strtofloatdef(myb,2);

  Sr:=strtofloatdef(msigrb,2);
  Sphi:=strtofloatdef(msigphib,2);

  //sigr:=strtofloatdef(msigrb,2);
  //sigphi:=strtofloatdef(msigphib,2);
  //sigtemp.setv(0,0,sigr);
  //sigtemp.setv(0,1,0);
  //sigtemp.setv(1,0,0);
  //sigtemp.setv(1,1,sigphi);
  //sigmaT:=sigtemp;

  LastObsBallState:=ObsBallState;
  ObsBallState.x:=xb;
  ObsBallState.y:=yb;
  ObsBallState.vx:=vxb;
  ObsBallState.vy:=vyb;

  gData.Free;

end;


initialization
  {$I Main.lrs}

end.



