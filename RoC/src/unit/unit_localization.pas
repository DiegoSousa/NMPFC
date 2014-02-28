unit unit_Localization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Spin, IniPropStorage, Grids, TAGraph, TASeries,
  unit_localizationAux, unit_Log;

const
  CDataConfigDir='data';
  CDataConfigFile='ConfigLocalization.ini';
  CNLogColumns=11+2*MaxPointsNumber;

type

  { TInputData }

  TLocInputData=record
    OdosData: TOdosData;
    Compass: TCompassData;
    VPointList: TPointList;
  end;

  TLocOutputData=record
    x, y, teta: double;
    Avaible: boolean;
    LocAdvV, LocAdvVn, LocAdvW: double;
  end;

  { TForm_Localization }

  TForm_Localization = class(TForm)
    ButtonSetClosePnt: TButton;
    Button_Reset: TButton;
    Button_RunOptimizeMatch1: TButton;
    Button_SetCompassOffSet: TButton;
    Button_ChartCompassClear: TButton;
    Button_RunOptimizeMatchGlobal1: TButton;
    Button_SetLocalizationTrakingParameters: TButton;
    Button_LoadSeedPointsFile: TButton;
    Button_RunOptimizeMatch: TButton;
    Button_DebugReset: TButton;
    Button_DebugPlay: TButton;
    Button_LoadMapFile: TButton;
    Button_RunOptimizeMatchGlobal: TButton;
    Button_SetProcessLocalizationStateParameters: TButton;
    Button_SetGlobalLocalizationParameters: TButton;
    Button_SetStartingPose: TButton;
    Button_test: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart2LineSeries1: TLineSeries;
    Chart_GlobalLocalizationData: TChart;
    ChartCompass: TChart;
    ChartVOdod: TChart;
    ChartCompassDiferenceLineSeries: TLineSeries;
    ChartCompassOkBarSeries: TBarSeries;
    ChartCompassLostBarSeries: TBarSeries;
    ChartCompassLineSeries: TLineSeries;
    ChartCompassFilteredLineSeries: TLineSeries;
    ChartCompassTetaLocLineSeries: TLineSeries;
    ChartWOdod: TChart;
    ChartVOdodLineSeries: TLineSeries;
    ChartWOdodLineSeries: TLineSeries;
    Chart_GlobalLocalizationDataBarSeriesCost: TBarSeries;
    CheckBox_DisableCompass: TCheckBox;
    CheckBox_Show: TCheckBox;
    CheckBox_ChartCompass: TCheckBox;
    CheckBox_ChartCompassFiltered: TCheckBox;
    CheckBox_ChartTetaLoc: TCheckBox;
    CheckBox_ChartDiference: TCheckBox;
    CheckBox_LimitMeasureMaxDist: TCheckBox;
    CheckBoxtimertest: TCheckBox;
    Edit_GlobalLocalizationMaxDeltaNumHypothesis: TEdit;
    Edit_GlobalLocalizationMinIterationDeltaNumHypothesis: TEdit;
    Edit_GlobalLocalizationTimesBetter: TEdit;
    Edit_ModeNav: TEdit;
    Edit_NavigateV: TEdit;
    Edit_NavigateW: TEdit;
    Edit_NumMinClosePoints: TEdit;
    Edit_MinDistClosePnt: TEdit;
    Edit_MaxDistClosePnt: TEdit;
    Edit_AngleClosePnt: TEdit;
    Edit_GlobalNumHypothesis: TEdit;
    Edit_GlobalLocalizationMode: TEdit;
    Edit_GlobalLocalizationMaxV: TEdit;
    Edit_GlobalLocalizationMaxW: TEdit;
    Edit_GlobalLocalizationFilterCutOff: TEdit;
    Edit_GlobalLocalizationMinIteration: TEdit;
    Edit_GlobalMaxAng2Elimination: TEdit;
    Edit_GlobalRPropIMinterationNumber: TEdit;
    Edit_GlobalMaxDist2Elimination: TEdit;
    Edit_OffsetAng: TEdit;
    Edit_Timings: TEdit;
    Edit_TrackingMode: TEdit;
    Edit_ProcessLocalizationFilterCutOff: TEdit;
    Edit_ProcessLocalizationMinIteration: TEdit;
    Edit_ProcessLocalizationMaxW: TEdit;
    Edit_ProcessLocalizationMaxV: TEdit;
    Edit_ProcessLocalizationMode: TEdit;
    Edit_LocalizationState: TEdit;
    Edit_ActualPose: TEdit;
    Edit_ActualSpeed: TEdit;
    Edit_CompassFilterCutoff: TEdit;
    Edit_SeedsPointsNumber: TEdit;
    Edit_MinVarXY: TEdit;
    Edit_MinVarTeta: TEdit;
    Edit_MeasureMaxDistance: TEdit;
    ImageLocMapGlobal: TImage;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    RadioGroup_FusionMode: TRadioGroup;
    RadioGroup_LocalizationDataSelectMap: TCheckGroup;
    Edit_SensorsKErrorTeta: TEdit;
    Edit_OdometryKErrorXY: TEdit;
    Edit_OdometryKErrorTeta: TEdit;
    Edit_SensorsKErrorXY: TEdit;
    Edit_RPropIterationNumber: TEdit;
    Edit_Lc: TEdit;
    Edit_StartingPoseVarY: TEdit;
    Edit_StartingPoseVarTeta: TEdit;
    Edit_StartingPoseX: TEdit;
    Edit_StartingPoseVarX: TEdit;
    Edit_StartingPoseY: TEdit;
    Edit_RunOptimizeMatchTime: TEdit;
    Edit_SeedPointsFileName: TEdit;
    Edit_MapHeight: TEdit;
    Edit_MapResolution: TEdit;
    Edit_MapWith: TEdit;
    Edit_DebugTime: TEdit;
    Edit_MousePosition: TEdit;
    Edit_MapFileName: TEdit;
    Edit_DebugRecordFolder: TEdit;
    Edit_DebugPlayFolder: TEdit;
    Edit_MousePositionTestOptMatch: TEdit;
    Edit_StartingPoseTeta: TEdit;
    Edit_test: TEdit;
    FloatSpinEdit_MaxDistance: TFloatSpinEdit;
    ImageLocMap: TImage;
    ImageLocMap_TestOptimizeMatch: TImage;
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
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PageControl: TPageControl;
    Panel1: TPanel;
    RadioGroup_LocalizationDataSelect: TRadioGroup;
    RadioGroup_Mode: TRadioGroup;
    RadioGroup_DebugMode: TRadioGroup;
    SpinEdit_SelectSeedPoint: TSpinEdit;
    SpinEdit_RPropIterationNumber: TSpinEdit;
    SpinEdit_DebugSampleNumber: TSpinEdit;
    StringGrid_LocalizationData: TStringGrid;
    StringGrid_LocalizationDataTestOptMatch: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet_LocMapGlobal: TTabSheet;
    TabSheet_LocalizationParameters1: TTabSheet;
    TabSheet_GraphsCompass: TTabSheet;
    TabSheet_Graphs: TTabSheet;
    TabSheet_TestOptimizeMatch: TTabSheet;
    TabSheet_DebugParameters: TTabSheet;
    SpinEdit_RecordTime: TSpinEdit;
    TabSheet_LocalizationParameters: TTabSheet;
    TabSheet_LocMap: TTabSheet;
    Timer_Debug: TTimer;
    Timer_GlobalStatus: TTimer;
    Timer_test: TTimer;
    procedure ButtonSetClosePntClick(Sender: TObject);
    procedure Button_ChartCompassClearClick(Sender: TObject);
    procedure Button_DebugResetClick(Sender: TObject);
    procedure Button_DebugPlayClick(Sender: TObject);
    procedure Button_LoadSeedPointsFileClick(Sender: TObject);
    procedure Button_ResetClick(Sender: TObject);
    procedure Button_RunOptimizeMatch1Click(Sender: TObject);
    procedure Button_RunOptimizeMatchClick(Sender: TObject);
    procedure Button_RunOptimizeMatchGlobal1Click(Sender: TObject);
    procedure Button_RunOptimizeMatchGlobalClick(Sender: TObject);
    procedure Button_SetGlobalLocalizationParametersClick(Sender: TObject);
    procedure Button_SetLocalizationTrakingParametersClick(Sender: TObject);
    procedure Button_SetProcessLocalizationStateParametersClick(Sender: TObject);
    procedure Button_SetStartingPoseClick(Sender: TObject);
    procedure Button_testClick(Sender: TObject);
    procedure Button_LoadMapFileClick(Sender: TObject);
    procedure CheckBoxtimertestChange(Sender: TObject);
    procedure CheckBox_ChartCompassChange(Sender: TObject);
    procedure CheckBox_ChartCompassFilteredChange(Sender: TObject);
    procedure CheckBox_ChartDiferenceChange(Sender: TObject);
    procedure CheckBox_ChartTetaLocChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageLocMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageLocMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageLocMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageLocMap_TestOptimizeMatchMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageLocMap_TestOptimizeMatchMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageLocMap_TestOptimizeMatchMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PageControlChange(Sender: TObject);
    procedure RadioGroup_DebugModeClick(Sender: TObject);
    procedure RadioGroup_LocalizationDataSelectClick(Sender: TObject);
    procedure SpinEdit_DebugSampleNumberChange(Sender: TObject);
    procedure SpinEdit_DebugSampleNumberClick(Sender: TObject);
    procedure SpinEdit_SelectSeedPointChange(Sender: TObject);
    procedure Timer_DebugTimer(Sender: TObject);
    procedure Timer_GlobalStatusTimer(Sender: TObject);
    procedure Timer_testTimer(Sender: TObject);
  private
    FDataDir: string;
    TempBitmap: TBitmap;
    imfactor: double;

    FLocParameters: TLocalizationParameters;

    FLastRadioGroup_DebugMode: integer;
    FLogInputsData: TLogText;
    FLogInputsTime: longword;
    FLogInputsIterator: longword;
    FLogInputsTickCount: DWord;

    FTestOptMatchPose: Tpose2;
    FMouseTempPose: TPose2;

    FSeedPointsListProcessed: TSeedsPointsProcessed;

    TimingEventSource: TTimings;
    TimingLoad: TTimings;

    FInputData: TLocInputData;
    FLocalizationData:TLocalizationData;

    FLowPassCompass: TLowPassCompass;
    FProcessLocalizationState: TProcessLocalizationState;
    FGlobalLocalization: TGlobalLocalization;
    FLostNavigation: TLostNavigation;

    //Charts
    FDrawChartsIterator: integer;

    procedure DrawArrayDist(var TheArray: TMapDist; SizeW, SizeH: integer; Image: TImage);
    procedure DrawLocalizationData3(RobotPose: TPose2; PointsList: TPointList;
                                    CanvasToDraw: TCanvas; DrawColor: TColor);
    procedure DrawPose(RobotPose: TPose2; CanvasToDraw: TCanvas; DrawColor: TColor);
    procedure DrawPoseNumber(RobotPose: TPose2; CanvasToDraw: TCanvas; DrawColor: TColor; Number: integer);
    function GetAdvV: double;
    function GetAdvVn: double;
    function GetAdvW: double;
    function getOk: boolean;
    procedure WriteLocatizationDataTable(Pose: TPose2; StringGrid: TStringGrid);
    procedure WriteLocalizationState(LocState: TLocalizationState; Edit: TEdit);
    procedure WriteActualSpeed(ActualSpeed: TSpeed; Edit: TEdit);
    procedure WriteActualPose(ActualPose: TPose2; Edit: TEdit);
    procedure DrawLocalizationError(RobotPose: TPose2; CanvasToDraw: TCanvas; DrawColor: TColor);
    procedure DrawLocalizationError2(RobotPose: TPose2; CanvasToDraw: TCanvas; DrawColor: TColor);
    procedure DrawLocalizationError3(RobotPose: TPose2; CanvasToDraw: TCanvas; DrawColor: TColor);
    procedure DrawSeedsPoints(SeedPoints: TSeedPointsList; CanvasToDraw: TCanvas);
    procedure DrawCompass(RobotPose: TPose2; Compass: TCompassData; CanvasToDraw: TCanvas; DrawColor: TColor);
    procedure DrawGlobalLocData(LocHypothesisSet: TLocHypothesisSet; CanvasToDraw: TCanvas);
    procedure DrawClosePnts(RobotPose: TPose2;CPointsList: TPointList;CanvasToDraw: TCanvas; DrawColor: TColor);
    procedure WriteGlobalLocData(LocHypothesisSet: TLocHypothesisSet; Edit2Write: TEdit;
                                 BarSeries2Write: TBarSeries);
    procedure DrawEllipse(Cnv: TCanvas; CovU, CovV, rot,EstX,EstY: double; DrawColor: TColor);
    procedure DrawEllipse2(Cnv: TCanvas; A2,B2,theta,Cx,Cy: double; DrawColor: TColor);
    procedure DrawCharts;
    function InicializeLogRecordInputs: boolean;
    function InicializeLogPlayInputs: boolean;
    function AddLogRecordNewInputs(TickCount: DWORD): boolean;
    function GetLogPlayNewInputs(Iterator: longword): boolean;
    function ProcessActualSpeed(ActualPose, LastPose: TPose2):TSpeed;

    procedure ProcessInputData;
    procedure Mode0;
    procedure Mode1;
    procedure Mode2;
    procedure Mode3;
  public
    procedure SetInputData(InputData: TLocInputData);
    function GetOutputData: TLocOutputData;
    property Ok: boolean read getOk;
    property AdvV: double read GetAdvV;
    property AdvVn: double read GetAdvVn;
    property Advw: double read GetAdvW;
  end; 

var
  Form_Localization: TForm_Localization;

implementation

uses
  IntfGraphics, LCLType, FPimage, Utils, math;

{ TForm_Localization }

procedure TForm_Localization.Button_LoadMapFileClick(Sender: TObject);
begin
  try
    LoadMap(FDataDir+'/'+Edit_MapFileName.Text);
    ImageLocMap.Width:=MapAreaW;
    ImageLocMap.Height:=MapAreaH;
    ImageLocMap.Left:=round(Width/2-ImageLocMap.Width/2);
    ImageLocMap_TestOptimizeMatch.Width:=MapAreaW;
    ImageLocMap_TestOptimizeMatch.Height:=MapAreaH;
    ImageLocMap_TestOptimizeMatch.Left:=round(Width/2-ImageLocMap_TestOptimizeMatch.Width/2);
    ImageLocMapGlobal.Width:=MapAreaW;
    ImageLocMapGlobal.Height:=MapAreaH;
    ImageLocMapGlobal.Left:=round(Width/2-ImageLocMapGlobal.Width/2);
    DrawArrayDist(MapDist, MapAreaW, MapAreaH, ImageLocMap);
    ImageLocMap.Canvas.Draw(0,0,TempBitmap); //cenas
    ImageLocMap_TestOptimizeMatch.Canvas.Draw(0,0,TempBitmap);
    ImageLocMapGlobal.Canvas.Draw(0,0,TempBitmap);
    Edit_MapWith.Text:=FloatToStr(MapAreaW*cell_size);
    Edit_MapHeight.Text:=FloatToStr(MapAreaH*cell_size);
    Edit_MapResolution.Text:=FloatToStr(cell_size);
    IniPropStorage.Save;
  except
    on E: Exception do begin
      showmessage(E.Message);
    end;
  end;
end;

procedure TForm_Localization.CheckBoxtimertestChange(Sender: TObject);
begin
  Timer_test.Enabled:=CheckBoxtimertest.Checked;
end;

procedure TForm_Localization.CheckBox_ChartCompassChange(Sender: TObject);
begin
  ChartCompassLineSeries.ShowLines:=CheckBox_ChartCompass.Checked;
end;

procedure TForm_Localization.CheckBox_ChartCompassFilteredChange(Sender: TObject);
begin
  ChartCompassFilteredLineSeries.ShowLines:=CheckBox_ChartCompassFiltered.Checked;
end;

procedure TForm_Localization.CheckBox_ChartDiferenceChange(Sender: TObject);
begin
  ChartCompassDiferenceLineSeries.ShowLines:=CheckBox_ChartDiference.Checked;
end;

procedure TForm_Localization.CheckBox_ChartTetaLocChange(Sender: TObject);
begin
  ChartCompassTetaLocLineSeries.ShowLines:=CheckBox_ChartTetaLoc.Checked;
end;

procedure TForm_Localization.Button_testClick(Sender: TObject);
begin
  RadioGroup_DebugMode.ItemIndex:=1;
end;

procedure TForm_Localization.Button_DebugPlayClick(Sender: TObject);
begin
  if RadioGroup_DebugMode.ItemIndex=2 then begin
    if Timer_Debug.Enabled then begin
      Timer_Debug.Enabled:=false;
      Button_DebugPlay.Caption:='Play';
    end
    else begin
      Timer_Debug.Enabled:=true;
      Button_DebugPlay.Caption:='Pause';
    end;
  end;
end;

procedure TForm_Localization.Button_LoadSeedPointsFileClick(Sender: TObject);
begin
  try
    FLocParameters.SeedPointsList:=LoadSeedsPoints(FDataDir+'/'+Edit_SeedPointsFileName.Text);
    Edit_SeedsPointsNumber.Text:=IntToStr(FLocParameters.SeedPointsList.PCount);
    DrawArrayDist(MapDist, MapAreaW, MapAreaH, ImageLocMap);
    DrawSeedsPoints(FLocParameters.SeedPointsList, TempBitmap.Canvas);
    ImageLocMap.Canvas.Draw(0,0,TempBitmap);
    ImageLocMap_TestOptimizeMatch.Canvas.Draw(0,0,TempBitmap);
    IniPropStorage.Save;
  except
    on E: Exception do begin
      Edit_SeedsPointsNumber.Text:='No seeds';
      showmessage(E.Message);
    end;
  end;
end;

procedure TForm_Localization.Button_ResetClick(Sender: TObject);
begin
  ImageLocMap.Canvas.Draw(0,0,TempBitmap);
  FLocalizationData.Pose:=FLocParameters.KPoseStarting;
  FProcessLocalizationState.Reset;
  FGlobalLocalization.Reset;
  FLocalizationData.State:=LocLost;
  ImageLocMap_TestOptimizeMatch.Canvas.Draw(0,0,TempBitmap);
  DrawLocalizationData3(FLocalizationData.Pose, FInputData.VPointList, ImageLocMap.Canvas, clBlue);
  DrawLocalizationError2(FLocalizationData.Pose, ImageLocMap.Canvas, clGreen);
  WriteLocatizationDataTable(FLocalizationData.Pose, StringGrid_LocalizationData);

  //inicialise global variables
  FDrawChartsIterator:=0;
end;

procedure TForm_Localization.Button_RunOptimizeMatch1Click(Sender: TObject);
var
  OptimizeResult: Tpose2;
  LastTime: DWord;
  PointList: TPointList;
  TestOptMatchPose: Tpose2;
  i: integer;
begin
  PointList:=LimitMaxDist(FInputData.VPointList, FloatSpinEdit_MaxDistance.Value);
  LastTime:=GetTickCount;
  for i:=1 to 1000 do begin
    OptimizeMatch2(OptimizeResult, FTestOptMatchPose, PointList, SpinEdit_RPropIterationNumber.Value, t1, t2);
  end;
  Edit_RunOptimizeMatchTime.Text:=IntToStr(GetTickCount-LastTime);
  ImageLocMap_TestOptimizeMatch.Canvas.Draw(0,0,TempBitmap);
  TestOptMatchPose.x:=OptimizeResult.x;
  TestOptMatchPose.y:=OptimizeResult.y;
  TestOptMatchPose.teta:=OptimizeResult.teta;
  DrawLocalizationData3(TestOptMatchPose, PointList, ImageLocMap_TestOptimizeMatch.Canvas, clBlue);
  WriteLocatizationDataTable(TestOptMatchPose, StringGrid_LocalizationDataTestOptMatch);
end;

procedure TForm_Localization.Button_RunOptimizeMatchClick(Sender: TObject);
var
  LastTime: DWord;
  PointList: TPointList;
  TestOptMatchPose: Tpose2;
  i: integer;
begin
  PointList:=LimitMaxDist(FInputData.VPointList, FloatSpinEdit_MaxDistance.Value);
  LastTime:=GetTickCount;
  for i:=1 to 1000 do begin
    OptimizeMatch(TestOptMatchPose, FTestOptMatchPose,
                  PointList, SpinEdit_RPropIterationNumber.Value, t1, t2);
  end;
  Edit_RunOptimizeMatchTime.Text:=IntToStr(GetTickCount-LastTime);
  ImageLocMap_TestOptimizeMatch.Canvas.Draw(0,0,TempBitmap);
  DrawLocalizationError2(TestOptMatchPose, ImageLocMap_TestOptimizeMatch.Canvas, clGreen);
  DrawLocalizationData3(TestOptMatchPose, PointList, ImageLocMap_TestOptimizeMatch.Canvas, clBlue);
  WriteLocatizationDataTable(TestOptMatchPose, StringGrid_LocalizationDataTestOptMatch);
end;

procedure TForm_Localization.Button_RunOptimizeMatchGlobal1Click(Sender: TObject);
var
  LastTime: DWord;
  PointList: TPointList;
  i: integer;
  x, y: double;
begin
  PointList:=LimitMaxDist(FInputData.VPointList, FloatSpinEdit_MaxDistance.Value);
  LastTime:=GetTickCount;
  ProcessSeedPoints2(PointList, FLocParameters.SeedPointsList, SpinEdit_RPropIterationNumber.Value,4,
                     FSeedPointsListProcessed);
  Edit_RunOptimizeMatchTime.Text:=IntToStr(GetTickCount-LastTime);

  ImageLocMap_TestOptimizeMatch.Canvas.Draw(0,0,TempBitmap);
  DrawLocalizationError2(FSeedPointsListProcessed.ActualSeedsPoints.PList[FSeedPointsListProcessed.OrderedCostValues[0]],
                        ImageLocMap_TestOptimizeMatch.Canvas, clGreen);
  DrawLocalizationData3(FSeedPointsListProcessed.ActualSeedsPoints.PList[FSeedPointsListProcessed.OrderedCostValues[0]],
                        PointList, ImageLocMap_TestOptimizeMatch.Canvas, clBlue);
  WriteLocatizationDataTable(FSeedPointsListProcessed.ActualSeedsPoints.PList[FSeedPointsListProcessed.OrderedCostValues[0]],
                             StringGrid_LocalizationDataTestOptMatch);
  SpinEdit_SelectSeedPoint.MaxValue:=FSeedPointsListProcessed.ActualSeedsPoints.PCount-1;

  //Draw chart
  Chart1LineSeries1.Clear;
  for i:=0 to FSeedPointsListProcessed.ActualSeedsPoints.PCount-1 do begin
    y:=FSeedPointsListProcessed.ActualSeedsPointsCostValues[FSeedPointsListProcessed.OrderedCostValues[i]];
    x:=i;
    Chart1LineSeries1.AddXY(x,y);
  end;
  Chart2LineSeries1.Clear;
  for i:=1 to FSeedPointsListProcessed.ActualSeedsPoints.PCount-1 do begin
    y:=FSeedPointsListProcessed.ActualSeedsPointsCostValues[FSeedPointsListProcessed.OrderedCostValues[i]]-
       FSeedPointsListProcessed.ActualSeedsPointsCostValues[FSeedPointsListProcessed.OrderedCostValues[i-1]];
    x:=i;
    Chart2LineSeries1.AddXY(x,y);
  end;
end;

procedure TForm_Localization.Button_RunOptimizeMatchGlobalClick(Sender: TObject);
var
  LastTime: Dword;
  PointList: TPointList;
begin
  PointList:=LimitMaxDist(FInputData.VPointList, FloatSpinEdit_MaxDistance.Value);
  LastTime:=GetTickCount;
  FTestOptMatchPose:=ProcessSeedPoints(PointList, FLocParameters.SeedPointsList, SpinEdit_RPropIterationNumber.Value,4);
  Edit_RunOptimizeMatchTime.Text:=IntToStr(GetTickCount-LastTime);
  ImageLocMap_TestOptimizeMatch.Canvas.Draw(0,0,TempBitmap);
  DrawLocalizationError2(FTestOptMatchPose, ImageLocMap_TestOptimizeMatch.Canvas, clGreen);
  DrawLocalizationData3(FTestOptMatchPose, PointList, ImageLocMap_TestOptimizeMatch.Canvas, clBlue);
end;

procedure TForm_Localization.Button_SetGlobalLocalizationParametersClick(
  Sender: TObject);
begin
  try
    FLocParameters.GlobalLocalizationMode:=StrToInt(Edit_GlobalLocalizationMode.Text);
    FLocParameters.GlobalLocalizationMaxV:=StrToFloat(Edit_GlobalLocalizationMaxV.Text);
    FLocParameters.GlobalLocalizationMaxW:=StrToFloat(Edit_GlobalLocalizationMaxW.Text);
    FLocParameters.GlobalLocalizationFilterCutOff:=StrToFloat(Edit_GlobalLocalizationFilterCutOff.Text);
    FLocParameters.GlobalLocalizationMinIteration:=StrToInt(Edit_GlobalLocalizationMinIteration.Text);
    FLocParameters.GlobalRPropMinIterationNumber:=StrToInt(Edit_GlobalRPropIMinterationNumber.Text);
    FLocParameters.GlobalRPropMaxIterationNumber:=10;
    FLocParameters.GlobalMaxDist2Elimination:=StrToFloat(Edit_GlobalMaxDist2Elimination.Text);
    FLocParameters.GlobalMaxAng2Elimination:=StrToFloat(Edit_GlobalMaxAng2Elimination.Text)*pi/180;
    FLocParameters.GlobalMaxDeltaNumHypothesis:=StrToInt(Edit_GlobalLocalizationMaxDeltaNumHypothesis.Text);
    FLocParameters.GlobalMinIterationDeltaNumHypothesis:=StrToInt(Edit_GlobalLocalizationMinIterationDeltaNumHypothesis.Text);
    FLocParameters.GlobalTimesBetter:=StrToFloat(Edit_GlobalLocalizationTimesBetter.Text);
    case FLocParameters.GlobalLocalizationMode of
      0: FGlobalLocalization.SetMode0;
      1: FGlobalLocalization.SetMode1(FLocParameters.SeedPointsList,
                                      FLocParameters.GlobalLocalizationMaxV,
                                      FLocParameters.GlobalLocalizationMaxW,
                                      FLocParameters.GlobalRPropMinIterationNumber,
                                      FLocParameters.GlobalRPropMaxIterationNumber,
                                      FLocParameters.GlobalMaxDist2Elimination,
                                      FLocParameters.GlobalMaxAng2Elimination,
                                      FLocParameters.GlobalLocalizationMinIteration,
                                      FLocParameters.GlobalLocalizationFilterCutOff);
      2: FGlobalLocalization.SetMode2(FLocParameters.SeedPointsList,
                                      FLocParameters.GlobalLocalizationMaxV,
                                      FLocParameters.GlobalLocalizationMaxW,
                                      FLocParameters.GlobalRPropMinIterationNumber,
                                      FLocParameters.GlobalRPropMaxIterationNumber,
                                      FLocParameters.GlobalMaxDist2Elimination,
                                      FLocParameters.GlobalMaxAng2Elimination,
                                      FLocParameters.GlobalLocalizationMinIteration,
                                      FLocParameters.GlobalLocalizationFilterCutOff,
                                      FLocParameters.GlobalMaxDeltaNumHypothesis,
                                      FLocParameters.GlobalMinIterationDeltaNumHypothesis,
                                      FLocParameters.GlobalTimesBetter);
      else begin
        raise Exception.Create('Invalide Mode Global Localization');
      end;
    end;
    IniPropStorage.Save;
  except
    on E: Exception do begin
      FGlobalLocalization.SetMode0;
      FLocParameters.GlobalLocalizationMode:=0;
      FLocParameters.GlobalLocalizationMaxV:=1;
      FLocParameters.GlobalLocalizationMaxV:=0.5;
      FLocParameters.GlobalLocalizationFilterCutOff:=1;
      FLocParameters.GlobalLocalizationMinIteration:=50;
      FLocParameters.GlobalRPropMinIterationNumber:=5;
      FLocParameters.GlobalRPropMaxIterationNumber:=10;
      FLocParameters.GlobalMaxDist2Elimination:=0.2;
      FLocParameters.GlobalMaxAng2Elimination:=5*pi/180;
      FLocParameters.GlobalMaxDeltaNumHypothesis:=1;
      FLocParameters.GlobalMinIterationDeltaNumHypothesis:=100;
      FLocParameters.GlobalTimesBetter:=1.5;
      showmessage(E.Message);
    end;
  end;
end;

procedure TForm_Localization.Button_SetLocalizationTrakingParametersClick(Sender: TObject);
begin
  try
    FLocParameters.OdometryKErrorXY:=StrToFloat(Edit_OdometryKErrorXY.Text);
    FLocParameters.OdometryKErrorTeta:=StrToFloat(Edit_OdometryKErrorTeta.Text);
    FLocParameters.RPropIterationNumber:=StrToInt(Edit_RPropIterationNumber.Text);
    FLocParameters.Lc:=StrToFloat(Edit_Lc.Text); //só para manter a coerencia
    Lc:=FLocParameters.Lc;
    LcSquared:=power(Lc,2);
    FLocParameters.SensorsKErrorXY:=StrToFloat(Edit_SensorsKErrorXY.Text);
    SensorsKErrorXY:=FLocParameters.SensorsKErrorXY;
    FLocParameters.SensorsKErrorTeta:=StrToFloat(Edit_SensorsKErrorTeta.Text);
    SensorsKErrorTeta:=FLocParameters.SensorsKErrorTeta;
    FLocParameters.MinVarXY:=StrToFloat(Edit_MinVarXY.Text);
    FLocParameters.MinVarTeta:=StrToFloat(Edit_MinVarTeta.Text);
    FLocParameters.LimitMeasureMaxDistance:=CheckBox_LimitMeasureMaxDist.Checked;
    FLocParameters.MeasureMaxDistance:=StrToFloat(Edit_MeasureMaxDistance.Text);
    FLocParameters.CompassFilterCutoff:=StrToFloat(Edit_CompassFilterCutoff.Text);
    FLowPassCompass.setFiltPar(FLocParameters.CompassFilterCutoff,CCycleTime/1000,CCompassMode);
    FLocParameters.CompassDisable:=CheckBox_DisableCompass.Checked;
    FLocParameters.TrackingMode:=StrToInt(Edit_TrackingMode.Text);
    if RadioGroup_FusionMode.ItemIndex=0 then begin
      FLocParameters.KalmanEnable:=false;
    end
    else begin
      FLocParameters.KalmanEnable:=true;
    end;
    IniPropStorage.Save;
  except
    on E: Exception do begin
      FLocParameters.OdometryKErrorXY:=0.35;
      FLocParameters.OdometryKErrorTeta:=0.35;
      FLocParameters.RPropIterationNumber:=10;
      FLocParameters.Lc:=250;
      Lc:=FLocParameters.Lc;
      LcSquared:=power(Lc,2);
      FLocParameters.SensorsKErrorXY:=1e-9;
      SensorsKErrorXY:=FLocParameters.SensorsKErrorXY;
      FLocParameters.SensorsKErrorTeta:=1e-9*0.01;
      SensorsKErrorTeta:=FLocParameters.SensorsKErrorTeta;
      FLocParameters.MinVarXY:=power(0.1/2,2);
      FLocParameters.MinVarTeta:=power((pi/180*5)/2,2);
      FLocParameters.LimitMeasureMaxDistance:=false;
      FLocParameters.MeasureMaxDistance:=10;
      FLocParameters.CompassFilterCutoff:=5;
      FLocParameters.TrackingMode:=0;
      showmessage(E.Message);
    end;
  end;
end;

procedure TForm_Localization.Button_SetProcessLocalizationStateParametersClick(
  Sender: TObject);
begin
  try
    FLocParameters.ProcessLocalizationMode:=StrToInt(Edit_ProcessLocalizationMode.Text);
    FLocParameters.ProcessLocalizationMaxV:=StrToFloat(Edit_ProcessLocalizationMaxV.Text);
    FLocParameters.ProcessLocalizationMaxW:=StrToFloat(Edit_ProcessLocalizationMaxW.Text);
    FLocParameters.ProcessLocalizationMinIteration:=StrToInt(Edit_ProcessLocalizationMinIteration.Text);
    case FLocParameters.ProcessLocalizationMode of
      0: begin
        with FLocParameters do begin
          FProcessLocalizationState.SetMode0(ProcessLocalizationMaxV,ProcessLocalizationMaxW,
                                             ProcessLocalizationMinIteration);
        end;
      end;
      1: begin
        with FLocParameters do begin
          FLocParameters.ProcessLocalizationFilterCutOff:=StrToFloat(Edit_ProcessLocalizationFilterCutOff.Text);
          FProcessLocalizationState.SetMode1(ProcessLocalizationMaxV,ProcessLocalizationMaxW,
                                             ProcessLocalizationMinIteration, ProcessLocalizationFilterCutOff);
        end;
      end;
      else begin
        raise Exception.Create('Invalide Mode Process LocalizationState');
      end;
    end;
    IniPropStorage.Save;
  except
    on E: Exception do begin
      FLocParameters.ProcessLocalizationMode:=0;
      FLocParameters.ProcessLocalizationMaxV:=10;
      FLocParameters.ProcessLocalizationMaxW:=10;
      FLocParameters.ProcessLocalizationMinIteration:=round(10/(CCycleTime/1000));
      with FLocParameters do begin
        FProcessLocalizationState.SetMode0(ProcessLocalizationMaxV,ProcessLocalizationMaxW,
                                           ProcessLocalizationMinIteration);
      end;
      showmessage(E.Message);
    end;
  end;
end;

procedure TForm_Localization.Button_SetStartingPoseClick(Sender: TObject);
begin
  try
    FLocParameters.KPoseStarting.x:=StrToFloat(Edit_StartingPoseX.Text);
    FLocParameters.KPoseStarting.y:=StrToFloat(Edit_StartingPoseY.Text);
    FLocParameters.KPoseStarting.teta:=0;
    FLocParameters.KPoseStarting.CovX:=StrToFloat(Edit_StartingPoseVarX.Text);
    FLocParameters.KPoseStarting.CovY:=StrToFloat(Edit_StartingPoseVarY.Text);
    FLocParameters.KPoseStarting.CovTeta:=StrToFloat(Edit_StartingPoseVarTeta.Text);
    IniPropStorage.Save;
  except
    on E: Exception do begin
      FLocParameters.KPoseStarting.x:=0;
      FLocParameters.KPoseStarting.y:=0;
      FLocParameters.KPoseStarting.teta:=0;
      FLocParameters.KPoseStarting.CovX:=100;
      FLocParameters.KPoseStarting.CovY:=25;
      FLocParameters.KPoseStarting.CovTeta:=10;
      showmessage(E.Message);
    end;
  end;
end;

procedure TForm_Localization.Button_DebugResetClick(Sender: TObject);
begin
  if RadioGroup_DebugMode.ItemIndex=2 then begin
    Button_DebugPlay.Caption:='Play';
    Timer_Debug.Enabled:=false;
    FLogInputsIterator:=0;
    SpinEdit_DebugSampleNumber.Value:=FLogInputsIterator;
  end;
end;

procedure TForm_Localization.Button_ChartCompassClearClick(Sender: TObject);
begin
  ChartCompassLineSeries.Clear;
  ChartCompassFilteredLineSeries.Clear;
  ChartCompassTetaLocLineSeries.Clear;
  ChartCompassOkBarSeries.Clear;
  ChartCompassLostBarSeries.Clear;
  ChartCompassDiferenceLineSeries.clear;
  ChartVOdodLineSeries.Clear;
  ChartWOdodLineSeries.clear;
  FDrawChartsIterator:=0;
end;

procedure TForm_Localization.ButtonSetClosePntClick(Sender: TObject);
begin
  try
    FLocParameters.NavLostMode:=StrToInt(Edit_ModeNav.Text);
    FLocParameters.NavLostNumMinClosePnt:=StrToInt(Edit_NumMinClosePoints.Text);
    FLocParameters.NavLostMaxClosePntDist:=StrToFloat(Edit_MaxDistClosePnt.Text);
    FLocParameters.NavLostMinClosePntDist:=StrToFloat(Edit_MinDistClosePnt.Text);
    FLocParameters.NavLostAngleClosePnt:=StrToFloat(Edit_AngleClosePnt.Text)*pi/180;
    FLocParameters.NavLostAdvisedNavigateV:=StrToFloat(Edit_NavigateV.Text);
    FLocParameters.NavLostAdvisedNavigateW:=StrToFloat(Edit_NavigateW.Text);
    FLocParameters.NavLostAngleOffset:=StrToFloat(Edit_OffsetAng.Text)*pi/180;
    case FLocParameters.NavLostMode of
      0: begin
        with FLocParameters do begin
          FLostNavigation.SetMode0(NavLostNumMinClosePnt,
                                   NavLostMaxClosePntDist,
                                   NavLostMinClosePntDist,
                                   NavLostAngleClosePnt,
                                   NavLostAdvisedNavigateV,
                                   NavLostAdvisedNavigateW,
                                   NavLostAngleOffset);
        end;
      end;
      else begin
        raise Exception.Create('Invalide Mode Lost Navigation');
      end;
    end;
    IniPropStorage.Save;
  except
    on E: Exception do begin
      FLocParameters.NavLostMode:=0;
      FLocParameters.NavLostNumMinClosePnt:=3;
      FLocParameters.NavLostMaxClosePntDist:=1;
      FLocParameters.NavLostMinClosePntDist:=0.3;
      FLocParameters.NavLostAngleClosePnt:=15*pi/180;
      FLocParameters.NavLostAdvisedNavigateV:=0.5;
      FLocParameters.NavLostAdvisedNavigateW:=0.5;
      FLocParameters.NavLostAngleOffset:=10*pi/180;
      with FLocParameters do begin
        FLostNavigation.SetMode0(NavLostNumMinClosePnt,
                                 NavLostMaxClosePntDist,
                                 NavLostMinClosePntDist,
                                 NavLostAngleClosePnt,
                                 NavLostAdvisedNavigateV,
                                 NavLostAdvisedNavigateW,
                                 NavLostAngleOffset);
      end;
      showmessage(E.Message);
    end;
  end;
end;

procedure TForm_Localization.FormCreate(Sender: TObject);
var
  i: integer;
begin
  WindowState:=wsNormal;
  decimalSeparator:='.';

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

  //inicialize

  TempBitmap:=TBitmap.Create;
  FLogInputsData:=TLogText.Create;
  FLowPassCompass:=TLowPassCompass.Create(FLocParameters.CompassFilterCutoff,CCycleTime/1000,CCompassMode);
  FGlobalLocalization:=TGlobalLocalization.Create;
  FProcessLocalizationState:=TProcessLocalizationState.Create;
  FLostNavigation:=TLostNavigation.Create;

  PageControl.ActivePageIndex:=0;
  Timer_Debug.Interval:=CCycleTime;
  FLastRadioGroup_DebugMode:=0;

  Button_LoadMapFileClick(Self);
  Button_LoadSeedPointsFileClick(Self);
  Button_SetLocalizationTrakingParametersClick(Self);
  ButtonSetClosePntClick(Self);
  Button_SetProcessLocalizationStateParametersClick(Self);
  Button_SetStartingPoseClick(Self);
  Button_SetGlobalLocalizationParametersClick(Self);
  Button_ResetClick(self);
end;

procedure TForm_Localization.FormDestroy(Sender: TObject);
begin
  TempBitmap.Free;
  FLogInputsData.Free;
  FLowPassCompass.Free;
  FProcessLocalizationState.Free;
  FGlobalLocalization.Free;
  FLostNavigation.Free;
end;

procedure TForm_Localization.ImageLocMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseTempPose.x:=(x-MapAreaW/2)*cell_size*imfactor;
  FMouseTempPose.y:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
end;

procedure TForm_Localization.ImageLocMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PositionX, PositionY: double;
  Distance: double;
begin
  PositionX:=(x-MapAreaW/2)*cell_size*imfactor;
  PositionY:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
  Distance:=MapDist[round((abs(MapAreaH-y))*imfactor),round(X*imfactor)];
  Edit_MousePosition.Text:=Format('x=%.2f, y=%.2f', [PositionX, PositionY]);
  Edit_MousePosition.Text:=Edit_MousePosition.Text+Format(', Dist=%.2f',[Distance]);
end;

procedure TForm_Localization.ImageLocMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  xtemp,ytemp:double;
begin
  Button_ResetClick(self);
  xtemp:=(x-MapAreaW/2)*cell_size*imfactor;
  ytemp:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
  FLocalizationData.Pose.x:=FMouseTempPose.x;
  FLocalizationData.Pose.y:=FMouseTempPose.y;
  FLocalizationData.Pose.teta:=arctan2(ytemp-FMouseTempPose.y, xtemp-FMouseTempPose.x);
  FLocalizationData.State:=LocOk;
  FLocalizationData.PoseSensors:=FLocalizationData.Pose;

  ImageLocMap.Canvas.Draw(0,0,TempBitmap);
  DrawLocalizationData3(FLocalizationData.Pose, FInputData.VPointList, ImageLocMap.Canvas, clBlue);
  WriteLocatizationDataTable(FLocalizationData.Pose, StringGrid_LocalizationData);
end;

procedure TForm_Localization.ImageLocMap_TestOptimizeMatchMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseTempPose.x:=(x-MapAreaW/2)*cell_size*imfactor;
  FMouseTempPose.y:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
end;

procedure TForm_Localization.ImageLocMap_TestOptimizeMatchMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PositionX, PositionY: double;
  Distance: double;
begin
  PositionX:=(x-MapAreaW/2)*cell_size*imfactor;
  PositionY:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
  Distance:=MapDist[round((abs(MapAreaH-y))*imfactor),round(X*imfactor)];
  Edit_MousePositionTestOptMatch.Text:=Format('x=%.2f, y=%.2f', [PositionX, PositionY]);
  Edit_MousePositionTestOptMatch.Text:=Edit_MousePositionTestOptMatch.Text+Format(', Dist=%.2f',[Distance]);
end;

procedure TForm_Localization.ImageLocMap_TestOptimizeMatchMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  xtemp,ytemp:double;
begin
  xtemp:=(x-MapAreaW/2)*cell_size*imfactor;
  ytemp:=(ImageLocMap.Height-MapAreaH/2-y)*cell_size*imfactor;
  FTestOptMatchPose.x:=FMouseTempPose.x;
  FTestOptMatchPose.y:=FMouseTempPose.y;
  FTestOptMatchPose.teta:=arctan2(ytemp-FMouseTempPose.y, xtemp-FMouseTempPose.x);
  FTestOptMatchPose.CovX:=FLocParameters.KPoseStarting.CovX;
  FTestOptMatchPose.CovY:=FLocParameters.KPoseStarting.CovY;
  FTestOptMatchPose.CovTeta:=FLocParameters.KPoseStarting.CovTeta;

  ImageLocMap_TestOptimizeMatch.Canvas.Draw(0,0,TempBitmap);
  DrawLocalizationData3(FTestOptMatchPose, FInputData.VPointList, ImageLocMap_TestOptimizeMatch.Canvas, clBlue);
  WriteLocatizationDataTable(FTestOptMatchPose, StringGrid_LocalizationDataTestOptMatch);
end;

procedure TForm_Localization.PageControlChange(Sender: TObject);
begin

end;

procedure TForm_Localization.RadioGroup_DebugModeClick(Sender: TObject);
begin
  case RadioGroup_DebugMode.ItemIndex of
    0: begin //disable
      Button_DebugReset.Visible:=false;
      Button_DebugPlay.Visible:=false;
      SpinEdit_DebugSampleNumber.Visible:=false;
      Edit_DebugTime.Visible:=false;
      if FLastRadioGroup_DebugMode=1 then begin
        FLogInputsData.SaveToFile;
      end
      else begin
        FLogInputsData.ResetAll;
      end;
    end;
    1: begin //record
      Button_DebugReset.Visible:=false;
      Button_DebugPlay.Visible:=false;
      SpinEdit_DebugSampleNumber.Visible:=false;
      Edit_DebugTime.Visible:=true;
      if not InicializeLogRecordInputs then begin
        RadioGroup_DebugMode.ItemIndex:=0;
      end;
    end;
    2: begin //play
      Button_DebugReset.Visible:=true;
      Button_DebugPlay.Visible:=true;
      SpinEdit_DebugSampleNumber.Visible:=true;
      Edit_DebugTime.Visible:=true;
      if FLastRadioGroup_DebugMode=1 then begin
        FLogInputsData.SaveToFile;
      end;
      if not InicializeLogPlayInputs then begin
        RadioGroup_DebugMode.ItemIndex:=0;
      end
      else begin
        GetLogPlayNewInputs(0);
        Edit_DebugTime.Text:='0/'+IntToStr(round((FLogInputsTime)*CCycleTime/1000));//CCycleTime in ms
      end;
    end;
  end;
  Timer_Debug.Enabled:=false;
  Button_DebugPlay.Caption:='Play';
  FLastRadioGroup_DebugMode:=RadioGroup_DebugMode.ItemIndex;
end;

procedure TForm_Localization.RadioGroup_LocalizationDataSelectClick(
  Sender: TObject);
begin
  case RadioGroup_LocalizationDataSelect.ItemIndex of
    0: WriteLocatizationDataTable(FLocalizationData.PoseOdos, StringGrid_LocalizationData);
    1: WriteLocatizationDataTable(FLocalizationData.PoseSensors, StringGrid_LocalizationData);
    2: WriteLocatizationDataTable(FLocalizationData.Pose, StringGrid_LocalizationData);
  end;
end;

procedure TForm_Localization.SpinEdit_DebugSampleNumberChange(Sender: TObject);
begin
  FLogInputsIterator:=SpinEdit_DebugSampleNumber.Value;
end;

procedure TForm_Localization.SpinEdit_DebugSampleNumberClick(Sender: TObject);
begin
  if not Timer_Debug.Enabled then begin
    Timer_DebugTimer(Self);
  end;
end;

procedure TForm_Localization.SpinEdit_SelectSeedPointChange(Sender: TObject);
var
  SeedPointIndex: integer;
  PointList: TPointList;
begin
  PointList:=LimitMaxDist(FInputData.VPointList, FloatSpinEdit_MaxDistance.Value);
  SeedPointIndex:=SpinEdit_SelectSeedPoint.Value;
  ImageLocMap_TestOptimizeMatch.Canvas.Draw(0,0,TempBitmap);
  DrawLocalizationError2(FSeedPointsListProcessed.ActualSeedsPoints.PList[FSeedPointsListProcessed.OrderedCostValues[SeedPointIndex]],
                        ImageLocMap_TestOptimizeMatch.Canvas, clGreen);
  DrawLocalizationData3(FSeedPointsListProcessed.ActualSeedsPoints.PList[FSeedPointsListProcessed.OrderedCostValues[SeedPointIndex]],
                        PointList, ImageLocMap_TestOptimizeMatch.Canvas, clBlue);
  WriteLocatizationDataTable(FSeedPointsListProcessed.ActualSeedsPoints.PList[FSeedPointsListProcessed.OrderedCostValues[SeedPointIndex]],
                             StringGrid_LocalizationDataTestOptMatch);
end;

procedure TForm_Localization.Timer_DebugTimer(Sender: TObject);
var
  deltatime: DWord;
  ActualTime, TotalTime: integer;
begin
  if (RadioGroup_DebugMode.ItemIndex=2) then begin
    TimingLoad.last:=GetTickCount;
    if GetLogPlayNewInputs(FLogInputsIterator) then begin //FInputData:=InputData;
      ProcessInputData;
      SpinEdit_DebugSampleNumber.Value:=FLogInputsIterator;
      inc(FLogInputsIterator);
    end
    else begin
      FLogInputsIterator:=0;
      Button_DebugResetClick(self);
    end;
    TimingLoad.actual:=GetTickCount;

    //timings
    TimingEventSource.actual:=FLogInputsTickCount;
    deltatime:=TimingEventSource.actual-TimingEventSource.last;
    if (deltatime)>TimingEventSource.max then begin
      TimingEventSource.max:=deltatime;
    end;
    TimingEventSource.last:=TimingEventSource.actual;
    deltatime:=TimingLoad.actual-TimingLoad.last;
    if (deltatime)>TimingLoad.max then begin
      TimingLoad.max:=deltatime;
    end;
    //edit debug timing
    TotalTime:=round((FLogInputsTime)*CCycleTime/1000); //CCycleTime in ms
    ActualTime:=round((FLogInputsIterator)*CCycleTime/1000);
    Edit_DebugTime.Text:=IntToStr(ActualTime)+'/'+IntToStr(TotalTime);
  end
  else begin
    RadioGroup_DebugMode.ItemIndex:=0;//só para garantir...
    RadioGroup_DebugModeClick(self);
    Button_DebugResetClick(self);
  end;
end;

procedure TForm_Localization.Timer_GlobalStatusTimer(Sender: TObject);
begin
  if Showing then begin
    Edit_Timings.Text:='Sample='+IntToStr(TimingEventSource.max)+
                       ' Load='+IntToStr(TimingLoad.max);
    WriteLocalizationState(FLocalizationData.State, Edit_LocalizationState);
  end;
  TimingEventSource.max:=0;
  TimingLoad.max:=0;
end;

procedure TForm_Localization.Timer_testTimer(Sender: TObject);
begin
 DrawLocalizationData3(FLocalizationData.Pose, FInputData.VPointList, ImageLocMap.Canvas, clBlue);
end;

procedure TForm_Localization.DrawArrayDist(var TheArray: TMapDist; SizeW, SizeH: integer; Image: TImage);
var
  SrcIntfImg: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  SrcBitmap: TBitmap;
  vi, ui, gray: integer;
  yi,xi:integer;
  fpcol: TFPcolor;
  min_val, max_val:double;
begin
  SrcBitmap:=TBitmap.Create;
  SrcIntfImg:=TLazIntfImage.Create(0,0);

  if SizeW/Image.Width > SizeH/Image.Height   then
     imfactor:=SizeW/Image.Width
  else
    imfactor:=SizeH/Image.Height;

  if (SizeW<Image.Width) and (SizeH<Image.Height) then
    imfactor:=1;

  SrcBitmap.Width:=Image.Width;
  SrcBitmap.Height:=Image.Height;

  SrcIntfImg.LoadFromBitmap(SrcBitmap.Handle, SrcBitmap.MaskHandle);
  max_val:=0;
  min_val:=100;
  for yi := 0 to SizeH-1 do begin
    for xi := 0 to SizeW-1 do begin
      if ((TheArray[yi,xi]>max_val)and (TheArray[yi,xi]<65535)) then
        max_val:=TheArray[yi,xi];
      if (TheArray[yi,xi]<min_val) then
        min_val:=TheArray[yi,xi];
    end;
  end;

  SrcIntfImg.FillPixels(colWhite);
  fpcol.alpha:=0;
  for vi :=0 to Image.Height-1 do begin
    for ui :=0 to Image.Width-1 do begin
      if ((vi*imfactor<SizeH-1) and (ui*imfactor<SizeW-1)) then  begin
        gray := round((TheArray[round(SizeH-1-imfactor*vi),round(imfactor*ui)]+abs(min_val))*65536/(max_val-min_val));
        if (gray > 65535 )then begin
          gray:=255*255;
          fpcol.red := gray;
          fpcol.green := gray;
          fpcol.blue := gray;
        end else begin
          fpcol.red := gray;
          fpcol.green := gray;
          fpcol.blue := gray;
        end;
        SrcIntfImg.Colors[round(ui*imfactor),round(Image.Height-SizeH+vi*imfactor)] := fpcol;
      end;
    end;
  end;

  SrcIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,true);
  TempBitmap.Handle:=ImgHandle;

  SrcIntfImg.Free;
  SrcBitmap.Free;
end;

procedure TForm_Localization.DrawLocalizationData3(RobotPose: TPose2;
  PointsList: TPointList; CanvasToDraw: TCanvas; DrawColor: TColor);
const
  size=1;
  LenghtTeta=10;
var
  i: integer;
  RobotPositionPixelX, RobotPositionPixelY: double;
  PointPositionX, PointPositionY: double;
  PointPositionPixelX, PointPositionPixelY: double;
begin
  RobotPositionPixelX:=MapAreaW/2+RobotPose.x/cell_size/imfactor;
  RobotPositionPixelY:=-RobotPose.y/cell_size/imfactor+MapAreaH/2;

  CanvasToDraw.Brush.Style:=bsClear;
  CanvasToDraw.Pen.Style:=psSolid;
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Pen.Width:=1;

  CanvasToDraw.Arc(round(RobotPositionPixelX-5), round(RobotPositionPixelY-5), round(RobotPositionPixelX+5), round(RobotPositionPixelY+5), 0, 360*16);
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX + LenghtTeta*cos(RobotPose.teta)), round(RobotPositionPixelY-LenghtTeta*sin(RobotPose.teta)));

  for i:=0  to PointsList.PCount-1 do begin
    PointPositionX:=RobotPose.x+cos(RobotPose.teta)*PointsList.PList[i].x
                    -sin(RobotPose.teta)*(PointsList.PList[i].y);
    PointPositionY:=RobotPose.y+sin(RobotPose.teta)*PointsList.PList[i].x
                    +cos(RobotPose.teta)*(PointsList.PList[i].y);

    PointPositionPixelX:=PointPositionX/cell_size/imfactor+MapAreaW/2;
    PointPositionPixelY:=-PointPositionY/cell_size/imfactor+MapAreaH/2;

    CanvasToDraw.Pen.Color:=clBlue;
    CanvasToDraw.Brush.Color:=clBlue;
    if (PointPositionPixelX>=0) and (PointPositionPixelY>=0) then begin
      CanvasToDraw.Rectangle(round(PointPositionPixelX-size),round(PointPositionPixelY-size),
                             round(PointPositionPixelX+size),round(PointPositionPixelY+size));
    end;
  end;
end;

procedure TForm_Localization.DrawPose(RobotPose: TPose2; CanvasToDraw: TCanvas; DrawColor: TColor);
const
  LenghtTeta=10;
var
  RobotPositionPixelX, RobotPositionPixelY: double;
begin
  RobotPositionPixelX:=MapAreaW/2+RobotPose.x/cell_size/imfactor;
  RobotPositionPixelY:=-RobotPose.y/cell_size/imfactor+MapAreaH/2;

  CanvasToDraw.Brush.Style:=bsClear;
  CanvasToDraw.Pen.Style:=psSolid;
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Pen.Width:=1;

  CanvasToDraw.Arc(round(RobotPositionPixelX-5), round(RobotPositionPixelY-5), round(RobotPositionPixelX+5), round(RobotPositionPixelY+5), 0, 360*16);
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX + LenghtTeta*cos(RobotPose.teta)),
                    round(RobotPositionPixelY-LenghtTeta*sin(RobotPose.teta)));
end;

procedure TForm_Localization.DrawPoseNumber(RobotPose: TPose2;
  CanvasToDraw: TCanvas; DrawColor: TColor; Number: integer);
const
  LenghtTeta=10;
var
  RobotPositionPixelX, RobotPositionPixelY: double;
begin
  RobotPositionPixelX:=MapAreaW/2+RobotPose.x/cell_size/imfactor;
  RobotPositionPixelY:=-RobotPose.y/cell_size/imfactor+MapAreaH/2;

  CanvasToDraw.Brush.Style:=bsClear;
  CanvasToDraw.Pen.Style:=psSolid;
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Pen.Width:=1;

  CanvasToDraw.Arc(round(RobotPositionPixelX-5), round(RobotPositionPixelY-5), round(RobotPositionPixelX+5), round(RobotPositionPixelY+5), 0, 360*16);
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX + LenghtTeta*cos(RobotPose.teta)),
                    round(RobotPositionPixelY-LenghtTeta*sin(RobotPose.teta)));

  CanvasToDraw.TextOut(round(RobotPositionPixelX), round(RobotPositionPixelY), IntToStr(Number));
end;

function TForm_Localization.GetAdvV: double;
begin
  if FLocalizationData.State=LocLost then begin
    Result:=FLocalizationData.AdviseSpeed.V;
  end
  else begin
    Result:=0;
  end;
end;

function TForm_Localization.GetAdvVn: double;
begin
  if FLocalizationData.State=LocLost then begin
    Result:=FLocalizationData.AdviseSpeed.Vn;
  end
  else begin
    Result:=0;
  end;
end;

function TForm_Localization.GetAdvW: double;
begin
  if FLocalizationData.State=LocLost then begin
    Result:=FLocalizationData.AdviseSpeed.W;
  end
  else begin
    Result:=0;
  end;
end;

function TForm_Localization.getOk: boolean;
begin
  if FLocalizationData.State=LocOk then begin
    Result:=true;
  end
  else begin
    Result:=false;
  end;
end;

procedure TForm_Localization.WriteLocatizationDataTable(Pose: TPose2;
  StringGrid: TStringGrid);
begin
  if (StringGrid.RowCount=4) and (StringGrid.ColCount=3) then begin
    StringGrid.Cells[1,1]:=Format('%.4F', [Pose.x]);
    StringGrid.Cells[1,2]:=Format('%.4F', [Pose.y]);
    StringGrid.Cells[1,3]:=Format('%.4F', [Pose.teta]);
    StringGrid.Cells[2,1]:=Format('%.4E', [Pose.CovX]);
    StringGrid.Cells[2,2]:=Format('%.4E', [Pose.CovY]);
    StringGrid.Cells[2,3]:=Format('%.4E', [Pose.CovTeta]);
  end;
end;

procedure TForm_Localization.WriteLocalizationState(LocState: TLocalizationState; Edit: TEdit);
begin
  case LocState of
    LocOk: Edit.Text:='Ok';
    LocLost: Edit.Text:='Lost';
    LocError: Edit.Text:='Error';
    LocInverted: Edit.Text:='Inverted';
  end;
end;

procedure TForm_Localization.WriteActualSpeed(ActualSpeed: TSpeed; Edit: TEdit);
begin
  Edit.Text:=Format('%.3F, %.3F, %.3F', [ActualSpeed.V, ActualSpeed.Vn, ActualSpeed.W]);
end;

procedure TForm_Localization.WriteActualPose(ActualPose: TPose2; Edit: TEdit);
begin
  Edit.Text:=Format('%.2F, %.2F, %.2F', [ActualPose.x, ActualPose.y, ActualPose.teta]);
end;

procedure TForm_Localization.DrawLocalizationError(RobotPose: TPose2;
  CanvasToDraw: TCanvas; DrawColor: TColor);
const
  LenghtTeta=10;
var
  RobotPositionPixelX, RobotPositionPixelY: double;
  SDX,SDY, SDTeta: double;
  SDXPixel, SDYPixel: double;
begin
  CanvasToDraw.Brush.Style:=bsClear;
  CanvasToDraw.Pen.Style:=psSolid;
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Pen.Width:=1;

  RobotPositionPixelX:=MapAreaW/2+RobotPose.x/cell_size/imfactor;
  RobotPositionPixelY:=-RobotPose.y/cell_size/imfactor+MapAreaH/2;

  SDX:=sqrt(RobotPose.CovX)*2;//86%
  SDY:=sqrt(RobotPose.CovY)*2;
  SDTeta:=sqrt(RobotPose.CovTeta)*2;
  if SDTeta>pi then begin
    SDTeta:=pi;
  end;
  SDXPixel:=SDX/cell_size/imfactor;
  SDYPixel:=SDY/cell_size/imfactor;

  CanvasToDraw.Ellipse(round(RobotPositionPixelX-SDXPixel), round(RobotPositionPixelY-SDYPixel),
                       round(RobotPositionPixelX+SDXPixel), round(RobotPositionPixelY+SDYPixel));

  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX+LenghtTeta*cos(RobotPose.teta+SDTeta)), round(RobotPositionPixelY-LenghtTeta*sin(RobotPose.teta+SDTeta)));
  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX+LenghtTeta*cos(RobotPose.teta-SDTeta)), round(RobotPositionPixelY-LenghtTeta*sin(RobotPose.teta-SDTeta)));
end;

procedure TForm_Localization.DrawLocalizationError2(RobotPose: TPose2;
  CanvasToDraw: TCanvas; DrawColor: TColor);
const
  LenghtTeta=10;
var
  RobotPositionPixelX, RobotPositionPixelY: double;
  SDX,SDY,SDXY, SDTeta,rot,CovU,CovV: double;
  SDXPixel, SDYPixel: double;
begin
  CanvasToDraw.Brush.Style:=bsClear;
  CanvasToDraw.Pen.Style:=psSolid;
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Pen.Width:=1;

  RobotPositionPixelX:=MapAreaW/2+RobotPose.x/cell_size/imfactor;
  RobotPositionPixelY:=-RobotPose.y/cell_size/imfactor+MapAreaH/2;
  SDX:=sqrt(RobotPose.CovX)*2;
  SDY:=sqrt(RobotPose.CovY)*2;
  SDXY:=RobotPose.CovXY;
  SDTeta:=RobotPose.CovTeta;
  //
  CovU:=SDX;
  CovV:=SDY;
  rot:=0;
  //
  if (not (SDX=SDY) and not(SDX=0) and not(SDY=0)) then begin
     rot:=arctan(-2*SDXY/(power(SDX,2)-power(SDY,2)))/2;
     CovU:=sqrt(abs(SDX+SDXY*tan(rot)));
     CovV:=sqrt(abs(SDY-SDXY*tan(rot)));
  end;
  CovU:=CovU/cell_size/imfactor;
  CovV:=CovV/cell_size/imfactor;
  //
  DrawEllipse(CanvasToDraw,CovU,CovV,rot,RobotPositionPixelX,RobotPositionPixelY, DrawColor);
  //
  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX+LenghtTeta*cos(RobotPose.teta+SDTeta)), round(RobotPositionPixelY-LenghtTeta*sin(RobotPose.teta+SDTeta)));
  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX+LenghtTeta*cos(RobotPose.teta-SDTeta)), round(RobotPositionPixelY-LenghtTeta*sin(RobotPose.teta-SDTeta)));
end;

procedure TForm_Localization.DrawLocalizationError3(RobotPose: TPose2;
  CanvasToDraw: TCanvas; DrawColor: TColor);
const
  LenghtTeta=10;
var
  RobotPositionPixelX, RobotPositionPixelY: double;
  SDX,SDY,SDXY, SDTeta,rot,CovU,CovV: double;
  SDXPixel, SDYPixel: double;
begin
  CanvasToDraw.Brush.Style:=bsClear;
  CanvasToDraw.Pen.Style:=psSolid;
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Pen.Width:=1;

  RobotPositionPixelX:=MapAreaW/2+RobotPose.x/cell_size/imfactor;
  RobotPositionPixelY:=-RobotPose.y/cell_size/imfactor+MapAreaH/2;
  SDX:=sqrt(RobotPose.CovX)*2;
  SDY:=sqrt(RobotPose.CovY)*2;
  SDXY:=RobotPose.CovXY;
  SDTeta:=RobotPose.CovTeta;
  //
  CovU:=SDX;
  CovV:=SDY;
  rot:=0;
  //
  if (not (SDX=SDY) and not(SDX=0) and not(SDY=0)) then begin
     rot:=arctan(-2*SDXY/(power(SDX,2)-power(SDY,2)))/2;
     CovU:=sqrt(abs(SDX+SDXY*tan(rot)));
     CovV:=sqrt(abs(SDY-SDXY*tan(rot)));
  end;
  CovU:=CovU/cell_size/imfactor;
  CovV:=CovV/cell_size/imfactor;
  //
  DrawEllipse2(CanvasToDraw,CovU,CovV,rot,RobotPositionPixelX,RobotPositionPixelY, DrawColor);
  //
  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX+LenghtTeta*cos(RobotPose.teta+SDTeta)), round(RobotPositionPixelY-LenghtTeta*sin(RobotPose.teta+SDTeta)));
  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX+LenghtTeta*cos(RobotPose.teta-SDTeta)), round(RobotPositionPixelY-LenghtTeta*sin(RobotPose.teta-SDTeta)));
end;

procedure TForm_Localization.DrawSeedsPoints(SeedPoints: TSeedPointsList; CanvasToDraw: TCanvas);
const
  size=1;
var
  i: integer;
  PointPositionPixelX, PointPositionPixelY: double;
begin
  for i:=0 to SeedPoints.PCount-1 do begin
    PointPositionPixelX:=SeedPoints.PList[i].x/cell_size/imfactor+MapAreaW/2;
    PointPositionPixelY:=-SeedPoints.PList[i].y/cell_size/imfactor+MapAreaH/2;

    CanvasToDraw.Pen.Color:=clRed;
    CanvasToDraw.Brush.Color:=clRed;
    if (PointPositionPixelX>=0) and (PointPositionPixelY>=0) then begin
      CanvasToDraw.Rectangle(round(PointPositionPixelX-size),round(PointPositionPixelY-size),
                             round(PointPositionPixelX+size),round(PointPositionPixelY+size));
    end;
  end;
end;

procedure TForm_Localization.DrawCompass(RobotPose: TPose2; Compass: TCompassData; CanvasToDraw: TCanvas; DrawColor: TColor);
const
  LenghtTeta=10;
var
  RobotPositionPixelX, RobotPositionPixelY: double;
begin
  CanvasToDraw.Brush.Style:=bsClear;
  CanvasToDraw.Pen.Style:=psSolid;
  if Compass.Avaible then begin
    CanvasToDraw.Pen.Color:=DrawColor;
  end
  else begin
    CanvasToDraw.Pen.Color:=clGray;
  end;
  CanvasToDraw.Pen.Width:=1;
  RobotPositionPixelX:=MapAreaW/2+RobotPose.x/cell_size/imfactor;
  RobotPositionPixelY:=-RobotPose.y/cell_size/imfactor+MapAreaH/2;

  CanvasToDraw.Line(round(RobotPositionPixelX), round(RobotPositionPixelY),
                    round(RobotPositionPixelX + LenghtTeta*cos(Compass.Angle)),
                    round(RobotPositionPixelY-LenghtTeta*sin(Compass.Angle)));
end;

procedure TForm_Localization.DrawGlobalLocData(LocHypothesisSet: TLocHypothesisSet; CanvasToDraw: TCanvas);
var
  i: integer;
  ListLocHypothesis: TListLocHypothesis;
  pose: TPose2;
  Color2Draw: TColor;
  LocState: TLocalizationState;
begin
  ListLocHypothesis:=LocHypothesisSet.ListLocHypothesisHead.Next^;
  for i:=0 to LocHypothesisSet.NumHypothesis-1 do begin
    pose:=ListLocHypothesis.Current^.Pose;
    LocState:=ListLocHypothesis.Current^.State;
    if LocState=LocOk then begin
      Color2Draw:=clBlue;
    end
    else begin
      if LocState=LocInverted then begin
        Color2Draw:=clGreen;
      end
      else begin
        Color2Draw:=clRed;
      end;
      //Timer_Debug.Enabled:=false;
      //Button_DebugPlay.Caption:='Play';
    end;
    DrawPoseNumber(pose,CanvasToDraw,Color2Draw,i);
    ListLocHypothesis:=ListLocHypothesis.Next^;
  end;
end;

procedure TForm_Localization.DrawClosePnts(RobotPose: TPose2;
  CPointsList: TPointList; CanvasToDraw: TCanvas; DrawColor: TColor);
const
  size=1;
  LenghtTeta=10;
var
  i: integer;
  RobotPositionPixelX, RobotPositionPixelY: double;
  PointPositionX, PointPositionY: double;
  PointPositionPixelX, PointPositionPixelY: double;
begin
  RobotPositionPixelX:=MapAreaW/2+RobotPose.x/cell_size/imfactor;
  RobotPositionPixelY:=-RobotPose.y/cell_size/imfactor+MapAreaH/2;

  CanvasToDraw.Brush.Style:=bsClear;
  CanvasToDraw.Pen.Style:=psSolid;
  CanvasToDraw.Pen.Color:=DrawColor;
  CanvasToDraw.Pen.Width:=1;

  for i:=0  to CPointsList.PCount-1 do begin
    PointPositionX:=RobotPose.x+cos(RobotPose.teta)*CPointsList.PList[i].x
                    -sin(RobotPose.teta)*(CPointsList.PList[i].y);
    PointPositionY:=RobotPose.y+sin(RobotPose.teta)*CPointsList.PList[i].x
                    +cos(RobotPose.teta)*(CPointsList.PList[i].y);

    PointPositionPixelX:=PointPositionX/cell_size/imfactor+MapAreaW/2;
    PointPositionPixelY:=-PointPositionY/cell_size/imfactor+MapAreaH/2;

    if (PointPositionPixelX>=0) and (PointPositionPixelY>=0) then begin
      CanvasToDraw.Rectangle(round(PointPositionPixelX-size),round(PointPositionPixelY-size),
                             round(PointPositionPixelX+size),round(PointPositionPixelY+size));
    end;
  end;
end;

procedure TForm_Localization.WriteGlobalLocData(
  LocHypothesisSet: TLocHypothesisSet; Edit2Write: TEdit;
  BarSeries2Write: TBarSeries);
var
  i: integer;
  ListLocHypothesis: TListLocHypothesis;
  Cost: double;
begin
  Edit2Write.Text:=IntToStr(LocHypothesisSet.NumHypothesis);
  BarSeries2Write.Clear;
  ListLocHypothesis:=LocHypothesisSet.ListLocHypothesisHead.Next^;
  for i:=0 to LocHypothesisSet.NumHypothesis-1 do begin
    Cost:=ListLocHypothesis.Current^.Cost;
    BarSeries2Write.AddXY(i, Cost);
    ListLocHypothesis:=ListLocHypothesis.Next^;
  end;
end;

procedure TForm_Localization.DrawEllipse(Cnv: TCanvas; CovU, CovV, rot, EstX,
  EstY: double; DrawColor: TColor);
var
  i:integer;
  xu,x,yu,y:double;
begin
  Cnv.Brush.Style:=bsClear;
  Cnv.Pen.Style:=psSolid;
  Cnv.Pen.Color:=DrawColor;
  Cnv.Pen.Width:=1;
  if (not(CovU=0) and not(CovV=0)) then begin
    xu:=-CovU;
    yu:=CovV*sqrt(abs(1-power(xu/CovU,2)));
    y:=sin(rot)*xu+cos(rot)*yu;
    x:=cos(rot)*xu-sin(rot)*yu;
    Cnv.MoveTo(round(EstX+x),round(EstY+y));
     for i:=-99 to 100 do begin
        xu:=CovU*i/100;
        yu:=CovV*sqrt(1-power(xu/CovU,2));
        y:=sin(rot)*xu+cos(rot)*yu;
        x:=cos(rot)*xu-sin(rot)*yu;
        Cnv.LineTo(round(EstX+x),round(EstY+y));
     end;
    xu:=-CovU;
    yu:=-CovV*sqrt(abs(1-power(xu/CovU,2)));
    y:=sin(rot)*xu+cos(rot)*yu;
    x:=cos(rot)*xu-sin(rot)*yu;
    Cnv.MoveTo(round(EstX+x),round(EstY+y));
     for i:=-99 to 100 do begin
        xu:=CovU*i/100;
        yu:=-CovV*sqrt(1-power(xu/CovU,2));
        y:=sin(rot)*xu+cos(rot)*yu;
        x:=cos(rot)*xu-sin(rot)*yu;
        Cnv.LineTo(round(EstX+x),round(EstY+y));
     end;
  end;
end;

procedure TForm_Localization.DrawEllipse2(Cnv: TCanvas; A2,B2,theta,Cx,Cy: double; DrawColor: TColor);
const
  EToBConst=0.2761423749154;
var
  cCtlPt: array[0..12] of TPoint;
  offsetCx, offsetCy: double;
  xtemp, ytemp: double;
  xtemp1, ytemp1: double;
  sinAng, cosAng : double;
  A, B: double;
begin
  Cnv.Brush.Style:=bsClear;
  Cnv.Pen.Style:=psSolid;
  Cnv.Pen.Color:=DrawColor;
  Cnv.Pen.Width:=1;

  A:=A2*2;
  B:=B2*2;
  sinAng:=sin(theta);
  cosAng:=cos(theta);

  offsetCx:=A*EToBConst;
  offsetCy:=B*EToBConst;

  xtemp:=Cx-A/2;
  ytemp:=Cy;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[0].x:=round(xtemp1);
  cCtlPt[0].y:=round(ytemp1);

  xtemp:=Cx-A/2;
  ytemp:=Cy+offsetCy;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[1].x:=round(xtemp1);
  cCtlPt[1].y:=round(ytemp1);

  xtemp:=Cx-offsetCx;
  ytemp:=Cy+B/2;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[2].x:=round(xtemp1);
  cCtlPt[2].y:=round(ytemp1);

  xtemp:=Cx;
  ytemp:=Cy+B/2;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[3].x:=round(xtemp1);
  cCtlPt[3].y:=round(ytemp1);

  xtemp:=Cx+offsetCx;
  ytemp:=Cy+B/2;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[4].x:=round(xtemp1);
  cCtlPt[4].y:=round(ytemp1);

  xtemp:=Cx+A/2;
  ytemp:=Cy+offsetCy;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[5].x:=round(xtemp1);
  cCtlPt[5].y:=round(ytemp1);

  xtemp:=Cx+A/2;
  ytemp:=Cy;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[6].x:=round(xtemp1);
  cCtlPt[6].y:=round(ytemp1);

  xtemp:=Cx+A/2;
  ytemp:=Cy-offsetCy;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[7].x:=round(xtemp1);
  cCtlPt[7].y:=round(ytemp1);

  xtemp:=Cx+offsetCx;
  ytemp:=Cy-B/2;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[8].x:=round(xtemp1);
  cCtlPt[8].y:=round(ytemp1);

  xtemp:=Cx;
  ytemp:=Cy-B/2;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[9].x:=round(xtemp1);
  cCtlPt[9].y:=round(ytemp1);

  xtemp:=Cx-offsetCx;
  ytemp:=Cy-B/2;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[10].x:=round(xtemp1);
  cCtlPt[10].y:=round(ytemp1);

  xtemp:=Cx-A/2;
  ytemp:=Cy-offsetCy;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[11].x:=round(xtemp1);
  cCtlPt[11].y:=round(ytemp1);

  xtemp:=Cx-A/2;
  ytemp:=Cy;
  xtemp1:=Cx+cosAng*(xtemp-Cx)-sinAng*(ytemp-Cy);//rotate
  ytemp1:=Cy+sinAng*(xtemp-Cx)+cosAng*(ytemp-Cy);
  cCtlPt[12].x:=round(xtemp1);
  cCtlPt[12].y:=round(ytemp1);

  //Cnv.Polyline(cCtlPt);
  Cnv.PolyBezier(cCtlPt[0..3], false, false);
  Cnv.PolyBezier(cCtlPt[3..6], false, false);
  Cnv.PolyBezier(cCtlPt[6..9], false, false);
  Cnv.PolyBezier(cCtlPt[9..12], false, false);
end;

procedure TForm_Localization.DrawCharts;
var
  Speed: double;
begin
  ChartCompassLineSeries.AddXY(FDrawChartsIterator, NormalizeAngle(FInputData.Compass.Angle)*180/pi);
  ChartCompassFilteredLineSeries.AddXY(FDrawChartsIterator, NormalizeAngle(FLocalizationData.CompassFiltered.Angle)*180/pi);
  ChartCompassTetaLocLineSeries.AddXY(FDrawChartsIterator, NormalizeAngle(FLocalizationData.Pose.teta)*180/pi);
  ChartCompassDiferenceLineSeries.AddXY(FDrawChartsIterator, NormalizeAngle(FLocalizationData.Pose.teta
                                                                           -FLocalizationData.CompassFiltered.Angle)*180/pi);
  if FInputData.Compass.Avaible then begin
    if FLocalizationData.State=LocLost then begin
      ChartCompassOkBarSeries.AddXY(FDrawChartsIterator, 0);
      ChartCompassLostBarSeries.AddXY(FDrawChartsIterator, 180);
      ChartCompassLostBarSeries.AddXY(FDrawChartsIterator, -180);
    end
    else begin
      ChartCompassOkBarSeries.AddXY(FDrawChartsIterator, 180);
      ChartCompassOkBarSeries.AddXY(FDrawChartsIterator, -180);
      ChartCompassLostBarSeries.AddXY(FDrawChartsIterator, 0);
    end;
  end
  else begin
    ChartCompassOkBarSeries.AddXY(FDrawChartsIterator, 0);
    ChartCompassLostBarSeries.AddXY(FDrawChartsIterator, 0);
  end;
  Speed:=sqrt(power(FInputData.OdosData.v,2)+power(FInputData.OdosData.vn,2));
  if FInputData.OdosData.Avaible then begin
    ChartVOdodLineSeries.AddXY(FDrawChartsIterator, Speed);
  end
  else begin
    ChartVOdodLineSeries.AddXY(FDrawChartsIterator, 0);
  end;
  if FInputData.OdosData.Avaible then begin
    ChartWOdodLineSeries.AddXY(FDrawChartsIterator, FInputData.OdosData.w);
  end
  else begin
    ChartWOdodLineSeries.AddXY(FDrawChartsIterator, 0);
  end;
  inc(FDrawChartsIterator);
end;

function TForm_Localization.InicializeLogRecordInputs: boolean;
var
  FileName: string;
  HeadString: string;
  i: integer;
begin
  FileName:=ExtractFilePath(Application.ExeName)+FDataDir+'/'+Edit_DebugRecordFolder.Text;
  FLogInputsTime:=round(SpinEdit_RecordTime.Value*60/(CCycleTime/1000)); //CCycleTime in ms
  Result:=FLogInputsData.InicializeRecord(FileName, FLogInputsTime, CNLogColumns);
  //TickCount, OdosAvaible, dv, dvn, dteta, v, vn, w, CompassAvaible, Angle, VPointListCont, x, y...
  HeadString:='TickCount,OdosAvaible,dv,dvn,dteta,v,vn,w,CompassAvaible,Angle,VPointListCont';
  for i:=0 to MaxPointsNumber-1 do begin
    HeadString:=HeadString+',x,y';
  end;
  FLogInputsData.HeadString:=HeadString;
end;

function TForm_Localization.InicializeLogPlayInputs: boolean;
var
  FileName: string;
begin
  FileName:=ExtractFilePath(Application.ExeName)+FDataDir+'/'+Edit_DebugPlayFolder.Text;
  Result:=FLogInputsData.InicializePlay(FileName);
  if FLogInputsData.NColumns<>CNLogColumns then begin
    ShowMessage('Invalide Log File');
    Result:=false;
  end;
  FLogInputsTime:=FLogInputsData.NLines;
  SpinEdit_DebugSampleNumber.MaxValue:=FLogInputsTime;
  SpinEdit_DebugSampleNumber.Value:=0;
end;

function TForm_Localization.AddLogRecordNewInputs(TickCount: DWORD): boolean;
var
  NewLogLine: array of double;
  i: integer;
begin
  //TickCount, OdosAvaible, dv, dvn, dteta, v, vn, w, CompassAvaible, Angle, VPointListCont, Points...
  SetLength(NewLogLine, CNLogColumns);
  NewLogLine[0]:=TickCount;
  if FInputData.OdosData.Avaible then begin
    NewLogLine[1]:=1;
  end
  else begin
    NewLogLine[1]:=0;
  end;
  NewLogLine[2]:=FInputData.OdosData.dv;
  NewLogLine[3]:=FInputData.OdosData.dvn;
  NewLogLine[4]:=FInputData.OdosData.dteta;
  NewLogLine[5]:=FInputData.OdosData.v;
  NewLogLine[6]:=FInputData.OdosData.vn;
  NewLogLine[7]:=FInputData.OdosData.w;
  if FInputData.Compass.Avaible then begin
    NewLogLine[8]:=1;
  end
  else begin
    NewLogLine[8]:=0;
  end;
  NewLogLine[9]:=FInputData.Compass.Angle;
  NewLogLine[10]:=FInputData.VPointList.PCount;
  for i:=0 to MaxPointsNumber-1 do begin
    NewLogLine[2*i+11]:=FInputData.VPointList.PList[i].x;
    NewLogLine[2*i+11+1]:=FInputData.VPointList.PList[i].y;
  end;
  Result:=FLogInputsData.AddNewLine(NewLogLine);
  SetLength(NewLogLine,0);
end;

function TForm_Localization.GetLogPlayNewInputs(Iterator: longword): boolean;
var
  NewLogLine: array of double;
  i: integer;
begin
  //TickCount, OdosAvaible, dv, dvn, dteta, v, vn, w, CompassAvaible, Angle, VPointListCont, Points...
  SetLength(NewLogLine, CNLogColumns);
  Result:=FLogInputsData.GetNewLine(NewLogLine, Iterator);

  FLogInputsTickCount:=round(NewLogLine[0]);
  if NewLogLine[1]=1 then begin
    FInputData.OdosData.Avaible:=true;
  end
  else begin
    FInputData.OdosData.Avaible:=false;
  end;
  FInputData.OdosData.dv:=NewLogLine[2];
  FInputData.OdosData.dvn:=NewLogLine[3];
  FInputData.OdosData.dteta:=NewLogLine[4];
  FInputData.OdosData.v:=NewLogLine[5];
  FInputData.OdosData.vn:=NewLogLine[6];
  FInputData.OdosData.w:=NewLogLine[7];
  if NewLogLine[8]=1 then begin
    FInputData.Compass.Avaible:=true;
  end
  else begin
    FInputData.Compass.Avaible:=false;
  end;
  FInputData.Compass.Angle:=NewLogLine[9];
  FInputData.VPointList.PCount:=round(NewLogLine[10]);
  for i:=0 to MaxPointsNumber-1 do begin
    FInputData.VPointList.PList[i].x:=NewLogLine[2*i+11];
    FInputData.VPointList.PList[i].y:=NewLogLine[2*i+11+1];
  end;

  SetLength(NewLogLine, 0);
end;

function TForm_Localization.ProcessActualSpeed(ActualPose, LastPose: TPose2): TSpeed;
var
  deltaX, deltaY: double;
  deltaV, deltaAng: double;
begin
  Result.W:=NormalizeAngle(ActualPose.teta-LastPose.teta)/(CCycleTime/1000);
  deltaX:=ActualPose.x-LastPose.x;
  deltaY:=ActualPose.y-LastPose.Y;
  deltaV:=sqrt(power(deltaX,2)+power(deltaY,2));
  deltaAng:=ATan2(deltaY,deltaX)-ActualPose.teta;
  Result.V:=deltaV*cos(deltaAng)/(CCycleTime/1000);
  Result.Vn:=deltaV*sin(deltaAng)/(CCycleTime/1000);
end;

procedure TForm_Localization.ProcessInputData;
begin
  if FLocParameters.LimitMeasureMaxDistance then begin
    FInputData.VPointList:=LimitMaxDist(FInputData.VPointList, FLocParameters.MeasureMaxDistance);
  end;

  if FLocParameters.CompassDisable then begin
    FInputData.Compass.Avaible:=false;
  end;

  FLocalizationData.CompassFiltered.Avaible:=FInputData.Compass.Avaible;
  FLocalizationData.CompassFiltered.Angle:=FLowPassCompass.calcCompassVal(FInputData.Compass.Angle);

  case RadioGroup_Mode.ItemIndex of
    0: begin
      Mode0;
    end;
    1: begin
      Mode1;
    end;
    2: begin
      Mode2;
    end;
    3: begin
      Mode3;
    end;
  end;

  FLocalizationData.Speed:=ProcessActualSpeed(FLocalizationData.Pose, FLocalizationData.LasPose);

  if Showing and CheckBox_Show.Checked then begin
    //Visualization Things
    WriteLocalizationState(FLocalizationData.State, Edit_LocalizationState);
    ImageLocMap.Canvas.Draw(0,0,TempBitmap);
    if RadioGroup_Mode.ItemIndex=0 then begin
      if RadioGroup_LocalizationDataSelectMap.Checked[3] then begin
        DrawCompass(FLocalizationData.Pose, FInputData.Compass, ImageLocMap.Canvas, clPurple);
      end;
      DrawLocalizationError3(FLocalizationData.Pose, ImageLocMap.Canvas, clGreen);
      DrawLocalizationData3(FLocalizationData.Pose, FInputData.VPointList, ImageLocMap.Canvas, clBlue);
    end
    else begin

      if RadioGroup_LocalizationDataSelectMap.Checked[3] then begin
        DrawCompass(FLocalizationData.Pose, FLocalizationData.CompassFiltered, ImageLocMap.Canvas, clPurple);
      end;
      DrawLocalizationError3(FLocalizationData.Pose, ImageLocMap.Canvas, clGreen);
      if RadioGroup_LocalizationDataSelectMap.Checked[0] then begin
        DrawPose(FLocalizationData.PoseOdos, ImageLocMap.Canvas,clRed);
      end;
      if RadioGroup_LocalizationDataSelectMap.Checked[1] then begin
        DrawPose(FLocalizationData.PoseSensors, ImageLocMap.Canvas, clYellow);
      end;
      if RadioGroup_LocalizationDataSelectMap.Checked[2] then begin
        DrawLocalizationData3(FLocalizationData.Pose, FInputData.VPointList, ImageLocMap.Canvas, clBlue);
      end;
      if RadioGroup_LocalizationDataSelectMap.Checked[4] then begin
        DrawClosePnts(FLocalizationData.Pose, FLostNavigation.CPointList, ImageLocMap.Canvas, clRed);
      end;
    end;
    case RadioGroup_LocalizationDataSelect.ItemIndex of
      0: WriteLocatizationDataTable(FLocalizationData.PoseOdos, StringGrid_LocalizationData);
      1: WriteLocatizationDataTable(FLocalizationData.PoseSensors, StringGrid_LocalizationData);
      2: WriteLocatizationDataTable(FLocalizationData.Pose, StringGrid_LocalizationData);
    end;
    WriteActualSpeed(FLocalizationData.Speed, Edit_ActualSpeed);
    WriteActualPose(FLocalizationData.Pose, Edit_ActualPose);
    DrawCharts;
    if FGlobalLocalization.MultiHypothesisOn then begin
      ImageLocMapGlobal.Canvas.Draw(0,0,TempBitmap);
      DrawGlobalLocData(FGlobalLocalization.LocHypothesisSet, ImageLocMapGlobal.Canvas);
      DrawGlobalLocData(FGlobalLocalization.LocHypothesisSet, ImageLocMap.Canvas);
      WriteGlobalLocData(FGlobalLocalization.LocHypothesisSet, Edit_GlobalNumHypothesis,
                         Chart_GlobalLocalizationDataBarSeriesCost);
    end;
  end;
  FLocalizationData.LasPose:=FLocalizationData.Pose;
end;

procedure TForm_Localization.Mode0;
begin
  if not FLocParameters.KalmanEnable then begin
    FLocalizationData.PoseOdos:=PropagateXYTetaOdos2(FLocalizationData.Pose, FInputData.OdosData,
                                                     FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
  end
  else begin
    FLocalizationData.PoseOdos:=PropagateXYTetaOdos3(FLocalizationData.Pose, FInputData.OdosData,
                                                     FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
  end;
  FLocalizationData.Pose:=FLocalizationData.PoseOdos;
  FLocalizationData.State:=LocOk;
end;

procedure TForm_Localization.Mode1;
begin
  //Check for actions
  if FLocalizationData.State=LocInverted then begin
    FLocalizationData.Pose.x:=-FLocalizationData.Pose.x;
    FLocalizationData.Pose.y:=-FLocalizationData.Pose.y;
    FLocalizationData.Pose.teta:=NormalizeAngle(FLocalizationData.Pose.teta+pi);
    FLocalizationData.State:=LocOk;
  end;

  FLocalizationData.PoseOdos:=PropagateXYTetaOdos2(FLocalizationData.Pose, FInputData.OdosData,
                                                   FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
  if FInputData.VPointList.PCount>=CNumMinPoins then begin
    OptimizeMatch(FLocalizationData.PoseSensors, FLocalizationData.PoseOdos,
                  FInputData.VPointList, FLocParameters.RPropIterationNumber,
                  t1, t2);
    FLocalizationData.Pose:=FLocalizationData.PoseSensors;
  end
  else begin
    FLocalizationData.Pose:=FLocalizationData.PoseOdos;
  end;

  //Process localization mode
  if FLocalizationData.State=LocOk then begin
    case FLocParameters.ProcessLocalizationMode of
      0: begin
        FLocalizationData.State:=FProcessLocalizationState.NewStateMode0(FLocalizationData.State,
                                                                         FInputData.OdosData,
                                                                         FLocalizationData.Pose,
                                                                         FLocalizationData.CompassFiltered);
      end;
      1: begin
        FLocalizationData.State:=FProcessLocalizationState.NewStateMode1(FLocalizationData.State,
                                                                         FInputData.OdosData,
                                                                         FLocalizationData.Pose,
                                                                         FInputData.Compass);
      end;
    end;
  end;
end;

procedure TForm_Localization.Mode2;
begin
  //Check for actions
  if FLocalizationData.State=LocInverted then begin
    FLocalizationData.Pose.x:=-FLocalizationData.Pose.x;
    FLocalizationData.Pose.y:=-FLocalizationData.Pose.y;
    FLocalizationData.Pose.teta:=NormalizeAngle(FLocalizationData.Pose.teta+pi);
    FLocalizationData.State:=LocOk;
    FLocalizationData.PoseSensors:=FLocalizationData.Pose;
  end;

  case FLocParameters.TrackingMode of
    0: begin
      if not FLocParameters.KalmanEnable then begin
        FLocalizationData.PoseOdos:=PropagateXYTetaOdos2(FLocalizationData.Pose, FInputData.OdosData,
                                                         FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
      end
      else begin
        FLocalizationData.PoseOdos:=PropagateXYTetaOdos3(FLocalizationData.Pose, FInputData.OdosData,
                                                         FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
      end;
      if FInputData.VPointList.PCount>=CNumMinPoins then begin
        OptimizeMatch(FLocalizationData.PoseSensors, FLocalizationData.PoseOdos,
                      FInputData.VPointList, FLocParameters.RPropIterationNumber,
                      t1, t2);
        if not FLocParameters.KalmanEnable then begin
          FLocalizationData.Pose:=EstimateFusion(FLocalizationData.PoseOdos, FLocalizationData.PoseSensors,
                                                 FLocParameters.MinVarXY, FLocParameters.MinVarTeta);
        end
        else begin
          FLocalizationData.Pose:=EstimateFusion1(FLocalizationData.PoseOdos, FLocalizationData.PoseSensors,
                                                  FLocParameters.MinVarXY, FLocParameters.MinVarTeta);
        end;
      end
      else begin
        FLocalizationData.Pose:=FLocalizationData.PoseOdos;
      end;
    end;
    1: begin
      FLocalizationData.PoseSensors:=PropagateXYTetaOdos0(FLocalizationData.PoseSensors, FInputData.OdosData);
      if not FLocParameters.KalmanEnable then begin
        FLocalizationData.PoseOdos:=PropagateXYTetaOdos2(FLocalizationData.Pose, FInputData.OdosData,
                                                         FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
      end
      else begin
        FLocalizationData.PoseOdos:=PropagateXYTetaOdos3(FLocalizationData.Pose, FInputData.OdosData,
                                                         FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
      end;
      if FInputData.VPointList.PCount>=CNumMinPoins then begin
        OptimizeMatch(FLocalizationData.PoseSensors, FLocalizationData.PoseSensors,
                      FInputData.VPointList, FLocParameters.RPropIterationNumber,
                      t1, t2);
        if not FLocParameters.KalmanEnable then begin
          FLocalizationData.Pose:=EstimateFusion(FLocalizationData.PoseOdos, FLocalizationData.PoseSensors,
                                                 FLocParameters.MinVarXY, FLocParameters.MinVarTeta);
        end
        else begin
          FLocalizationData.Pose:=EstimateFusion1(FLocalizationData.PoseOdos, FLocalizationData.PoseSensors,
                                                  FLocParameters.MinVarXY, FLocParameters.MinVarTeta);
        end;
      end
      else begin
        FLocalizationData.Pose:=FLocalizationData.PoseOdos;
      end;
    end;
  else
    begin
      FLocParameters.TrackingMode:=0;
      raise Exception.Create('Invalide tracking Mode');
    end;
  end;

  //Process localization mode
  if FLocalizationData.State=LocOk then begin
    case FLocParameters.ProcessLocalizationMode of
      0: begin
        FLocalizationData.State:=FProcessLocalizationState.NewStateMode0(FLocalizationData.State,
                                                                         FInputData.OdosData,
                                                                         FLocalizationData.Pose,
                                                                         FLocalizationData.CompassFiltered);
      end;
      1: begin
        FLocalizationData.State:=FProcessLocalizationState.NewStateMode1(FLocalizationData.State,
                                                                         FInputData.OdosData,
                                                                         FLocalizationData.Pose,
                                                                         FInputData.Compass);
      end;
    end;
  end;
end;

procedure TForm_Localization.Mode3; //global
var
  NewLocalizationData: TLocalizationData;
begin
  //Check for actions
  if FLocalizationData.State=LocInverted then begin
    FLocalizationData.Pose.x:=-FLocalizationData.Pose.x;
    FLocalizationData.Pose.y:=-FLocalizationData.Pose.y;
    FLocalizationData.Pose.teta:=NormalizeAngle(FLocalizationData.Pose.teta+pi);
    FLocalizationData.State:=LocOk;
    FLocalizationData.PoseSensors:=FLocalizationData.Pose;
  end;
  case FLocParameters.TrackingMode of
    0: begin
      if not FLocParameters.KalmanEnable then begin
        FLocalizationData.PoseOdos:=PropagateXYTetaOdos2(FLocalizationData.Pose, FInputData.OdosData,
                                                         FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
      end
      else begin
        FLocalizationData.PoseOdos:=PropagateXYTetaOdos3(FLocalizationData.Pose, FInputData.OdosData,
                                                          FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
      end;
      if FInputData.VPointList.PCount>=CNumMinPoins then begin
        OptimizeMatch(FLocalizationData.PoseSensors, FLocalizationData.PoseOdos,
                      FInputData.VPointList, FLocParameters.RPropIterationNumber,
                      t1, t2);
        if not FLocParameters.KalmanEnable then begin
          FLocalizationData.Pose:=EstimateFusion(FLocalizationData.PoseOdos, FLocalizationData.PoseSensors,
                                                 FLocParameters.MinVarXY, FLocParameters.MinVarTeta);
        end
        else begin
          FLocalizationData.Pose:=EstimateFusion1(FLocalizationData.PoseOdos, FLocalizationData.PoseSensors,
                                                  FLocParameters.MinVarXY, FLocParameters.MinVarTeta);
        end;
      end
      else begin
        FLocalizationData.Pose:=FLocalizationData.PoseOdos;
      end;
    end;
    1: begin
      FLocalizationData.PoseSensors:=PropagateXYTetaOdos0(FLocalizationData.PoseSensors, FInputData.OdosData);
      if not FLocParameters.KalmanEnable then begin
        FLocalizationData.PoseOdos:=PropagateXYTetaOdos2(FLocalizationData.Pose, FInputData.OdosData,
                                                         FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
      end
      else begin
        FLocalizationData.PoseOdos:=PropagateXYTetaOdos3(FLocalizationData.Pose, FInputData.OdosData,
                                                         FLocParameters.OdometryKErrorXY, FLocParameters.OdometryKErrorTeta);
      end;
      if FInputData.VPointList.PCount>=CNumMinPoins then begin
        OptimizeMatch(FLocalizationData.PoseSensors, FLocalizationData.PoseSensors,
                      FInputData.VPointList, FLocParameters.RPropIterationNumber,
                      t1, t2);
        if not FLocParameters.KalmanEnable then begin
          FLocalizationData.Pose:=EstimateFusion(FLocalizationData.PoseOdos, FLocalizationData.PoseSensors,
                                                 FLocParameters.MinVarXY, FLocParameters.MinVarTeta);
        end
        else begin
          FLocalizationData.Pose:=EstimateFusion1(FLocalizationData.PoseOdos, FLocalizationData.PoseSensors,
                                                  FLocParameters.MinVarXY, FLocParameters.MinVarTeta);
        end;
      end
      else begin
        FLocalizationData.Pose:=FLocalizationData.PoseOdos;
      end;
    end;
  else
    begin
      FLocParameters.TrackingMode:=0;
      raise Exception.Create('Invalide tracking Mode');
    end;
  end;

  //Process localization mode
  if FLocalizationData.State=LocOk then begin
    case FLocParameters.ProcessLocalizationMode of
      0: begin
        FLocalizationData.State:=FProcessLocalizationState.NewStateMode0(FLocalizationData.State,
                                                                         FInputData.OdosData,
                                                                         FLocalizationData.Pose,
                                                                         FLocalizationData.CompassFiltered);
      end;
      1: begin
        FLocalizationData.State:=FProcessLocalizationState.NewStateMode1(FLocalizationData.State,
                                                                         FInputData.OdosData,
                                                                         FLocalizationData.Pose,
                                                                         FInputData.Compass);
      end;
    end;
  end
  else begin
    case FLocParameters.GlobalLocalizationMode of
      0: NewLocalizationData:=FGlobalLocalization.NewPoseMode0(FLocalizationData);
      1: NewLocalizationData:=FGlobalLocalization.NewPoseMode1(FLocalizationData, FInputData.OdosData,
                                                               FInputData.Compass, FInputData.VPointList);
      2: NewLocalizationData:=FGlobalLocalization.NewPoseMode2(FLocalizationData, FInputData.OdosData,
                                                               FInputData.Compass, FInputData.VPointList);
    end;
    case FLocParameters.NavLostMode of
      0: begin
        FLostNavigation.CalcAdvisedSpeed0(FLocalizationData.State, FInputData.VPointList, FInputData.OdosData.dteta);
        FLocalizationData.AdviseSpeed:=FLostNavigation.AdviseSpeed;
      end;
    end;
    if NewLocalizationData.State=LocOk then begin
      FLocalizationData:=NewLocalizationData;
      FLocalizationData.PoseSensors:=FLocalizationData.Pose;
      FLocalizationData.PoseOdos:=FLocalizationData.Pose;
      FProcessLocalizationState.Reset;
      FGlobalLocalization.Reset;
      FLostNavigation.Reset;
    end;
  end;
end;

procedure TForm_Localization.SetInputData(InputData: TLocInputData);
var
  deltatime: DWord;
begin
  if not (RadioGroup_DebugMode.ItemIndex=2) then begin
    TimingLoad.last:=GetTickCount;
    FInputData:=InputData;
    ProcessInputData;

    if RadioGroup_DebugMode.ItemIndex=1 then begin
      if not AddLogRecordNewInputs(GetTickCount) then begin
        RadioGroup_DebugMode.ItemIndex:=0;
      end;
    end;
    TimingLoad.actual:=GetTickCount;

    //timings
    TimingEventSource.actual:=GetTickCount;
    deltatime:=TimingEventSource.actual-TimingEventSource.last;
    if (deltatime)>TimingEventSource.max then begin
      TimingEventSource.max:=deltatime;
    end;
    TimingEventSource.last:=TimingEventSource.actual;
    deltatime:=TimingLoad.actual-TimingLoad.last;
    if (deltatime)>TimingLoad.max then begin
      TimingLoad.max:=deltatime;
    end;
  end;
end;

function TForm_Localization.GetOutputData: TLocOutputData;
begin
  if not (RadioGroup_DebugMode.ItemIndex=2) then begin
    Result.x:=FLocalizationData.Pose.x;
    Result.y:=FLocalizationData.Pose.y;
    Result.teta:=FLocalizationData.Pose.teta;
    if FLocalizationData.State=LocOk then begin
      Result.Avaible:=true;
      Result.LocAdvV:=0;
      Result.LocAdvVn:=0;
      Result.LocAdvW:=0;
    end
    else begin
      Result.Avaible:=false;
      Result.LocAdvV:=FLocalizationData.AdviseSpeed.V;
      Result.LocAdvVn:=FLocalizationData.AdviseSpeed.Vn;
      Result.LocAdvW:=FLocalizationData.AdviseSpeed.W;
    end;
  end
  else begin
    Result.x:=0;
    Result.y:=0;
    Result.teta:=0;
    Result.Avaible:=false;
    Result.LocAdvV:=0;
    Result.LocAdvVn:=0;
    Result.LocAdvW:=0;
  end;
end;

initialization
  {$I unit_localization.lrs}

end.

