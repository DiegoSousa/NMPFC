unit Unit_Hardware; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, IniPropStorage, Menus, ExtCtrls, Grids, Spin,
  lNetComponents, lNet, Unit_FlashBus;

const
  CDataConfigDir='data';
  CDataConfigFile='ConfigHardware.ini';

  //Motor outputs convertions
  KPwmRaw2Processed=100/127;
  KAngVelRefRaw2Processed=1;
  KMaxVelocity=66;

  //Motor inputs convertions
  KSamplePeriodRaw2Processed=10;
  KTicks2Rad=0.0005;//confirmar este valor

  //Motor parameters convertions
  KMaxCurrentRaw2Processed=1;
  KPidPeriodRaw2Processed=1;

  //kicker
  KMaxDischargeTime=40;

  //Sensors
  KRollerPwmRaw2Processed=100/255;
  KCompassRaw2Processed=2*pi/255;

  //mischelandia
  KMaxArmTime=5000;

type
  TGlobalState=(Ok, DriverDeads, NoSouceEvent, NoFlashBus);
  TTimings=record
    last, actual, max: DWORD;
  end;
  TSpecialKickerStateMachine=(SKickerIdle, SKickerArm, SKickerSolenoid);
  TSpecialKickerCom=(SComIDlekicker, SComHighkicker, SComLowkicker);

  {TMotorRaw}

  TMotorRaw=record
    //Outputs
    ControlSource: byte;
    OutputRef: Smallint;

    //Inputs
    ResetFlag: byte;
    EncoderTicks: word;
    SamplePeriod: word;
    PWMOutA: Shortint;
    PWMOutB: Shortint;
    Current: Byte;

    //Parameters
    MaxCurrent: byte;
    PidPeriod: byte;
    PwmMaxSlope: byte;
    PID_FILT_EV: Single;
    PID_K: Single;
    PID_INVTI: Single;
    PID_ALPHA: Single;
    PID_TD: Single;
    PID_FILT_REF: Single;
  end;

  {TMotorProcessed}

  TControlMode=(Stop=0, PWM, PID);

  TMotorProcessed=record
    //Outputs
    ControlSource: TControlMode;
    AngVelRef: double; //Rads/s
    PWM: double; //%

    //Inputs
    ExistAuxOldSample: boolean;
    ResetCount: integer;

    AuxOldSamplePeriod: word;
    DeltaSamplePeriod: integer; //ms

    EncoderTicks: integer;
    AuxOldEncoderTicks: word;
    DeltaEncoderTicks: integer;
    NewSumDeltaEncoderTicks: integer; //para tirar
    SumDeltaEncoderTicks: integer;

    AngVel: double; //Rads/s

    PWMOutA: double; //%
    PWMOutB: double; //%

    Current: double;

    //Parameters
    MaxCurrent: double; //Amperes
    PidPeriod: integer; //ms
    PwmMaxSlope: double; //%/ms
    PID_FILT_EV: Single;
    PID_K: Single;
    PID_INVTI: Single;
    PID_ALPHA: Single;
    PID_TD: Single;
    PID_FILT_REF: Single;
  end;

  {TKickerRaw}

  TKickerRaw=record
    //Outputs
    Kick: byte;
    //Inputs
    Vbat: word;
    Vcap: word;
    //Parameters
    Vcap_Target: word;
    Vcap_Lvl2: word;
    Vcap_Lvl1: word;
    Vcap_Critical: word;
    Cur_PWM: byte;
    PWM_Target: byte;
    PWM_Lvl2: byte;
    PWM_Lvl1: byte;
    PWM_Limit: byte;
    VtoF_SamplePer: byte;
    dummy: byte;
    MaxDischargeTime: byte;
    Auto_Charge_OnOFF: byte;
  end;

  {TKickerProcessed}

  TKickerProcessed=record
    //Outputs
    Kick: boolean;
    DischargeTime: byte;
    //Inputs
    Vbat: double;
    Vcap: double;
    //Parameters
    Vcap_Target: double;
    Vcap_Lvl2: double;
    Vcap_Lvl1: double;
    Vcap_Critical: double;
    Cur_PWM: byte;
    PWM_Target: byte;
    PWM_Lvl2: byte;
    PWM_Lvl1: byte;
    PWM_Limit: byte;
    VtoF_SamplePer: byte;
    dummy: byte;
    MaxDischargeTime: byte;
    Auto_Charge_OnOFF: byte;

    //temp
    VbatGain: double;
    VbatOffset: double;
    VcapGain: double;
    VcapOffset: double;
  end;

  {TSensorsRaw}

  TSensorsRaw=record
    //Outputs
    Solenoid: byte;
    RollerPWM: byte;
    RollerSpeedRef: byte;
    RollerMode: byte;
    RollerDir: byte;
    //Inputs
    SolenoidTime: byte;
    Compass: byte;
    BallSensor: byte;
    RollerSpeed: byte;
  end;

  {TSensorsProcessed}

  TSensorsProcessed=record
    //Outputs
    Solenoid: boolean;
    RollerPWM: double;
    RollerSpeedRef: double;
    RollerMode: TControlMode;
    //Inputs
    SolenoidTime: integer;
    Compass: double;
    CompassOffSet: double;
    NewCompass: boolean;
    BallSensor: Boolean;
    RollerSpeed: double;
  end;

  { TForm_Hardware }

  TForm_Hardware = class(TForm)
    Button_Compass_SetOffset: TButton;
    Button_HightKick: TButton;
    Button_LowKick: TButton;
    Button_Kick: TButton;
    Button_KickerNumberLoad: TButton;
    Button_Motor1_Max: TButton;
    Button_Motor3_Max: TButton;
    Button_Motor2_Max: TButton;
    Button_Motor2_Min: TButton;
    Button_Motor3_Min: TButton;
    Button_Motor1_Stop: TButton;
    Button_Motor1_Min: TButton;
    Button_Motor2_Stop: TButton;
    Button_Kicker_Parameters_Read: TButton;
    Button_Kicker_Parameters_Write: TButton;
    Button_Motor3_Stop: TButton;
    Button_Motor2_Parameters_Read: TButton;
    Button_Motor3_Parameters_Read: TButton;
    Button_Motor1_Parameters_Write: TButton;
    Button_Motor1_Parameters_Read: TButton;
    Button_Motor2_Parameters_Write: TButton;
    Button_Motor3_Parameters_Write: TButton;
    CheckBox_Enable_Timer: TCheckBox;
    CheckBox_Connect: TCheckBox;
    CheckBox_Sensors_Enable: TCheckBox;
    CheckBox_Motor1_Enable: TCheckBox;
    CheckBox_Motor2_Enable: TCheckBox;
    CheckBox_Motor3_Enable: TCheckBox;
    CheckBox_Kicker_Enable: TCheckBox;
    Edit_RollerOutputbar: TEdit;
    Edit_Sensors_Status: TEdit;
    Edit_Motor1_Output_Value: TEdit;
    Edit_Motor2_Status: TEdit;
    Edit_KickerDischargeTime: TEdit;
    Edit_Motor3_Status: TEdit;
    Edit_Motor2_Output_Value: TEdit;
    Edit_Motor3_Output_Value: TEdit;
    Edit_Kicker_Status: TEdit;
    Edit_Timings: TEdit;
    Edit_GloblaStatus: TEdit;
    Edit_Motor1_Status: TEdit;
    FloatSpinEdit_CompassOffset: TFloatSpinEdit;
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
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem_Hardware: TMenuItem;
    Parameters: TLabel;
    LUDP: TLUDPComponent;
    MainMenu: TMainMenu;
    MenuItem_UDP: TMenuItem;
    MenuItem_Configure: TMenuItem;
    PageControl: TPageControl;
    Parameters1: TLabel;
    Parameters2: TLabel;
    Parameters3: TLabel;
    RadioGroup_Motor1_Mode: TRadioGroup;
    RadioGroup_Roller_Mode: TRadioGroup;
    RadioGroup_Motor2_Mode: TRadioGroup;
    RadioGroup_Motor3_Mode: TRadioGroup;
    ScrollBar_KickerDischargeTime: TScrollBar;
    ScrollBar_RollerOutput: TScrollBar;
    ScrollBar_Motor1_Output: TScrollBar;
    ScrollBar_Motor2_Output: TScrollBar;
    ScrollBar_Motor3_Output: TScrollBar;
    SpinEdit_KickerNumber: TSpinEdit;
    StringGrid_Sensors_Inputs: TStringGrid;
    StringGrid_Kicker_Parameters1: TStringGrid;
    StringGrid_Motor2_Inputs: TStringGrid;
    StringGrid_Motor3_Inputs: TStringGrid;
    StringGrid_Motor1_Parameters: TStringGrid;
    StringGrid_Motor1_Inputs: TStringGrid;
    StringGrid_Kicker_Inputs: TStringGrid;
    StringGrid_Motor3_Parameters: TStringGrid;
    StringGrid_Motor2_Parameters: TStringGrid;
    StringGrid_Kicker_Parameters: TStringGrid;
    RobotManualControl: TTabSheet;
    TabSheet_Motor2: TTabSheet;
    TabSheet_Motor3: TTabSheet;
    TabSheet_Motor1: TTabSheet;
    TabSheet_Kicker: TTabSheet;
    TabSheet_Sensors: TTabSheet;
    Timer: TTimer;
    Timer_GlobalStatus: TTimer;
    ToggleBox_Solenoid: TToggleBox;
    procedure Button_Compass_SetOffsetClick(Sender: TObject);
    procedure Button_HightKickClick(Sender: TObject);
    procedure Button_LowKickClick(Sender: TObject);
    procedure Button_KickClick(Sender: TObject);
    procedure Button_KickerNumberLoadClick(Sender: TObject);
    procedure Button_Kicker_Parameters_ReadClick(Sender: TObject);
    procedure Button_Kicker_Parameters_WriteClick(Sender: TObject);
    procedure Button_Motor1_MaxClick(Sender: TObject);
    procedure Button_Motor1_MinClick(Sender: TObject);
    procedure Button_Motor2_MaxClick(Sender: TObject);
    procedure Button_Motor2_MinClick(Sender: TObject);
    procedure Button_Motor2_StopClick(Sender: TObject);
    procedure Button_Motor3_MaxClick(Sender: TObject);
    procedure Button_Motor3_MinClick(Sender: TObject);
    procedure Button_Motor3_StopClick(Sender: TObject);
    procedure FloatSpinEdit_CompassOffsetChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LUDPReceive(aSocket: TLSocket);
    procedure RadioGroup_Motor2_ModeClick(Sender: TObject);
    procedure RadioGroup_Motor3_ModeClick(Sender: TObject);
    procedure RadioGroup_Roller_ModeClick(Sender: TObject);
    procedure ScrollBar_KickerDischargeTimeChange(Sender: TObject);
    procedure ScrollBar_Motor2_OutputChange(Sender: TObject);
    procedure ScrollBar_Motor3_OutputChange(Sender: TObject);
    procedure ScrollBar_RollerOutputChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Button_Motor1_StopClick(Sender: TObject);
    procedure Button_Motor1_Parameters_ReadClick(Sender: TObject);
    procedure Button_Motor1_Parameters_WriteClick(Sender: TObject);
    procedure Button_Motor2_Parameters_ReadClick(Sender: TObject);
    procedure Button_Motor2_Parameters_WriteClick(Sender: TObject);
    procedure Button_Motor3_Parameters_ReadClick(Sender: TObject);
    procedure Button_Motor3_Parameters_WriteClick(Sender: TObject);
    procedure CheckBox_ConnectChange(Sender: TObject);
    procedure CheckBox_Driver_EnableChange(Sender: TObject);
    procedure CheckBox_Enable_TimerChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure LUDPError(const msg: string; aSocket: TLSocket);
    procedure MenuItem_HardwareClick(Sender: TObject);
    procedure MenuItem_UDPClick(Sender: TObject);
    procedure RadioGroup_Motor1_ModeClick(Sender: TObject);
    procedure ScrollBar_Motor1_OutputChange(Sender: TObject);
    procedure Timer_GlobalStatusTimer(Sender: TObject);
    procedure ToggleBox_SolenoidChange(Sender: TObject);
  private
    GlobalState: TGlobalState;
    FFlashBus: TFlashBus;

    TimingEventSource: TTimings;
    TimingDriversResponse: TTimings;

    FMotorRaw: array[1..3] of TMotorRaw;
    FMotorProcessed: array[1..3] of TMotorProcessed;

    FKickerRaw: TKickerRaw;
    FKickerProcessed: TKickerProcessed;

    FSensorsRaw: TSensorsRaw;
    FSensorsProcessed: TSensorsProcessed;

    //temp code
    FLogKickerParameters: TStringList;

    //Special Kicker
    FSpecialKickerState: TSpecialKickerStateMachine;
    FLastSpecialKickerState: TSpecialKickerStateMachine;
    FSpecialKickerStateTiming: Dword;
    FSpecialKickerCom: TSpecialKickerCom;
    FSpecialDischargeTime: byte;


    Procedure Inicialize;

    //Motors
    procedure FillFlashBusTablesMotorsOutputs;
    procedure MotorOutputsProcessed2Raw(MotorNum: integer);
    procedure MotorOutputsFromGui(MotorNum: integer; RadioGroup: TRadioGroup; ScrollBar: TScrollBar);
    //procedure MotorOutputs2Gui(MotorNum: integer; RadioGroup: TRadioGroup; ScrollBar: TScrollBar);

    procedure FillFlashBusTablesMotorsInputs;
    procedure MotorInputsRaw2Processed(MotorNum: integer);
    procedure MotorInputsRaw2Processed2(MotorNum: integer);
    procedure MotorInputs2Gui(MotorNum: integer; StringGrid: TStringGrid);

    procedure FillFlashBusTablesMotorsParameters;
    procedure MotorParametersRaw2Processed(MotorNum: integer);
    procedure MotorParametersProcessed2Raw(MotorNum: integer);
    procedure MotorParameters2Gui(MotorNum: integer; StringGrid: TStringGrid);
    procedure MotorParametersFromGui(MotorNum: integer; StringGrid: TStringGrid);

    //Kicker
    procedure FillFlashBusTablesKickerOutputs;
    procedure KickerOutputsProcessed2Raw;
    procedure KickerOutputsFromGui(ScrollBar: TScrollBar);
    //procedure KickerOutputs2Gui;

    procedure FillFlashBusTablesKickerInputs;
    procedure KickerInputsRaw2Processed;
    procedure KickerInputs2Gui(StringGrid: TStringGrid);

    procedure FillFlashBusTablesKickerParameters;
    procedure KickerParametersRaw2Processed;
    procedure KickerParametersProcessed2Raw;
    procedure KickerParameters2Gui(StringGrid: TStringGrid);
    procedure KickerParametersFromGui(StringGrid: TStringGrid);

    //Sensors
    procedure FillFlashBusTablesSensorsOutputs;
    procedure SensorsOutputsProcessed2Raw;
    procedure SensorsOutputsFromGui(ToggleBox: TToggleBox; ScrollBar: TScrollBar; RadioGroup: TRadioGroup);

    procedure FillFlashBusTablesSensorsInputs;
    procedure SensorsInputsRaw2Processed;
    procedure SensorsInputs2Gui(StringGrid: TStringGrid);

    //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    procedure SaveParameters;
    procedure LoadParameters;

    procedure FSend_outputs;
    procedure FRequest_inputs;

    procedure Gui2FlashBus;
    procedure FlashBus2Gui;
    //special functions
    procedure FSpecialKickerStateMachine;
    procedure ProcessOdometry; //em construccao...
  public
    procedure SetAngVelRefWheels(w1,w2,w3: double);
    procedure SetPWMWheels(PWM1,PWM2,PWM3: double);
    procedure SetKicker(DischargeTime: integer);
    procedure SetSpecialKicker(DischargeTime: integer; LowKick: boolean);
    procedure SetRoller(RollerEnable: boolean);
    procedure SetRoller(RollerSpeed: double);
    function GetDeltaAngWheels(out w1,w2,w3: double): boolean;
    procedure GetSpeedAngWheels(out w1,w2,w3: double);
    function GetCompass(out value: double): boolean;
    function GetBallSensor: boolean;
    function Failure: boolean;
    procedure Send_outputs;
    procedure Request_inputs;
  end; 

var
  Form_Hardware: TForm_Hardware;

implementation

uses
  Unit_Hardware_Configure_UDP, unit_hardware_configure, LCLIntf, math, Utils;

procedure TForm_Hardware.FormCreate(Sender: TObject);
var
  i: integer;
  dataDir: string;
  TestFloat: double;
begin
  FFlashBus:=TFlashBus.Create;

  WindowState:=wsNormal;
  decimalSeparator:='.';

  dataDir:=CDataConfigDir;
  for i := 1 to paramcount do begin
    if not (paramstr(i) = '-coach') then begin
      if directoryExists(extractfilepath(application.ExeName)+dataDir) then begin
        DataDir:=paramstr(i);
      end
      else begin
         mkdir(ExtractFilePath(Application.ExeName)+DataDir);
      end;
    end;
  end;

  if not DirectoryExists(ExtractFilePath(Application.ExeName)+DataDir) then begin
    mkdir(ExtractFilePath(Application.ExeName)+DataDir);
  end;

  IniPropStorage.IniFileName:=ExtractFilePath(Application.ExeName)+DataDir+'/'+CDataConfigFile;
  IniPropStorage.Restore;
  CheckBox_Connect.Checked:=true;
  LoadParameters;

  FillFlashBusTablesMotorsOutputs;
  FillFlashBusTablesMotorsInputs;
  FillFlashBusTablesMotorsParameters;

  FillFlashBusTablesKickerOutputs;
  FillFlashBusTablesKickerInputs;
  FillFlashBusTablesKickerParameters;

  FillFlashBusTablesSensorsOutputs;
  FillFlashBusTablesSensorsInputs;

  //inicialize
  MotorOutputsFromGui(Motor1, RadioGroup_Motor1_Mode, ScrollBar_Motor1_Output);
  MotorOutputsFromGui(Motor2, RadioGroup_Motor2_Mode, ScrollBar_Motor2_Output);
  MotorOutputsFromGui(Motor3, RadioGroup_Motor3_Mode, ScrollBar_Motor3_Output);

  Gui2FlashBus;

  //temp code e aperfeiçoar!!!!
  FLogKickerParameters:=TStringList.Create;
  Button_KickerNumberLoadClick(Sender);
end;

procedure TForm_Hardware.FormShow(Sender: TObject);
begin

end;

procedure TForm_Hardware.Button_Motor1_MinClick(Sender: TObject);
begin
  ScrollBar_Motor1_Output.Position:=-100;
end;

procedure TForm_Hardware.Button_Motor2_MaxClick(Sender: TObject);
begin
  ScrollBar_Motor2_Output.Position:=100;
end;

procedure TForm_Hardware.Button_Motor2_MinClick(Sender: TObject);
begin
  ScrollBar_Motor2_Output.Position:=-100;
end;

procedure TForm_Hardware.Button_Motor2_StopClick(Sender: TObject);
begin
  ScrollBar_Motor2_Output.Position:=0;
end;

procedure TForm_Hardware.Button_Motor3_MaxClick(Sender: TObject);
begin
  ScrollBar_Motor3_Output.Position:=100;
end;

procedure TForm_Hardware.Button_Motor3_MinClick(Sender: TObject);
begin
  ScrollBar_Motor3_Output.Position:=-100;
end;

procedure TForm_Hardware.Button_Motor3_StopClick(Sender: TObject);
begin
  ScrollBar_Motor3_Output.Position:=0;
end;

procedure TForm_Hardware.FloatSpinEdit_CompassOffsetChange(Sender: TObject);
begin
  FSensorsProcessed.CompassOffSet:=NormalizeAngle(FloatSpinEdit_CompassOffset.Value)*pi/180;
end;

procedure TForm_Hardware.Button_Motor1_MaxClick(Sender: TObject);
begin
  ScrollBar_Motor1_Output.Position:=100;
end;

procedure TForm_Hardware.Button_Kicker_Parameters_ReadClick(Sender: TObject);
var
  i: integer;
begin
  for i:=1 to StringGrid_Kicker_Parameters.RowCount-1 do begin
    StringGrid_Kicker_Parameters.Cells[1,i]:='waiting';
  end;
  LUDP.SendMessage(FFlashBus.RequestParameters(Kicker), LUDP.Host);
end;

procedure TForm_Hardware.Button_KickerNumberLoadClick(Sender: TObject);
var
  i: integer;
  id: integer;
  TestFloat: double;
  Assigned: boolean=false;
begin
  try
    FLogKickerParameters.LoadFromFile(ExtractFilePath(Application.ExeName)+'LogKickerParameters');
    if ((FLogKickerParameters.Count mod 5)<>0) or (FLogKickerParameters.Count<=0) then begin
      ShowMessage('Log Kicker parameters load fail!');
    end
    else begin
      for i:=0 to (FLogKickerParameters.Count div 5)-1 do begin
        id:=StrToInt(FLogKickerParameters[i*5]);
        if id=SpinEdit_KickerNumber.Value then begin
          FKickerProcessed.VbatGain:=StrToFloat(FLogKickerParameters[i*5+1]);
          FKickerProcessed.VbatOffset:=StrToFloat(FLogKickerParameters[i*5+2]);
          FKickerProcessed.VcapGain:=StrToFloat(FLogKickerParameters[i*5+3]);
          FKickerProcessed.VcapOffset:=StrToFloat(FLogKickerParameters[i*5+4]);
          Assigned:=true;
          break;
        end;
      end;
      if not Assigned then begin
        FKickerProcessed.VbatGain:=0;
        FKickerProcessed.VbatOffset:=0;
        FKickerProcessed.VcapGain:=0;
        FKickerProcessed.VcapOffset:=0;
        ShowMessage('Log Kicker parameters load fail!');
      end;
      StringGrid_Kicker_Parameters1.Cells[1,1]:=FloatToStr(FKickerProcessed.VbatGain);
      StringGrid_Kicker_Parameters1.Cells[1,2]:=FloatToStr(FKickerProcessed.VbatOffset);
      StringGrid_Kicker_Parameters1.Cells[1,3]:=FloatToStr(FKickerProcessed.VcapGain);
      StringGrid_Kicker_Parameters1.Cells[1,4]:=FloatToStr(FKickerProcessed.VcapOffset);
    end;
  except
    FLogKickerParameters.Clear;
    ShowMessage('Log Kicker parameters load fail!');
  end;
end;

procedure TForm_Hardware.Button_KickClick(Sender: TObject);
begin
  KickerOutputsFromGui(ScrollBar_KickerDischargeTime);
end;

procedure TForm_Hardware.Button_HightKickClick(Sender: TObject);
begin
  FKickerProcessed.DischargeTime:=round(ScrollBar_KickerDischargeTime.Position/100*KMaxDischargeTime);
  FSpecialKickerCom:=SComHighkicker;
end;

procedure TForm_Hardware.Button_Compass_SetOffsetClick(Sender: TObject);
begin
  FSensorsProcessed.CompassOffSet:=NormalizeAngle(FSensorsProcessed.Compass+FSensorsProcessed.CompassOffSet);
  FloatSpinEdit_CompassOffset.Value:=FSensorsProcessed.CompassOffSet*180/pi;
end;

procedure TForm_Hardware.Button_LowKickClick(Sender: TObject);
begin
  FKickerProcessed.DischargeTime:=round(ScrollBar_KickerDischargeTime.Position/100*KMaxDischargeTime);
  FSpecialKickerCom:=SComLowkicker;
end;

procedure TForm_Hardware.Button_Kicker_Parameters_WriteClick(Sender: TObject);
var
  data2send: string;
begin
  KickerParametersFromGui(StringGrid_Kicker_Parameters);
  KickerParametersProcessed2Raw;
  data2send:=FFlashBus.WriteParameters(Kicker);
  LUDP.SendMessage(data2send, LUDP.Host);
  Button_Kicker_Parameters_ReadClick(Sender);
end;

procedure TForm_Hardware.FormDestroy(Sender: TObject);
begin
  LUDP.Disconnect(true);
  SaveParameters;

  FFlashBus.Free;

  //temp code
  FLogKickerParameters.Free;
end;

procedure TForm_Hardware.LUDPError(const msg: string; aSocket: TLSocket);
begin
  CheckBox_Connect.Checked:=false; //mudar isto
  ShowMessage(msg);
end;

procedure TForm_Hardware.LUDPReceive(aSocket: TLSocket);
var
  datain: string;
  i: integer;
  allread, allMotorsRead: boolean;
  deltatime: DWord;
begin
  LUDP.GetMessage(datain);
  FFlashBus.ReadMessage(datain);
  //inputs
  for i:=1 to NumDrivers do begin
    if FFlashBus.InputsChanged[i] then begin
      case i of
        Motor1, Motor2, Motor3: begin
          MotorInputsRaw2Processed(i);
          if Showing then begin
            FlashBus2Gui;
            case i of
              Motor1: MotorInputs2Gui(Motor1, StringGrid_Motor1_Inputs);
              Motor2: MotorInputs2Gui(Motor2, StringGrid_Motor2_Inputs);
              Motor3: MotorInputs2Gui(Motor3, StringGrid_Motor3_Inputs);
            end;
          end;
        end;
        Kicker: begin
          KickerInputsRaw2Processed;
          if Showing then begin
            FlashBus2Gui;
            KickerInputs2Gui(StringGrid_Kicker_Inputs);
          end;
        end;
        Sensors: begin
          SensorsInputsRaw2Processed;
          if Showing then begin
            FlashBus2Gui;
            SensorsInputs2Gui(StringGrid_Sensors_Inputs);
          end;
        end;
      end;
      FFlashBus.InputsChanged[i]:=false;
    end;
  end;
  //parameters
  for i:=1 to NumDrivers do begin
    if FFlashBus.ParametersChanged[i] then begin
      case i of
        Motor1, Motor2, Motor3: begin
          MotorParametersRaw2Processed(i);
          if Showing then begin
            case i of
              Motor1: MotorParameters2Gui(i, StringGrid_Motor1_Parameters);
              Motor2: MotorParameters2Gui(i, StringGrid_Motor2_Parameters);
              Motor3: MotorParameters2Gui(i, StringGrid_Motor3_Parameters);
            end;
          end;
        end;
        Kicker: begin
          KickerParametersRaw2Processed;
          if Showing then begin
            KickerParameters2Gui(StringGrid_Kicker_Parameters);
          end;
        end;
      end;
      FFlashBus.ParametersChanged[i]:=false;
    end;
  end;

  //Time meassures and Global State
  allread:=true;
  for i:=1 to NumDrivers do begin
    if (FFlashBus.DriverEnable[i]=true) and (FFlashBus.InputsState[i]=WaitingResponse) then begin
      allread:=false;
    end;
  end;
  if allread then begin
    TimingDriversResponse.actual:=GetTickCount;
    deltatime:=TimingDriversResponse.actual-TimingDriversResponse.last;
    if (deltatime)>TimingDriversResponse.max then begin
      TimingDriversResponse.max:=deltatime;
    end;
    GlobalState:=Ok;
    for i:=1 to NumDrivers do begin
      if (FFlashBus.DriverEnable[i]=true) and
         (((FFlashBus.InputsState[i]=FailResponse) or (FFlashBus.InputsState[i]=OverFlowResponse))) then begin
        GlobalState:=DriverDeads;
      end;
    end;
  end;
end;

procedure TForm_Hardware.RadioGroup_Motor2_ModeClick(Sender: TObject);
begin
  MotorOutputsFromGui(Motor2, RadioGroup_Motor2_Mode, ScrollBar_Motor2_Output);
  ScrollBar_Motor2_Output.Position:=0;
end;

procedure TForm_Hardware.RadioGroup_Motor3_ModeClick(Sender: TObject);
begin
  MotorOutputsFromGui(Motor3, RadioGroup_Motor1_Mode, ScrollBar_Motor3_Output);
  ScrollBar_Motor3_Output.Position:=0;
end;

procedure TForm_Hardware.RadioGroup_Roller_ModeClick(Sender: TObject);
begin
  SensorsOutputsFromGui(ToggleBox_Solenoid,ScrollBar_RollerOutput, RadioGroup_Roller_Mode);
end;

procedure TForm_Hardware.ScrollBar_KickerDischargeTimeChange(Sender: TObject);
begin
  Edit_KickerDischargeTime.Text:=IntToStr(round(ScrollBar_KickerDischargeTime.Position/100*KMaxDischargeTime));
end;

procedure TForm_Hardware.ScrollBar_Motor2_OutputChange(Sender: TObject);
begin
  MotorOutputsFromGui(Motor2, RadioGroup_Motor2_Mode, ScrollBar_Motor2_Output);
  case RadioGroup_Motor2_Mode.ItemIndex of
    0: begin
      Edit_Motor2_Output_Value.Text:='--';
    end;
    1: begin
      Edit_Motor2_Output_Value.Text:=IntToStr(ScrollBar_Motor2_Output.Position);
    end;
    2: begin
      Edit_Motor2_Output_Value.Text:=IntToStr(round(double(ScrollBar_Motor2_Output.Position)/100*KMaxVelocity));
    end;
  end;
  if (ScrollBar_Motor2_Output.Position<>0) and (RadioGroup_Motor2_Mode.ItemIndex=0) then begin
    ScrollBar_Motor2_Output.Position:=0;
  end;
end;

procedure TForm_Hardware.ScrollBar_Motor3_OutputChange(Sender: TObject);
begin
  MotorOutputsFromGui(Motor3, RadioGroup_Motor3_Mode, ScrollBar_Motor3_Output);
  case RadioGroup_Motor3_Mode.ItemIndex of
    0: begin
      Edit_Motor3_Output_Value.Text:='--';
    end;
    1: begin
      Edit_Motor3_Output_Value.Text:=IntToStr(ScrollBar_Motor3_Output.Position);
    end;
    2: begin
      Edit_Motor3_Output_Value.Text:=IntToStr(round(double(ScrollBar_Motor3_Output.Position)/100*KMaxVelocity));
    end;
  end;
  if (ScrollBar_Motor3_Output.Position<>0) and (RadioGroup_Motor3_Mode.ItemIndex=0) then begin
    ScrollBar_Motor3_Output.Position:=0;
  end;
end;

procedure TForm_Hardware.ScrollBar_RollerOutputChange(Sender: TObject);
begin
  SensorsOutputsFromGui(ToggleBox_Solenoid, ScrollBar_RollerOutput, RadioGroup_Roller_Mode);
  Edit_RollerOutputbar.Text:=IntToStr(ScrollBar_RollerOutput.Position);
end;

procedure TForm_Hardware.MenuItem_HardwareClick(Sender: TObject);
begin
  Form_Hardware_Configure.Edit_Timer_Interval.Text:=IntToStr(Timer.Interval);
  Form_Hardware_Configure.Edit_Timer_GlobalStatus.Text:=IntToStr(Timer_GlobalStatus.Interval);
  Form_Hardware_Configure.ShowModal;
  try
    Timer.Interval:=StrToInt(Form_Hardware_Configure.Edit_Timer_Interval.Text);
    Timer_GlobalStatus.Interval:=StrToInt(Form_Hardware_Configure.Edit_Timer_GlobalStatus.Text);
  except
    on E: Exception do begin
      showmessage(E.Message);
    end;
  end;
end;

procedure TForm_Hardware.MenuItem_UDPClick(Sender: TObject);
var
  i: integer;
  datain, dataout :string;
  flag: boolean=false;
begin
  with Form_Hardware_Configure_UDP do begin
    Edit_Port.Text:=IntToStr(LUDP.Port);
    datain:=LUDP.Host;
    dataout:='';
    for i:=1 to Length(datain) do begin
      if not flag then begin
        if not (datain[i]=':') then begin
          dataout:=dataout+datain[i];
        end
        else begin
          flag:=true;
          Edit_HostIP.Text:=dataout;
          dataout:='';
        end;
      end
      else begin
        dataout:=dataout+datain[i];
      end;
    end;
    Edit_HostPort.Text:=dataout;
    ShowModal;
    LUDP.Port:=StrToIntDef(Edit_Port.Text,0);
    LUDP.Host:=Edit_HostIP.Text+':'+Edit_HostPort.Text;
    if CheckBox_Connect.Checked then begin
      LUDP.Disconnect(true);
      LUDP.Listen(LUDP.Port);
    end;
  end;
end;

procedure TForm_Hardware.RadioGroup_Motor1_ModeClick(Sender: TObject);
begin
  MotorOutputsFromGui(Motor1, RadioGroup_Motor1_Mode, ScrollBar_Motor1_Output);
  ScrollBar_Motor1_Output.Position:=0;
end;

procedure TForm_Hardware.ScrollBar_Motor1_OutputChange(Sender: TObject);
begin
  MotorOutputsFromGui(Motor1, RadioGroup_Motor1_Mode, ScrollBar_Motor1_Output);
  case RadioGroup_Motor1_Mode.ItemIndex of
    0: begin
      Edit_Motor1_Output_Value.Text:='--';
    end;
    1: begin
      Edit_Motor1_Output_Value.Text:=IntToStr(ScrollBar_Motor1_Output.Position);
    end;
    2: begin
      Edit_Motor1_Output_Value.Text:=IntToStr(round(double(ScrollBar_Motor1_Output.Position)/100*KMaxVelocity));//melhorar 2
    end;
  end;
  if (ScrollBar_Motor1_Output.Position<>0) and (RadioGroup_Motor1_Mode.ItemIndex=0) then begin
    ScrollBar_Motor1_Output.Position:=0;
  end;
end;

procedure TForm_Hardware.Timer_GlobalStatusTimer(Sender: TObject);
var
  auxTime: Dword;
begin
  if Showing then begin
    Edit_Timings.Text:='Sample= '+IntToStr(TimingEventSource.max)+', '+
                       'Response= '+IntToStr(TimingDriversResponse.max);
  end;
  TimingEventSource.max:=0;
  TimingDriversResponse.max:=0;
  //Check for event source (hearquico!)
  auxTime:=GetTickCount;
  if (auxTime-TimingEventSource.actual)>10*Timer.Interval then begin
    GlobalState:=NoSouceEvent;
    TimingEventSource.last:=0;
    TimingEventSource.actual:=0;
    TimingEventSource.max:=0;
  end
  else begin
    if (auxTime-TimingDriversResponse.actual)>10*Timer.Interval then begin
      GlobalState:=NoFlashBus;
      TimingDriversResponse.last:=0;
      TimingDriversResponse.actual:=0;
      TimingDriversResponse.max:=0;
    end;
  end;
  if Showing then begin
    case GlobalState of
      NoSouceEvent: begin
        Edit_GloblaStatus.Text:='No source Event';
      end;
      NoFlashBus: begin
        Edit_GloblaStatus.Text:='No Flash Bus';
      end;
      DriverDeads: begin
        Edit_GloblaStatus.Text:='Drivers Dead';
      end;
      Ok: begin
        Edit_GloblaStatus.Text:='Ok';
      end;
    end;
  end;
end;

procedure TForm_Hardware.ToggleBox_SolenoidChange(Sender: TObject);
begin
  SensorsOutputsFromGui(ToggleBox_Solenoid, ScrollBar_RollerOutput, RadioGroup_Roller_Mode);
end;

procedure TForm_Hardware.Inicialize;
var
  i: integer;
begin
  for i:=1 to 3 do begin
    FMotorProcessed[i].ExistAuxOldSample:=false;
  end;
  FSpecialKickerCom:=SComIDlekicker;
  FSensorsProcessed.CompassOffSet:=NormalizeAngle(FloatSpinEdit_CompassOffset.Value*pi/180);
  FSensorsProcessed.NewCompass:=false;
  FloatSpinEdit_CompassOffset.Value:=FloatSpinEdit_CompassOffset.Value*180/pi;
  SensorsOutputsFromGui(ToggleBox_Solenoid, ScrollBar_RollerOutput, RadioGroup_Roller_Mode);
end;

procedure TForm_Hardware.TimerTimer(Sender: TObject);
begin
  FSend_outputs;
  Frequest_inputs;
end;

procedure TForm_Hardware.FillFlashBusTablesMotorsOutputs;
var
  MotorNum, i: integer;
  auxpointer: PByte;
  auxSizeof: integer;
begin
  for MotorNum:=1 to 3 do begin
    auxpointer:=@FMotorRaw[MotorNum].ControlSource;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].ControlSource);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddOutputsPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].OutputRef;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].OutputRef);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddOutputsPointer(auxpointer+auxSizeof-i);
    end;
  end;
end;

procedure TForm_Hardware.MotorOutputsProcessed2Raw(MotorNum: integer);
begin
  if (MotorNum>=1) and (MotorNum<=3) then begin
    case FMotorProcessed[MotorNum].ControlSource of
      Stop: begin
        FMotorRaw[MotorNum].ControlSource:=0;
        FMotorRaw[MotorNum].OutputRef:=0;
      end;
      PWM: begin
        FMotorRaw[MotorNum].ControlSource:=0;
        FMotorRaw[MotorNum].OutputRef:=byte(round(FMotorProcessed[MotorNum].PWM/KPwmRaw2Processed));
      end;
      PID: begin
        FMotorRaw[MotorNum].ControlSource:=1;
        FMotorRaw[MotorNum].OutputRef:=Smallint(round(FMotorProcessed[MotorNum].AngVelRef/
                                                      KTicks2Rad*(KSamplePeriodRaw2Processed/1000)));
      end;
    end;
  end
  else begin
    raise Exception.Create('Invalide Motor Number');
  end;
end;

procedure TForm_Hardware.MotorOutputsFromGui(MotorNum: integer;
  RadioGroup: TRadioGroup; ScrollBar: TScrollBar);
begin
  if (MotorNum>=1) and (MotorNum<=3) then begin
    case RadioGroup.ItemIndex of
      0: begin
        FMotorProcessed[MotorNum].ControlSource:=Stop;
      end;
      1: begin
        FMotorProcessed[MotorNum].ControlSource:=PWM;
        FMotorProcessed[MotorNum].PWM:=double(ScrollBar.Position);
      end;
      2: begin
        FMotorProcessed[MotorNum].ControlSource:=PID;
        FMotorProcessed[MotorNum].AngVelRef:=double(ScrollBar.Position)/100*KMaxVelocity;
      end;
    end;
  end
  else begin
    raise Exception.Create('Invalide Motor Number');
  end;
end;

procedure TForm_Hardware.FillFlashBusTablesMotorsInputs;
var
  MotorNum, i: integer;
  auxpointer: PByte;
  auxSizeof: integer;
begin
  for MotorNum:=1 to 3 do begin
    auxpointer:=@FMotorRaw[MotorNum].ResetFlag;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].ResetFlag);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddInputPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].EncoderTicks;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].EncoderTicks);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddInputPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].SamplePeriod;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].SamplePeriod);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddInputPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PWMOutA;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PWMOutA);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddInputPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PWMOutB;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PWMOutB);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddInputPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].Current;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].Current);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddInputPointer(auxpointer+auxSizeof-i);
    end;
  end;
end;

procedure TForm_Hardware.MotorInputsRaw2Processed(MotorNum: integer);
begin
  if FFlashBus.InputsState[MotorNum]=Idle then begin
    if FMotorRaw[MotorNum].ResetFlag=1 then begin
      inc(FMotorProcessed[MotorNum].ResetCount);
      FMotorProcessed[MotorNum].ExistAuxOldSample:=false;
    end;

    FMotorProcessed[MotorNum].EncoderTicks:=FMotorRaw[MotorNum].EncoderTicks;
    FMotorProcessed[MotorNum].PWMOutA:=FMotorRaw[MotorNum].PWMOutA*KPwmRaw2Processed;
    FMotorProcessed[MotorNum].PWMOutB:=FMotorRaw[MotorNum].PWMOutB*KPwmRaw2Processed;
    FMotorProcessed[MotorNum].Current:=double(FMotorRaw[MotorNum].Current);

    if FMotorProcessed[MotorNum].ExistAuxOldSample then begin
      FMotorProcessed[MotorNum].DeltaSamplePeriod:=word(FMotorRaw[MotorNum].SamplePeriod-
                                                 FMotorProcessed[MotorNum].AuxOldSamplePeriod)*KSamplePeriodRaw2Processed;//melhorar
      FMotorProcessed[MotorNum].DeltaEncoderTicks:=Smallint(FMotorRaw[MotorNum].EncoderTicks-
                                                 FMotorProcessed[MotorNum].AuxOldEncoderTicks);

      if not CheckBox_Enable_Timer.Checked then begin //no modo manual ñ há integração!
        inc(FMotorProcessed[MotorNum].NewSumDeltaEncoderTicks); //odometry data
        FMotorProcessed[MotorNum].SumDeltaEncoderTicks:=FMotorProcessed[MotorNum].SumDeltaEncoderTicks+
                                                        FMotorProcessed[MotorNum].DeltaEncoderTicks;
      end
      else begin
        FMotorProcessed[MotorNum].NewSumDeltaEncoderTicks:=0;
        FMotorProcessed[MotorNum].SumDeltaEncoderTicks:=0;
      end;

      if FMotorProcessed[MotorNum].DeltaSamplePeriod>0 then begin
        FMotorProcessed[MotorNum].AngVel:=(FMotorProcessed[MotorNum].DeltaEncoderTicks*KTicks2Rad)/
                                           FMotorProcessed[MotorNum].DeltaSamplePeriod*1000;//sample period in ms}
      end;
    end
    else begin
      FMotorProcessed[MotorNum].NewSumDeltaEncoderTicks:=0;
      FMotorProcessed[MotorNum].SumDeltaEncoderTicks:=0;
      FMotorProcessed[MotorNum].ExistAuxOldSample:=True;
    end;
    FMotorProcessed[MotorNum].AuxOldSamplePeriod:=FMotorRaw[MotorNum].SamplePeriod;
    FMotorProcessed[MotorNum].AuxOldEncoderTicks:=FMotorRaw[MotorNum].EncoderTicks;
  end
  else begin
    if FFlashBus.InputsState[MotorNum]=FailResponse then begin
      //FMotorProcessed[MotorNum].ExistAuxOldSample:=false; //segurança para o caso de alguma msg de reset tenha falhado
    end;
  end;
end;

procedure TForm_Hardware.MotorInputsRaw2Processed2(MotorNum: integer);
begin
  if FFlashBus.InputsState[MotorNum]=Idle then begin
    if FMotorRaw[MotorNum].ResetFlag=1 then begin
      inc(FMotorProcessed[MotorNum].ResetCount);
      FMotorProcessed[MotorNum].ExistAuxOldSample:=false;
    end;

    FMotorProcessed[MotorNum].EncoderTicks:=FMotorRaw[MotorNum].EncoderTicks;
    FMotorProcessed[MotorNum].PWMOutA:=FMotorRaw[MotorNum].PWMOutA*KPwmRaw2Processed;
    FMotorProcessed[MotorNum].PWMOutB:=FMotorRaw[MotorNum].PWMOutB*KPwmRaw2Processed;
    FMotorProcessed[MotorNum].Current:=double(FMotorRaw[MotorNum].Current);

    if FMotorProcessed[MotorNum].ExistAuxOldSample then begin
      FMotorProcessed[MotorNum].DeltaSamplePeriod:=word(FMotorRaw[MotorNum].SamplePeriod-
                                                 FMotorProcessed[MotorNum].AuxOldSamplePeriod)*KSamplePeriodRaw2Processed;//melhorar
      FMotorProcessed[MotorNum].DeltaEncoderTicks:=Smallint(FMotorRaw[MotorNum].EncoderTicks-
                                                 FMotorProcessed[MotorNum].AuxOldEncoderTicks);

      if FMotorProcessed[MotorNum].DeltaSamplePeriod>0 then begin
        FMotorProcessed[MotorNum].AngVel:=(FMotorProcessed[MotorNum].DeltaEncoderTicks*KTicks2Rad)/
                                           FMotorProcessed[MotorNum].DeltaSamplePeriod*1000;//sample period in ms}
      end;
    end
    else begin
      FMotorProcessed[MotorNum].ExistAuxOldSample:=True;
    end;
    FMotorProcessed[MotorNum].AuxOldSamplePeriod:=FMotorRaw[MotorNum].SamplePeriod;
    FMotorProcessed[MotorNum].AuxOldEncoderTicks:=FMotorRaw[MotorNum].EncoderTicks;
  end;
end;

procedure TForm_Hardware.MotorInputs2Gui(MotorNum: integer; StringGrid: TStringGrid);
begin
  if (MotorNum>=1) and (MotorNum<=3) then begin
    StringGrid.Cells[1,1]:=IntToStr(FMotorProcessed[MotorNum].ResetCount);
    StringGrid.Cells[1,2]:=IntToStr(FMotorProcessed[MotorNum].EncoderTicks);
    StringGrid.Cells[1,3]:=IntToStr(FMotorProcessed[MotorNum].DeltaEncoderTicks);
    StringGrid.Cells[1,4]:=IntToStr(FMotorProcessed[MotorNum].DeltaSamplePeriod);
    StringGrid.Cells[1,5]:=Format('%.1f',[FMotorProcessed[MotorNum].PWMOutA]);
    StringGrid.Cells[1,6]:=Format('%.1f',[FMotorProcessed[MotorNum].PWMOutB]);
    StringGrid.Cells[1,7]:=Format('%.1f',[FMotorProcessed[MotorNum].AngVel]);
    StringGrid.Cells[1,8]:=Format('%.1f',[FMotorProcessed[MotorNum].Current]);
  end
  else begin
    raise Exception.Create('Invalide Motor Number');
  end;
end;

procedure TForm_Hardware.CheckBox_ConnectChange(Sender: TObject);
begin
  if CheckBox_Connect.Checked then begin
    Inicialize;
    LUDP.Listen(LUDP.Port);
  end
  else begin
    LUDP.Disconnect(true);
  end;
end;

procedure TForm_Hardware.CheckBox_Driver_EnableChange(Sender: TObject);
begin
  Gui2FlashBus;
end;

procedure TForm_Hardware.CheckBox_Enable_TimerChange(Sender: TObject);
begin
  if CheckBox_Enable_Timer.Checked then begin
    RadioGroup_Motor1_Mode.ItemIndex:=0;
    RadioGroup_Motor2_Mode.ItemIndex:=0;
    RadioGroup_Motor3_Mode.ItemIndex:=0;
    RadioGroup_Motor1_ModeClick(Sender);
    RadioGroup_Motor2_ModeClick(Sender);
    RadioGroup_Motor3_ModeClick(Sender);
    Timer.Enabled:=true;
  end
  else begin
    Timer.Enabled:=false;
  end;
end;

procedure TForm_Hardware.Button_Motor1_Parameters_ReadClick(Sender: TObject);
var
  i: integer;
begin
  for i:=1 to StringGrid_Motor1_Parameters.RowCount-1 do begin
    StringGrid_Motor1_Parameters.Cells[1,i]:='waiting';
  end;
  LUDP.SendMessage(FFlashBus.RequestParameters(Motor1), LUDP.Host);
end;

procedure TForm_Hardware.Button_Motor1_StopClick(Sender: TObject);
begin
  ScrollBar_Motor1_Output.Position:=0;
end;

procedure TForm_Hardware.Button_Motor1_Parameters_WriteClick(Sender: TObject);
var
  data2send: string;
begin
  MotorParametersFromGui(Motor1,StringGrid_Motor1_Parameters);
  MotorParametersProcessed2Raw(Motor1);
  data2send:=FFlashBus.WriteParameters(Motor1);
  LUDP.SendMessage(data2send, LUDP.Host);
  Button_Motor1_Parameters_ReadClick(Sender);
end;

procedure TForm_Hardware.Button_Motor2_Parameters_ReadClick(Sender: TObject);
var
  i: integer;
begin
  for i:=1 to StringGrid_Motor2_Parameters.RowCount-1 do begin
    StringGrid_Motor2_Parameters.Cells[1,i]:='waiting';
  end;
  LUDP.SendMessage(FFlashBus.RequestParameters(Motor2), LUDP.Host);
end;

procedure TForm_Hardware.Button_Motor2_Parameters_WriteClick(Sender: TObject);
var
  data2send: string;
begin
  MotorParametersFromGui(Motor2,StringGrid_Motor2_Parameters);
  MotorParametersProcessed2Raw(Motor2);
  data2send:=FFlashBus.WriteParameters(Motor2);
  LUDP.SendMessage(data2send, LUDP.Host);
  Button_Motor2_Parameters_ReadClick(Sender);
end;

procedure TForm_Hardware.Button_Motor3_Parameters_ReadClick(Sender: TObject);
var
  i: integer;
begin
  for i:=1 to StringGrid_Motor3_Parameters.RowCount-1 do begin
    StringGrid_Motor3_Parameters.Cells[1,i]:='waiting';
  end;
  LUDP.SendMessage(FFlashBus.RequestParameters(Motor3), LUDP.Host);
end;

procedure TForm_Hardware.Button_Motor3_Parameters_WriteClick(Sender: TObject);
var
  data2send: string;
begin
  MotorParametersFromGui(Motor3,StringGrid_Motor3_Parameters);
  MotorParametersProcessed2Raw(Motor3);
  data2send:=FFlashBus.WriteParameters(Motor3);
  LUDP.SendMessage(data2send, LUDP.Host);
  Button_Motor3_Parameters_ReadClick(Sender);
end;

procedure TForm_Hardware.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CheckBox_Enable_Timer.Checked:=false;
end;

procedure TForm_Hardware.FillFlashBusTablesMotorsParameters; //Google it "Big or Little Endian", cuidado com os indios
var
  MotorNum, i: integer;
  auxpointer: PByte;
  auxSizeof: integer;
begin
  for MotorNum:=1 to 3 do begin
    auxpointer:=@FMotorRaw[MotorNum].MaxCurrent;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].MaxCurrent);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddParametersPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PidPeriod;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PidPeriod);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddParametersPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PwmMaxSlope;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PwmMaxSlope);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddParametersPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PID_FILT_EV;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PID_FILT_EV);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddParametersPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PID_K;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PID_K);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddParametersPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PID_INVTI;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PID_INVTI);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddParametersPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PID_ALPHA;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PID_ALPHA);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddParametersPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PID_TD;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PID_TD);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddParametersPointer(auxpointer+auxSizeof-i);
    end;
    auxpointer:=@FMotorRaw[MotorNum].PID_FILT_REF;
    auxSizeof:=SizeOf(FMotorRaw[MotorNum].PID_FILT_EV);
    for i:=1 to auxSizeof do begin
      FFlashBus.Table[MotorNum].AddParametersPointer(auxpointer+auxSizeof-i);
    end;
  end;
end;

procedure TForm_Hardware.SaveParameters;
var
  i: integer;
begin
  for i:=1 to StringGrid_Motor1_Parameters.RowCount-1 do begin
    IniPropStorage.WriteString('Motor1'+StringGrid_Motor1_Parameters.Cells[0,i],
                               StringGrid_Motor1_Parameters.Cells[2,i]);
  end;
  for i:=1 to StringGrid_Motor2_Parameters.RowCount-1 do begin
    IniPropStorage.WriteString('Motor2'+StringGrid_Motor2_Parameters.Cells[0,i],
                               StringGrid_Motor2_Parameters.Cells[2,i]);
  end;
  for i:=1 to StringGrid_Motor3_Parameters.RowCount-1 do begin
    IniPropStorage.WriteString('Motor3'+StringGrid_Motor3_Parameters.Cells[0,i],
                               StringGrid_Motor3_Parameters.Cells[2,i]);
  end;
  for i:=1 to StringGrid_Kicker_Parameters.RowCount-1 do begin
    IniPropStorage.WriteString('Kicker'+StringGrid_Kicker_Parameters.Cells[0,i],
                               StringGrid_Kicker_Parameters.Cells[2,i]);
  end;
end;

procedure TForm_Hardware.LoadParameters;
var
  i: integer;
begin
  for i:=1 to StringGrid_Motor1_Parameters.RowCount-1 do begin
    StringGrid_Motor1_Parameters.Cells[2,i]:=IniPropStorage.ReadString('Motor1'+
                                    StringGrid_Motor1_Parameters.Cells[0,i],'');
  end;
  for i:=1 to StringGrid_Motor2_Parameters.RowCount-1 do begin
    StringGrid_Motor2_Parameters.Cells[2,i]:=IniPropStorage.ReadString('Motor2'+
                                    StringGrid_Motor2_Parameters.Cells[0,i],'');
  end;
  for i:=1 to StringGrid_Motor3_Parameters.RowCount-1 do begin
    StringGrid_Motor3_Parameters.Cells[2,i]:=IniPropStorage.ReadString('Motor3'+
                                    StringGrid_Motor3_Parameters.Cells[0,i],'');
  end;
  for i:=1 to StringGrid_Kicker_Parameters.RowCount-1 do begin
    StringGrid_Kicker_Parameters.Cells[2,i]:=IniPropStorage.ReadString('Kicker'+
                                    StringGrid_Kicker_Parameters.Cells[0,i],'');
  end;
end;

procedure TForm_Hardware.MotorParametersRaw2Processed(MotorNum: integer);
begin
  if (MotorNum>=1) and (MotorNum<=3) then begin
    FMotorProcessed[MotorNum].MaxCurrent:=double(FMotorRaw[MotorNum].MaxCurrent)*KMaxCurrentRaw2Processed;
    FMotorProcessed[MotorNum].PidPeriod:=FMotorRaw[MotorNum].PidPeriod*KPidPeriodRaw2Processed;
    {FMotorProcessed[MotorNum].PwmMaxSlope:=double(FMotorRaw[MotorNum].PwmMaxSlope)*KPwmRaw2Processed/
                                           double(FMotorProcessed[MotorNum].PidPeriod);}
    FMotorProcessed[MotorNum].PwmMaxSlope:=double(FMotorRaw[MotorNum].PwmMaxSlope);
    FMotorProcessed[MotorNum].PID_FILT_EV:=FMotorRaw[MotorNum].PID_FILT_EV;
    FMotorProcessed[MotorNum].PID_K:=FMotorRaw[MotorNum].PID_K;
    FMotorProcessed[MotorNum].PID_INVTI:=FMotorRaw[MotorNum].PID_INVTI;
    FMotorProcessed[MotorNum].PID_ALPHA:=FMotorRaw[MotorNum].PID_ALPHA;
    FMotorProcessed[MotorNum].PID_TD:=FMotorRaw[MotorNum].PID_TD;
    FMotorProcessed[MotorNum].PID_FILT_REF:=FMotorRaw[MotorNum].PID_FILT_REF;
  end
  else begin
    raise Exception.Create('Invalide Motor Number');
  end;
end;

procedure TForm_Hardware.MotorParametersProcessed2Raw(MotorNum: integer);
begin
  if (MotorNum>=1) and (MotorNum<=3) then begin
    FMotorRaw[MotorNum].MaxCurrent:=byte(round(FMotorProcessed[MotorNum].MaxCurrent/KMaxCurrentRaw2Processed));
    FMotorRaw[MotorNum].PidPeriod:=byte(round(FMotorProcessed[MotorNum].PidPeriod/KPidPeriodRaw2Processed));
    {FMotorRaw[MotorNum].PwmMaxSlope:=byte(round(FMotorProcessed[MotorNum].PwmMaxSlope/KPwmRaw2Processed*
                                                             double(FMotorProcessed[MotorNum].PidPeriod)));}
    FMotorRaw[MotorNum].PwmMaxSlope:=byte(round(FMotorProcessed[MotorNum].PwmMaxSlope));
    FMotorRaw[MotorNum].PID_FILT_EV:=FMotorProcessed[MotorNum].PID_FILT_EV;
    FMotorRaw[MotorNum].PID_K:=FMotorProcessed[MotorNum].PID_K;
    FMotorRaw[MotorNum].PID_INVTI:=FMotorProcessed[MotorNum].PID_INVTI;
    FMotorRaw[MotorNum].PID_ALPHA:=FMotorProcessed[MotorNum].PID_ALPHA;
    FMotorRaw[MotorNum].PID_TD:=FMotorProcessed[MotorNum].PID_TD;
    FMotorRaw[MotorNum].PID_FILT_REF:=FMotorProcessed[MotorNum].PID_FILT_REF;
  end
  else begin
    raise Exception.Create('Invalide Motor Number');
  end;
end;

procedure TForm_Hardware.MotorParameters2Gui(MotorNum: integer;
  StringGrid: TStringGrid);
var
  i: integer;
begin
  if (MotorNum>=1) and (MotorNum<=3) then begin
    case FFlashBus.ParametersState[MotorNum] of
      Idle: begin
        StringGrid.Cells[1,1]:=Format('%.1f', [FMotorProcessed[MotorNum].MaxCurrent]);
        StringGrid.Cells[1,2]:=IntToStr(FMotorProcessed[MotorNum].PidPeriod);
        StringGrid.Cells[1,3]:=Format('%.2f', [FMotorProcessed[MotorNum].PwmMaxSlope]);
        StringGrid.Cells[1,4]:=Format('%.4f', [FMotorProcessed[MotorNum].PID_FILT_EV]);
        StringGrid.Cells[1,5]:=Format('%.4f', [FMotorProcessed[MotorNum].PID_K]);
        StringGrid.Cells[1,6]:=Format('%.4f', [FMotorProcessed[MotorNum].PID_INVTI]);
        StringGrid.Cells[1,7]:=Format('%.4f', [FMotorProcessed[MotorNum].PID_ALPHA]);
        StringGrid.Cells[1,8]:=Format('%.4f', [FMotorProcessed[MotorNum].PID_TD]);
        StringGrid.Cells[1,9]:=Format('%.4f', [FMotorProcessed[MotorNum].PID_FILT_REF]);
      end;
      FailResponse: begin
        for i:=1 to StringGrid.RowCount-1 do begin
          StringGrid.Cells[1,i]:='Fail';
        end;
      end;
      OverFlowResponse: begin
        for i:=1 to StringGrid.RowCount-1 do begin
          StringGrid.Cells[1,i]:='Try again';
        end;
      end;
    end;
  end
  else begin
    raise Exception.Create('Invalide Motor Number');
  end;
end;

procedure TForm_Hardware.MotorParametersFromGui(MotorNum: integer; StringGrid: TStringGrid);
const
  GridColIndex=2;
var
  GridLineIndex: integer=1;
  auxInt: integer;
  auxFloat: double;
begin
  try
    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FMotorProcessed[MotorNum].MaxCurrent:=auxFloat;
    Inc(GridLineIndex);
    auxInt:=StrToInt(StringGrid_Motor1_Parameters.Cells[GridColIndex,GridLineIndex]);
    FMotorProcessed[MotorNum].PidPeriod:=auxInt;
    Inc(GridLineIndex);
    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FMotorProcessed[MotorNum].PwmMaxSlope:=auxFloat;
    Inc(GridLineIndex);
    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FMotorProcessed[MotorNum].PID_FILT_EV:=auxFloat;
    Inc(GridLineIndex);
    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FMotorProcessed[MotorNum].PID_K:=auxFloat;
    Inc(GridLineIndex);
    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FMotorProcessed[MotorNum].PID_INVTI:=auxFloat;
    Inc(GridLineIndex);
    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FMotorProcessed[MotorNum].PID_ALPHA:=auxFloat;
    Inc(GridLineIndex);
    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FMotorProcessed[MotorNum].PID_TD:=auxFloat;
    Inc(GridLineIndex);
    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FMotorProcessed[MotorNum].PID_FILT_REF:=auxFloat;
  except
    ShowMessage('Invalid '+StringGrid.Cells[0,GridLineIndex]+' Value');
  end;
end;

procedure TForm_Hardware.FillFlashBusTablesKickerOutputs;
var
  i: integer;
  auxpointer: PByte;
  auxSizeof: integer;
begin
  auxpointer:=@FKickerRaw.Kick;
  auxSizeof:=SizeOf(FKickerRaw.Kick);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddOutputsPointer(auxpointer+i-1);
  end;
end;

procedure TForm_Hardware.KickerOutputsProcessed2Raw;
begin
  if FKickerProcessed.DischargeTime>KMaxDischargeTime then begin
    FKickerProcessed.DischargeTime:=KMaxDischargeTime;
  end;
  FKickerRaw.Kick:=FKickerProcessed.DischargeTime;
  if FKickerProcessed.Kick then begin
    FKickerRaw.Kick:=(1 shl 7) or FKickerRaw.Kick;
  end;
  FKickerProcessed.Kick:=false;
end;

procedure TForm_Hardware.KickerOutputsFromGui(ScrollBar: TScrollBar);
begin
  FKickerProcessed.DischargeTime:=round(ScrollBar.Position/100*KMaxDischargeTime);
  FKickerProcessed.Kick:=true;
end;

procedure TForm_Hardware.FillFlashBusTablesKickerInputs;
var
  i: integer;
  auxpointer: PByte;
  auxSizeof: integer;
begin
  auxpointer:=@FKickerRaw.Vbat;
  auxSizeof:=SizeOf(FKickerRaw.Vbat);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddInputPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.Vcap;
  auxSizeof:=SizeOf(FKickerRaw.Vcap);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddInputPointer(auxpointer+i-1);
  end;
end;

procedure TForm_Hardware.KickerInputsRaw2Processed;
begin
  if FFlashBus.InputsState[Kicker]=Idle then begin
    FKickerProcessed.Vbat:=FKickerProcessed.VbatGain*double(FKickerRaw.Vbat)+FKickerProcessed.VbatOffset;
    FKickerProcessed.Vcap:=FKickerProcessed.VcapGain*double(FKickerRaw.Vcap)+FKickerProcessed.VcapOffset;
  end
  else begin
    ;
  end;
end;

procedure TForm_Hardware.KickerInputs2Gui(StringGrid: TStringGrid);
begin
  StringGrid.Cells[1,1]:=Format('%.1f',[FKickerProcessed.Vbat]);
  StringGrid.Cells[1,2]:=Format('%.1f',[FKickerProcessed.Vcap]);
end;

procedure TForm_Hardware.FillFlashBusTablesKickerParameters;
var
  i: integer;
  auxpointer: PByte;
  auxSizeof: integer;
begin
  auxpointer:=@FKickerRaw.Vcap_Target;
  auxSizeof:=SizeOf(FKickerRaw.Vcap_Target);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.Vcap_Lvl2;
  auxSizeof:=SizeOf(FKickerRaw.Vcap_Lvl2);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.Vcap_Lvl1;
  auxSizeof:=SizeOf(FKickerRaw.Vcap_Lvl1);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.Vcap_Critical;
  auxSizeof:=SizeOf(FKickerRaw.Vcap_Critical);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.Cur_PWM;
  auxSizeof:=SizeOf(FKickerRaw.Cur_PWM);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.PWM_Target;
  auxSizeof:=SizeOf(FKickerRaw.PWM_Target);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.PWM_Lvl2;
  auxSizeof:=SizeOf(FKickerRaw.PWM_Lvl2);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.PWM_Lvl1;
  auxSizeof:=SizeOf(FKickerRaw.PWM_Lvl1);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.PWM_Limit;
  auxSizeof:=SizeOf(FKickerRaw.PWM_Limit);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.VtoF_SamplePer;
  auxSizeof:=SizeOf(FKickerRaw.VtoF_SamplePer);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.dummy;
  auxSizeof:=SizeOf(FKickerRaw.dummy);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.MaxDischargeTime;
  auxSizeof:=SizeOf(FKickerRaw.MaxDischargeTime);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
  auxpointer:=@FKickerRaw.Auto_Charge_OnOFF;
  auxSizeof:=SizeOf(FKickerRaw.Auto_Charge_OnOFF);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Kicker].AddParametersPointer(auxpointer+i-1);
  end;
end;

procedure TForm_Hardware.KickerParametersRaw2Processed;
begin
  FKickerProcessed.Vcap_Target:=FKickerProcessed.VcapGain*FKickerRaw.Vcap_Target+FKickerProcessed.VcapOffset;
  FKickerProcessed.Vcap_Lvl2:=FKickerProcessed.VcapGain*FKickerRaw.Vcap_Lvl2+FKickerProcessed.VcapOffset;
  FKickerProcessed.Vcap_Lvl1:=FKickerProcessed.VcapGain*FKickerRaw.Vcap_Lvl1+FKickerProcessed.VcapOffset;
  FKickerProcessed.Vcap_Critical:=FKickerProcessed.VcapGain*FKickerRaw.Vcap_Critical+FKickerProcessed.VcapOffset;
  FKickerProcessed.Cur_PWM:=FKickerRaw.Cur_PWM;
  FKickerProcessed.PWM_Target:=FKickerRaw.PWM_Target;
  FKickerProcessed.PWM_Lvl2:=FKickerRaw.PWM_Lvl2;
  FKickerProcessed.PWM_Lvl1:=FKickerRaw.PWM_Lvl1;
  FKickerProcessed.PWM_Limit:=FKickerRaw.PWM_Limit;
  FKickerProcessed.VtoF_SamplePer:=FKickerRaw.VtoF_SamplePer;
  FKickerProcessed.dummy:=FKickerRaw.dummy;
  FKickerProcessed.MaxDischargeTime:=FKickerRaw.MaxDischargeTime;
  FKickerProcessed.Auto_Charge_OnOFF:=FKickerRaw.Auto_Charge_OnOFF;
end;

procedure TForm_Hardware.KickerParametersProcessed2Raw;
const
  VbatGain=0.0090193271; //temp!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  VbatOffset=1.1;
  VcapGain=0.0524;
  VcapOffset=0.3659;
begin
  FKickerRaw.Vcap_Target:=word(round((FKickerProcessed.Vcap_Target-VcapOffset) / VcapGain));
  FKickerRaw.Vcap_Lvl2:=word(round((FKickerProcessed.Vcap_Lvl2-VcapOffset) / VcapGain));
  FKickerRaw.Vcap_Lvl1:=word(round((FKickerProcessed.Vcap_Lvl1-VcapOffset) / VcapGain));
  FKickerRaw.Vcap_Critical:=word(round((FKickerProcessed.Vcap_Critical-VcapOffset) / VcapGain));
  FKickerRaw.Cur_PWM:=FKickerProcessed.Cur_PWM;
  FKickerRaw.PWM_Target:=FKickerProcessed.PWM_Target;
  FKickerRaw.PWM_Lvl2:=FKickerProcessed.PWM_Lvl2;
  FKickerRaw.PWM_Lvl1:=FKickerProcessed.PWM_Lvl1;
  FKickerRaw.PWM_Limit:=FKickerProcessed.PWM_Limit;
  FKickerRaw.VtoF_SamplePer:=FKickerProcessed.VtoF_SamplePer;
  FKickerRaw.dummy:=FKickerProcessed.dummy;
  FKickerRaw.MaxDischargeTime:=FKickerProcessed.MaxDischargeTime;
  FKickerRaw.Auto_Charge_OnOFF:=FKickerProcessed.Auto_Charge_OnOFF;
end;

procedure TForm_Hardware.KickerParameters2Gui(StringGrid: TStringGrid);
var
  i: integer;
begin
  case FFlashBus.ParametersState[Kicker] of
    Idle: begin
      StringGrid.Cells[1,1]:=Format('%.1f', [FKickerProcessed.Vcap_Target]);
      StringGrid.Cells[1,2]:=Format('%.1f', [FKickerProcessed.Vcap_Lvl2]);
      StringGrid.Cells[1,3]:=Format('%.1f', [FKickerProcessed.Vcap_Lvl1]);
      StringGrid.Cells[1,4]:=Format('%.1f', [FKickerProcessed.Vcap_Critical]);
      StringGrid.Cells[1,5]:=IntToStr(FKickerProcessed.Cur_PWM);
      StringGrid.Cells[1,6]:=IntToStr(FKickerProcessed.PWM_Target);
      StringGrid.Cells[1,7]:=IntToStr(FKickerProcessed.PWM_Lvl2);
      StringGrid.Cells[1,8]:=IntToStr(FKickerProcessed.PWM_Lvl1);
      StringGrid.Cells[1,9]:=IntToStr(FKickerProcessed.PWM_Limit);
      StringGrid.Cells[1,10]:=IntToStr(FKickerProcessed.VtoF_SamplePer);
      StringGrid.Cells[1,11]:=IntToStr(FKickerProcessed.dummy);
      StringGrid.Cells[1,12]:=IntToStr(FKickerProcessed.MaxDischargeTime);
      StringGrid.Cells[1,13]:=IntToStr(FKickerProcessed.Auto_Charge_OnOFF);
    end;
    FailResponse: begin
      for i:=1 to StringGrid.RowCount-1 do begin
        StringGrid.Cells[1,i]:='Fail';
      end;
    end;
    OverFlowResponse: begin
      for i:=1 to StringGrid.RowCount-1 do begin
        StringGrid.Cells[1,i]:='Try again';
      end;
    end;
  end;
end;

procedure TForm_Hardware.KickerParametersFromGui(StringGrid: TStringGrid);
const
  GridColIndex=2;
var
  GridLineIndex: integer=1;
  auxInt: integer;
  auxFloat: double;
begin
  try
    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.Vcap_Target:=auxFloat;
    Inc(GridLineIndex);

    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.Vcap_Lvl2:=auxFloat;
    Inc(GridLineIndex);

    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.Vcap_Lvl1:=auxFloat;
    Inc(GridLineIndex);

    auxFloat:=StrToFloat(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.Vcap_Critical:=auxFloat;
    Inc(GridLineIndex);

    auxInt:=StrToInt(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.Cur_PWM:=auxInt;
    Inc(GridLineIndex);

    auxInt:=StrToInt(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.PWM_Target:=auxInt;
    Inc(GridLineIndex);

    auxInt:=StrToInt(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.PWM_Lvl2:=auxInt;
    Inc(GridLineIndex);

    auxInt:=StrToInt(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.PWM_Lvl1:=auxInt;
    Inc(GridLineIndex);

    auxInt:=StrToInt(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.PWM_Limit:=auxInt;
    Inc(GridLineIndex);

    auxInt:=StrToInt(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.VtoF_SamplePer:=auxInt;
    Inc(GridLineIndex);

    auxInt:=StrToInt(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.dummy:=auxInt;
    Inc(GridLineIndex);

    auxInt:=StrToInt(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.MaxDischargeTime:=auxInt;
    Inc(GridLineIndex);

    auxInt:=StrToInt(StringGrid.Cells[GridColIndex,GridLineIndex]);
    FKickerProcessed.Auto_Charge_OnOFF:=auxInt;
  except
    ShowMessage('Invalid '+StringGrid.Cells[0,GridLineIndex]+' Value');
  end;
end;

procedure TForm_Hardware.FillFlashBusTablesSensorsOutputs;
var
  i: integer;
  auxpointer: PByte;
  auxSizeof: integer;
begin
  auxpointer:=@FSensorsRaw.Solenoid;
  auxSizeof:=SizeOf(FSensorsRaw.Solenoid);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Sensors].AddOutputsPointer(auxpointer+i-1);
  end;

  auxpointer:=@FSensorsRaw.RollerPWM;
  auxSizeof:=SizeOf(FSensorsRaw.RollerPWM);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Sensors].AddOutputsPointer(auxpointer+i-1);
  end;

  auxpointer:=@FSensorsRaw.RollerSpeedRef;
  auxSizeof:=SizeOf(FSensorsRaw.RollerSpeedRef);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Sensors].AddOutputsPointer(auxpointer+i-1);
  end;

  auxpointer:=@FSensorsRaw.RollerMode;
  auxSizeof:=SizeOf(FSensorsRaw.RollerMode);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Sensors].AddOutputsPointer(auxpointer+i-1);
  end;

  auxpointer:=@FSensorsRaw.RollerDir;
  auxSizeof:=SizeOf(FSensorsRaw.RollerDir);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Sensors].AddOutputsPointer(auxpointer+i-1);
  end;
end;

procedure TForm_Hardware.SensorsOutputsProcessed2Raw;
begin
  if FSensorsProcessed.Solenoid then begin
    FSensorsRaw.Solenoid:=$FF;
  end
  else begin
    FSensorsRaw.Solenoid:=0;
  end;
  FSensorsRaw.RollerPWM:=byte(round(abs(FSensorsProcessed.RollerPWM)/KRollerPwmRaw2Processed));
  FSensorsRaw.RollerSpeedRef:=byte(round(abs(FSensorsProcessed.RollerSpeedRef)/KRollerPwmRaw2Processed));
  case FSensorsProcessed.RollerMode of
    Stop: FSensorsRaw.RollerMode:=0;
    PWM: begin
      FSensorsRaw.RollerMode:=1;
      if FSensorsProcessed.RollerPWM<0 then begin
        FSensorsRaw.RollerDir:=1;
      end
      else begin
        FSensorsRaw.RollerDir:=0;
      end;
    end;
    PID: begin
      FSensorsRaw.RollerMode:=2;
      if FSensorsProcessed.RollerSpeedRef<0 then begin
        FSensorsRaw.RollerDir:=1;
      end
      else begin
        FSensorsRaw.RollerDir:=0;
      end;
    end;
  end;
end;

procedure TForm_Hardware.SensorsOutputsFromGui(ToggleBox: TToggleBox; ScrollBar: TScrollBar; RadioGroup: TRadioGroup);
begin
  FSensorsProcessed.Solenoid:=ToggleBox.Checked;
  case RadioGroup.ItemIndex of
    0: FSensorsProcessed.RollerMode:=Stop;
    1: FSensorsProcessed.RollerMode:=PWM;
    2: FSensorsProcessed.RollerMode:=PID;
  end;
  FSensorsProcessed.RollerPWM:=ScrollBar.Position;
  FSensorsProcessed.RollerSpeedRef:=ScrollBar.Position;
end;

procedure TForm_Hardware.FillFlashBusTablesSensorsInputs;
var
  i: integer;
  auxpointer: PByte;
  auxSizeof: integer;
begin
  auxpointer:=@FSensorsRaw.SolenoidTime;
  auxSizeof:=SizeOf(FSensorsRaw.SolenoidTime);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Sensors].AddInputPointer(auxpointer+i-1);
  end;

  auxpointer:=@FSensorsRaw.Compass;
  auxSizeof:=SizeOf(FSensorsRaw.Compass);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Sensors].AddInputPointer(auxpointer+i-1);
  end;

  auxpointer:=@FSensorsRaw.BallSensor;
  auxSizeof:=SizeOf(FSensorsRaw.BallSensor);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Sensors].AddInputPointer(auxpointer+i-1);
  end;

  auxpointer:=@FSensorsRaw.RollerSpeed;
  auxSizeof:=SizeOf(FSensorsRaw.RollerSpeed);
  for i:=1 to auxSizeof do begin
    FFlashBus.Table[Sensors].AddInputPointer(auxpointer+i-1);
  end;
end;

procedure TForm_Hardware.SensorsInputsRaw2Processed;
begin
  if FFlashBus.InputsState[Sensors]=Idle then begin
    FSensorsProcessed.SolenoidTime:=FSensorsRaw.SolenoidTime;
    FSensorsProcessed.Compass:=NormalizeAngle(double(FSensorsRaw.Compass)*KCompassRaw2Processed-FSensorsProcessed.CompassOffSet);
    FSensorsProcessed.NewCompass:=true;
    if FSensorsRaw.BallSensor=0 then begin
      FSensorsProcessed.BallSensor:=false;
    end
    else begin
      FSensorsProcessed.BallSensor:=true;
    end;
    FSensorsProcessed.RollerSpeed:=double(FSensorsRaw.RollerSpeed);
  end;
end;

procedure TForm_Hardware.SensorsInputs2Gui(StringGrid: TStringGrid);
begin
  StringGrid.Cells[1,1]:=IntToStr(FSensorsProcessed.SolenoidTime);
  StringGrid.Cells[1,2]:=Format('%.1f',[FSensorsProcessed.Compass*180/pi]);
  if FSensorsProcessed.BallSensor then begin
    StringGrid.Cells[1,3]:='True';
  end
  else begin
    StringGrid.Cells[1,3]:='False';
  end;
  StringGrid.Cells[1,4]:=Format('%.1f',[FSensorsProcessed.RollerSpeed]);
end;

procedure TForm_Hardware.FlashBus2Gui;
begin
  //Motor1
  if FFlashBus.DriverEnable[Motor1] then begin
    case FFlashBus.InputsState[Motor1] of
      Idle: begin
        Edit_Motor1_Status.Text:='Alive';
      end;
      FailResponse: begin
        Edit_Motor1_Status.Text:='Dead';
      end;
    end;
  end
  else begin
    Edit_Motor1_Status.Text:='Disable';
  end;
  //Motor2
  if FFlashBus.DriverEnable[Motor2] then begin
    case FFlashBus.InputsState[Motor2] of
      Idle: begin
        Edit_Motor2_Status.Text:='Alive';
      end;
      FailResponse: begin
        Edit_Motor2_Status.Text:='Dead';
      end;
    end;
  end
  else begin
    Edit_Motor2_Status.Text:='Disable';
  end;
  //Motor3
  if FFlashBus.DriverEnable[Motor3] then begin
    case FFlashBus.InputsState[Motor3] of
      Idle: begin
        Edit_Motor3_Status.Text:='Alive';
      end;
      FailResponse: begin
        Edit_Motor3_Status.Text:='Dead';
      end;
    end;
  end
  else begin
    Edit_Motor3_Status.Text:='Disable';
  end;
  //Kicker
  if FFlashBus.DriverEnable[Kicker] then begin
    case FFlashBus.InputsState[Kicker] of
      Idle: begin
        Edit_Kicker_Status.Text:='Alive';
      end;
      FailResponse: begin
        Edit_Kicker_Status.Text:='Dead';
      end;
    end;
  end
  else begin
    Edit_Kicker_Status.Text:='Disable';
  end;
  //Sensors
  if FFlashBus.DriverEnable[Sensors] then begin
    case FFlashBus.InputsState[Sensors] of
      Idle: begin
        Edit_Sensors_Status.Text:='Alive';
      end;
      FailResponse: begin
        Edit_Sensors_Status.Text:='Dead';
      end;
    end;
  end
  else begin
    Edit_Sensors_Status.Text:='Disable';
  end;
end;

procedure TForm_Hardware.FSpecialKickerStateMachine;
begin
  case FSpecialKickerState of
    SKickerIdle: begin
      case FSpecialKickerCom of
        SComHighkicker: begin
          FSpecialKickerState:=SKickerArm;
          FSpecialDischargeTime:=FKickerProcessed.DischargeTime;
        end;
        SComLowkicker: begin
          FSpecialKickerState:=SKickerSolenoid;
          FSpecialDischargeTime:=FKickerProcessed.DischargeTime;
        end;
      end;
    end;
    SKickerArm: begin
      if FSensorsProcessed.BallSensor then begin
        FKickerProcessed.DischargeTime:=FSpecialDischargeTime;
        FKickerProcessed.Kick:=true;
        FSpecialKickerState:=SKickerIdle;
        FSensorsProcessed.Solenoid:=false;
        FSpecialKickerCom:=SComIDlekicker;
      end;
    end;
    SKickerSolenoid: begin
      FSensorsProcessed.Solenoid:=true;
      if FSensorsProcessed.SolenoidTime>=255 then begin
        FSpecialKickerState:=SKickerArm;
      end;
    end;
  end;

  if (FSpecialKickerState<>FLastSpecialKickerState) and (FSpecialKickerState=SKickerArm) then begin
    FSpecialKickerStateTiming:=GetTickCount;
  end;
  if FSpecialKickerState=SKickerArm then begin
    if DWord(GetTickCount-FSpecialKickerStateTiming)>KMaxArmTime then begin
      FSpecialKickerState:=SKickerIdle;
      FSensorsProcessed.Solenoid:=false;
      FSpecialKickerCom:=SComIDlekicker;
    end;
  end;
  FLastSpecialKickerState:=FSpecialKickerState;
end;

procedure TForm_Hardware.ProcessOdometry;
var
  AllEnable, AllRead, OneFail: boolean;
begin
  AllEnable:=(FFlashBus.DriverEnable[Motor1]=true) and (FFlashBus.DriverEnable[Motor2]=true) and
             (FFlashBus.DriverEnable[Motor3]=true);
  AllRead:=not((FFlashBus.InputsState[Motor1]=WaitingResponse) or (FFlashBus.InputsState[Motor2]=WaitingResponse) or
               (FFlashBus.InputsState[Motor3]=WaitingResponse) or
               (FFlashBus.InputsState[Motor1]=OverFlowResponse) or (FFlashBus.InputsState[Motor2]=OverFlowResponse) or
               (FFlashBus.InputsState[Motor3]=OverFlowResponse));
  if AllEnable and AllRead then begin
     OneFail:=(FFlashBus.InputsState[Motor1]=FailResponse) or (FFlashBus.InputsState[Motor2]=FailResponse) or
              (FFlashBus.InputsState[Motor3]=FailResponse);
  end;
end;

procedure TForm_Hardware.SetAngVelRefWheels(w1, w2, w3: double);
var
  i: integer;
begin
  if not CheckBox_Enable_Timer.Checked then begin
    for i:=1 to 3 do begin
      FMotorProcessed[i].ControlSource:=PID;
    end;
    FMotorProcessed[1].AngVelRef:=w1;
    FMotorProcessed[2].AngVelRef:=w2;
    FMotorProcessed[3].AngVelRef:=w3;
  end;
end;

procedure TForm_Hardware.SetPWMWheels(PWM1, PWM2, PWM3: double);
var
  i: integer;
begin
  if not CheckBox_Enable_Timer.Checked then begin
    for i:=1 to 3 do begin
      FMotorProcessed[i].ControlSource:=PWM;
    end;
    FMotorProcessed[1].PWM:=PWM1;
    FMotorProcessed[2].PWM:=PWM2;
    FMotorProcessed[3].PWM:=PWM3;
  end;
end;

procedure TForm_Hardware.SetKicker(DischargeTime: integer);
begin
  if not CheckBox_Enable_Timer.Checked then begin
    if DischargeTime>0 then begin
      FKickerProcessed.Kick:=true;
      FKickerProcessed.DischargeTime:=DischargeTime;
    end
    else begin
      FKickerProcessed.Kick:=false;
    end;
  end;
end;

procedure TForm_Hardware.SetSpecialKicker(DischargeTime: integer;
  LowKick: boolean);
begin
  if not CheckBox_Enable_Timer.Checked then begin
    if DischargeTime>0 then begin
      FKickerProcessed.DischargeTime:=DischargeTime;
      if LowKick then begin
        FSpecialKickerCom:=SComLowkicker;
      end
      else begin
        FSpecialKickerCom:=SComHighkicker;
      end;
    end
    else begin
      FSpecialKickerCom:=SComIDlekicker;
    end;
  end;
end;

procedure TForm_Hardware.SetRoller(RollerEnable: boolean);
begin
  if not CheckBox_Enable_Timer.Checked then begin
    if not RollerEnable  then begin
      FSensorsProcessed.RollerMode:=Stop;
    end
    else begin
      case RadioGroup_Roller_Mode.ItemIndex of
        0: FSensorsProcessed.RollerMode:=Stop;
        1: FSensorsProcessed.RollerMode:=PWM;
        2: FSensorsProcessed.RollerMode:=PID;
      end;
      FSensorsProcessed.RollerPWM:=ScrollBar_RollerOutput.Position;
      FSensorsProcessed.RollerSpeedRef:=ScrollBar_RollerOutput.Position;
    end;
  end;
end;

procedure TForm_Hardware.SetRoller(RollerSpeed: double);
begin
  if not CheckBox_Enable_Timer.Checked then begin
    case RadioGroup_Roller_Mode.ItemIndex of
      0: FSensorsProcessed.RollerMode:=Stop;
      1: FSensorsProcessed.RollerMode:=PWM;
      2: FSensorsProcessed.RollerMode:=PID;
    end;
    if (RollerSpeed>=-100) and (RollerSpeed<=100) then begin
      FSensorsProcessed.RollerPWM:=RollerSpeed;
      FSensorsProcessed.RollerSpeedRef:=RollerSpeed;
    end
    else begin
      if RollerSpeed>100 then begin
        FSensorsProcessed.RollerPWM:=100;
        FSensorsProcessed.RollerSpeedRef:=100;
      end
      else begin
        FSensorsProcessed.RollerPWM:=-100;
        FSensorsProcessed.RollerSpeedRef:=-100;
      end;
    end;
  end;
end;

function TForm_Hardware.GetDeltaAngWheels(out w1, w2, w3: double): boolean;
const
  CMaxIntegration=2;
var
  AllRead, AllEnable, OneFail, AllOK, OneMaxIntegration: boolean;
  auxInt: integer;
begin
  AllEnable:=(FFlashBus.DriverEnable[Motor1]=true) and (FFlashBus.DriverEnable[Motor2]=true) and
             (FFlashBus.DriverEnable[Motor3]=true);
  AllRead:=not((FFlashBus.InputsState[Motor1]=WaitingResponse) or (FFlashBus.InputsState[Motor2]=WaitingResponse) or
               (FFlashBus.InputsState[Motor3]=WaitingResponse));

  if AllEnable and AllRead then begin
    OneFail:=(FFlashBus.InputsState[Motor1]=FailResponse) or (FFlashBus.InputsState[Motor2]=FailResponse) or
             (FFlashBus.InputsState[Motor3]=FailResponse);
    if OneFail then begin
      w1:=0;
      w2:=0;
      w3:=0;
      FMotorProcessed[Motor1].SumDeltaEncoderTicks:=0;
      FMotorProcessed[Motor1].NewSumDeltaEncoderTicks:=0;
      FMotorProcessed[Motor2].SumDeltaEncoderTicks:=0;
      FMotorProcessed[Motor2].NewSumDeltaEncoderTicks:=0;
      FMotorProcessed[Motor3].SumDeltaEncoderTicks:=0;
      FMotorProcessed[Motor3].NewSumDeltaEncoderTicks:=0;
      Result:=false;
    end
    else begin
      AllOK:=(FFlashBus.InputsState[Motor1]=Idle) and (FFlashBus.InputsState[Motor2]=Idle) and
             (FFlashBus.InputsState[Motor3]=Idle);
      if AllOK then begin
        OneMaxIntegration:=(FMotorProcessed[Motor1].NewSumDeltaEncoderTicks>CMaxIntegration) or
                           (FMotorProcessed[Motor2].NewSumDeltaEncoderTicks>CMaxIntegration) or
                           (FMotorProcessed[Motor3].NewSumDeltaEncoderTicks>CMaxIntegration);
        if OneMaxIntegration then begin
          w1:=0;
          w2:=0;
          w3:=0;
          FMotorProcessed[Motor1].SumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor1].NewSumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor2].SumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor2].NewSumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor3].SumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor3].NewSumDeltaEncoderTicks:=0;
          Result:=false;
        end
        else begin
          w1:=double(FMotorProcessed[Motor1].SumDeltaEncoderTicks)*KTicks2Rad;
          w2:=double(FMotorProcessed[Motor2].SumDeltaEncoderTicks)*KTicks2Rad;
          w3:=double(FMotorProcessed[Motor3].SumDeltaEncoderTicks)*KTicks2Rad;
          FMotorProcessed[Motor1].SumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor1].NewSumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor2].SumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor2].NewSumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor3].SumDeltaEncoderTicks:=0;
          FMotorProcessed[Motor3].NewSumDeltaEncoderTicks:=0;
          Result:=true;
        end;
      end
      else begin
        w1:=0;
        w2:=0;
        w3:=0;
        Result:=false;
      end;
    end;
  end
  else begin
    w1:=0;
    w2:=0;
    w3:=0;
    Result:=false;
  end;
end;

procedure TForm_Hardware.GetSpeedAngWheels(out w1, w2, w3: double);
begin
  w1:=FMotorProcessed[1].AngVel;
  w2:=FMotorProcessed[2].AngVel;
  w3:=FMotorProcessed[3].AngVel;
end;

function TForm_Hardware.GetCompass(out value: double): boolean;
begin
  value:=FSensorsProcessed.Compass;
  Result:=FSensorsProcessed.NewCompass;
  FSensorsProcessed.NewCompass:=false;
end;

function TForm_Hardware.GetBallSensor: boolean;
begin
  Result:=FSensorsProcessed.BallSensor;
end;

function TForm_Hardware.Failure: boolean;
begin
  if GlobalState=Ok then begin
    Result:=false;
  end
  else begin
    Result:=true;
  end;
end;

procedure TForm_Hardware.Send_outputs;
begin
  if not CheckBox_Enable_Timer.Checked then begin
    FSend_outputs;
  end;
end;

procedure TForm_Hardware.Request_inputs;
begin
  if not CheckBox_Enable_Timer.Checked then begin
    FRequest_inputs;
  end;
end;

procedure TForm_Hardware.Gui2FlashBus;
begin
  //motor1
  FFlashBus.DriverEnable[Motor1]:=CheckBox_Motor1_Enable.Checked;
  //motor2
  FFlashBus.DriverEnable[Motor2]:=CheckBox_Motor2_Enable.Checked;
  //motor3
  FFlashBus.DriverEnable[Motor3]:=CheckBox_Motor3_Enable.Checked;
  //Kicker
  FFlashBus.DriverEnable[Kicker]:=CheckBox_Kicker_Enable.Checked;
  //Sensors
  FFlashBus.DriverEnable[Sensors]:=CheckBox_Sensors_Enable.Checked;
end;

procedure TForm_Hardware.FRequest_inputs;
var
  data2send: string;
  auxtime: DWord;
  deltatime: DWord;
begin
  data2send:=FFlashBus.RequestInputs;
  if Length(data2send)>0 then begin
    LUDP.SendMessage(data2send, LUDP.Host);
  end;
  //timings
  auxtime:=GetTickCount;
  TimingEventSource.last:=TimingEventSource.actual;
  TimingEventSource.actual:=auxtime;
  TimingDriversResponse.last:=auxtime;
  deltatime:=TimingEventSource.actual-TimingEventSource.last;
  if (deltatime)>TimingEventSource.max then begin
    TimingEventSource.max:=deltatime;
  end;
end;

procedure TForm_Hardware.Fsend_outputs;
var
  i: integer;
  data2send: string;
begin
  for i:=Motor1 to Motor3 do begin
    MotorOutputsProcessed2Raw(i);
  end;
  KickerOutputsProcessed2Raw;
  SensorsOutputsProcessed2Raw;
  data2send:=FFlashBus.WriteOutputs;
  if Length(data2send)>0 then begin
    LUDP.SendMessage(data2send,LUDP.Host);
  end;

  //Special Kicker State Machine
  FSpecialKickerStateMachine;
end;

initialization
  {$I unit_hardware.lrs}

end.

