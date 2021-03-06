unit omni3;

{$mode objfpc}{$H+}
{$define MDEC}

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, daisybin, ExtCtrls, Grids, IniFiles,StrUtils, math,
  SdpoSerial, LResources,
  {$ifdef MDEC}
  RLan,
  {$endif}
  IniPropStorage;

const
  PACK_TYPE_NOP = 0;
  PACK_TYPE_ID  = 1;

  PACK_TYPE_READ_EEPROM     = 3;
  PACK_TYPE_WRITE_EEPROM    = 4;

  PACK_TYPE_PWM     = 5;
  PACK_TYPE_PID     = 6;
  PACK_TYPE_PID_PWM = 7;

  PACK_TYPE_READ_ENC     = 9;
  
  PACK_TYPE_READ_VCAP = 10;
  PACK_TYPE_KICK     = 11;

  // motor driver eeprom addresses
  EEADDR_MOT_I_MAX   = 0;	// byte

  EEADDR_PID_PERIOD  = 1;	// byte

  EEADDR_PWM_MAX_SLOPE=2;	// byte

  EEADDR_PID_FILT_EV = 4;	// float
  EEADDR_PID_K       = 8;	// float
  EEADDR_PID_INVTI   =12;	// float
  EEADDR_PID_ALPHA   =16;	// float
  EEADDR_PID_TD      =20;	// float
  EEADDR_PID_FILT_REF=24;	// float

  V_SCALE_FACTOR = 1;

  NumMotors = 3;

  VACT_LIMIT  =6000;

  DRIVESTART  =0;
  DRIVEOK     =1;
  DRIVEWAIT   =2;
  DRIVEERROR  =3;
  DRIVECHECKERROR  =4;

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

  TOmniErrors=record
    checksumerror: boolean;
    checksumerror_count: integer;
    resetMotor_count: array[0..NumMotors-1] of integer;
    resetEncoder_count: array[0..NumMotors-1] of integer;
    resetSample_count: array[0..NumMotors-1] of integer;
  end;

type

  { TFOmni3 }

  TFOmni3 = class(TForm)
    CBShow: TCheckBox;
    CBLan: TCheckBox;
    EditRobotActive: TEdit;
    EditPWMMaxSlope: TEdit;
    EditHardMotIMax: TEdit;
    FormStorage: TIniPropStorage;
    Label15: TLabel;
    Label16: TLabel;
    Comport: TSdpoSerial;
    Timer: TTimer;
    EditDebug: TEdit;
    LBDebug: TListBox;
    CBRun: TCheckBox;
    BStop: TButton;
    EditVRef: TEdit;
    Label4: TLabel;
    BWrite: TButton;
    BRead: TButton;
    BSendNop: TButton;
    Label1: TLabel;
    SBPWM: TScrollBar;
    EditPWM: TEdit;
    BSend: TButton;
    BReadID: TButton;
    BMin: TButton;
    BZero: TButton;
    BMax: TButton;
    BSet: TButton;
    Label12: TLabel;
    EditPIDPeriod: TEdit;
    EditPIDFiltEV: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    EditPIDK: TEdit;
    EditPIDInvTI: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    EditPIDAlpha: TEdit;
    EditPIDTD: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    EditPIDFiltRef: TEdit;
    RGMotor: TRadioGroup;
    RGMode: TRadioGroup;
    SBPIDref: TScrollBar;
    EditEnc: TEdit;
    EditSpeed: TEdit;
    EditDelta: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label11: TLabel;
    EditOutPWM: TEdit;
    Label13: TLabel;
    EditActCurrent: TEdit;
    Label14: TLabel;
    EditResetCount: TEdit;
    procedure ComPortRxData(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBDebugClick(Sender: TObject);
    procedure BStopClick(Sender: TObject);
    procedure BWriteClick(Sender: TObject);
    procedure BReadClick(Sender: TObject);
    procedure BSendNopClick(Sender: TObject);
    procedure SBPWMChange(Sender: TObject);
    procedure BSendClick(Sender: TObject);
    procedure BReadIDClick(Sender: TObject);
    procedure BMinClick(Sender: TObject);
    procedure BZeroClick(Sender: TObject);
    procedure BMaxClick(Sender: TObject);
    procedure BSetClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure CBRunClick(Sender: TObject);
    procedure RGMotorClick(Sender: TObject);
  private
    { Private declarations }
  public
    CurDriver: integer;
    {$ifdef MDEC}
    NetOutBufferOmni3: TUDPBuffer;

    procedure WriteSerialUDP(data: string);
    {$endif}
    
    procedure DStateFromGUI(var ds: TDriverState);
    procedure DStateToGUI(var ds: TDriverState);
    procedure DStateFromIni(var ds: TDriverState; ini: TIniFile; section: string);
    procedure DStateToIni(var ds: TDriverState; ini: TIniFile; section: string);

    procedure ProcessPosition(dev: TDaisyDevice; data: string);
    procedure ProcessPositionSimTwo(dev: TDaisyDevice; data: string);
    procedure ProcessEEPROMData(dev: TDaisyDevice; data: string);

    procedure ReadParsValues;
    procedure WriteParsValues;
  end;

function BinStrToHex(data: string): string;
function SWordTobuf( value: smallint): string;
function Calc_CheckSum(msg: string): byte;
procedure SendPWM;
procedure SendPID_PWM;

var
  FOmni3: TFOmni3;
  daisy: TDaisy;

  ProgReset: boolean;
  sample, last_sample: integer;
  DStates: array[0..NumMotors-1] of TDriverState;
  OmnyErrors: TOmniErrors;
  DriveState: array[0..2] of integer;
  SaveLastEnc: array[0..2] of double;
  reset_count: integer;
  ActDStateIndex: integer;
  AD24V: double;

  act_tick, last_tick: DWORD;
  abs_pos, last_abs_pos: integer;
  last_enc: word;

  state_count, substate_count: integer;
  state_pos_record: integer;

  FirstTickCountSec: Longint;

implementation

uses Main, kicker,
     {$ifdef MDEC}
     Param,
     {$endif}
     LCLIntf;

function BinStrToHex(data: string): string;
var i: integer;
begin
  result:='';
  for i:=1 to length(data) do begin
    result:=result + inttohex(ord(data[i]),2);
  end;
end;

function SWordTobuf( value: smallint): string;
begin
  result := chr(value and $FF) + chr((value shr 8) and $FF) ;
end;

function SpeedToDaisy(speed: double): string;
var ispeed: integer;
begin
  ispeed:= round(speed);

  if ispeed < 0 then begin
    result:= chr(255 and $FF);
    ispeed := ispeed - 1;
  end else begin
    result:= chr(0)
  end;

  if ispeed > 255 then begin
    ispeed:= 255;
  end else if ispeed < -256 then begin
    ispeed:= -256;
  end;

  result:= result + chr(ispeed and $FF);
end;

function int16ToDaisy(i16: integer): string;
begin
  result:= chr(i16 and $FF) + chr((i16 shr 8) and $FF);
end;


function ComPortWriteData(data: string): integer;
begin
  result := FOmni3.ComPort.WriteData(data);
end;


procedure TFOmni3.ComPortRxData(Sender: TObject);
begin
  Daisy.ProcessRawData(ComPort.ReadData);
end;


procedure AddDebugLine(aText: string);
begin
  if FOmni3.LBDebug.Items.Count > 100 then
    FOmni3.LBDebug.Items.Delete(0);
  FOmni3.LBDebug.Items.Add(aText);
  FOmni3.LBDebug.Selected[FOmni3.LBDebug.Items.Count-1]:=true;
end;


function DaisyDataToHex(dev: TDaisyDevice; DataSent, DataRecv: string): string;
var i: integer;
begin
  result := inttohex(ord(dev),2)+': '+ inttohex(length(DataSent),2);
  for i:=1 to length(DataSent) do begin
    result := result + ' '+ inttohex(ord(DataSent[i]),2);
  end;

  result := result + ' - ' + inttohex(length(DataRecv),2);
  for i:=1 to length(DataRecv) do begin
    result := result + ' ' + inttohex(ord(DataRecv[i]),2);
  end;
end;


procedure DaisyDataRecv(dev: TDaisyDevice; DataSent, DataRecv: string);
begin
  //AddDebugLine(DaisyDataToHex(dev, Datasent, DataRecv));
  if length(DataSent)<1 then exit;
  case ord(DataSent[1]) of
    PACK_TYPE_ID: begin
      if FKicker.CBOnlyKicker.Checked and (dev = 0) then begin
          FKicker.AddDebugLine('ID: '+DataRecv);
          exit;
      end else begin
        if  dev < NumMotors then begin
          AddDebugLine('ID: '+DataRecv);
          exit;
        end else begin
          FKicker.AddDebugLine('ID: '+DataRecv);
          exit;
        end;
      end;
    end;
    PACK_TYPE_READ_EEPROM: begin
      if FKicker.CBOnlyKicker.Checked and (dev = 0) then begin
          FKicker.ProcessEEPROMData(DataRecv);
          exit;
      end else begin
        if  dev < NumMotors then begin
          FOmni3.ProcessEEPROMData(dev,DataRecv);
          exit;
        end else begin
          FKicker.ProcessEEPROMData(DataRecv);
          exit;
        end;
      end;
    end;
    PACK_TYPE_PWM: begin
      exit;
    end;
    PACK_TYPE_PID_PWM: begin
      exit;
    end;
    PACK_TYPE_READ_ENC: begin
      FOmni3.ProcessPosition(dev,DataRecv);
      exit;
    end;
    PACK_TYPE_READ_VCAP: begin
      FKicker.ProcessKicker(DataRecv);
      exit;
    end;
    PACK_TYPE_KICK: begin
      exit;
    end;
  end;
//  AddDebugLine(inttostr(dev)+': '+DataSent+' - '+DataRecv);
//  AddDebugLine(format('%d:(%d)%s - (%d)%s',[dev, length(DataSent), DataSent, length(DataRecv), DataRecv]));
  AddDebugLine(DaisyDataToHex(dev, Datasent, DataRecv));
end;


procedure SendPID_PWM;
var
  CheckSum: byte;
  i: integer;
begin
  for i:=0 to NumMotors-1 do begin
    CheckSum:= Calc_CheckSum(chr(PACK_TYPE_PID_PWM) +
                   int16ToDaisy(DStates[i].ref) +
                   int16ToDaisy(DStates[i].pwm));

    Daisy.SendData(i, chr(PACK_TYPE_PID_PWM) +
                   int16ToDaisy(DStates[i].ref) +
                   int16ToDaisy(DStates[i].pwm) +
                   chr(checksum), 0);
  end;
end;


procedure SendPWM;
var
  CheckSum: byte;
  i: integer;
begin
  for i:=0 to NumMotors-1 do begin
    CheckSum:= Calc_CheckSum(chr(PACK_TYPE_PWM) +
                   SpeedToDaisy(DStates[i].pwm));

    Daisy.SendData(i, chr(PACK_TYPE_PWM) +
                   SpeedToDaisy(DStates[i].pwm) +
                   chr(CheckSum), 0);
  end;
end;


procedure TFOmni3.FormShow(Sender: TObject);
begin
  //ComPort.Open;
//  ReadParsValues;
  //Daisy.SendData(0, chr(PACK_TYPE_READ_EEPROM) + chr(0), 28);
  //ComPortWriteData(Daisy.Flush);

{  Daisy.SendData(0, chr(PACK_TYPE_PWM) +
                 SpeedToDaisy(0) +
                 SpeedToDaisy(0) +
                 SpeedToDaisy(0), 0);
  ComPortWriteData(Daisy.Flush);}
  if RGMotor.ItemIndex < 0 then exit;
  DStateToGUI(DStates[RGMotor.ItemIndex]);
end;

procedure TFOmni3.FormClose(Sender: TObject; var closeAction: TCloseAction);
begin
  CBRun.Checked:=false;
  Timer.Enabled:=false;
  ComPort.Close;
  {$ifdef MDEC}
  Fmain.AuxFormClosed('Omni3');
  {$endif}
end;

procedure TFOmni3.FormCreate(Sender: TObject);
var
  i: integer;
  ini: TIniFile;
  SessionPropsList: TStringList;
  SessionPropsFileName: string;
begin
  SessionPropsFileName := ExtractFilePath(Application.ExeName)+FMain.DataDir+'/SessionPropsOmni3.txt';
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
  FormStorage.IniFileName:=extractfilepath(application.exename)+FMain.DataDir+'/Omni3.ini';
  FormStorage.Restore;
  
  ProgReset := true;
  Daisy := TDaisy.Create;
  Daisy.SetRecvProc(@DaisyDataRecv);
  //abs_pos := $7FFFFFFF;

  ini:=TIniFile.Create(extractfilepath(application.exename)+FMain.DataDir+'/Omni3.ini');
  for i:=0 to NumMotors-1 do
    DStateFromIni(DStates[i],ini,'MOTOR'+inttostr(i+1));
  ini.Free;
  
  {$ifdef MDEC}
  Fmain.InsertAuxForms(FOmni3,'Omni3');
  {$endif}
end;

procedure TFOmni3.FormDestroy(Sender: TObject);
var
  ini: TIniFile;
  i: integer;
begin
  daisy.Free;
  
  ini:=TIniFile.Create(extractfilepath(application.exename)+FMain.DataDir+'/Omni3.ini');
  for i:=0 to NumMotors-1 do
    DStateToIni(DStates[i],ini,'MOTOR'+inttostr(i+1));
  ini.Free;
end;

procedure TFOmni3.LBDebugClick(Sender: TObject);
begin
  EditDebug.Text:=LBDebug.Items[LBDebug.ItemIndex];
end;

procedure TFOmni3.BStopClick(Sender: TObject);
begin
  CBRun.Checked := false;
end;


procedure TFOmni3.ProcessEEPROMData(dev: TDaisyDevice; data: string);
var i: integer;
begin
  i := RGMotor.ItemIndex;
  //if i<0 then exit;

  if length(data) < 28 then exit;

  DStates[dev].PID_period := ord(data[EEADDR_PID_PERIOD + 1]);
  DStates[dev].PWM_MaxSlope := ord(data[EEADDR_PWM_MAX_SLOPE + 1]);
  DStates[dev].HardMotIMax := ord(data[EEADDR_MOT_I_MAX + 1]);
  //EditPIDPeriod.Text := IntToStr(PID_period);

  DStates[dev].filt_ev := PSingle(@data[EEADDR_PID_FILT_EV + 1])^;
  DStates[dev].K := PSingle(@data[EEADDR_PID_K + 1])^;
  DStates[dev].invTi := PSingle(@data[EEADDR_PID_INVTI + 1])^;
  DStates[dev].alpha := PSingle(@data[EEADDR_PID_ALPHA + 1])^;
  DStates[dev].TD := PSingle(@data[EEADDR_PID_TD + 1])^;
  DStates[dev].filt_ref := PSingle(@data[EEADDR_PID_FILT_REF + 1])^;

  if i<0 then exit;
  if i<>dev then exit;
  DStateToGUI(DStates[i]);

  {EditPIDFiltEV.Text := Format('%.3f', [ PSingle(@data[EEADDR_PID_FILT_EV + 1])^ ]);
  EditPIDK.Text := Format('%.3f', [ PSingle(@data[EEADDR_PID_K + 1])^ ]);
  EditPIDInvTI.Text := Format('%.3f', [ PSingle(@data[EEADDR_PID_INVTI + 1])^ ]);
  EditPIDAlpha.Text := Format('%.3f', [ PSingle(@data[EEADDR_PID_ALPHA + 1])^ ]);
  EditPIDTD.Text := Format('%.3f', [ PSingle(@data[EEADDR_PID_TD + 1])^ ]);
  EditPIDFiltRef.Text := Format('%.3f', [ PSingle(@data[EEADDR_PID_FILT_REF + 1])^ ]);}
end;

procedure TFOmni3.ReadParsValues;
begin
  Daisy.SendData(RGMotor.ItemIndex, chr(PACK_TYPE_READ_EEPROM) + chr(0), 28);
end;


procedure TFOmni3.WriteParsValues;
var eeprom_data: string;
    i: integer;
begin
  i := RGMotor.ItemIndex;
  if i<0 then exit;

  DStateFromGUI(DStates[i]);
  
  eeprom_data := StringOfChar(chr(0), 28);

  eeprom_data[EEADDR_PID_PERIOD + 1] := chr(DStates[i].PID_Period);
  eeprom_data[EEADDR_PWM_MAX_SLOPE + 1] := chr(DStates[i].PWM_MaxSlope);
  eeprom_data[EEADDR_MOT_I_MAX + 1] := chr(DStates[i].HardMotIMax);
  //eeprom_data[EEADDR_PID_PERIOD + 1] := chr(StrToIntDef(EditPIDPeriod.Text, 100));

  PSingle(@eeprom_data[EEADDR_PID_FILT_EV + 1])^ := DStates[i].filt_ev;
  PSingle(@eeprom_data[EEADDR_PID_K + 1])^ := DStates[i].K;
  PSingle(@eeprom_data[EEADDR_PID_INVTI + 1])^ := DStates[i].invTi;
  PSingle(@eeprom_data[EEADDR_PID_ALPHA + 1])^ := DStates[i].alpha;
  PSingle(@eeprom_data[EEADDR_PID_TD + 1])^ := DStates[i].TD;
  PSingle(@eeprom_data[EEADDR_PID_FILT_REF + 1])^ := DStates[i].filt_ref;

  {eeprom_data[EEADDR_PID_PERIOD + 1] := chr(StrToIntDef(EditPIDPeriod.Text, 100));

  PSingle(@eeprom_data[EEADDR_PID_FILT_EV + 1])^ := StrToFloatDef(EditPIDFiltEV.Text, 0.0);
  PSingle(@eeprom_data[EEADDR_PID_K + 1])^ := StrToFloatDef(EditPIDK.Text, 0.0);
  PSingle(@eeprom_data[EEADDR_PID_INVTI + 1])^ := StrToFloatDef(EditPIDInvTI.Text, 0.0);
  PSingle(@eeprom_data[EEADDR_PID_ALPHA + 1])^ := StrToFloatDef(EditPIDAlpha.Text, 0.0);
  PSingle(@eeprom_data[EEADDR_PID_TD + 1])^ := StrToFloatDef(EditPIDTD.Text, 0.0);
  PSingle(@eeprom_data[EEADDR_PID_FILT_REF + 1])^ := StrToFloatDef(EditPIDFiltRef.Text, 0.0);}

  Daisy.SendData(i, chr(PACK_TYPE_WRITE_EEPROM) + chr(0) + eeprom_data, 0);
end;

procedure TFOmni3.ProcessPosition(dev: TDaisyDevice; data: string);
var
    re: word;
    act_enc: smallint;
    i, b, flags, delta:integer;
    check_s:byte;
    DReset: boolean;
    msg: string;
    errorcheck:boolean;
    drivers_good: boolean;
    test:integer;
begin

  errorcheck:=false;
  drivers_good:=true;

  //verify data size
  test:=Length(data);
  if length(data) < 8 then begin
    exit;
  end;

  //verify data checksum
  msg:=copy(data,1,length(data) - 1);
  check_s:=Calc_CheckSum(msg);
  if check_s <> ord(data[8]) then begin
    OmnyErrors.checksumerror:=true;
    inc(OmnyErrors.checksumerror_count);
  end;

  //verify reset on drives
  flags:= ord(data[7]);
  if (flags and $80 <> 0) then begin    //test resets on motor drives
    drivers_good:=false;
    inc(OmnyErrors.resetMotor_count[dev]);
  end;

  //verify reset on encoders
  if (flags and $40 <> 0) then begin    //test resets on encoders
    drivers_good:=false;
    inc(OmnyErrors.resetEncoder_count[dev]);
  end;

  //verify sample jumps
  if (flags and $20 <> 0) then begin    //fault in sample data
    drivers_good:=false;
    inc(OmnyErrors.resetSample_count[dev]);
  end;

  DStates[dev].RobotActive := ((flags and $10) > 0);


  //verify out_pwm
  b := ord(data[6]);
  if flags and 1 <> 0 then b:= -(256 - b);
    DStates[dev].out_pwm := b;

  //verify current
  if abs(DStates[dev].out_pwm) > 1 then
    DStates[dev].Iact := (ord(data[5])*(255/abs(DStates[dev].out_pwm)))/115.7
  else
    DStates[dev].Iact:=0;

  //AddDebugLine(format('%d -> %d %d %d %d %d %d %d %d',[dev, flags and $80,flags and $40,flags and $20,flags and $10, flags and 8, flags and 4,flags and 2,flags and 1]));




  //verify odos integrity
  if ProgReset then begin
    DriveState[dev]:= DRIVESTART;
  end else if OmnyErrors.checksumerror then begin
    DriveState[dev]:=DRIVECHECKERROR;
  end else if not drivers_good then begin
    DriveState[dev]:=DRIVEERROR;
  end else if ((drivers_good) and (DriveState[dev]=DRIVEERROR)) then begin
    DriveState[dev]:=DRIVEWAIT;
    re:= ord(data[3]) + ord(data[4]) * 256;
    DStates[dev].enc := smallint(re);
  end else if ((drivers_good) and (DriveState[dev]=DRIVEWAIT)) then begin
    DriveState[dev]:=DRIVEOK;
    re:= ord(data[3]) + ord(data[4]) * 256;
    act_enc:= smallint(re);
  end else begin
    re:= ord(data[3]) + ord(data[4]) * 256;
    act_enc:= smallint(re);
    DriveState[dev]:=DRIVEOK;
  end;

  //obtain odometry
  case DriveState[dev] of
    DRIVESTART: begin
      DStates[dev].Vact:= 0;
    end;
    DRIVEOK: begin
      re := ord(data[3]) + ord(data[4]) * 256;
      act_enc := smallint(re);      //actual encoder value
      DStates[dev].Vact:=smallint(act_enc - DStates[dev].enc);  //tick count variation
      if (abs(DStates[dev].Vact)>VACT_LIMIT)then begin
        DriveState[dev]:=DRIVEERROR;
        DStates[dev].Vact:=SaveLastEnc[dev];
      end;
      //SaveLastEnc[dev]:=DStates[dev].Vact;
      DStates[dev].enc := act_enc;
    end;
    DRIVEWAIT : begin
    end;
    DRIVEERROR: begin
    end;
    DRIVECHECKERROR: begin
      for i:=0 to 2 do begin
        DriveState[i]:=DRIVECHECKERROR;
        DStates[i].Vact:=SaveLastEnc[i];
      end;
    end;
  end;

  View.Odos[0].speedw[dev]:=DStates[dev].Vact*Ktic_rad*(Wheel1Diameter/2);
  DStates[dev].ticks_act := TRUE;


  if (DStates[0].ticks_act = TRUE) and (DStates[1].ticks_act = TRUE) and (DStates[2].ticks_act = TRUE) then begin
    for i:=0 to 2 do begin
      if (DriveState[i]=DRIVEOK) then begin
        SaveLastEnc[i]:=DStates[i].Vact;
      end else if (DriveState[i]=DRIVECHECKERROR) then begin
        DriveState[i]:=DRIVEERROR;
        OmnyErrors.checksumerror:=false;
      end;
      DStates[i].ticks_act := false;
    end;

    //deslocamento_robot();
    ProgReset := false;    //Reset for the first time the program runs
  end;



  // this is only for omny3 form
  if not CBShow.Checked then exit;
  i := RGMotor.ItemIndex;
  if i<0 then exit;
  if i<>dev then exit;

  EditEnc.Text := inttostr(DStates[i].enc);
  //EditSpeed.Text := format('%.2f', [DStates[i].Vact]);

  sample := ord(data[1]) + ord(data[2]) * 256;
  //enc := smallint(re);
  if not DReset and not ProgReset then begin
    delta := sample - last_sample;
  end else begin
    delta := 0;
  end;
  last_sample := sample;

  EditDelta.Text := inttostr(delta);

  EditSpeed.Text := format('%.2f', [DStates[i].Vact / max(1, delta)]);

  act_tick := GetTickCount;
  if abs(DStates[i].Vact - 1000) > 20 then
    AddDebugLine(format('%d %.2f', [act_tick - last_tick, DStates[i].Vact]));
    //AddDebugLine(format('%d %.2f', [act_tick - last_tick, DStates[i].Vact/(act_tick - last_tick)]));
  last_tick:= act_tick;

  EditActCurrent.Text:= format('%.2f',[DStates[i].Iact]);

  EditDebug.Text:= format('%.2f %.2f %.2f',[DStates[0].Iact, DStates[1].Iact, DStates[2].Iact]);

  EditOutPWM.Text:= format('%0.3d',[DStates[i].out_pwm]);

  EditDebug.Text:= EditDebug.Text + format(' [%3d %3d %3d]',[DStates[0].out_pwm, DStates[1].out_pwm, DStates[2].out_pwm]);

  //if dev=2 then begin
    if DStates[0].RobotActive then begin
      EditRobotActive.Text:='Active';
      EditRobotActive.Color:=clGreen;
    end else begin
      EditRobotActive.Text:='Not Active';
      EditRobotActive.Color:=clRed;
    end;
  //end;
 // ProgReset := false;

end;

procedure TFOmni3.ProcessPositionSimTwo(dev: TDaisyDevice; data: string);
var
    re: word;
    act_enc: smallint;
    i, b, flags, delta:integer;
    check_s:byte;
    DReset: boolean;
    msg: string;
    errorcheck:boolean;
    drivers_good: boolean;
    test:integer;
begin
  //act_enc := smallint(StrToInt(data));      //actual encoder value
  //DStates[dev].Vact:=smallint(act_enc - DStates[dev].enc);  //tick count variation
  //DStates[dev].enc := act_enc;
  DStates[dev].Vact:=StrToInt(data);
  View.Odos[0].speedw[dev]:=DStates[dev].Vact*Ktic_rad*(Wheel1Diameter/2);
  DStates[dev].ticks_act := TRUE;

  if (DStates[0].ticks_act = TRUE) and (DStates[1].ticks_act = TRUE) and (DStates[2].ticks_act = TRUE) then begin
    for i:=0 to NumMotors-1 do begin
      DStates[i].ticks_act := false;
    end;
    ProgReset := false;    //Reset for the first time the program runs
  end;

  // this is only for omny3 form
  if not CBShow.Checked then exit;
  i := RGMotor.ItemIndex;
  if i<0 then exit;
  if i<>dev then exit;
 // EditEnc.Text := inttostr(DStates[i].Vact);
end;


{procedure TFOmni3.ProcessPosition(dev: TDaisyDevice; data: string);
var re: word;
    act_enc: smallint;
    i, b, flags, delta: integer;
    DReset: boolean;
begin
  if length(data) < 7 then exit;
  flags := ord(data[7]);
  if flags and $80 <> 0 then begin
    DReset := true;
    inc(reset_count);
    EditResetCount.Text := inttostr(reset_count);
  end else begin
    DReset := false;
  end;
  //AddDebugLine(inttostr(dev));
  //exit;
  re := ord(data[3]) + ord(data[4]) * 256;
  act_enc := smallint(re);
  if not DReset and not ProgReset then begin
    DStates[dev].Vact := smallint(act_enc - DStates[dev].enc);
  end else begin
    DStates[dev].Vact := 0;
  end;
  DStates[dev].enc := act_enc;
  
  DStates[dev].Iact := ord(data[5])/ (1e2 * 0.22);

  b := ord(data[6]);
  if flags and 1 <> 0 then b:= -(256 - b);
  DStates[dev].out_pwm := b;

  DStates[dev].RobotActive := ((flags and $20) > 0);
  
  ProgReset := false;

  if not CBShow.Checked then exit;
  i := RGMotor.ItemIndex;
  if i<0 then exit;
  if i<>dev then exit;

  EditEnc.Text := inttostr(DStates[i].enc);
  //EditSpeed.Text := format('%.2f', [DStates[i].Vact]);

  sample := ord(data[1]) + ord(data[2]) * 256;
  //enc := smallint(re);
  if not DReset and not ProgReset then begin
    delta := sample - last_sample;
  end else begin
    delta := 0;
  end;
  last_sample := sample;
  
  EditDelta.Text := inttostr(delta);
  
  EditSpeed.Text := format('%.2f', [DStates[i].Vact / max(1, delta)]);

  act_tick := GetTickCount;
  if abs(DStates[i].Vact - 1000) > 20 then
    AddDebugLine(format('%d %.2f', [act_tick - last_tick, DStates[i].Vact]));
    //AddDebugLine(format('%d %.2f', [act_tick - last_tick, DStates[i].Vact/(act_tick - last_tick)]));
  last_tick:= act_tick;

  EditActCurrent.Text:= format('%.2f',[DStates[i].Iact]);

  EditDebug.Text:= format('%.2f %.2f %.2f',[DStates[0].Iact, DStates[1].Iact, DStates[2].Iact]);

  EditOutPWM.Text:= format('%0.3d',[DStates[i].out_pwm]);

  EditDebug.Text:= EditDebug.Text + format(' [%3d %3d %3d]',[DStates[0].out_pwm, DStates[1].out_pwm, DStates[2].out_pwm]);

  if DStates[i].RobotActive then begin
    EditRobotActive.Text:='Active';
    EditRobotActive.Color:=clWhite;
  end else begin
    EditRobotActive.Text:='Not Active';
    EditRobotActive.Color:=clRed;
  end;

  ProgReset := false;
{  if not ChHoldChart.Checked then begin
    Series1.BeginUpdate;
    Series1.AddXY(GetTickCount * 0.001, sp);
    if Series1.Count >= 80 then begin
      Series1.Delete(0);
    end;
    Series1.EndUpdate;
    Chart.Invalidate;
  end;}
end;}

{$ifdef MDEC}
procedure TFOmni3.WriteSerialUDP(data: string);
begin
  ClearUDPBuffer(NetOutBufferOmni3);
  NetPutByte(NetOutBufferOmni3,ord('1'));
  NetPutString(NetOutBufferOmni3,data);

  NetPutByte(NetOutBufferOmni3,ord('2'));
  NetPutString(NetOutBufferOmni3,'');

  FMain.SdpoUDP.Send(NetOutBufferOmni3.data,NetOutBufferOmni3.MessSize,FParam.EditVisionIP.Text+':6969');
end;
{$endif}

procedure TFOmni3.BWriteClick(Sender: TObject);
var
  s: string;
begin
  WriteParsValues;
  {$ifdef MDEC}
  if CBLan.Checked then
    WriteSerialUDP(Daisy.Flush)
  else if CBRun.Checked then begin
    ComPortWriteData(Daisy.Flush);
  end;
  {$else}
  if CBRun.Checked then
    ComPortWriteData(Daisy.Flush)
  else
    s:=Daisy.Flush;
  {$endif}
end;

procedure TFOmni3.BReadClick(Sender: TObject);
var
  s: string;
begin
  ReadParsValues;
  {$ifdef MDEC}
  if CBLan.Checked then
    WriteSerialUDP(Daisy.Flush)
  else if CBRun.Checked then begin
    ComPortWriteData(Daisy.Flush)
  end;
  {$else}
  if CBRun.Checked then
    ComPortWriteData(Daisy.Flush)
  else
    s:=Daisy.Flush;
  {$endif}
end;

procedure TFOmni3.BSendNopClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  for i:=0 to NumMotors-1 do
    Daisy.SendData(i, chr(PACK_TYPE_NOP), 0);
  {$ifdef MDEC}
  if CBLan.Checked then
    WriteSerialUDP(Daisy.Flush)
  else if CBRun.Checked then begin
    ComPortWriteData(Daisy.Flush)
  end;
  {$else}
  if CBRun.Checked then
    ComPortWriteData(Daisy.Flush)
  else
    s:=Daisy.Flush;
  {$endif}
end;


procedure TFOmni3.SBPWMChange(Sender: TObject);
var  i: integer;
begin
  i := RGMotor.ItemIndex;
  if i<0 then exit;

  DStateFromGUI(DStates[i]);
  DStateToGUI(DStates[i]);

  //EditPWM.Text:=inttostr(SBPWM.position);
end;


function Calc_CheckSum(msg: string): byte;
var
  i,check: byte;
begin
  check := 0;
  for i:=1 to length(msg) do begin
    check := check + ord(msg[i]);
  end;
  result:= check;
end;

procedure TFOmni3.BSendClick(Sender: TObject);
var
  s: string;
begin
  SendPWM;
  {$ifdef MDEC}
  if CBLan.Checked then
    WriteSerialUDP(Daisy.Flush)
  else if CBRun.Checked then begin
    ComPortWriteData(Daisy.Flush)
  end;
  {$else}
  if CBRun.Checked then
    ComPortWriteData(Daisy.Flush)
  else
    s:=Daisy.Flush;
  {$endif}
end;

procedure TFOmni3.BReadIDClick(Sender: TObject);
var
  s: string;
begin
  Daisy.SendData(RGMotor.ItemIndex, chr(PACK_TYPE_ID), 4);
  {$ifdef MDEC}
  if CBLan.Checked then
    WriteSerialUDP(Daisy.Flush)
  else if CBRun.Checked then begin
    ComPortWriteData(Daisy.Flush)
  end;
  {$else}
  if CBRun.Checked then
    ComPortWriteData(Daisy.Flush)
  else
    s:=Daisy.Flush;
  {$endif}
end;

procedure TFOmni3.BMinClick(Sender: TObject);
begin
  SBPWM.Position := SBPWM.Min;
end;

procedure TFOmni3.BZeroClick(Sender: TObject);
begin
  SBPWM.Position := 0;
  SBPIDref.Position := 0;
end;

procedure TFOmni3.BMaxClick(Sender: TObject);
begin
  SBPWM.Position := SBPWM.Max;
end;

procedure TFOmni3.BSetClick(Sender: TObject);
begin
  SBPWM.Position := strtointDef(EditPWM.Text, SBPWM.Position);
  SBPIDref.Position := strtointDef(EditVRef.Text, SBPIDref.Position);
end;

procedure TFOmni3.TimerTimer(Sender: TObject);
var
  i: integer;
begin
//  if SBPWM.Position <> strtointdef(EditStep.Text,0) then

  case RGMode.ItemIndex of
    0: SendPWM;
    1: SendPID_PWM;
  end;
  
  if CBLan.Checked then begin
    {$ifdef MDEC}
    WriteSerialUDP(Daisy.Flush);
    {$endif}
  end else begin
    ComPortWriteData(Daisy.Flush);
    
    for i:=0 to NumMotors-1 do begin
      Daisy.SendData(i, chr(PACK_TYPE_READ_ENC), 8);
    end;
    ComPortWriteData(Daisy.Flush);
  end;
  
end;

procedure TFOmni3.CBRunClick(Sender: TObject);
begin
  if not CBLan.Checked then
    ComPort.Open;
  //else
  //  ComPort.Close;

  Timer.Enabled := CBRun.Checked;
end;

procedure TFOmni3.DStateFromGUI(var ds: TDriverState);
begin
  with ds do begin
    if (RGMode.ItemIndex >= 0) and (RGMode.ItemIndex <= ord(high(mode))) then begin
      mode := TDriverStateMode(RGMode.ItemIndex);
    end;

    pwm:=SBPWM.Position;

    //ref:=strtointdef(EditVref.Text,0);
    ref:=SBPIDref.Position;

    alpha:=strtofloatdef(EditPIDalpha.Text,0.0);
    filt_ev:=strtofloatdef(EditPIDfiltEV.Text,0.0);
    K:=strtofloatdef(EditPIDK.Text,0.0);
    invTi:=strtofloatdef(EditPIDinvTi.Text,0.0);
    Td:=strtofloatdef(EditPIDTd.Text,0.0);
    filt_ref:=strtofloatdef(EditPIDfiltRef.Text,0.0);

{    AlphaPos:=strtofloatdef(EditPosAlpha.Text,0.0);
    AlphaNeg:=strtofloatdef(EditNegAlpha.Text,0.0);
    BetaPos:=strtofloatdef(EditPosBeta.Text,0.0);
    BetaNeg:=strtofloatdef(EditNegBeta.Text,0.0);
    AlphaSimple:=strtofloatdef(EditSimpleAlpha.Text,0.0);
    BetaSimple:=strtofloatdef(EditSimpleBeta.Text,0.0);
}
    PID_period:=strtointdef(EditPIDPeriod.Text,20);
    PWM_MaxSlope:=StrToIntDef(EditPWMMaxSlope.Text,40);
    HardMotIMax:=StrToIntDef(EditHardMotIMax.Text,100);
  end;
end;


function FToStr(f: double): string;
begin
  result:=Format('%.6f',[f]);
  result:=AnsiReplaceStr(TrimRight(AnsiReplaceStr(result,'0',' ')),' ','0');
  if result[length(result)]='.' then result:=result+'0';
end;


procedure TFOmni3.DStateToGUI(var ds: TDriverState);
begin
  with ds do begin
    RGMode.ItemIndex:= ord(mode);

    SBPWM.Position:=pwm;
    EditPWM.Text:=inttostr(pwm);

    SBPIDref.Position:=ref;
    EditVRef.Text:=inttostr(ref);

    EditPIDalpha.Text:=FToStr(alpha);
    EditPIDfiltEv.Text:=FToStr(filt_ev);
    EditPIDK.Text:=FToStr(K);
    EditPIDinvTi.Text:=FToStr(invTi);
    EditPIDTd.Text:=FToStr(Td);
    EditPIDfiltRef.Text:=FToStr(filt_ref);

{    EditPosAlpha.Text:=FToStr(AlphaPos);
    EditNegAlpha.Text:=FToStr(AlphaNeg);
    EditPosBeta.Text:=FToStr(BetaPos);
    EditNegBeta.Text:=FToStr(BetaNeg);
    EditSimpleAlpha.Text:=FToStr(AlphaSimple);
    EditSimpleBeta.Text:=FToStr(BetaSimple);
}
    EditPIDPeriod.Text:=inttostr(PID_period);
    EditPWMMaxSlope.Text:=inttostr(PWM_MaxSlope);
    EditHardMotIMax.Text:=inttostr(HardMotIMax);
  end;
end;

procedure TFOmni3.DStateFromIni(var ds: TDriverState; ini: TIniFile; section: string);
begin
  with ds, ini do begin
    Mode:=TDriverStateMode(ReadInteger(section,'PIDmode',0));

    pwm:=ReadInteger(section,'pwm',0);
    ref:=ReadInteger(section,'ref',0);

    alpha:=ReadFloat(section,'alpha',0.0);
    filt_ev:=ReadFloat(section,'filt_ev',0.0);
    K:=ReadFloat(section,'K',0.0);
    invTi:=ReadFloat(section,'invTi',0.0);
    Td:=ReadFloat(section,'TD',0.0);
    filt_ref:=ReadFloat(section,'filt_ref',0.0);

    AlphaPos:=ReadFloat(section,'AlphaPos',0.0);
    AlphaNeg:=ReadFloat(section,'AlphaNeg',0.0);
    BetaPos:=ReadFloat(section,'BetaPos',0.0);
    BetaNeg:=ReadFloat(section,'BetaNeg',0.0);
//    AlphaSimple:=ReadFloat(section,'AlphaSimple',0.0);
//    BetaSimple:=ReadFloat(section,'BetaSimple',0.0);

    PID_period:=ReadInteger(section,'PID_period',200);
    PWM_MaxSlope:=ReadInteger(section,'PWM_MaxSlope',40);
    HardMotIMax:=ReadInteger(section,'HardMotIMax',100);
  end
end;

procedure TFOmni3.DStateToIni(var ds: TDriverState; ini: TIniFile; section: string);
begin
  with ds, ini do begin
    WriteInteger(section,'PIDmode',ord(Mode));

    WriteInteger(section,'pwm',pwm);
    WriteInteger(section,'ref',ref);

    WriteFloat(section,'alpha',alpha);                  
    WriteFloat(section,'filt_ev',filt_ev);
    WriteFloat(section,'K',K);
    WriteFloat(section,'invTi',invTi);
    WriteFloat(section,'TD',Td);
    WriteFloat(section,'filt_ref',filt_ref);

    WriteFloat(section,'AlphaPos',AlphaPos);
    WriteFloat(section,'AlphaNeg',AlphaNeg);
    WriteFloat(section,'BetaPos',BetaPos);
    WriteFloat(section,'BetaNeg',BetaNeg);
//    WriteFloat(section,'AlphaSimple',AlphaSimple);
//    WriteFloat(section,'BetaSimple',BetaSimple);

    WriteInteger(section,'PID_period',PID_period);
    WriteInteger(section,'PWM_MaxSlope',PWM_MaxSlope);
    WriteInteger(section,'HardMotIMax',HardMotIMax);
  end;
end;

{
procedure TFOmni3.FormCreate(Sender: TObject);
var i: integer;
    ini: TIniFile;
begin
  daisy := TDaisy.Create;

  FormStorage.IniFileName:=extractfilepath(application.exename)+'Omni3.ini';
  FormStorage.LoadProps;

  ini:=TIniFile.Create(extractfilepath(application.exename)+'drivers.ini');
  for i:=0 to NumDrivers-1 do
    DStateFromIni(DStates[i],ini,'driver'+inttostr(i+1));
  ini.Free;

  EncArray[0,0]:=0;
  EncArray[0,1]:=0;
  CntArray:=0;
  EncReq:=0;
  LastEncReq:=0;
  SerialInBuf:='';
end;}


procedure TFOmni3.RGMotorClick(Sender: TObject);
var  i: integer;
begin
  i := RGMotor.ItemIndex;
  if i<0 then exit;

  if i = ActDStateIndex then exit;
  if (ActDStateIndex>=0) and (ActDStateIndex<NumMotors) then
    DStateFromGUI(DStates[ActDStateIndex]);

  DStateToGUI(DStates[i]);
  ActDStateIndex := i;
end;

initialization
  {$I omni3.lrs}

end.











