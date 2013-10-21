unit kicker;

{$mode objfpc}{$H+}
{$define MDEC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SdpoSerial, IniPropStorage, ExtCtrls;

const
  // motor driver eeprom addresses
  EEADDR_MAX_VCAP = 0;  //          maximum capacitor voltage
  EEADDR_DUTY_CYCLE = 1;  //        duty cycle
  EEADDR_MAX_VCAP_LEVEL1 = 2;  //   capacitor voltage in level 1 charge
  EEADDR_DUTY_CYCLE_LEVEL1 = 3;  // duty cycle until level 1
  EEADDR_MAX_VCAP_LEVEL2 = 4;  //   capacitor voltage in level 2 charge
  EEADDR_DUTY_CYCLE_LEVEL2 = 5;  // duty cycle until level 2
  
type
  TKickerState=record
    Vact,Vbat: double;
    //RobotBatery:string;
    DutyCycle: byte;
    Compass: byte;
    CompassActive: boolean;
    ReadAdError:integer;
  end;
  
type

  { TFKicker }

  TFKicker = class(TForm)
    BKick: TButton;
    BReadEEPROM: TButton;
    BReadID: TButton;
    BWriteEEPROM: TButton;
    CBRun: TCheckBox;
    CBShow: TCheckBox;
    CBLan: TCheckBox;
    CBOnlyKicker: TCheckBox;
    EditReadAdcError: TEdit;
    EditCurDutyCycle: TEdit;
    EditDutyCycleLv1: TEdit;
    EditDutyCycleLv2: TEdit;
    EditKickPulse: TEdit;
    EditMaxVCap: TEdit;
    EditDutyCycle: TEdit;
    EditDebug: TEdit;
    EditMaxVCapLv1: TEdit;
    EditMaxVCapLv2: TEdit;
    EditVoltage: TEdit;
    EditCompass: TEdit;
    EditBattery: TEdit;
    FormStorage: TIniPropStorage;
    ComPort: TSdpoSerial;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LBDebug: TListBox;
    Panel1: TPanel;
    Timer: TTimer;
    procedure BKickClick(Sender: TObject);
    procedure BReadEEPROMClick(Sender: TObject);
    procedure BWriteEEPROMClick(Sender: TObject);
    procedure BReadIDClick(Sender: TObject);
    procedure CBRunClick(Sender: TObject);
    procedure ComPortRxData(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LBDebugClick(Sender: TObject);
    procedure SendKick(Pulse: double);
    procedure ProcessKicker(data: string);
    procedure TimerTimer(Sender: TObject);
    procedure WriteParsValues;
    procedure ReadParsValues;
  private
    { private declarations }
  public
    { public declarations }

    procedure AddDebugLine(aText: string);
    procedure ProcessEEPROMData(data: string);
    function ComPortWriteData(data: string): integer;
  end;

procedure ReadKicker;

var
  FKicker: TFKicker;
  //daisy: TDaisy;
  ProgReset: boolean;
  KickerState: TKickerState;

implementation

uses omni3, Main;

function int16ToDaisy(i16: integer): string;
begin
  result:= chr(i16 and $FF) + chr((i16 shr 8) and $FF);
end;

procedure TFKicker.AddDebugLine(aText: string);
begin
  if FKicker.LBDebug.Items.Count > 100 then
    FKicker.LBDebug.Items.Delete(0);
  FKicker.LBDebug.Items.Add(aText);
  FKicker.LBDebug.Selected[FKicker.LBDebug.Items.Count-1]:=true;
end;

function TFKicker.ComPortWriteData(data: string): integer;
begin
  result := FKicker.ComPort.WriteData(data);
  //AddDebugLine(data);
end;

{ TFKicker }

procedure TFKicker.ProcessEEPROMData(data: string);
begin
  editmaxvcap.Text:= IntToStr(ord(data[EEADDR_MAX_VCAP + 1]));
  editdutycycle.text :=  IntToStr(ord(data[EEADDR_DUTY_CYCLE + 1]));
  EditDutyCycleLv1.Text:=IntToStr(ord(data[EEADDR_DUTY_CYCLE_LEVEL1 + 1]));
  EditDutyCycleLv2.Text:=IntToStr(ord(data[EEADDR_DUTY_CYCLE_LEVEL2 + 1]));
  EditMaxVCapLv1.Text:=IntToStr(ord(data[EEADDR_MAX_VCAP_LEVEL1 + 1]));
  EditMaxVCapLv2.Text:=IntToStr(ord(data[EEADDR_MAX_VCAP_LEVEL2 + 1]));
end;

procedure TFKicker.ProcessKicker(data: string);
//var
  //k: double;
begin
  //k:=;
  if length(data) < 5 then exit;
  //EditDebug.Text:=data;
  with KickerState do begin
    Vact:=(ord(data[1]) + ord(data[2]) * 256)/1024*5*50;
    DutyCycle:=ord(data[3]);
    Vbat:=Vbat*0.8+((ord(data[4]) + ord(data[5]) * 256)/1024*5*17.5)*0.2;
    //RobotBatery:='01';//Copy(data,1,2);
    Compass:=ord(data[6]);
    CompassActive:=true;//(ord(data[7]) > 0);
    ReadAdError:=ReadAdError+ord(data[7]);
    EditReadAdcError.Text:=IntToStr(ReadAdError);
    if CBShow.Checked then begin
      EditVoltage.Text:=format('%2f',[Vact]);//IntToStr(Vact);
      EditBattery.Text:=format('%2f',[Vbat]);//IntToStr(Vact);
      EditCurDutyCycle.Text:=IntToStr(DutyCycle);
      EditCompass.Text:=IntToStr(Compass);
      if CompassActive then
        EditCompass.Color:=clWhite
      else
        EditCompass.Color:=clRed;
    end;
  end;
end;

procedure ReadKicker;
begin
  if FKicker.CBOnlyKicker.Checked then begin
    Daisy.SendData(0, chr(PACK_TYPE_READ_VCAP), 7);
  end else begin
    Daisy.SendData(3, chr(PACK_TYPE_READ_VCAP), 7);
  end;
end;


procedure TFKicker.TimerTimer(Sender: TObject);
var
  s: string;
begin
              //Daisy.SendData(StrToIntDef(EditDaisyKickPosition.Text,0), chr(PACK_TYPE_READ_VCAP), 4);
  ReadKicker;
  {$ifdef MDEC}
  if CBLan.Checked then
    FOmni3.WriteSerialUDP(Daisy.Flush)
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

procedure TFKicker.FormCreate(Sender: TObject);
var
  SessionPropsList: TStringList;
  SessionPropsFileName: string;
begin
  SessionPropsFileName := ExtractFilePath(Application.ExeName)+FMain.DataDir+'/SessionPropsKicker.txt';
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
  FormStorage.IniFileName:=extractfilepath(application.exename)+FMain.DataDir+'/Kicker.ini';
  FormStorage.Restore;
  ProgReset := true;

  {$ifdef MDEC}
  Fmain.InsertAuxForms(FKicker,'Kicker');
  {$endif}
end;

procedure TFKicker.LBDebugClick(Sender: TObject);
begin
  EditDebug.Text:=LBDebug.Items[LBDebug.ItemIndex];
end;

procedure TFKicker.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CBRun.Checked:=False;
  ComPort.Close;
  {$ifdef MDEC}
  Fmain.AuxFormClosed('Kicker');
  {$endif}
end;

procedure TFKicker.BKickClick(Sender: TObject);
var
  s: string;
begin
  SendKick(strtofloatdef(editkickpulse.text,0));
  {$ifdef MDEC}
  if CBLan.Checked then
    FOmni3.WriteSerialUDP(Daisy.Flush)
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

procedure TFKicker.BReadEEPROMClick(Sender: TObject);
var
  s: string;
begin
  ReadParsValues;
  {$ifdef MDEC}
  if CBLan.Checked then
    FOmni3.WriteSerialUDP(Daisy.Flush)
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

procedure TFKicker.BWriteEEPROMClick(Sender: TObject);
var
  s: string;
begin
  WriteParsValues;
  {$ifdef MDEC}
  if CBLan.Checked then
    FOmni3.WriteSerialUDP(Daisy.Flush)
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

procedure TFKicker.BReadIDClick(Sender: TObject);
var
  s: string;
begin
  //Daisy.SendData(StrToIntDef(EditDaisyKickPosition.Text,0), chr(PACK_TYPE_ID), 4);
  if CBOnlyKicker.Checked then begin
    Daisy.SendData(0, chr(PACK_TYPE_ID), 4);
  end else begin
    Daisy.SendData(3, chr(PACK_TYPE_ID), 4);
  end;
  {$ifdef MDEC}
  if CBLan.Checked then
    FOmni3.WriteSerialUDP(Daisy.Flush)
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


procedure TFKicker.CBRunClick(Sender: TObject);
begin
  if not CBLan.Checked then
    ComPort.Open;
    
  Timer.Enabled:=CBRun.Checked;
end;

procedure TFKicker.ComPortRxData(Sender: TObject);
begin
  Daisy.ProcessRawData(ComPort.ReadData);

  {if EditDaisyKickPosition.Text <> '3' then begin
    data := ComPort.ReadData;
    AddDebugLine(data);
    // change data[i] to dev 3
    Daisy.ProcessRawData(data);
  end else begin
    Daisy.ProcessRawData(ComPort.ReadData);
  end;}
end;

procedure TFKicker.SendKick(Pulse: double);
var
  CheckSum: byte;
  InternalPulse: integer;
begin
  InternalPulse:=round(Pulse*10);

  CheckSum:= Calc_CheckSum(chr(PACK_TYPE_KICK) +
                 int16ToDaisy(InternalPulse));

  {Daisy.SendData(StrToIntDef(EditDaisyKickPosition.Text,0),
                 chr(PACK_TYPE_KICK) +
                 int16ToDaisy(InternalPulse) +
                 chr(CheckSum), 0); }
  if CBOnlyKicker.Checked then begin
    Daisy.SendData(0,chr(PACK_TYPE_KICK) +
                 int16ToDaisy(InternalPulse) +
                 chr(CheckSum), 0);
  end else begin
    Daisy.SendData(3,chr(PACK_TYPE_KICK) +
                 int16ToDaisy(InternalPulse) +
                 chr(CheckSum), 0);
  end;
end;


procedure TFKicker.WriteParsValues;
var
  eeprom_data: string;
begin
  eeprom_data := StringOfChar(chr(0), 6);

  eeprom_data[EEADDR_MAX_VCAP + 1] := chr(StrToIntDef(EditMaxVCap.Text,80));
  eeprom_data[EEADDR_DUTY_CYCLE + 1] := chr(StrToIntDef(EditDutyCycle.Text,100));
  eeprom_data[EEADDR_MAX_VCAP_LEVEL1 + 1] := chr(StrToIntDef(EditMaxVCapLv1.Text,255));
  eeprom_data[EEADDR_DUTY_CYCLE_LEVEL1 + 1] := chr(StrToIntDef(EditDutyCycleLv1.Text,80));
  eeprom_data[EEADDR_MAX_VCAP_LEVEL2 + 1] := chr(StrToIntDef(EditMaxVCapLv2.Text,255));
  eeprom_data[EEADDR_DUTY_CYCLE_LEVEL2 + 1] := chr(StrToIntDef(EditDutyCycleLv2.Text,80));

  //Daisy.SendData(StrToIntDef(EditDaisyKickPosition.Text,0), chr(PACK_TYPE_WRITE_EEPROM) + chr(0) + eeprom_data, 0);
  if CBOnlyKicker.Checked then begin
    Daisy.SendData(0, chr(PACK_TYPE_WRITE_EEPROM) + chr(0) + eeprom_data, 0);
  end else begin
    Daisy.SendData(3, chr(PACK_TYPE_WRITE_EEPROM) + chr(0) + eeprom_data, 0);
  end;
end;


procedure TFKicker.ReadParsValues;
begin
  if CBOnlyKicker.Checked then begin
    Daisy.SendData(0, chr(PACK_TYPE_READ_EEPROM) + chr(0), 6);
  end else begin
    Daisy.SendData(3, chr(PACK_TYPE_READ_EEPROM) + chr(0), 6);
  end;
end;

initialization
  {$I kicker.lrs}

end.

