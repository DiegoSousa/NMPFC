unit Unit_joystick;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, ExtCtrls, SdpoJoystick;

const
  JoyAxisMaxValue=32767;

type

  { TForm_joystick }

  TForm_joystick = class(TForm)
    CheckBox_TestLoc: TCheckBox;
    Crasy_MF_Test: TCheckBox;
    CheckBox_Connect: TCheckBox;
    Edit_LocalAxis: TEdit;
    Edit_Device: TEdit;
    Edit_LocalButtons: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ScrollBar_V: TScrollBar;
    ScrollBar_Vn: TScrollBar;
    ScrollBar_W: TScrollBar;
    ScrollBar_KickForce: TScrollBar;
    SdpoJoystick: TSdpoJoystick;
    StringGrid_MaxVVnW: TStringGrid;
    Timer: TTimer;
    Timer_Crazy_MFT: TTimer;
    ToggleBox_Kick: TToggleBox;
    procedure CheckBox_ConnectChange(Sender: TObject);
    procedure Crasy_MF_TestChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Timer_Crazy_MFTTimer(Sender: TObject);
  private
    ax: array[0..3] of integer;
    bt: array[0..11] of integer;

    randV, randVn, randW: double;

    function GetkickPulse: integer;
    function GetV: double;
    function GetVn: double;
    function GetW: double;
  public
    property V: double read GetV;
    property Vn: double read GetVn;
    property W: double read GetW;
    property kickPulse: integer read GetkickPulse;
  end; 

var
  Form_joystick: TForm_joystick;

implementation

uses
  unit_Localization;

{ TForm_joystick }

procedure TForm_joystick.CheckBox_ConnectChange(Sender: TObject);
begin
  try
    if CheckBox_Connect.Checked then begin
      SdpoJoystick.DeviceLin:=Edit_Device.Text;
      Timer.Enabled:=true;
      SdpoJoystick.Init;
    end
    else begin
      Timer.Enabled:=false;
      SdpoJoystick.Close;
    end;
  except
    on E: Exception do begin
      SdpoJoystick.Close;
      CheckBox_Connect.Checked:=false;
      showmessage(E.Message);
    end;
  end;
end;

procedure TForm_joystick.Crasy_MF_TestChange(Sender: TObject);
begin
  if Crasy_MF_Test.Checked then begin
    Timer_Crazy_MFT.Enabled:=true;
  end
  else begin
    Timer_Crazy_MFT.Enabled:=false;
  end;
end;

procedure TForm_joystick.FormCreate(Sender: TObject);
begin
  WindowState:=wsNormal;
  decimalSeparator:='.';
end;

procedure TForm_joystick.TimerTimer(Sender: TObject);
var
  i: integer;
begin
  if SdpoJoystick.Active and Showing then begin
    SdpoJoystick.Read(ax,bt);
    Edit_LocalAxis.Text:='A ';
    Edit_LocalButtons.Text:='B ';
    for i:=0 to 3 do begin
      Edit_LocalAxis.Text:=Edit_LocalAxis.Text+IntToStr(ax[i])+' ';
    end;
    for i:=0 to 11 do begin
      Edit_LocalButtons.Text:=Edit_LocalButtons.Text+IntToStr(bt[i])+' ';
    end;
    ScrollBar_V.Position:=-ax[1];
    ScrollBar_Vn.Position:=ax[0];
    ScrollBar_W.Position:=ax[2];

    if (bt[1]=1) and (bt[6]=1) and (bt[7]=1) then begin
      ToggleBox_Kick.Checked:=true;
    end
    else begin
      ToggleBox_Kick.Checked:=false;
    end;
    if (bt[4]=1) or (bt[5]=1) then begin
      if bt[4]=1 then begin
        ScrollBar_KickForce.Position:=ScrollBar_KickForce.Position-1;
      end
      else begin
        ScrollBar_KickForce.Position:=ScrollBar_KickForce.Position+1;
      end;
    end;
  end;
end;

procedure TForm_joystick.Timer_Crazy_MFTTimer(Sender: TObject);
begin
  randV:=(Random(JoyAxisMaxValue*2)-JoyAxisMaxValue)/JoyAxisMaxValue*StrToFloatDef(StringGrid_MaxVVnW.Cells[1,1],0.5);
  randVn:=(Random(JoyAxisMaxValue*2)-JoyAxisMaxValue)/JoyAxisMaxValue*StrToFloatDef(StringGrid_MaxVVnW.Cells[1,2],0.5);
  randW:=(Random(JoyAxisMaxValue*2)-JoyAxisMaxValue)/JoyAxisMaxValue*StrToFloatDef(StringGrid_MaxVVnW.Cells[1,3],1.2);
end;

function TForm_joystick.GetV: double;
begin
  if Crasy_MF_Test.Checked then begin
    Result:=randV;
    exit;
  end;

  if SdpoJoystick.Active then begin
    if (not CheckBox_TestLoc.Checked) or (Form_Localization.Ok) then begin
      Result:=-ax[1]/JoyAxisMaxValue*StrToFloatDef(StringGrid_MaxVVnW.Cells[1,1],0.5);
    end
    else begin
      Result:=Form_Localization.AdvV;
    end;
  end
  else begin
    Result:=0;
  end;
end;

function TForm_joystick.GetkickPulse: integer;
begin
  if ToggleBox_Kick.Checked then begin
    Result:=ScrollBar_KickForce.Position;
    ToggleBox_Kick.Checked:=false;
  end
  else begin
    Result:=0;
  end;
end;

function TForm_joystick.GetVn: double;
begin
  if Crasy_MF_Test.Checked then begin
    Result:=randVn;
    exit;
  end;

  if SdpoJoystick.Active then begin
    if (not CheckBox_TestLoc.Checked) or (Form_Localization.Ok) then begin
      Result:=-ax[0]/JoyAxisMaxValue*StrToFloatDef(StringGrid_MaxVVnW.Cells[1,2],0.5);
    end
    else begin
      Result:=Form_Localization.AdvVn;
    end;
  end
  else begin
    Result:=0;
  end;
end;

function TForm_joystick.GetW: double;
begin
  if Crasy_MF_Test.Checked then begin
    Result:=randW;
    exit;
  end;

  if SdpoJoystick.Active then begin
    if (not CheckBox_TestLoc.Checked) or (Form_Localization.Ok) then begin
      Result:=-ax[2]/JoyAxisMaxValue*StrToFloatDef(StringGrid_MaxVVnW.Cells[1,3],1.2);
    end
    else begin
      Result:=Form_Localization.Advw;
    end;
  end
  else begin
    Result:=0;
  end;
end;

initialization
  {$I unit_joystick.lrs}

end.

