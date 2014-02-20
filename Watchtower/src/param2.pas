unit Param2;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, LResources,
  ComCtrls, StdCtrls, Coach, ExtCtrls, IniPropStorage, DecConsts;

type

  { TFParam2 }

  TFParam2 = class(TForm)
    BOk1: TButton;
    EditIPBase: TEdit;
    EditSimTwoIP: TEdit;
    EditSimTwoListenPort: TEdit;
    EditSimTwoPort: TEdit;
    FormStorage: TIniPropStorage;
    Label1: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl: TPageControl;
    RGRobotNumber: TRadioGroup;
    TabConfig: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure RGRobotNumberClick(Sender: TObject);
  private
  public

  end;

var
  FParam2: TFParam2;

implementation

uses  Utils,StrUtils, Field;


function FToStr(f: double): string;
begin
  result:=Format('%.6f',[f]);
  result:=AnsiReplaceStr(TrimRight(AnsiReplaceStr(result,'0',' ')),' ','0');
  if result[length(result)]='.' then result:=result+'0';
end;


procedure TFParam2.FormCreate(Sender: TObject);
var
  SessionPropsList: TStringList;
  SessionPropsFileName: string;
begin
  SessionPropsFileName := ExtractFilePath(Application.ExeName)+FCoach.DataDir+'/SessionPropsParam.txt';
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
  FormStorage.IniFileName:=FCoach.FormStorage.IniFileName;
  myNumber:=RGRobotNumber.ItemIndex;
end;

procedure TFParam2.RGRobotNumberClick(Sender: TObject);
begin
  myNumber:=RGRobotNumber.ItemIndex;
  RobotState[myNumber].teta:=pi/2;
  RobotState[myNumber].x:=0;
  RobotState[myNumber].y:=-FieldDims.fieldwidth/2;
end;

function EditToFloatDef(edit: TEdit; default: double): double;
begin
  try
    result:=strtofloat(edit.text);
  except
    result:=default;
    edit.text:=Format('%.8g',[default]);
  end;
end;

procedure TFParam2.BOkClick(Sender: TObject);
begin
  myNumber:=RGRobotNumber.ItemIndex;
end;



initialization
  {$I param2.lrs}

end.
