unit Param;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, LResources,
  ComCtrls, StdCtrls, Coach, ExtCtrls, IniPropStorage, DecConsts,
  CheckLst;

  //RLan

const
  Ktic_rad = 5e-4;
  Ktic_rad_out = 0.0501; //(1/2.08)*(2*pi/60);

  Ktic_pwm = 5.3;

type
  paramType=(paramSoftKick,paramNoRoleChange,paramNoFieldClear,
             paramDrawObstacleAvoid,paramKickConfidence,
             paramDefensive, param2AtackerSupport,
             paramWingmanTactic, paramAtackerPassesOnTopLine,
             paramAdvanceDefense, paramDoubleMamao,
             paramUseBarrierAux
             );

const
  CparamTypeString: array[paramType] of string =(
     'soft kicker', 'no role change', 'no field clear',
     'draw obstacle avoidance', 'kick with more confidence',
     'defensive strategy', 'second atacker supports first',
     'wingman tactic', 'pass to atacker 2 on top line',
     'advance defense', 'double Mamao', 'use 2 on the barrier');

type

  { TFParam }

  TFParam = class(TForm)
    BCommand: TButton;
    ChListPar: TCheckListBox;
    EditBadCount: TEdit;
    EditBadIPCount: TEdit;
    EditCommand: TEdit;
    EditIPBase: TEdit;
    EditSimTwoIP: TEdit;
    EditSimTwoListenPort: TEdit;
    EditSimTwoPort: TEdit;
    EditSuperIP: TEdit;
    EditVisionIP: TEdit;
    FormStorage: TIniPropStorage;
    BOk: TButton;
    Label1: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    PageControl: TPageControl;
    RGRobotNumber: TRadioGroup;
    RGSide: TRadioGroup;
    RGTeamColor: TRadioGroup;
    TabConfig: TTabSheet;
    TabTactic: TTabSheet;
    procedure ChListParClickCheck(Sender: TObject);
    procedure EditSimTwoListenPortEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure FillListNames;
    procedure LoadArray;
    procedure RGRobotNumberClick(Sender: TObject);
    procedure RGSideClick(Sender: TObject);
    procedure RGTeamColorClick(Sender: TObject);
    procedure SaveArray;
    procedure ListToArray;
    procedure ArrayToList;
  private
  public

// Adicionado por Scolari - 11/11/04 //////////
    NetOutBufferParam: TUDPBuffer;


/////////////////////////////////////////////////
    procedure ComponentsToParameters;

  end;

var
  FParam: TFParam;

  Params: array[paramType] of boolean;

  BallFrictionCoef: double=0.95;
  BallQualityDecay: double=0.995;
  BallLinearQualityDecay: double=5;
  BallDistancekick: double=0.4;
  Ballqualitytreshold: double=100;
  kickforce: string='75';
  kickforce1: string='20';
  kickforce2: string='15';
  kickforce3: string='10';
  distkickforce: double=6;
  distkickforce1: double=4;
  distkickforce2: double=2;
  distkickforce3: double=0;
  kickdelay: integer=2000;

  WheeltoCenterdist: double=0.185;
  Wheel1Diameter: double=0.1;
  Wheel2Diameter: double=0.1;
  Wheel3Diameter: double=0.1;

  CompassOffset: double=0;

  Xstart: double=0;
  Ystart: double=0;
  Xstop: double=0;
  Ystop: double=0;

  PIDK: double=1;
  PIDInvTi: double=0;
  PIDTd: double=0;
  PIDForward: double=0;
  PIDLambdaRef: double=0;
  PIDLambdaMeasure: double=0;
  PIDPeriod: double=0;

  tkSpeed: double;
  tkToBallDist: double;

  coefA:double=0.4048;
  coefB:double=2;
  coefC:double=5;

implementation

uses  Utils,StrUtils, Field;


function FToStr(f: double): string;
begin
  result:=Format('%.6f',[f]);
  result:=AnsiReplaceStr(TrimRight(AnsiReplaceStr(result,'0',' ')),' ','0');
  if result[length(result)]='.' then result:=result+'0';
end;


procedure TFParam.FormCreate(Sender: TObject);
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
  FormStorage.Restore;
  ComponentsToParameters;
  FCoach.InsertAuxForms(FParam,'Parameters');
  LoadArray;
  FillListNames;
  ArrayToList;
end;

procedure TFParam.ChListParClickCheck(Sender: TObject);
begin
  ListToArray;
end;

procedure TFParam.EditSimTwoListenPortEditingDone(Sender: TObject);
begin

end;

procedure TFParam.ListToArray;
var i: paramType;
begin
  for i:=Low(paramType) to High(paramType) do begin
    Params[i]:=ChListPar.Checked[ord(i)];
  end;
end;

procedure TFParam.ArrayToList;
var i: paramType;
begin
  for i:=Low(paramType) to High(paramType) do begin
    ChListPar.Checked[ord(i)]:=Params[i];
  end;
end;

procedure TFParam.SaveArray;
var sl: TStringList;
    i: paramType;
begin
  sl:=TStringList.Create;
  try
    for i:=Low(paramType) to High(paramType) do begin
      sl.Values['param'+inttostr(ord(i))]:=inttostr(ord(Params[i]));
    end;
    sl.SaveToFile(extractfilepath(application.ExeName)+FCoach.DataDir+'/Params.ini');
  finally
    sl.Free;
  end;
end;

procedure TFParam.LoadArray;
var sl: TStringList;
    i: paramType;
begin
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(extractfilepath(application.ExeName)+FCoach.DataDir+'/Params.ini');
    for i:=Low(paramType) to High(paramType) do begin
      Params[i]:=(strtointdef(sl.values['param'+inttostr(ord(i))],0)<>0);
    end;
  except
  end;
  sl.Free;
end;

procedure TFParam.RGRobotNumberClick(Sender: TObject);
begin
  myNumber:=RGRobotNumber.ItemIndex;
  RobotState[myNumber].teta:=pi/2;
  RobotState[myNumber].x:=0;
  RobotState[myNumber].y:=-FieldDims.fieldwidth/2;
end;

procedure TFParam.RGSideClick(Sender: TObject);
begin
  AttackGoal:=TGoalColor(RGSide.itemindex);
  FCoach.caption:='5dpo Coach 2011 V1 - Attack:' + CGoalColorStr[AttackGoal];
end;

procedure TFParam.RGTeamColorClick(Sender: TObject);
begin
  TeamColor:=TTeamColor(RGTeamColor.itemindex);
end;

procedure TFParam.FillListNames;
var i: paramType;
begin
  ChListPar.Items.Clear;
  for i:=Low(paramType) to High(paramType) do begin
    ChListPar.Items.Add(CparamTypeString[i]);
  end;
end;

procedure TFParam.FormClose(Sender: TObject);
begin
  ListToArray;
  SaveArray;

  FCoach.AuxFormClosed('Parameters');
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

procedure TFParam.ComponentsToParameters;
begin
  myNumber:=RGRobotNumber.ItemIndex;
end;

procedure TFParam.BOkClick(Sender: TObject);
begin
  ComponentsToParameters;
end;


initialization
  {$I Param.lrs}

end.
