unit Camera;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Forms, Dialogs,
  StdCtrls, ExtCtrls, Main, Math, LResources,
  Controls, IniPropStorage;

type

  { TFCamera }

  TFCamera = class(TForm)
    //FormStorage: TFormStorage;
    BCamsRefresh: TButton;
    FormStorage: TIniPropStorage;
    Label2: TLabel;
    EditLCamAngle: TEdit;
    Label3: TLabel;
    EditLCamDist: TEdit;
    EditLCamHeight: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    EditLCamMt: TEdit;
    Label15: TLabel;
    EditCamZBall: TEdit;
    EditCamZrobot: TEdit;
    EditCamZadv: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    EditCamZBlack: TEdit;
    EditCamZGoal: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure BCamsRefreshClick(Sender: TObject);
  private
    { Private declarations }
  public
    CamZBall, CamZRobot, CamZadv, CamZGoal, CamZBlack: double;
    LCamAngle, LCamDist, LCamHeight, LCamMt: double;

    procedure CamsRefresh;

    procedure CamRoTetaZToXYD(ro,teta,zw: double; var d,xw,yw: double);
    procedure CamZDtoRo(zw, d: double; var ro: double);
  end;

var
  FCamera: TFCamera;
  ColorZ: array[0..CTrackColors-1] of double;

implementation

uses Utils;

procedure TFCamera.FormCreate(Sender: TObject);
var
  SessionPropsList: TStringList;
  SessionPropsFileName: string;
begin
  SessionPropsFileName := ExtractFilePath(Application.ExeName)+FMain.DataDir+'/SessionPropsCamera.txt';
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
  FormStorage.IniFileName:=FMain.FormStorage.IniFileName;
  FormStorage.Restore;
  //FormStorage.LoadProps;
  Fmain.InsertAuxForms(FCamera,'Camera');
end;

procedure TFCamera.FormClose(Sender: TObject);
begin
  Fmain.AuxFormClosed('Camera');
end;


procedure TFCamera.BCamsRefreshClick(Sender: TObject);
begin
  CamsRefresh;
end;


//------------------------------------------------------------------------------
// Camera stuff
procedure TFCamera.CamRoTetaZToXYD(ro,teta,zw: double; var d,xw,yw: double);
begin
  d:=tan(ro)*(LCamHeight-zw);
  xw:=d*cos(teta+LCamAngle*pi/180);
  yw:=d*sin(teta+LCamAngle*pi/180);
end;

procedure TFCamera.CamZDtoRo(zw, d: double; var ro: double);
var t: double;
begin
  //d:=tan(ro)*(LCamHeight-zw);
  //ro := atan(d/(LCamHeight-zw));
  t := (LCamHeight-zw);
  if t <> 0 then begin
    ro := arctan(d/t);
  end else begin
    ro := pi/2;
  end;
end;


procedure TFCamera.CamsRefresh;
begin
  try
    LCamAngle:=strtofloat(EditLCamAngle.text);
    LCamDist:=strtofloat(EditLCamDist.text);
    LCamHeight:=strtofloat(EditLCamHeight.text);
    LCamMt:=strtofloat(EditLCamMt.text);


    CamZBall:=strtofloat(EditCamZBall.text);
    CamZRobot:=strtofloat(EditCamZrobot.text);
    CamZadv:=strtofloat(EditCamZadv.text);
    CamZGoal:=strtofloat(EditCamZGoal.text);
    CamZBlack:=strtofloat(EditCamZBlack.text);

    ColorZ[0]:=CamZBall;
    ColorZ[1]:=CamZGoal;
    ColorZ[2]:=CamZGoal;
//    if Fmain.RGTeamColor.ItemIndex=0 then begin
      ColorZ[3]:=CamZRobot;
      ColorZ[4]:=CamZadv;
//    end else begin
      ColorZ[3]:=CamZadv;
      ColorZ[4]:=CamZRobot;
//    end;
    ColorZ[5]:=CamZBlack;
  finally
  end;
end;

initialization
  {$I Camera.lrs}

end.
