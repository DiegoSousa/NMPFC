unit Unit_RolesAux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, TAGraph, TASeries;

type

  { TForm_Roles }

  TForm_Roles = class(TForm)
    Button_Clear: TButton;
    Chart1: TChart;
    Chart1LineSeries_Input: TLineSeries;
    Chart1LineSeries_Output: TLineSeries;
    Chart1LineSeries_Ref: TLineSeries;
    Chart2: TChart;
    Chart2LineSeries_State: TLineSeries;
    CheckBox_Enable: TCheckBox;
    CheckBox_Visible: TCheckBox;
    procedure Button_ClearClick(Sender: TObject);
    procedure CheckBox_VisibleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FChart1Iterator1, FChart1Iterator2: integer;
  public
    procedure AddDataLogCatchBall(Ref, Input, Output: double);
    procedure AddDtaLogCathBallState(State: integer);
  end; 

var
  Form_Roles: TForm_Roles;

implementation

{ TForm_Roles }

procedure TForm_Roles.Button_ClearClick(Sender: TObject);
begin
  FChart1Iterator1:=0;
  FChart1Iterator2:=0;
  Chart1LineSeries_Ref.Clear;
  Chart1LineSeries_Input.Clear;
  Chart1LineSeries_Output.Clear;
  Chart2LineSeries_State.Clear;
  CheckBox_Visible.Checked:=false;
  CheckBox_VisibleChange(Self);
end;

procedure TForm_Roles.CheckBox_VisibleChange(Sender: TObject);
begin
  Chart1.Visible:=CheckBox_Visible.Checked;
  Chart2.Visible:=CheckBox_Visible.Checked;
end;

procedure TForm_Roles.FormCreate(Sender: TObject);
begin
  //Button_ClearClick(Self);
  //Show;
end;

procedure TForm_Roles.AddDataLogCatchBall(Ref, Input, Output: double);
begin
  if not CheckBox_Enable.Checked then begin
    exit;
  end;
  //Chart1LineSeries_Ref.AddXY(FChart1Iterator,Ref);
  //Chart1LineSeries_Input.AddXY(FChart1Iterator,Input);
  //Chart1LineSeries_Output.AddXY(FChart1Iterator,Output);
  //inc(FChart1Iterator1)
end;

procedure TForm_Roles.AddDtaLogCathBallState(State: integer);
begin
  if not CheckBox_Enable.Checked then begin
    exit;
  end;
  //Chart2LineSeries_State.AddXY(FChart1Iterator,State);
  //Inc(FChart1Iterator2);
end;

initialization
  {$I unit_rolesaux.lrs}

end.

