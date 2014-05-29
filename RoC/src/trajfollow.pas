//------------------------------------------------------------------------------
//
// TrajFollow.pas
//
//------------------------------------------------------------------------------
//
// This unit implements two trajetories: hook and gate. The follow steps should
//be done in the program:
// 1. Criate form
// 2. Add trajetories (Hook and gate)
// 3. Select trajetory
// 4. Finish form
// 5. Create trajetory
// 6. Recieve robot position
// 7. Save trajetory in a file
//
//------------------------------------------------------------------------------

unit TrajFollow;

{$mode objfpc}{$H+}
{$define MDEC}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, IniPropStorage, ComCtrls, ExtCtrls, Main, Tasks, Robots, Roles, genTraj,
  obsavoid, actions, MPC;

type

  { TFTrajFollow }

  TFTrajFollow = class(TForm)
    ButtonGenerate: TButton;
    ButtonRun: TButton;
    ButtonSaveTrajectory: TButton;
    ButtonSwitchSpeed: TButton;
    CheckBoxAtStart: TCheckBox;
    CheckBoxSpeedRef: TCheckBox;
    CheckBoxVarTeta: TCheckBox;
    Edit90Deg4CL1: TEdit;
    Edit90Deg4CL2: TEdit;
    Edit90Deg4CL3: TEdit;
    Edit90Deg4CL4: TEdit;
    Edit90Deg4CL5: TEdit;
    Edit90Deg4CStartX: TEdit;
    Edit90Deg4CStartY: TEdit;
    EditHookCircleLength: TEdit;
    EditHookCircleRadius: TEdit;
    EditHookEndX: TEdit;
    EditHookEndY: TEdit;
    EditHookStartX: TEdit;
    EditHookStartY: TEdit;
    EditLoadFromFile: TEdit;
    EditSpeed: TEdit;
    EditSpeed1: TEdit;
    EditSpeedOnTg: TEdit;
    EditTrajectoryFile: TEdit;
    FormStorage: TIniPropStorage;
    GroupBoxGenerate: TGroupBox;
    GroupBoxRun: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    ListBoxTrajectories: TListBox;
    PageControlTrajectories: TPageControl;
    RadioButtonGoToXYT: TRadioButton;
    RadioButtonMPC: TRadioButton;
    RadioGroupController: TRadioGroup;
    Tab90Deg4Corners: TTabSheet;
    TabHook: TTabSheet;
    TabLoadFromFile: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure ButtonGenerateClick(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure ButtonSaveTrajectoryClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure ButtonSwitchSpeedClick(Sender: TObject);
    procedure CheckBoxSpeedRefChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxTrajectoriesSelectionChange(Sender: TObject; User: boolean
      );
    procedure PageControlTrajectoriesChange(Sender: TObject);
    procedure RadioGroupControllerClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FTrajFollow: TFTrajFollow;
  ProgReset: boolean;

implementation

{ TFTrajFollow }

//----------------------------------------------------------------------------
// Auxiliary Procedures
//----------------------------------------------------------------------------


//----------------------------------------------------------------------------
// Form Procedures
//----------------------------------------------------------------------------

//editbox to float
function EditToFloatDef(edit: TEdit; default: double): double;
begin
  try
    result:=strtofloat(edit.text);
  except
    result:=default;
    edit.text:=Format('%.8g',[default]);
  end;
end;

//Create form, add to main form
procedure TFTrajFollow.FormCreate(Sender: TObject);
var
  SessionPropsList: TStringList;
  SessionPropsFileName: string;
begin

  //SessionProps code
  SessionPropsFileName := ExtractFilePath(Application.ExeName)+FMain.DataDir+'/SessionPropsTrajFollow.txt';
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
  FormStorage.IniFileName:=extractfilepath(application.exename)+FMain.DataDir+'/TrajFollow.ini';
  FormStorage.Restore;
  ProgReset := true;

  //Add trajectories to listbox
  ListBoxTrajectories.Items.Add('Load from file...');
  ListBoxTrajectories.Items.Add('90 Degree 4 Corners');
  ListBoxTrajectories.Items.Add('Hook');

  //default values
  staticSpeed := EditToFloatDef(EditSpeed,1);
  staticSpeedOnTarget:= EditToFloatDef(EditSpeedOnTg,0);


  {$ifdef MDEC}
  Fmain.InsertAuxForms(FTrajFollow,'Follow Trajectory');
  {$endif}

end;

//select trajectories
procedure TFTrajFollow.ListBoxTrajectoriesSelectionChange(Sender: TObject; User: boolean);
var
   index : integer;
begin

   index := ListBoxTrajectories.ItemIndex;
   PageControlTrajectories.TabIndex:=index;

end;

procedure TFTrajFollow.PageControlTrajectoriesChange(Sender: TObject);
begin

end;

procedure TFTrajFollow.RadioGroupControllerClick(Sender: TObject);
begin
end;

//Close Form
procedure TFTrajFollow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$ifdef MDEC}
  Fmain.AuxFormClosed('Follow Trajectory');
  {$endif}
end;

//Generate Trajectory
procedure TFTrajFollow.ButtonGenerateClick(Sender: TObject);
var
   x_center,y_center,x_i,y_i,x_f,y_f,radius, length,xmov,ymov : double;
   index : integer;
   i : integer;
   testTrajectory : TTrajectory;
begin

     // set index
     index := ListBoxTrajectories.ItemIndex;

     case index of
          0:
            generateTrajectoryLoadFromFile(staticTraj,EditLoadFromFile.Text);
          1:
          begin
               generateTrajectory90Deg4Corners(staticTraj,EditToFloatDef(Edit90Deg4CStartX,0),
               EditToFloatDef(Edit90Deg4CStarty,0),EditToFloatDef(Edit90Deg4CL1,1)
               ,EditToFloatDef(Edit90Deg4CL2,1),EditToFloatDef(Edit90Deg4CL3,1)
               ,EditToFloatDef(Edit90Deg4CL4,1),EditToFloatDef(Edit90Deg4CL5,1),CheckBoxVarTeta.Checked);
          end;
          2:
          begin
            x_i := EditToFloatDef(EditHookStartX,0);
            y_i := EditToFloatDef(EditHookStartY,0);
            x_f := EditToFloatDef(EditHookEndX,0);
            y_f := EditToFloatDef(EditHookEndY,0);
            radius := EditToFloatDef(EditHookCircleRadius,1);
            length := EditToFloatDef(EditHookCircleLength,1);
            generateTrajectoryHook(staticTraj,x_i,y_i,x_f,y_f,radius,length,RobotState[myNumber].teta,2,0.5);
          end;
     end;

     //start trajectory at robot position
     if CheckBoxAtStart.Checked then begin
        with RobotState[myNumber] do begin
             xmov := (x-staticTraj.pts[0].x);
             ymov := (y-staticTraj.pts[0].y);
             for i := 0 to staticTraj.count-1 do begin
                 staticTraj.pts[i].x:=staticTraj.pts[i].x + xmov;
                 staticTraj.pts[i].y:=staticTraj.pts[i].y + ymov;
             end;
        end;

     end;

end;

procedure TFTrajFollow.Button1Click(Sender: TObject);
begin
  generateTrajectoryLoadFromFile(staticTraj,'Trajectories/meh');
end;

procedure TFTrajFollow.ButtonRunClick(Sender: TObject);
begin

     if RadioButtonGoToXYT.Checked then
        controllerIndex := 0
     else
     if RadioButtonMPC.Checked then
        controllerIndex :=1;


     //Set run flag
     if(RolePars[myNumber].runningTraj) then begin
        RolePars[myNumber].runningTraj:=false;
        ButtonRun.Caption:='Run';
     end else begin
        RolePars[myNumber].runningTraj:=true;
        staticSpeed := EditToFloatDef(EditSpeed,2);
        staticSpeedOnTarget:= EditToFloatDef(EditSpeedOnTg,0);
        ButtonRun.Caption:='Stop';
     end;

end;

//Save trajectory to file
procedure TFTrajFollow.ButtonSaveTrajectoryClick(Sender: TObject);
begin
   saveTrajToFile(staticTraj,'Trajectories/'+EditTrajectoryFile.Text);
end;

procedure TFTrajFollow.FormClick(Sender: TObject);
begin

end;

procedure TFTrajFollow.ButtonSwitchSpeedClick(Sender: TObject);
var
   tempSpeed : string;
begin

     tempSpeed:=EditSpeed.Text;
     EditSpeed.Text:=EditSpeed1.Text;
     EditSpeed1.Text:= tempSpeed;
     staticSpeed := EditToFloatDef(EditSpeed,2);

end;

procedure TFTrajFollow.CheckBoxSpeedRefChange(Sender: TObject);
begin
  EditSpeed.Enabled:=not(CheckBoxSpeedRef.Checked);
  EditSpeed1.Enabled:=not(CheckBoxSpeedRef.Checked);
  staticTraj.varSpeed:=CheckBoxSpeedRef.Checked;
end;

initialization
  {$I trajfollow.lrs}

end.

