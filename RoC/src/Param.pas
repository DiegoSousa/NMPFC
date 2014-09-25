unit Param;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, LResources,
  ComCtrls, StdCtrls, Main, ExtCtrls, inifiles, RLan, IniPropStorage, DecConsts,
  CheckLst;

const

  //V_SCALE_FACTOR = 1;

  //NumMotors = 3;
  //NumDrivers = 3;

  //Ktic_rad = 0.0125;   //(1/2.08)*(2*pi/60)*(1/4);
                        // (ticks->rpm)*(rpm->rad/s)*(4x da amostragem)
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

{type
  TDriverStateMode=(dsmPWM,dsmPID);

type

  TDriverState=record
    pwm, out_pwm: integer;
    ref: integer;
    filt_ev, filt_ref, K, invTi, TD, alpha: single;
    Vact, Iact: double;
    enc: Word;


    Mode: TDriverStateMode;
    AlphaPos,AlphaNeg,BetaPos,BetaNeg: double;

  end;  }


{  TDriverState=record
    Mode: TDriverStateMode;
    pwm: integer;
    ref, PID_period: integer;
    Lamb_m, Lamb_ref, K, invTi, TD, forw: single;
    Imax, Iact: double;
    encA, encB: Word;

    osc_period, osc_on_time: integer;
    reset_count: integer;

    last_eeprom_read: integer;
  end;
}


type

  { TFParam }

  TFParam = class(TForm)
    BCommand: TButton;
    ButtonSetStartStop: TButton;
    ChListPar: TCheckListBox;
    E_minKick: TEdit;
    E_slopeKick: TEdit;
    E_originKick: TEdit;
    E_speedMin: TEdit;
    E_Forward: TEdit;
    EditXstart: TEdit;
    EditYstart: TEdit;
    EditXstop: TEdit;
    EditYstop: TEdit;
    EditKeeperWLine: TEdit;
    EditKickBallDist: TEdit;
    EditMaxLinearAcceleration: TEdit;
    EditMaxAngularAcceleration: TEdit;
    EditRobotSpace: TEdit;
    ETaskBehindBallSpeed: TEdit;
    ETaskBehindBallDistToBall: TEdit;
    EditSimTwoListenPort: TEdit;
    EditSimTwoIP: TEdit;
    EditSimTwoPort: TEdit;
    EditAvgRobotSpeed1: TEdit;
    EditBadCount: TEdit;
    EditBadIPCount: TEdit;
    editballdistkick1: TEdit;
    EditBallSpace: TEdit;
    EditBallLinearQualityDecay1: TEdit;
    EditBallQualityDecay1: TEdit;
    editballqualitytreshold1: TEdit;
    EditCentralLineX1: TEdit;
    EditCommand: TEdit;
    EditDefenderLineX1: TEdit;
    Editdistkickforce4: TEdit;
    Editdistkickforce5: TEdit;
    Editdistkickforce6: TEdit;
    Editdistkickforce7: TEdit;
    EditKeeperLineX1: TEdit;
    EditKeeperMaxY1: TEdit;
    Editkickforce4: TEdit;
    Editkickforce5: TEdit;
    Editkickforce6: TEdit;
    Editkickforce7: TEdit;
    EditLocQuality1: TEdit;
    EditMaxIntersectTime: TEdit;
    EditCompassOffset: TEdit;
    EditMaxRobotSpeed: TEdit;
    EditAvgRobotSpeed: TEdit;
    EditMaxRobotSpeed1: TEdit;
    EditSuperIP: TEdit;
    Edittimedelay1: TEdit;
    EditVisionIP: TEdit;
    EditWheel1Diameter1: TEdit;
    EditWheel2Diameter1: TEdit;
    EditWheel3Diameter1: TEdit;
    EditWheeltoCenterdist1: TEdit;
    FormStorage: TIniPropStorage;
    Label1: TLabel;
    Label10: TLabel;
    Label13: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label64: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    //FormStorage: TFormStorage;
    PageControl: TPageControl;
    BOk: TButton;
    PageControl1: TPageControl;
    RGRobotNumber: TRadioGroup;
    RGSide: TRadioGroup;
    RGTeamColor: TRadioGroup;
    TabBall: TTabSheet;
    Label11: TLabel;
    EditBallFrictionCoef: TEdit;
    Label12: TLabel;
    EditBallQualityDecay: TEdit;
    Label14: TLabel;
    TabBall1: TTabSheet;
    TabConfig1: TTabSheet;
    TabKeeperLineY: TTabSheet;
    EditBallLinearQualityDecay: TEdit;
    Label15: TLabel;
    EditKeeperLine: TEdit;
    Label16: TLabel;
    EditLastDefenderDistToArea: TEdit;
    TabRobot: TTabSheet;
    Label26: TLabel;
    EditWheel1Diameter: TEdit;
    Label17: TLabel;
    EditWheel2Diameter: TEdit;
    Label25: TLabel;
    EditWheeltoCenterdist: TEdit;
    Label38: TLabel;
    EditWheel3Diameter: TEdit;
    Label49: TLabel;
    editballdistkick: TEdit;
    Label50: TLabel;
    editballqualitytreshold: TEdit;
    Label51: TLabel;
    Label52: TLabel;
    Editkickforce: TEdit;
    Editkickforce2: TEdit;
    Label53: TLabel;
    Label54: TLabel;
    EditLastDefenderLine: TEdit;
    EditOtherDefenderLine: TEdit;
    Label55: TLabel;
    Editkickforce3: TEdit;
    Label63: TLabel;
    EditStopDistance: TEdit;
    Edittimedelay: TEdit;
    Label22: TLabel;
    Editkickforce1: TEdit;
    Label23: TLabel;
    Editdistkickforce: TEdit;
    Editdistkickforce1: TEdit;
    Editdistkickforce2: TEdit;
    Editdistkickforce3: TEdit;
    TabKeeperLineY1: TTabSheet;
    TabConfig: TTabSheet;
    TabRoles: TTabSheet;
    TabBehindBall: TTabSheet;
    TabTasks: TTabSheet;
    TabTactic: TTabSheet;
    procedure ButtonSetStartStopClick(Sender: TObject);
    procedure ChListParClickCheck(Sender: TObject);
    procedure EditBadIPCountChange(Sender: TObject);
    procedure EditBallSpaceChange(Sender: TObject);
    procedure EditIPBaseChange(Sender: TObject);
    procedure EditSimTwoIPChange(Sender: TObject);
    procedure EditSimTwoListenPortChange(Sender: TObject);
    procedure EditSimTwoListenPortEditingDone(Sender: TObject);
    procedure EditSimTwoPortChange(Sender: TObject);
    procedure EditSuperIPChange(Sender: TObject);
    procedure EditVisionIPChange(Sender: TObject);
    procedure EditXstartChange(Sender: TObject);
    procedure ETaskBehindBallSpeedExit(Sender: TObject);
    procedure ETaskBehindBallDistToBallExit(Sender: TObject);
    procedure E_minKickChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure BtnWritePIDClick(Sender: TObject);
    procedure BtnreadPIDClick(Sender: TObject);
    procedure BtnreadkickerClick(Sender: TObject);
    procedure BtnwritekickerClick(Sender: TObject);
    procedure FillListNames;
    procedure Label19Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure LoadArray;
    procedure RGRobotNumberClick(Sender: TObject);
    procedure RGSideClick(Sender: TObject);
    procedure RGTeamColorClick(Sender: TObject);
    procedure SaveArray;
    procedure ListToArray;
    procedure ArrayToList;
    procedure TabBehindBallContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
  public

// Adicionado por Scolari - 11/11/04 //////////
    NetOutBufferParam: TUDPBuffer;


/////////////////////////////////////////////////
    procedure ComponentsToParameters;

  end;

var
  FParam: TFParam;

  //CurDriver: integer;
  Params: array[paramType] of boolean;

  BallFrictionCoef: double=0.95;
  BallQualityDecay: double=0.90;
  BallLinearQualityDecay: double=10;
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

  step_forward: integer=1;
  minVelBall:double=0.1;

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

  vxnTarget:double=0.5;

{//////// Scolari ////////
////////// Piramide parameters //////////////////
  Maxv1 : double=1.5;
  Maxv2 : double=1.5;
  Maxv3 : double=1.5;
  Maxv4 : double=1.5;

  EqPlano1: array[0..3] of double; // = {0,1,-WheelDist,Maxv3}
  EqPlano2: array[0..3] of double; // = (0,1,WheelDist,-Maxv1);
  EqPlano3: array[0..3] of double; // = (1,0,-WheelBackDist,Maxv2);
  EqPlano4: array[0..3] of double; // = (1,0,WheelBackDist,-Maxv4);
  EqPlano5: array[0..3] of double; // = (0,1,-WheelDist,-Maxv1);
  EqPlano6: array[0..3] of double; // = (0,1,WheelDist,Maxv3);
  EqPlano7: array[0..3] of double; // = (1,0,WheelBackDist,Maxv2);
  EqPlano8: array[0..3] of double; // = (1,0,-WheelBackDist,-Maxv4);
//////////////////////////////////////////////////  }

  
  //IRGains: array[0..MaxIRSensors-1] of double = (16.77, 16.77);
  //IRDisps: array[0..MaxIRSensors-1] of double = (0.011293, 0.011293);
  //IROffSets: array[0..MaxIRSensors-1] of double = (47.24, 47.24);

  {LocLineDistTresh: double=1.0;
  LocLineAngleTresh: double=30/180*pi;
  LocLineDistCov: double=0.5;
  LocLineAngleCov: double=0.05;

  LocPoleDistTresh: double=10.0;
  LocPoleAngleTresh: double=30/180*pi;
  LocPoleDistCov: double=0.5;
  LocPoleAngleCov: double=0.05;}

  PIDK: double=1;
  PIDInvTi: double=0;
  PIDTd: double=0;
  PIDForward: double=0;
  PIDLambdaRef: double=0;
  PIDLambdaMeasure: double=0;
  PIDPeriod: double=0;

// Por Scolari - 11/11/04
  //DStates: array[0..NumDrivers-1] of TDriverState;
//

//manuel
//tasks and roles
  tkSpeed: double;
  tkToBallDist: double;

  minKick:integer=13;
  slopeKick:double=2.7;
  originKick:double=10.5;

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
  i:integer;
  SessionPropsList: TStringList;
  SessionPropsFileName: string;
begin
  SessionPropsFileName := ExtractFilePath(Application.ExeName)+FMain.DataDir+'/SessionPropsParam.txt';
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
  ComponentsToParameters;
  FMain.InsertAuxForms(FParam,'Parameters');
  //===>  FCoachMain.InsertAuxForms(FParam,'Parameters');

  LoadArray;
  FillListNames;
  ArrayToList;
end;

procedure TFParam.ChListParClickCheck(Sender: TObject);
begin
  ListToArray;

  //if Params[paramNoRoleChange] then FMain.RGMode.color:=clRed
  //else FMain.RGMode.color:=clBtnFace;
end;

procedure TFParam.EditBadIPCountChange(Sender: TObject);
begin

end;

procedure TFParam.ButtonSetStartStopClick(Sender: TObject);
begin
    Xstart:=strtofloatDef(EditXstart.Text,0);
    Ystart:=strtofloatDef(EditYstart.Text,0);
    Xstop:=strtofloatDef(EditXstop.Text,0);
    Ystop:=strtofloatDef(EditYstop.Text,0);
end;

procedure TFParam.EditBallSpaceChange(Sender: TObject);
begin

end;

procedure TFParam.EditIPBaseChange(Sender: TObject);
begin

end;

procedure TFParam.EditSimTwoIPChange(Sender: TObject);
begin

end;

procedure TFParam.EditSimTwoListenPortChange(Sender: TObject);
begin

end;

procedure TFParam.EditSimTwoListenPortEditingDone(Sender: TObject);
begin
  FMain.SdpoUDPSimTwo.Disconnect;
  FMain.SdpoUDPSimTwo.Listen(StrToInt(EditSimTwoListenPort.text));
end;

procedure TFParam.EditSimTwoPortChange(Sender: TObject);
begin

end;

procedure TFParam.EditSuperIPChange(Sender: TObject);
begin

end;

procedure TFParam.EditVisionIPChange(Sender: TObject);
begin

end;

procedure TFParam.EditXstartChange(Sender: TObject);
begin

end;

procedure TFParam.ETaskBehindBallSpeedExit(Sender: TObject);
begin
  tkSpeed:=StrToFloat(ETaskBehindBallSpeed.Text);
end;

procedure TFParam.ETaskBehindBallDistToBallExit(Sender: TObject);
begin
  tkToBallDist:=StrToFloat(ETaskBehindBallDistToBall.Text);
end;

procedure TFParam.E_minKickChange(Sender: TObject);
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

procedure TFParam.TabBehindBallContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

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
    sl.SaveToFile(extractfilepath(application.ExeName)+FMain.DataDir+'/Params.ini');
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
    sl.LoadFromFile(extractfilepath(application.ExeName)+FMain.DataDir+'/Params.ini');
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
  Fmain.caption:='RoC - Attack:' + CGoalColorStr[AttackGoal];
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

procedure TFParam.Label19Click(Sender: TObject);
begin

end;

procedure TFParam.Label1Click(Sender: TObject);
begin

end;

procedure TFParam.Label2Click(Sender: TObject); 
begin

end;



procedure TFParam.FormClose(Sender: TObject);
begin
  ListToArray;
  SaveArray;

  FMain.AuxFormClosed('Parameters');
  //===>  FCoachMain.AuxFormClosed('Parameters');
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
var
i: integer;
begin
//  xxxxx:=EditToFloatDef(Edit,xxxxx);
  //PoleClusterExpFactor:=EditToFloatDef(EditPoleClusterExpFactor,PoleClusterExpFactor);
  //PoleClusterLinearDecay:=EditToFloatDef(EditPoleClusterLinearDecay,PoleClusterLinearDecay);
  //PoleClusterMinQualAccept:=EditToFloatDef(EditPoleClusterMinQualAccept,PoleClusterMinQualAccept);
  //PoleClusterMinQuality:=EditToFloatDef(EditPoleClusterMinQuality,PoleClusterMinQuality);
  //PoleClusterMaxError:=EditToFloatDef(EditPoleClusterMaxError,PoleClusterMaxError);
  //PoleClusterDistTreshold:=EditToFloatDef(EditPoleClusterDistTreshold,PoleClusterDistTreshold);
  //PoleMaxCenterDeltaTeta:=EditToFloatDef(EditPoleMaxCenterDeltaTeta,PoleMaxCenterDeltaTeta);
  //PoleClusterPosNoiseCov:=EditToFloatDef(EditPoleClusterPosNoiseCov,PoleClusterPosNoiseCov);
  //OutofFieldTol:=EditToFloatDef(EditOutofFieldTol,OutofFieldTol);
  //PosUpdateCovTol:=EditToFloatDef(EditPosUpdateCovTol,PosUpdateCovTol);

  originKick:=strtofloatDef(E_originKick.Text,10.5);
  slopeKick:=strtofloatDef(E_slopeKick.Text,2.7);
  minKick:=StrToIntDef(E_minKick.Text,13);

  BallFrictionCoef:=EditToFloatDef(EditBallFrictionCoef,BallFrictionCoef);
  BallQualityDecay:=EditToFloatDef(EditBallQualityDecay,BallQualityDecay);
  BallLinearQualityDecay:=EditToFloatDef(EditBallLinearQualityDecay,BallLinearQualityDecay);
  BallDistancekick:=EditToFloatDef(editballdistkick,BallDistancekick);
  Ballqualitytreshold:=EditToFloatDef(editballqualitytreshold,Ballqualitytreshold);
  kickforce:=editkickforce.Text;
  kickforce1:=editkickforce1.Text;
  kickforce2:=editkickforce2.Text;
  kickforce3:=editkickforce3.Text;
  distkickforce:=EditToFloatDef(editdistkickforce,distkickforce);
  distkickforce1:=EditToFloatDef(editdistkickforce1,distkickforce1);
  distkickforce2:=EditToFloatDef(editdistkickforce2,distkickforce2);
  distkickforce3:=EditToFloatDef(editdistkickforce3,distkickforce3);

  Edittimedelay.Text:='10';
  kickdelay:=strtoint(Edittimedelay.Text);

  minVelBall:=strtofloatDef(E_speedMin.Text,0.1);
  step_forward:=StrToIntDef(E_Forward.Text,2);
  //KeeperLineX:=EditToFloatDef(EditKeeperLine,KeeperLineX);
  //CentralLineX:=EditToFloatDef(EditLastDefenderLine,CentralLineX);
  //DefenderLineX:=EditToFloatDef(EditOtherDefenderLine,DefenderLineX);
  //LocQuality:=EditToFloatDef(EditStopDistance,LocQuality);

  //KeeperMaxY:=EditToFloatDef(EditLastDefenderDistToArea,KeeperMaxY);

  //ID_DevicePID:=strtoint(EditDevicePID.Text);
  //ID_DeviceKicker:=strtoint(EditDeviceKicker.Text);


  Wheel1Diameter:=EditToFloatDef(EditWheel1Diameter,Wheel1Diameter);
  Wheel2Diameter:=EditToFloatDef(EditWheel2Diameter,Wheel2Diameter);
  Wheel3Diameter:=EditToFloatDef(EditWheel3Diameter,Wheel3Diameter);
  WheeltoCenterdist:=EditToFloatDef(EditWheeltoCenterdist,WheeltoCenterdist);

  {LocLineDistTresh:=EditToFloatDef(EditLocLineDistTresh,LocLineDistTresh);
  LocLineAngleTresh:=rad(EditToFloatDef(EditLocLineAngleTresh,Deg(LocLineAngleTresh)));
  LocLineDistCov:=EditToFloatDef(EditLocLineDistCov,LocLineDistCov);
  LocLineAngleCov:=EditToFloatDef(EditLocLineAngleCov,LocLineAngleCov);

  LocPoleDistTresh:=EditToFloatDef(EditLocPoleDistTresh,LocPoleDistTresh);
  LocPoleAngleTresh:=rad(EditToFloatDef(EditLocPoleAngleTresh,Deg(LocPoleAngleTresh)));
  LocPoleDistCov:=EditToFloatDef(EditLocPoleDistCov,LocPoleDistCov);
  LocPoleAngleCov:=EditToFloatDef(EditLocPoleAngleCov,LocPoleAngleCov);}

 //omni3 FControl.PiramideParameters(Maxv1,Maxv2,Maxv3,Maxv4);
 
  // from DecConsts
  // dynamic parameters
  AxialDistance:=WheeltoCenterdist;
  Kw:=1/AxialDistance;
  SpeedMax:=EditToFloatDef(EditMaxRobotSpeed,SpeedMax);
  AvgSpeed:=EditToFloatDef(EditAvgRobotSpeed,AvgSpeed);
  MaxIntersectTime:=EditToFloatDef(EditMaxIntersectTime,MaxIntersectTime);

  // TODO - add kick
  {MaxKickSpeed: double;
  MaxKickPulse: integer;
  MaxChipDist: double;
  MaxChipKickPulse: integer;
  KickAngle: double;}
  
  BallSpace:=EditToFloatDef(EditBallSpace,BallSpace);
  //manuel
  RobotSpace:=EditToFloatDef(EditRobotSpace,RobotSpace);
  StopDistance:=EditToFloatDef(EditStopDistance,StopDistance);
  KickBallDist:=EditToFloatDef(EditKickBallDist, KickBallDist);

  // defender offsets
  KeeperLine:=strtofloatDef(EditKeeperLine.Text,1);
  KeeperWidthLine:=strtofloatDef(EditKeeperWLine.Text,1);
  LastDefenderDistToArea:=strtofloatDef(EditLastDefenderDistToArea.Text,FieldDims.AreaDepth+0.2);
  OtherDefenderLine:=strtofloatDef(EditOtherDefenderLine.Text,FieldDims.AreaDepth+1);
  
  CompassOffset:=EditToFloatDef(EditCompassOffset,CompassOffset);
  
  myNumber:=RGRobotNumber.ItemIndex;

  //manuel
  tkToBallDist:=StrToFloat(ETaskBehindBallDistToBall.Text);
  tkSpeed:=StrToFloat(ETaskBehindBallSpeed.Text);

  max_linear_acceleration:=EditToFloatDef(EditMaxLinearAcceleration, 0.1);
  max_angular_acceleration:=EditToFloatDef(EditMaxAngularAcceleration,0.1);

end;

procedure TFParam.BOkClick(Sender: TObject);
begin
  ComponentsToParameters;
end;


procedure TFParam.BtnWritePIDClick(Sender: TObject);
var
  i: integer;
begin

  {WriteParsPIDValues;

  ClearUDPBuffer(NetOutBufferParam);
  NetPutByte(NetOutBufferParam,ord('1'));
  NetPutString(NetOutBufferParam,Daisy.Flush);

  NetPutByte(NetOutBufferParam,ord('2'));
  NetPutString(NetOutBufferParam,'');

  //FMain.SdpoUDP.WriteBuf(NetOutBufferParam.data,NetOutBufferParam.MessSize);
  FMain.SdpoUDP.Send(NetOutBufferParam.data,NetOutBufferParam.MessSize,FConfig.EditVisionIP.Text+':6969');}

end;

procedure TFParam.BtnreadPIDClick(Sender: TObject);
begin
  {ReadParsPIDValues;

  ClearUDPBuffer(NetOutBufferParam);
  NetPutByte(NetOutBufferParam,ord('1'));
  NetPutString(NetOutBufferParam,Daisy.Flush);

  NetPutByte(NetOutBufferParam,ord('2'));
  NetPutString(NetOutBufferParam,'');

  FMain.SdpoUDP.Send(NetOutBufferParam.data,NetOutBufferParam.MessSize,FConfig.EditVisionIP.Text+':6969');}

end;


procedure TFParam.BtnreadkickerClick(Sender: TObject);
begin
  {ReadParskickerValues;

  ClearUDPBuffer(NetOutBufferParam);
  NetPutByte(NetOutBufferParam,ord('1'));
  NetPutString(NetOutBufferParam,Daisy.Flush);

  NetPutByte(NetOutBufferParam,ord('2'));
  NetPutString(NetOutBufferParam,'');

  FMain.SdpoUDP.Send(NetOutBufferParam.data,NetOutBufferParam.MessSize,FConfig.EditVisionIP.Text+':6969');}

end;

procedure TFParam.BtnwritekickerClick(Sender: TObject);
begin
  //WriteParskickerValues;
end;

initialization
  {$I Param.lrs}

end.
