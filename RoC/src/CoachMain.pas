unit CoachMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, IniPropStorage, IniFiles, Roles, StdCtrls, lNetComponents,
  LogCompat, DecConsts, lNet, WLan, Main, DOM, XMLRead;
  
type
  TRobotStateForm = record
    CBRobotActive, CBForceRobotActive: TCheckBox;
    EditRobotState: TEdit;
  end;

  TObsStates = record
    Obs: array[0..MaxRobots-1] of TObstacle;
  end;


  { TFCoachMain }

  TFCoachMain = class(TForm)
    BRefBoxConnect: TButton;
    BRefBoxDisconnect: TButton;
    ButtonStop: TButton;
    ButtonStart: TButton;
    CBForceRobotActive2: TCheckBox;
    CBForceRobotActive3: TCheckBox;
    CBForceRobotActive4: TCheckBox;
    CBForceRobotActive5: TCheckBox;
    CBForceRobotActive6: TCheckBox;
    CBRobotActive1: TCheckBox;
    CBRobotActive2: TCheckBox;
    CBRobotActive3: TCheckBox;
    CBRobotActive4: TCheckBox;
    CBRobotActive5: TCheckBox;
    CBRobotActive6: TCheckBox;
    CBForceRobotActive1: TCheckBox;
    CBRobotForceActive2: TCheckBox;
    CBRobotForceActive3: TCheckBox;
    CBRobotForceActive4: TCheckBox;
    CBRobotForceActive5: TCheckBox;
    CBRobotForceActive6: TCheckBox;
    CBKeeperActive: TCheckBox;
    CheckBoxOtherTactic: TCheckBox;
    EditVbatRobot5: TEdit;
    EditVbatRobot3: TEdit;
    EditVbatRobot2: TEdit;
    EditVbatRobot4: TEdit;
    EditVbatRobot1: TEdit;
    EditRobotState1: TEdit;
    EditRobotActive2: TEdit;
    EditRobotActive3: TEdit;
    EditRobotActive4: TEdit;
    EditRobotActive5: TEdit;
    EditRobotActive6: TEdit;
    EditRobotAvailableCount: TEdit;
    EditPlayState: TEdit;
    EditRefBoxIP: TEdit;
    EditMessIP: TEdit;
    EditRefState: TEdit;
    EditRobotState2: TEdit;
    EditRobotState3: TEdit;
    EditRobotState4: TEdit;
    EditRobotState5: TEdit;
    EditRobotState6: TEdit;
    GBRefBox: TGroupBox;
    GBRobotInfo1: TGroupBox;
    GBRobotInfo2: TGroupBox;
    GBRobotInfo3: TGroupBox;
    GBRobotInfo4: TGroupBox;
    GBRobotInfo5: TGroupBox;
    GBRobotInfo6: TGroupBox;
    ImageMap: TPaintBox;
    FormStorage: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    UDPRefBox: TLUDPComponent;
    MemoRefBoxBad: TMemo;
    MemoRefBox: TMemo;
    RGDecision: TRadioGroup;
    RGRobotSel: TRadioGroup;
    TCPRefBox: TLTCPComponent;
    MainMenu: TMainMenu;
    MenuAbout: TMenuItem;
    MenuExit: TMenuItem;
    MenuFile: TMenuItem;
    MenuWindows: TMenuItem;
    N1: TMenuItem;
    TimerDoTactic: TTimer;
    procedure BRefBoxConnectClick(Sender: TObject);
    procedure BRefBoxDisconnectClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure CBForceRobotActive1Change(Sender: TObject);
    procedure CBForceRobotActive2Change(Sender: TObject);
    procedure CBKeeperActiveChange(Sender: TObject);
    procedure CBRobotActive1Change(Sender: TObject);
    procedure CBRobotActive2Change(Sender: TObject);
    procedure EditRefBoxIPChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuWinDefaultClick(Sender: TObject);
    procedure RGDecisionClick(Sender: TObject);
    procedure SdpoUDPCoachReceive(aSocket: TLSocket);
    procedure ShowAll;
    procedure MainLoop;
    procedure TCPRefBoxAccept(aSocket: TLSocket);
    procedure TCPRefBoxReceive(aSocket: TLSocket);
    procedure TimerDoTacticTimer(Sender: TObject);
    procedure ParseRefBoxData(data: String);
    procedure ParseRefBoxUDPData(data: String);
    procedure ParseRefBoxUDPData(data, attr: String);
    procedure ParseXmlData(xmldata: string);
    procedure ShowRobotTimes(var RS: TRobotstate; var RSF: TRobotStateForm; robnum:integer);
    procedure MergeBallState;
    procedure UDPRefBoxReceive(aSocket: TLSocket);
  private
    { private declarations }
  public
    { public declarations }
    NetTime: DWORD;
    deb_txt: string;
    AuxForms: array[0..MaxAuxForms-1] of TForm;
    NumAuxForms: integer;
    DataDir: string;
    down_x,down_y: integer;
    
    procedure ProcessPlayerPacket(packet_str: string);
    procedure SendCoachInfo;

    procedure InsertAuxForms(Fm: TForm; cap: string);
    procedure AuxFormClosed(cap: string);
    procedure SaveAuxForms;
    procedure CloseAuxForms;
    procedure RestoreAuxForms;
    
    procedure SetRobotStateForm;
  end; 

var
  FCoachMain: TFCoachMain;
  BallStates,LastBallStates: array[0..MaxRobots-1] of TBallState;
  BallsFiltered: array[0..MaxRobots-1] of double;
  ObsStates: array[0..MaxRobots-1] of TObsStates;
  RobotStateForm: array[0..MaxRobots-1] of TRobotStateForm;
  
implementation

uses Field, Robots, Utils, Tactic, Param;

{ TFCoachMain }

procedure TFCoachMain.MenuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFCoachMain.MenuAboutClick(Sender: TObject);
begin
  Showmessage('5dpo2000 Coach'+#$0d+'(c)2008 5dpo');
end;

procedure TFCoachMain.FormCreate(Sender: TObject);
var SessionPropsList: TStringList;
    SessionPropsFileName: string;
begin
  NumAuxForms:=0;
  if paramcount>=1 then begin
    DataDir:=paramstr(1);
    if not directoryExists(extractfilepath(application.ExeName)+DataDir) then DataDir:=DataPath+'data';
  end else begin
    DataDir:=DataPath+'data'
  end;
//  FormStorage.IniFileName:=extractfilepath(application.ExeName)+'\'+dataDir+'\Config.ini';
  if not DirectoryExists(ExtractFilePath(Application.ExeName)+DataDir) then
    mkdir(ExtractFilePath(Application.ExeName)+DataDir);

  SessionPropsFileName := ExtractFilePath(Application.ExeName)+DataDir+'/SessionPropsMain.txt';
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
  FormStorage.IniFileName:=ExtractFilePath(Application.ExeName)+DataDir+'/Config.ini';
  FormStorage.Restore;
  
  SetRobotStateForm;
end;

procedure TFCoachMain.SetRobotStateForm;
begin
  with RobotStateForm[0] do begin
    CBForceRobotActive:=CBForceRobotActive1;
    CBRobotActive:=CBRobotActive1;
    EditRobotState:=EditRobotState1;
  end;
  with RobotStateForm[1] do begin
    CBForceRobotActive:=CBForceRobotActive2;
    CBRobotActive:=CBRobotActive2;
    EditRobotState:=EditRobotState2;
  end;
  with RobotStateForm[2] do begin
    CBForceRobotActive:=CBForceRobotActive3;
    CBRobotActive:=CBRobotActive3;
    EditRobotState:=EditRobotState3;
  end;
  with RobotStateForm[3] do begin
    CBForceRobotActive:=CBForceRobotActive4;
    CBRobotActive:=CBRobotActive4;
    EditRobotState:=EditRobotState4;
  end;
  with RobotStateForm[4] do begin
    CBForceRobotActive:=CBForceRobotActive5;
    CBRobotActive:=CBRobotActive5;
    EditRobotState:=EditRobotState5;
  end;
  with RobotStateForm[5] do begin
    CBForceRobotActive:=CBForceRobotActive6;
    CBRobotActive:=CBRobotActive6;
    EditRobotState:=EditRobotState6;
  end;
end;

procedure TFCoachMain.FormShow(Sender: TObject);
begin
  RestoreAuxForms;
  UpdateFieldDims;
  Dfieldmag:=Dfieldmag/20*35; //Big canvas ratio
  TimerDoTactic.Enabled := true;
end;

procedure TFCoachMain.ImageMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  down_x:=x;
  down_y:=y;
end;

procedure TFCoachMain.ImageMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Xw,Yw,V: double;
    //Mess: THighMessage;
    new_teta: double;
begin
//  new_teta:=atan2(-Y+down_y,X-down_x);
  new_teta:=atan2(-Y+down_y,X-down_x);
  MapToWorld(down_x,down_y,xw,yw);
  V:=sqrt(sqr(-Y+down_y)+sqr(X-down_x));

{    if CBSimulator.Checked then begin
      with BallState do begin
        x:=xw;
        y:=yw;
        Vx:=V*cos(new_teta)*0.1;
        Vy:=V*sin(new_teta)*0.1;
      end;
    end;
  end else begin
    if RGRobotSel.ItemIndex<>6 then begin
      with RobotState[RGRobotSel.ItemIndex] do begin
        x:=xw;
        y:=yw;
        teta:=new_teta;
      end; }

  if ssShift in Shift then begin
      with BallState do begin
        x:=xw;
        y:=yw;
        //Vx:=V*cos(new_teta)*0.1;
        //Vy:=V*sin(new_teta)*0.1;
      end;
  end else begin
    with RobotState[RGRobotSel.ItemIndex] do begin
      x:=xw;
      y:=yw;
      teta:=new_teta;
    end;
  end;
end;

procedure TFCoachMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveAuxForms;
  CloseAuxForms;
  TCPRefBox.Disconnect;
  FMain.Close;
end;

procedure TFCoachMain.BRefBoxConnectClick(Sender: TObject);
begin
  TCPRefBox.Connect(EditRefBoxIP.Text, 28097);
  UDPRefBox.Listen(30000);
end;

procedure TFCoachMain.BRefBoxDisconnectClick(Sender: TObject);
begin
  TCPRefBox.Disconnect;
  UDPRefBox.Disconnect;
end;

procedure TFCoachMain.ButtonStartClick(Sender: TObject);
begin
 RefereeState:=rsStartPos;
end;

procedure TFCoachMain.ButtonStopClick(Sender: TObject);
begin
 RefereeState:=rsStopPos;
end;

procedure TFCoachMain.CBForceRobotActive1Change(Sender: TObject);
begin

end;

procedure TFCoachMain.CBForceRobotActive2Change(Sender: TObject);
begin

end;

procedure TFCoachMain.CBKeeperActiveChange(Sender: TObject);
begin
  if CBKeeperActive.Checked then begin
     KeeperWay:=roleKeeper;
  end else begin
     KeeperWay:=roleKeeperPassive;
  end;
  //RobotStatus[0].default_role:=KeeperWay;
end;

procedure TFCoachMain.CBRobotActive1Change(Sender: TObject);
begin

end;

procedure TFCoachMain.CBRobotActive2Change(Sender: TObject);
begin

end;

procedure TFCoachMain.EditRefBoxIPChange(Sender: TObject);
begin

end;


procedure TFCoachMain.InsertAuxForms(Fm: TForm; cap: string);
var NewItem: TMenuItem;
begin
  NewItem := TMenuItem.Create(Self); //first create the separator
  NewItem.Caption := cap;
  NewItem.tag := NumAuxForms;
  NewItem.OnClick := @MenuWinDefaultClick;
  MenuWindows.Add(NewItem); //add the new item to the Windows menu
  if NumAuxForms<MaxAuxForms-1 then begin
    AuxForms[NumAuxForms]:=Fm;
    inc(NumAuxForms);
  end;
end;

procedure TFCoachMain.AuxFormClosed(cap: string);
var i: integer;
begin
  for i := 0 to NumAuxForms - 1 do begin
    if MenuWindows.Items[i].Caption = cap then begin
      MenuWindows.Items[i].Checked := false;
      exit;
    end;
  end;
end;

procedure TFCoachMain.MenuWinDefaultClick(Sender: TObject);
var MenuItem: TMenuItem;
begin
  MenuItem:=Sender as TMenuItem;
//  if not AuxForms[MenuItem.Tag].visible then begin
    MenuItem.Checked:=true;
    AuxForms[MenuItem.Tag].Show;
    AuxForms[MenuItem.Tag].BringToFront;
//  end;
end;

procedure TFCoachMain.RGDecisionClick(Sender: TObject);
begin

end;

procedure TFCoachMain.SdpoUDPCoachReceive(aSocket: TLSocket);
var PlayerInfo: TPlayerInfo;
    packet_str: string;
    i, RobotNumber: integer;
begin
  try
    aSocket.GetMessage(packet_str);

    // only accpet valid length packets
    if length(packet_str) <> sizeof(PlayerInfo) then exit;

    copymemory(@PlayerInfo, @(packet_str[1]), sizeof(PlayerInfo));

    if PlayerInfo.Magic <> PACKET_PLAYER_MAGIC then exit;
    EditMessIP.Text:= aSocket.PeerAddress;
    
{    RobotNumber := PlayerInfo.RobotState.;

    // send my robot state
    with RobotState[MyNumber] do begin
      PlayerInfo.RobotState.x := x;
      PlayerInfo.RobotState.y := y;
      PlayerInfo.RobotState.teta := teta;
      PlayerInfo.RobotState.conf := count;  //TODO: calculate confidence level
    end;

    // send the ball state
    PlayerInfo.BallState.x := BallState.x;
    PlayerInfo.BallState.y := BallState.y;
    PlayerInfo.BallState.conf := BallState.quality;

    // send the obstacles
    // TODO: actually send the obstacles
    zeromemory(@(PlayerInfo.ObsState), sizeof(PlayerInfo.ObsState));

    packet_str := StringOfChar(#0, sizeof(PlayerInfo));
    copymemory(@(packet_str[1]), @PlayerInfo, sizeof(PlayerInfo));

    SdpoUDPSuper.SendMessage(packet_str, FParam.EditSuperIP.Text + ':7373');}

  except
  end;

end;

procedure TFCoachMain.SaveAuxForms;
var i: integer;
    Ini: TIniFile;
begin
  Ini := TIniFile.Create(FormStorage.IniFileName);
  try
    for i:=0 to MenuWindows.Count-1 do begin
      Ini.WriteInteger('WINDOWS',MenuWindows.items[i].Caption+'_Visible',ord(MenuWindows.items[i].Checked));
    end;
  finally
    Ini.Free;
  end;
end;

procedure TFCoachMain.CloseAuxForms;
var
  i: integer;
begin
  for i:=0 to MenuWindows.Count-1 do begin
    AuxForms[MenuWindows.items[i].tag].Close;
  end;
end;

procedure TFCoachMain.RestoreAuxForms;
var i: integer;
    Ini: TIniFile;
begin
  Ini := TIniFile.Create(FormStorage.IniFileName);
  try
    for i:=0 to MenuWindows.Count-1 do begin
      if Ini.readInteger('WINDOWS',MenuWindows.items[i].Caption+'_Visible',0)<>0 then begin
        MenuWindows.items[i].Checked:=true;
        AuxForms[MenuWindows.items[i].tag].Show;
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TFCoachMain.ShowRobotTimes(var RS: TRobotstate; var RSF: TRobotStateForm; robnum:integer);
begin
  RSF.EditRobotState.Text:=format('%.2f',[(getTickcount-RS.timestamp)/1000]);//format('%2f',[GetTickCount-RS.timestamp]);
  if  (getTickcount-RS.timestamp)/1000 > 10 then begin
    RobotStatus[robnum].Active:=false;
  end;

  if RobotStatus[robnum].Active then begin
   RSF.EditRobotState.Color:=clGreen;
  end else begin
    RSF.EditRobotState.Color:=clRed;
  end;
end;

procedure TFCoachMain.ShowAll;
var i,tx,ty: integer;
    cl: Tcolor;
begin
  //if CBShow.Checked then begin
    FMain.DrawFieldMap(ImageMap.canvas);
    for i:=0 to MaxRobots-1 do begin
      //if i=myNumber then cl:=clwhite else cl:=CTeamColorColor24[TeamColor];
      DrawRobot(RobotState[i],cl,ImageMap.canvas);
      DrawRobotInfo(RobotState[i],RobotInfo[i],ImageMap.canvas);
      ShowRobotTimes(RobotState[i],RobotStateForm[i],i);
    end;
    FMain.DrawBall(ImageMap.canvas,BallState,clred);

    {with ImageShow.Canvas do begin
      brush.Style:=bsSolid;
      brush.Color:=clBlack;
      pen.color:=clBlack;
      Rectangle(0,0,width,height);
    end;
    if CBShowCenters.Checked then ShowCenters;
    if CBShowEdges.Checked then ShowEdges;
    if CBShowRadar.Checked then ShowRadar;

    if CBShowRegions.Checked then ShowRegions;

    ShowLine(GWLine);
    ShowLine(WGLine);

    Gtext.Clear;
    FillShowValues(Gtext,RobotState[myNumber], BallState, View);
    if CBShowLog.Checked then FLog.LogToStrings(GText,FLog.TreeView,LogBufferIn);

    tx:=draw_x;
    ty:=draw_y;
    ImageShow.canvas.Font.Color:=clgray;
    ImageShow.canvas.brush.Style:=bsClear;
    StringsDraw(ImageShow.canvas,Gtext,tx,ty);}
  //end;
end;

procedure TFCoachMain.MainLoop;
var  i,j: integer;
begin
  NetTime:=getTickcount();
  deb_txt:='';

  {ResetView;
  ParseLanMess;
  UpdateEdges;
  UpdateRadar;
  UpdateOdos;
  UpdateCenters([iYellowGoal,iBlueGoal]);

  // TODO localization
  //Localize;

  UpdateCenters([iBallColor,iPurpleTeam,iCyanTeam]);
  UpdateObstacles;
  PropagateObstacles;

  for i:=0 to MaxRobots-1 do begin
    if i=mynumber then
      PropagateXYTeta(RobotState[i])
    else
      PropagateXYTetaOthers(RobotState[i]);
  end;

  LastBallState:=BallState;
  LastObsBallState:=ObsBallState;
  PropagateXYBall(BallState);
  ExtractBallFromCenters(ObsBallState);
  if ObsBallState.quality>=0 then begin
    MergeBallState(LastObsBallState,ObsBallState);
    MergeBallState(BallState,ObsBallState);
  end;

  MainControl;
  deb_txt:=deb_txt+format('%3d',[getTickcount()-NetTime]);
  sleep(1);
  //MainHighMessage;
  SendPlayerInfo;
  deb_txt:=deb_txt+format(',%3d',[getTickcount()-NetTime]);

  if RGController.ItemIndex=0 then  // freeze
    FLog.LogFrame(GetTickCount,0,0)
  else
    FLog.LogFrame(GetTickCount,0,1);
  deb_txt:=deb_txt+format(',%3d',[getTickcount()-NetTime]);
  }
  ShowAll;
  deb_txt:=deb_txt+format(',%3d',[getTickcount()-NetTime]);

  //LastView:=View;

  //EditTimes.text:=deb_txt;
  //inc(cycleCount);
end;

procedure TFCoachMain.TCPRefBoxAccept(aSocket: TLSocket);
begin

end;

procedure TFCoachMain.TCPRefBoxReceive(aSocket: TLSocket);
var data: string;
begin
  TCPRefBox.GetMessage(data);

  //MemoRefBox.Append(data);
  ParseRefBoxData(data);
end;


procedure TFCoachMain.ParseRefBoxData(data: String);
var i: integer;
    command,s: string;
    c: char;
begin
  //LastData := data;
  //StartCount:=GetTickCount();
  command := '';
  //while MemoRefBox.Lines.Count > 100 do MemoRefBox.Lines.Delete(0);
  while MemoRefBoxBad.Lines.Count > 4 do MemoRefBoxBad.Lines.Delete(0);

  for i:=1 to length(data) do begin
    if data[i]=#0 then continue;
    command:=command+data[i];
  end;

  if (length(command) <> 1) and (command<>'1s') and (command<>'2s')
     and (command<>'Hh') and (command<>'He')  and (command<>'N') then
  begin
    MemoRefBoxBad.Lines.Add(command);
    exit;
  end;

   //if ( (command <> 'K' ) ) then exit;
  //MemoRefBoxBad.Lines.Add(command);
  //MemoRefBox.Lines.Add(command);
  //FCoachMain.MemoRefBox.Clear;

  //FCoachMain.MemoRefBox.Append(command[1]);

  c := command[1];
  s := copy(command,1,1);
  FCoachMain.MemoRefBox.Append(c);
   FCoachMain.MemoRefBox.Append(s);

  if command = '1s' then  c := 's';
  if command = '2s' then  c := 's';

  if c='*' then ;  // nothing
  
  TacticProcessRefereeComand(c);

end;

procedure TFCoachMain.ParseRefBoxUDPData(data: String);
var i: integer;
    command: string;
    c: char;
begin
  c:=' ';
  if data='GameStart' then c:='s'
  else if data='GameStop' then c:='S'
  else if data='Cancel' then c:='H' //?
  else if data='DroppedBall' then c:='@' //?
  else if data='StageChange' then ;   //????
  TacticProcessRefereeComand(c);
end;

procedure TFCoachMain.ParseRefBoxUDPData(data, attr: String);
var i: integer;
    command: string;
    c: char;
begin
  c:=' ';
  //assumes that we are cyan, need to review this
  if data='KickOff' then begin
    if attr='Cyan' then
      c:='k'
    else
      c:='K';
  end else  if data='FreeKick' then begin
    if attr='Cyan' then
      c:='f'
    else
      c:='F';
  end else  if data='GoalKick' then begin
  end else if data='ThrowIn' then begin
  end else if data='Corner' then begin
  end else if data='Penalty' then begin
    if attr='Cyan' then
      c:='p'
    else
      c:='P';
  end else if data='Substitution' then begin
  end else if data='CardAwarded' then begin
  end else if data='GoalAwarded' then begin
  end;
  TacticProcessRefereeComand(c);
end;

procedure TFCoachMain.ParseXmlData(xmldata: string);
var
  Child: TDOMNode;
  doc: TXMLDocument;
  s: TStringStream;
  i: integer;
begin
  s:=TStringStream.Create(xmldata);
  try
    s.Position:=0;
    ReadXMLFile(doc,s);
    Child:=doc.DocumentElement.FirstChild;
    while Assigned(Child) do begin
      EditMessIP.Text:=Child.Attributes.Item[2].NodeValue+':  '+Child.Attributes.Item[1].NodeValue;
      with Child.ChildNodes do
      try
        for i := 0 to (Count-1) do  begin
          if ((Item[i].NodeName = 'GameStart') or (Item[i].NodeName = 'GameStop') or (Item[i].NodeName = 'Cancel') or (Item[i].NodeName = 'DroppedBall') or (Item[i].NodeName = 'StageChange')) then
            ParseRefBoxUDPData(Item[i].NodeName)
          else
            ParseRefBoxUDPData(Item[i].NodeName, Item[i].Attributes.Item[0].NodeValue);
        end;
      finally
        free;
      end;
      Child:=Child.NextSibling;
    end;
  finally
    s.Free;
  end;
end;

procedure TFCoachMain.MergeBallState;
var ball_filt, BestBall: double;
    i,BestBallIdx,quality: integer;
begin
  ball_filt := 0.7;
  BestBall:=0;
  BestBallIdx:=-1;
  quality:=0;
  for i:=0 to MaxRobots-1 do begin
    // filtro
    BallsFiltered[i] := ball_filt*BallsFiltered[i] + (1-ball_filt)*BallStates[i].quality;
    
    if RobotStatus[i].active and (BallsFiltered[i]>BestBall) then begin
      BestBall:=BallsFiltered[i];
      BestBallIdx:=i;
    end;
  end;

  // the best robot seeing the ball is used to update the new ball position
  if BestBallIdx>=0 then begin
   BallState:=BallStates[BestBallIdx];
   BallState.timestamp:=GetTickCount;
  end;
end;

procedure TFCoachMain.UDPRefBoxReceive(aSocket: TLSocket);
var
  xmldata: string;
begin
  UDPRefBox.GetMessage(xmldata);
  ParseXmlData(xmldata);
end;

procedure TFCoachMain.TimerDoTacticTimer(Sender: TObject);
begin
  MergeBallState;
  DoTactic;
  SendCoachInfo;
  ShowAll;
end;

{procedure TFCoachMain.SdpoUDPSuperReceive(aSocket: TLSocket);
var CoachInfo: TCoachInfo;
    packet_str: string;
    i: integer;

begin
  try
    aSocket.GetMessage(packet_str);

    // only accpet valid length packets
    if length(packet_str) <> sizeof(CoachInfo) then exit;

    copymemory(@CoachInfo, @(packet_str[1]), sizeof(CoachInfo));

    if CoachInfo.Magic <> PACKET_COACH_MAGIC then exit;

    for i := 0 to MaxRobots - 1 do begin
      // update robot role
      if RobotInfo[i].role <> CoachInfo.RobotState[i].role then begin
        with RobotInfo[i] do begin
          roleTime := GetTickCount;
          last_role := role;
          role := CoachInfo.RobotState[i].role;
          ZeroMemory(@TaskPars[i], sizeof(TaskPars[0]));
        end;
      end;

      // don't update my own state
      if i = MyNumber then continue;

      // update all others
      with CoachInfo.RobotState[i] do begin
        RobotState[i].count := Ord(active);
        RobotState[i].x := x;
        RobotState[i].y := y;
        RobotState[i].teta := teta;

        RobotStatus[i].active := active;
      end;
    end;

    // if I'm not seeing the ball, I have to trust the coach
    if BallState.quality < 0 then begin
      BallState.x := CoachInfo.BallState.x;
      BallState.y := CoachInfo.BallState.y;
      BallState.quality := 0;
    end;

  except
  end;
end;}


{procedure TFCoachMain.SendPlayerInfo;
var PlayerInfo: TPlayerInfo;
    packet_str: string;
    i: integer;
begin
  try
    PlayerInfo.Magic := PACKET_PLAYER_MAGIC;

    // send my robot state
    with RobotState[MyNumber] do begin
      PlayerInfo.RobotState.x := x;
      PlayerInfo.RobotState.y := y;
      PlayerInfo.RobotState.teta := teta;
      PlayerInfo.RobotState.conf := count;  //TODO: calculate confidence level
    end;

    // send the ball state
    PlayerInfo.BallState.x := BallState.x;
    PlayerInfo.BallState.y := BallState.y;
    PlayerInfo.BallState.conf := BallState.quality;

    // send the obstacles
    // TODO: actually send the obstacles
    zeromemory(@(PlayerInfo.ObsState), sizeof(PlayerInfo.ObsState));

    packet_str := StringOfChar(#0, sizeof(PlayerInfo));
    copymemory(@(packet_str[1]), @PlayerInfo, sizeof(PlayerInfo));

    SdpoUDPSuper.SendMessage(packet_str, FParam.EditSuperIP.Text + ':7373');

  except
  end;
end;}


procedure TFCoachMain.ProcessPlayerPacket(packet_str: string);
var PlayerInfo: TPlayerInfo;
    i: integer;
begin
  if length(packet_str) <> sizeof(PlayerInfo) then exit;

  copymemory(@PlayerInfo, @(packet_str[1]), sizeof(PlayerInfo));

  if PlayerInfo.Magic <> PACKET_PLAYER_MAGIC then exit;

  
  // update ONE robot state
  with RobotState[PlayerInfo.num] do begin
    case PlayerInfo.num of
    0:EditVbatRobot1.Text:=format('%2f',[Vbatery]);
    1:EditVbatRobot2.Text:=format('%2f',[Vbatery]);
    2:EditVbatRobot3.Text:=format('%2f',[Vbatery]);
    3:EditVbatRobot4.Text:=format('%2f',[Vbatery]);
    4:EditVbatRobot5.Text:=format('%2f',[Vbatery]);
    end;
    x:=PlayerInfo.RobotState.x;
    y:=PlayerInfo.RobotState.y;
    Vbatery:=PlayerInfo.Batery;
    teta:=PlayerInfo.RobotState.teta;
    count:=Round(PlayerInfo.RobotState.conf);
    timestamp:=GetTickCount;
  end;

  
  with RobotStatus[PlayerInfo.num] do begin
    if RobotStateForm[PlayerInfo.num].CBForceRobotActive.Checked then begin
      Active:=RobotStateForm[PlayerInfo.num].CBRobotActive.Checked
    end else
    if (RobotStateForm[PlayerInfo.num].CBRobotActive.Checked) and (PlayerInfo.RobotState.Active) then begin
      active:=true;
    end else begin
      active:=false;
    end;
  end;
  // update this robot's ball state for future merging
  with BallStates[PlayerInfo.num] do begin
    x:=PlayerInfo.BallState.x;
    y:=PlayerInfo.BallState.y;
    vx:=PlayerInfo.BallState.vx;
    vy:=PlayerInfo.BallState.vy;
    x_n:=PlayerInfo.BallState.x_n;
    y_n:=PlayerInfo.BallState.y_n;
    vx_n:=PlayerInfo.BallState.vx_n;
    vy_n:=PlayerInfo.BallState.vy_n;
    x_next:=PlayerInfo.BallState.x_next;
    y_next:=PlayerInfo.BallState.y_next;
    quality:=PlayerInfo.BallState.quality;
  end;
  for i:=0 to MaxRobots-1 do begin
    with ObsStates[PlayerInfo.num] do begin
      Obs[i].xw:=PlayerInfo.ObsState[i].x;
      Obs[i].yw:=PlayerInfo.ObsState[i].y;
      Obs[i].quality:=PlayerInfo.ObsState[i].conf;
    end;
  end;
end;

procedure TFCoachMain.SendCoachInfo;
var CoachInfo: TCoachInfo;
    packet_str: string;
    i: integer;
begin
  try
    CoachInfo.Magic := PACKET_COACH_MAGIC;
    
    for i := 0 to MaxRobots-1 do begin
      with RobotState[i] do begin
        CoachInfo.RobotState[i].x := x;
        CoachInfo.RobotState[i].y := y;
        CoachInfo.RobotState[i].teta := teta;
        CoachInfo.RobotState[i].active := RobotStatus[i].active;
      end;
      CoachInfo.RobotState[i].role := RobotInfo[i].role;
    end;
    
    with BallState do begin
      CoachInfo.BallState.x:=x;
      CoachInfo.BallState.y:=y;
      CoachInfo.BallState.vx:=vx;
      CoachInfo.BallState.vy:=vy;
      CoachInfo.BallState.x_next:=x_next;
      CoachInfo.BallState.y_next:=y_next;
      CoachInfo.BallState.coachQuality:=quality;
    end;
    
    CoachInfo.Play:=Play;
    
    packet_str := StringOfChar(#0, sizeof(CoachInfo));
    copymemory(@(packet_str[1]), @CoachInfo, sizeof(CoachInfo));

    for i := 0 to MaxRobots-1 do begin
      // robots ip+port must be 'IPBase'.'101-106':'7271-7276'
      if RGDecision.ItemIndex=0 then  //Local Host
        FMain.SdpoUDPSuper.SendMessage(packet_str, '127.0.0.1:'+inttostr(7271 + i))
      else   //Net
        FMain.SdpoUDPSuper.SendMessage(packet_str, FParam.EditIPBase.Text +'.'+IntToStr(101 + i)+ ':'+inttostr(7272 + i));
    end;
  except
  end;

{  try
    PlayerInfo.Magic := PACKET_PLAYER_MAGIC;

    // send my robot state
    with RobotState[MyNumber] do begin
      PlayerInfo.RobotState.x := x;
      PlayerInfo.RobotState.y := y;
      PlayerInfo.RobotState.teta := teta;
      PlayerInfo.RobotState.conf := count;  //TODO: calculate confidence level
    end;

    // send the ball state
    PlayerInfo.BallState.x := BallState.x;
    PlayerInfo.BallState.y := BallState.y;
    PlayerInfo.BallState.conf := BallState.quality;

    // send the obstacles
    // TODO: actually send the obstacles

    num := myNumber;

    zeromemory(@(PlayerInfo.ObsState), sizeof(PlayerInfo.ObsState));

    packet_str := StringOfChar(#0, sizeof(PlayerInfo));
    copymemory(@(packet_str[1]), @PlayerInfo, sizeof(PlayerInfo));

    SdpoUDPSuper.SendMessage(packet_str, FParam.EditSuperIP.Text + ':7373');

  except
  end;  }
end;


initialization
  {$I CoachMain.lrs}
   RobotState[0].Vbatery:=0;
   RobotState[1].Vbatery:=0;
   RobotState[2].Vbatery:=0;
   RobotState[3].Vbatery:=0;
   RobotState[4].Vbatery:=0;


end.

