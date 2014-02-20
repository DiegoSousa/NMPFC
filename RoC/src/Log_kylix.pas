unit Log;

interface

uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  Tactic, Main, QStdCtrls, QComCtrls, QImgList, QGrids, Types,
  QExtCtrls, Roles,Tasks,Actions, QMenus,
  QTypes, FormStorage, TeEngine, Series, TeeProcs, Chart;

const
  LogFrames=1024;

type
  pdouble=^double;
  pinteger=^integer;
  pword=^word;
  pboolean=^boolean;
  pRole=^TRole;
  pTask=^TTask;
  pAction=^TAction;
  //pKick=^TKickState;
  //pHandler=^THandlerState;
  //pAvoidSet=^TAvoidSet;

  LogRobot=record
    RobotState: TRobotState;
    // Motor Currents
    MotCurrents: TMotsCurrent;
    // velocities
    Vmotor: TOdo;
    NeckState: TNeckState;
    // tactical state after decision
    RobotInfo: TRobotInfo;
    ActionPar: TActionPars;
    // tactical commands
    TacticCommand: TTacticCommand;
    Motpwm: TAbsOdo;

  end;

  LogRecord=record
    frame_timestamp,camera: integer;
    // world state
    BallState: TBallState;
    //RobotState: array[0..MaxRobots-1] of LogRobot;
    RobotState: LogRobot;
    //OponentState: array[0..MaxOponents-1] of TOponentState;

  end;

  //TLogType=(logDouble,logInt,logBool,logRole,logTask,logAction,logKick,logHandler,logAvoid);
  TLogType=(logDouble,logInt,LogWord,logBool,logRole,logTask,logAction,logAvoid);



type
  TFLog = class(TForm)
    FormStorage: TFormStorage;
    TreeView: TTreeView;
    ILCheckBox: TImageList;
    VSplitter: TSplitter;
    Panel1: TPanel;
    SGLog: TStringGrid;
    Splitter1: TSplitter;
    PopupMenuLog: TPopupMenu;
    MenuExport: TMenuItem;
    SaveDialogLog: TSaveDialog;
    MenuClear: TMenuItem;
    Chart: TChart;
    Series1: TFastLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChartMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuExportClick(Sender: TObject);
    procedure MenuClearClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    function OffsetTypeToPointer(var x; tipo: TLogType): Pointer;
    procedure PointerToOffsetType(p: Pointer; var offset: integer; var tipo: TLogType);
    procedure FillRobotStateTreeView(num: integer; root: TTreeNode; tree: TTreeView);
// Adicionado por Andre Scolari - 09/11/04
//    procedure FillMotsCurrentTreeView(num: integer; root: TTreeNode; tree: TTreeView);
//
//    procedure FillNeckStateTreeView(num: integer; root: TTreeNode; tree: TTreeView);
    procedure FillRobotInfoTreeView(num: integer; root: TTreeNode; tree: TTreeView);
 //   procedure FillActionParsTreeView(num: integer; root: TTreeNode; tree: TTreeView);
 //   procedure FillTacticCommandTreeView(num: integer; root: TTreeNode; tree: TTreeView);
    procedure FillRobotTreeView(num: integer; root: TTreeNode; tree: TTreeView);
    procedure FillBallTreeView(root: TTreeNode; tree: TTreeView);
    procedure FillGeneralTreeView(root: TTreeNode; tree: TTreeView);

    function GetLogString(tree: TTreeView; prop,index: integer): string;
    function GetLogValue(tree: TTreeView; prop,index: integer): double;

  public
    { Public declarations }
    IndexMenu: integer;

    function LogFrame(par_frame_timestamp,par_camera,increment: integer): integer;
    procedure RefreshGrid(tree: TTreeView);
    procedure LogToStrings(SL: TStrings; tree: TTreeView; idx: integer);

    function LogFrameShowing: integer;

    procedure FillTreeView(tree: TTreeView);
    procedure SaveTree(tree: TTreeView);
    procedure LoadTree(tree: TTreeView);
  end;

var
  FLog: TFLog;

  LogBuffer: array[0..LogFrames-1] of LogRecord;
  LogBufferIn: integer = 0;
  LogBufferCount: integer = 0;


implementation

uses IniFiles, Robots;

{$R *.dfm}

// initialization

procedure TFLog.FormCreate(Sender: TObject);
begin
  FormStorage.IniFileName:=FMain.FormStorage.IniFileName;
  IndexMenu:=Fmain.InsertAuxForms(FLog,'Log');
  FormStorage.LoadProps;
end;

procedure TFLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveTree(TreeView);
  FormStorage.SaveProps;
  if IndexMenu<>-1 then Fmain.MenuWindows[IndexMenu].Checked:=false;
end;

procedure TFLog.FormDestroy(Sender: TObject);
begin
  SaveTree(TreeView);
  FormStorage.SaveProps;
end;

// treeview support

procedure TFLog.SaveTree(tree: TTreeView);
var ini: TIniFile;
    i: integer;
begin
  ini:=TIniFile.Create(extractfilepath(application.ExeName)+FMain.dataDir+'/tree.ini'); // \
  for i:=0 to tree.Items.Count-1 do begin
    ini.WriteInteger(tree.Name,'item'+inttostr(i),tree.Items[i].ImageIndex);
    ini.WriteBool(tree.Name,'expand'+inttostr(i),tree.Items[i].Expanded);
  end;
  ini.UpdateFile;
  ini.Free;
end;

procedure TFLog.LoadTree(tree: TTreeView);
var ini: TIniFile;
    i,def: integer;
begin
  ini:=TIniFile.Create(extractfilepath(application.ExeName)+FMain.dataDir+'/tree.ini');
  for i:=0 to tree.Items.Count-1 do begin
    if tree.Items[i].data=nil then def:=2
    else def:=0;
    tree.Items[i].ImageIndex:=ini.ReadInteger(tree.Name,'item'+inttostr(i),def);
    tree.Items[i].Expanded:=ini.ReadBool(tree.Name,'expand'+inttostr(i),false);
  end;
  ini.Free;
end;

function TFLog.OffsetTypeToPointer(var x; tipo: TLogType): Pointer;
var offset: integer;
begin
  offset:=pchar(@x)-pchar(@LogBuffer[0]);
  result:=Pointer(offset+(ord(tipo) shl 16));
end;

procedure TFLog.PointerToOffsetType(p: Pointer; var offset: integer; var tipo: TLogType);
var i: integer;
begin
  i:=integer(p);
  offset:=i and $FFFF;
  tipo:=TLogType(i shr 16);
end;

procedure TFLog.FillRobotStateTreeView(num: integer; root: TTreeNode; tree: TTreeView);
var node: TTreeNode;

begin


  with tree.Items,LogBuffer[0].RobotState.RobotState,LogBuffer[0].RobotState.Vmotor do begin
    node:=AddChild(root,'x');
    node.Data:=OffsetTypeToPointer(x,logDouble);
    node:=AddChild(root,'y');
    node.Data:=OffsetTypeToPointer(y,logDouble);
    node:=AddChild(root,'teta');
    node.Data:=OffsetTypeToPointer(teta,logDouble);
    node:=AddChild(root,'v');
    node.Data:=OffsetTypeToPointer(v,logDouble);
    node:=AddChild(root,'vn');
    node.Data:=OffsetTypeToPointer(vn,logDouble);
    node:=AddChild(root,'w');

    node.Data:=OffsetTypeToPointer(w,logDouble);
    node:=AddChild(root,'v1');
    node.Data:=OffsetTypeToPointer(speedw[0],logDouble);
    node:=AddChild(root,'v2');
    node.Data:=OffsetTypeToPointer(speedw[1],logDouble);
    node:=AddChild(root,'v3');
    node.Data:=OffsetTypeToPointer(speedw[2],logDouble);

   end;


end;

(*

// Adicionado por Andre Scolari - 09/11/04
procedure TFLog.FillMotsCurrentTreeView(num: integer; root: TTreeNode; tree: TTreeView);
var node: TTreeNode;
begin
  with tree.Items,LogBuffer[0].RobotState.MotCurrents do begin
    node:=AddChild(root,'I0');
    node.Data:=OffsetTypeToPointer(IAmp[0],logDouble);
    node:=AddChild(root,'I1');
    node.Data:=OffsetTypeToPointer(IAmp[1],logDouble);
    node:=AddChild(root,'I2');
    node.Data:=OffsetTypeToPointer(IAmp[2],logDouble);
    node:=AddChild(root,'I3');
    node.Data:=OffsetTypeToPointer(IAmp[3],logDouble);

    node:=AddChild(root,'Imot0');
    node.Data:=OffsetTypeToPointer(Imotor[0],logDouble);
    node:=AddChild(root,'Imot1');
    node.Data:=OffsetTypeToPointer(Imotor[1],logDouble);
    node:=AddChild(root,'Imot2');
    node.Data:=OffsetTypeToPointer(Imotor[2],logDouble);
    node:=AddChild(root,'Imot3');
    node.Data:=OffsetTypeToPointer(Imotor[3],logDouble);

    node:=AddChild(root,'PWM0');
    node.Data:=OffsetTypeToPointer(pwm[0],logDouble);
    node:=AddChild(root,'PWM1');
    node.Data:=OffsetTypeToPointer(pwm[1],logDouble);
    node:=AddChild(root,'PWM2');
    node.Data:=OffsetTypeToPointer(pwm[2],logDouble);
    node:=AddChild(root,'PWM3');
    node.Data:=OffsetTypeToPointer(pwm[3],logDouble);

    node:=AddChild(root,'DIR0');
    node.Data:=OffsetTypeToPointer(dir[0],logInt);
    node:=AddChild(root,'DIR1');
    node.Data:=OffsetTypeToPointer(dir[1],logInt);
    node:=AddChild(root,'DIR2');
    node.Data:=OffsetTypeToPointer(dir[2],logInt);
    node:=AddChild(root,'DIR3');
    node.Data:=OffsetTypeToPointer(dir[3],logInt);
 

  end;

end;
 *)

(*
procedure TFLog.FillNeckStateTreeView(num: integer; root: TTreeNode; tree: TTreeView);
var node: TTreeNode;
begin
  with tree.Items,LogBuffer[0].RobotState.NeckState do begin
    node:=AddChild(root,'Teta');
    node.Data:=OffsetTypeToPointer(teta,logDouble);
    node:=AddChild(root,'Side');
    node.Data:=OffsetTypeToPointer(side,logWord);
  end;
end;
*)

procedure TFLog.FillRobotInfoTreeView(num: integer; root: TTreeNode; tree: TTreeView);
var node: TTreeNode;
begin
  with tree.Items,LogBuffer[0].RobotState.RobotInfo do begin
    node:=AddChild(root,'lastRole');
    node.Data:=OffsetTypeToPointer(last_role,logRole);
    node:=AddChild(root,'Role');
    node.Data:=OffsetTypeToPointer(Role,logRole);
    node:=AddChild(root,'roleTime');
    node.Data:=OffsetTypeToPointer(RoleTime,logInt);

    node:=AddChild(root,'lastTask');
    node.Data:=OffsetTypeToPointer(last_task,logTask);
    node:=AddChild(root,'Task');
    node.Data:=OffsetTypeToPointer(Task,logTask);
    node:=AddChild(root,'taskTime');
    node.Data:=OffsetTypeToPointer(TaskTime,logInt);

    node:=AddChild(root,'lastAction');
    node.Data:=OffsetTypeToPointer(last_action,logAction);
    node:=AddChild(root,'Action');
    node.Data:=OffsetTypeToPointer(Action,logAction);
    node:=AddChild(root,'actionTime');
    node.Data:=OffsetTypeToPointer(actionTime,logInt);

    node:=AddChild(root,'action_complete');
    node.Data:=OffsetTypeToPointer(action_complete,logBool);
  end;
end;

(*
procedure TFLog.FillActionParsTreeView(num: integer; root: TTreeNode; tree: TTreeView);
var node: TTreeNode;
begin
  with tree.Items,LogBuffer[0].RobotState.ActionPar do begin
    node:=AddChild(root,'x');
    node.Data:=OffsetTypeToPointer(x,logDouble);
    node:=AddChild(root,'y');
    node.Data:=OffsetTypeToPointer(y,logDouble);
    node:=AddChild(root,'ws');
    node.Data:=OffsetTypeToPointer(ws,logDouble);
    node:=AddChild(root,'teta');
    node.Data:=OffsetTypeToPointer(teta,logDouble);
    node:=AddChild(root,'anyway');
    node.Data:=OffsetTypeToPointer(anyway,logBool);
    node:=AddChild(root,'speed');
    node.Data:=OffsetTypeToPointer(speed,logDouble);
    node:=AddChild(root,'speed_on_target');
    node.Data:=OffsetTypeToPointer(speed_on_target,logDouble);
    node:=AddChild(root,'kick');
    node.Data:=OffsetTypeToPointer(kick,logBool);
    //node:=AddChild(root,'avoid');
    //node.Data:=OffsetTypeToPointer(avoid,logAvoid);
//    node:=AddChild(root,'avoidLevel');
 //   node.Data:=OffsetTypeToPointer(avoidLevel,logDouble);
  //  node:=AddChild(root,'subaction');
   // node.Data:=OffsetTypeToPointer(subaction,logAction);
  end;
end;

*)

(*
procedure TFLog.FillTacticCommandTreeView(num: integer; root: TTreeNode; tree: TTreeView);
var node: TTreeNode;
begin
  with tree.Items,LogBuffer[0].RobotState.TacticCommand do begin
    node:=AddChild(root,'v');
    node.Data:=OffsetTypeToPointer(v,logDouble);
    node:=AddChild(root,'w');
    node.Data:=OffsetTypeToPointer(w,logDouble);
    node:=AddChild(root,'kick');
    node.Data:=OffsetTypeToPointer(kick,logBool);
    //node:=AddChild(root,'handler');
    //node.Data:=OffsetTypeToPointer(handler,logHandler);
  end;
end;
*)
procedure TFLog.FillRobotTreeView(num: integer; root: TTreeNode; tree: TTreeView);
var node: TTreeNode;
begin
  with tree.Items do begin
    node:=AddChild(root,'State');
    node.Data:=nil;
    FillRobotStateTreeView(num,node,tree);
//    node:=AddChild(root,'Neck');
//    node.Data:=nil;
//    FillNeckStateTreeView(num,node,tree);
 //   node:=AddChild(root,'MotCurrents');
//    node.Data:=nil;
 //   FillMotsCurrentTreeView(num,node,tree);
    node:=AddChild(root,'Info');
    node.Data:=nil;
    FillRobotInfoTreeView(num,node,tree);
//    node:=AddChild(root,'ActionPars');
//    node.Data:=nil;
//    FillActionParsTreeView(num,node,tree);
 //   node:=AddChild(root,'Commands');
 //   node.Data:=nil;
 //   FillTacticCommandTreeView(num,node,tree);

  end;
end;


procedure TFLog.FillBallTreeView(root: TTreeNode; tree: TTreeView);
var node: TTreeNode;
begin
  with tree.Items,LogBuffer[0].BallState do begin
    node:=AddChild(root,'x');
    node.Data:=OffsetTypeToPointer(x,logDouble);
    node:=AddChild(root,'y');
    node.Data:=OffsetTypeToPointer(y,logDouble);
    node:=AddChild(root,'vx');
    node.Data:=OffsetTypeToPointer(vx,logDouble);
    node:=AddChild(root,'vy');
    node.Data:=OffsetTypeToPointer(vy,logDouble);
    //node:=AddChild(root,'rejected');
    //node.Data:=OffsetTypeToPointer(rejected,logInt);
    node:=AddChild(root,'timestamp');
    node.Data:=OffsetTypeToPointer(timestamp,logInt);
    //node:=AddChild(root,'count');
    //node.Data:=OffsetTypeToPointer(count,logInt);
  end;
end;

procedure TFLog.FillGeneralTreeView(root: TTreeNode; tree: TTreeView);
var node: TTreeNode;
begin
  with tree.Items do begin
    node:=AddChild(root,'Camera');
    node.Data:=OffsetTypeToPointer(LogBuffer[0].camera,logInt);
    node:=AddChild(root,'TimeStamp');
    node.Data:=OffsetTypeToPointer(LogBuffer[0].frame_timestamp,logInt);
  end;
end;

procedure TFLog.FillTreeView(tree: TTreeView);
var node: TTreeNode;
    i: integer;
begin
  tree.Items.Clear;
  with tree.Items do begin
    node:=Add(nil,'General');
    node.Data:=nil;
    FillGeneralTreeView(node,tree);
    node:=Add(nil,'Ball');
    node.Data:=nil;
    FillBallTreeView(node,tree);
{    for i:=0 to MaxRobots-1 do begin
      node:=Add(nil,'Robot '+inttostr(i+1));
      node.Data:=nil;
      FillRobotTreeView(i,node,tree);
    end;}

    node:=Add(nil,'Robot');
    node.Data:=nil;
    FillRobotTreeView(0,node,tree);


    for i:=0 to Count-1 do begin
      if tree.Items[i].Data=nil then begin
        tree.Items[i].ImageIndex:=2
      end else begin
        tree.Items[i].ImageIndex:=0;
      end;
      tree.Items[i].SelectedIndex:=tree.Items[i].ImageIndex;
      // TODO ?
//      tree.Items[i].StateIndex:=-1;
    end;
  end;
end;

// click procedure

procedure TFLog.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var node: TTreeNode;
// TODO
//    HT: THitTests;
begin
  node:=(Sender as TTreeView).GetNodeAt(X,Y);
  if node=nil then exit;
//  HT:=(Sender as TTreeView).GetHitTestInfoAt(X,Y);
//  if htOnIndent in HT then exit;
  if node.ImageIndex=0 then begin
    node.ImageIndex:=1;
    node.SelectedIndex:=1;
  end else begin
    if node.ImageIndex=1 then begin
      node.ImageIndex:=0;
      node.SelectedIndex:=0;
    end;
  end;

  RefreshGrid(TreeView);
end;

function TFLog.LogFrame(par_frame_timestamp,par_camera,increment: integer): integer;
//var i: integer;
begin
  with LogBuffer[LogBufferIn] do begin
    frame_timestamp:=par_frame_timestamp;
    camera:=par_camera;
  end;
  LogBuffer[LogBufferIn].BallState:=BallState;
  //for i:=0 to MaxOponents-1 do begin
  //  LogBuffer[LogBufferIn].OponentState[i]:=OponentState[i];
  //end;

  LogBuffer[LogBufferIn].RobotState.RobotState:=RobotState[myNumber];
  LogBuffer[LogBufferIn].RobotState.MotCurrents:=View.MotsCurrent;
  LogBuffer[LogBufferIn].RobotState.Vmotor:= View.Odos[0];
  LogBuffer[LogBufferIn].RobotState.NeckState:=View.NeckState;
  LogBuffer[LogBufferIn].RobotState.RobotInfo:=RobotInfo;
  LogBuffer[LogBufferIn].RobotState.ActionPar:=ActionPars;
  LogBuffer[LogBufferIn].RobotState.TacticCommand:=TacticCommands;

  //for i:=0 to MaxRobots-1 do begin
  //  LogBuffer[LogBufferIn].RobotState[i].RobotState:=RobotState[i];
  //  LogBuffer[LogBufferIn].RobotState[i].RobotInfo:=RobotInfo[i];
  //  LogBuffer[LogBufferIn].RobotState[i].ActionPar:=ActionPars[i];
  //  LogBuffer[LogBufferIn].RobotState[i].TacticCommand:=TacticCommands[i];
  //end;

  result:=LogBufferIn;

  if increment<>0 then begin
    Inc(LogBufferIn);
    if LogBufferIn>=LogFrames then LogBufferIn:=0;

    if LogBufferCount<LogFrames then Inc(LogBufferCount);
  end;
end;


function TFLog.GetLogString(tree: TTreeView; prop,index: integer): string;
var offset: integer;
    tipo: TLogType;
    ptr: Pointer;
begin
  result:='';
  if tree.Items[prop].Data=nil then exit;

  PointerToOffsetType(tree.Items[prop].Data,offset,tipo);
  ptr:=pchar(@(LogBuffer[index]))+offset;

  case tipo of
    logInt: result:=inttostr(pinteger(ptr)^);
    logDouble: result:=Format('%.3f',[pdouble(ptr)^]);
    logBool: if pboolean(ptr)^ then result:='True' else result:='False';
    logRole: result:=CRoleString[pRole(ptr)^];
    logTask: result:=CTaskString[pTask(ptr)^];
    logAction: result:=CActionString[pAction(ptr)^];
    logWord: result:=inttostr(pword(ptr)^);
    //logKick: result:=CKickString[pKick(ptr)^];
    //logHandler: result:=CHandlerString[pHandler(ptr)^];
    //logAvoid: begin
    //  result:='';
    //  if avoidBall in pAvoidSet(ptr)^ then result:=result+',Ball';
    //  if avoidOponent in pAvoidSet(ptr)^ then result:=result+',Op.';
    //  if avoidRobot in pAvoidSet(ptr)^ then result:=result+',Team';
    //  if avoidWalls in pAvoidSet(ptr)^ then result:=result+',Wall';
    //  if avoidOurArea in pAvoidSet(ptr)^ then result:=result+',Area';
    //end;
  end;
end;


function TFLog.GetLogValue(tree: TTreeView; prop,index: integer): double;
var offset: integer;
    tipo: TLogType;
    ptr: Pointer;
begin
  result:=0.0;
  if tree.Items[prop].Data=nil then exit;

  PointerToOffsetType(tree.Items[prop].Data,offset,tipo);
  ptr:=pchar(@(LogBuffer[index]))+offset;

  case tipo of
    logInt: result:=pinteger(ptr)^;
    logDouble: result:=pdouble(ptr)^;
    logBool: result:=ord(pboolean(ptr)^);
    logRole: result:=ord(pRole(ptr)^);
    logTask: result:=ord(pTask(ptr)^);
    logAction: result:=ord(pAction(ptr)^);
    //logKick: result:=ord(pKick(ptr)^);
    //logHandler: result:=ord(pHandler(ptr)^);
    //logAvoid: begin
    //  result:=0;
    //  if avoidBall in pAvoidSet(ptr)^ then result:=result+1;
    //  if avoidOponent in pAvoidSet(ptr)^ then result:=result+2;
    //  if avoidRobot in pAvoidSet(ptr)^ then result:=result+4;
    //  if avoidWalls in pAvoidSet(ptr)^ then result:=result+8;
    //  if avoidOurArea in pAvoidSet(ptr)^ then result:=result+16;
    //end;
  end;
end;

function RandColor(i: integer): TColor;
begin
  RandSeed:=i;
  random(256);
  result:=random(128) or (random(128) shl 8) or (random(128) shl 16);
end;

function GetPropName(tree: TTreeView; idx: integer): string;
var node: TTreeNode;
begin
  result:='';
  node:=tree.Items[idx];
  while node<>nil do begin
    result:='.'+node.Text+result;
    node:=node.Parent;
  end;
  if result<>'' then result:=copy(result,2,1000);
end;

procedure TFLog.RefreshGrid(tree: TTreeView);
var i,j,cnt,idx: integer;
    cs: TChartSeries;
begin
  with tree.Items do begin

    // clear chart series
    with Chart do begin
      While SeriesList.Count>0 do begin
        cs:=Series[0];
        RemoveSeries(cs);
        cs.Free;
      end;
    end;

    cnt:=0;
    for i:=0 to Count-1 do begin
      if tree.Items[i].ImageIndex=1 then begin
        Inc(cnt);

        // create new chart series for each variable
        cs:=TFastLineSeries.Create(FLog);
        cs.SeriesColor:=RandColor(i);
        Chart.AddSeries(cs);
      end;
    end;

    // set grid size
    if cnt<1 then cnt:=1;
    SGLog.RowCount:=cnt+1;
    if LogBufferCount<1 then begin
      SGLog.ColCount:=2;
      SGLog.Cols[1].Clear;
    end else
      SGLog.ColCount:=LogBufferCount+1;
    SGLog.FixedRows:=1;
    SGLog.FixedCols:=1;
    SGLog.ColWidths[0]:=150;

    cnt:=0;
    for i:=0 to Count-1 do begin
      if tree.Items[i].ImageIndex=1 then begin
        Inc(cnt);
        SGLog.Cells[0,cnt]:=GetPropName(tree,i);
        for j:=0 to LogBufferCount-1 do begin
          idx:=(LogBufferIn-LogBufferCount+j+LogFrames) mod LogFrames;
          SGLog.Cells[j+1,cnt]:=GetLogString(tree,i,idx);
          Chart.Series[cnt-1].AddXY(j,GetLogValue(tree,i,idx),'',clBlack);
        end;
      end;
    end;
  end;
end;

procedure TFLog.LogToStrings(SL:TStrings; tree: TTreeView; idx: integer);
var i: integer;
begin
  with tree.Items do begin
    for i:=0 to Count-1 do begin
      if tree.Items[i].ImageIndex=1 then begin
        SL.add(GetPropName(tree,i)+': '+GetLogString(tree,i,idx));
      end;
    end;
  end;
end;


procedure TFLog.ChartMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var r: TRect;
    selec: integer;
    Sel: TGridRect;
begin
  with Chart do begin
    r:=ChartRect;
    if (x<r.Left) or (x>r.right) then exit;
    if (r.right-r.left)<=0 then exit;

    selec:=round(((x-r.left)*LogBufferCount)/(r.right-r.left))+1;

    if selec>SGLog.ColCount-1 then selec:=SGLog.ColCount-1;
    if selec<1 then selec:=1;

    Sel.Left:=selec;
    Sel.Right:=selec;
    Sel.Top:=1;
    Sel.Bottom:=SGLog.RowCount-1;
    SGLog.Selection:=Sel;

    if SGLog.LeftCol>selec then
      SGLog.LeftCol:=selec;
    if SGLog.LeftCol+SGLog.VisibleColCount<=selec then
      SGLog.LeftCol:=selec-SGLog.VisibleColCount+1;
  end;
end;

function TFLog.LogFrameShowing: integer;
begin
  if not FLog.Showing then begin
    result:=-1;
  end else begin
    result:=(LogBufferIn - LogBufferCount + SGLog.Selection.Left + LogFrames) mod LogFrames;
  end;
end;

procedure TFLog.FormShow(Sender: TObject);
begin
  FillTreeView(TreeView);
  LoadTree(TreeView);
  RefreshGrid(TreeView);
end;

procedure TFLog.MenuExportClick(Sender: TObject);
var i,j: integer;
    s: string;
    sl: TStringList;
begin
  if SaveDialogLog.Execute then begin
    SaveDialogLog.InitialDir:=ExtractFilePath(SaveDialogLog.FileName);

    sl:=TStringList.Create;
    try
      for i:=1 to SGLog.ColCount-1 do begin
        s:='';
        for j:=1 to SGLog.RowCount-1 do begin
          s:=s+Format('%10s ',[SGLog.Cells[i,j]]);
        end;
        sl.Add(s);
      end;
      sl.SaveToFile(SaveDialogLog.FileName);
    finally
      sl.Free;
    end;
  end;
end;

procedure TFLog.MenuClearClick(Sender: TObject);
begin
  LogBufferIn:=0;
  LogBufferCount:=0;
  RefreshGrid(TreeView);
end;

procedure TFLog.FormActivate(Sender: TObject);
begin
  RefreshGrid(TreeView);
end;


end.