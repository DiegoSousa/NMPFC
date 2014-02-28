unit unit_localizationAux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  CNumMinPoins=10;
  MaxPointsNumber=200;
  MaxSeedPoints=100;
  MapW=4098;
  MapH=4098;
  VAR_CONST=1e-9; //Parametro fantasma
  MAX_VAR=500; //caso de singularidade, já que a variancia é 1/segunda_derivada
  t1=0.01;
  t2=0.05;
  CCompassMode=0;
  CCycleTime=40; //in ms

type

  { TLowPassFilter }

  TLowPassFilter=class
    private
      FactualVal:double;
      FprevVal:double;
      FsampleRate:double;
      Ffc,Fwc,Ftau,Falpha:double;
      FinitState:boolean;
    public
      constructor Create(cutoff,smpRate:double;mode:integer);
      function calcLowPassVal(meassure:double):double;
      procedure setFiltPar(cutoff,smpRate:double;mode:integer);
      procedure reset;
  end;

  { TLowPassCompass }

  TLowPassCompass=class(TLowPassFilter)
    function calcCompassVal(meassure:double):double;
  end;

  TTimings=record
    last, actual, max: DWORD;
  end;

  TOdosData=record
    dv, dvn, dteta: double;
    v, vn, w: double;
    Avaible: boolean;
  end;

  TCompassData=record
    Angle: double;
    Avaible: boolean;
  end;

  TPose=record
    x: double;
    y: double;
    teta:double;
  end;

  TSpeed=record
    V: double;
    Vn: double;
    W:double;
  end;

  TPose2=record
    x, y, teta: double;
    CovX, CovY, CovTeta: double;
    CovXY,CovXTeta,CovYTeta:double;
  end;

  TSeedPointsArray = array [0..MaxSeedPoints*4-1] of TPose2;

  TSeedPointsList=record
    PList: TSeedPointsArray;
    PCount: integer;
  end;

  TLocalizationParameters=record
    OdometryKErrorXY: double;
    OdometryKErrorTeta: double;
    SensorsKErrorXY: double;
    SensorsKErrorTeta: double;
    RPropIterationNumber: byte;
    KPoseStarting: TPose2;
    Lc: double;
    MinVarXY: double;
    MinVarTeta: double;
    LimitMeasureMaxDistance: boolean;
    MeasureMaxDistance: double;
    CompassFilterCutoff: double;
    CompassDisable: boolean;
    ProcessLocalizationMode: integer;
    ProcessLocalizationMaxV: double;
    ProcessLocalizationMaxW: double;
    ProcessLocalizationMinIteration: integer;
    ProcessLocalizationFilterCutOff: double;
    TrackingMode: integer;
    KalmanEnable: boolean;
    GlobalLocalizationMode: integer;
    SeedPointsList: TSeedPointsList;
    GlobalLocalizationMaxV: double;
    GlobalLocalizationMaxW: double;
    GlobalLocalizationMinIteration: integer;
    GlobalLocalizationFilterCutOff: double;
    GlobalRPropMinIterationNumber: byte;
    GlobalRPropMaxIterationNumber: byte;
    GlobalMaxDist2Elimination: double;
    GlobalMaxAng2Elimination: double;
    GlobalMaxDeltaNumHypothesis: integer;
    GlobalMinIterationDeltaNumHypothesis: integer;
    GlobalTimesBetter: double;
    NavLostMode: integer;
    NavLostNumMinClosePnt:integer;
    NavLostMaxClosePntDist:double;
    NavLostMinClosePntDist:double;
    NavLostAngleClosePnt:double;
    NavLostAdvisedNavigateV:double;
    NavLostAdvisedNavigateW:double;
    NavLostAngleOffset: double;
  end;

  TPointListArray = array [0..MaxPointsNumber-1] of TPose;

  TPointList = record
    PList: TPointListArray;
    PCount: integer;
  end;

  TLocalizationState=(LocError, LocLost, LocInverted, LocOk);

  TSeedsPointsProcessed=record
    ActualSeedsPoints: TSeedPointsList;
    ActualSeedsPointsCostValues: array [0..MaxSeedPoints*4] of double;
    OrderedCostValues: array [0..MaxSeedPoints*4] of Word;
  end;

  TLocalizationData=record
    Pose: TPose2;
    LasPose: Tpose2;
    PoseOdos: TPose2;
    PoseSensors: Tpose2;
    State: TLocalizationState;
    CompassFiltered: TCompassData;
    Speed: TSpeed;
    AdviseSpeed: TSpeed;
  end;

  //array do mapa
  TMapDist = array [0..MapW-1,0..MapH-1] of double;
  pTMapDist=^TMapDist;
  //array do gradiente
  TMapGrad = record
    x: TMapDist;
    y: TMapDist;
  end;

  { TProcessLocalizationState }

  TProcessLocalizationState=class
    private
      FMode: integer;
      FMaxV, FMaxW: double;
      FMinIteration: integer;
      FIterationCountLost: integer;
      FIterationCountInverted: integer;
      FFilterCutOff: double;
      FErrorFiltered: double;
      FLowPassFilter: TLowPassCompass;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Reset;
      function NewStateMode0(Actualstate:TLocalizationState; OdosData: TOdosData;
                             ActualPose: TPose2; CompassFiltered: TCompassData): TLocalizationState;
      function NewStateMode1(Actualstate:TLocalizationState; OdosData: TOdosData;
                             ActualPose: TPose2; CompassNotFiltered: TCompassData): TLocalizationState;
      function NewStateMode1(OdosData: TOdosData; ActualPose: TPose; CompassNotFiltered: TCompassData): TLocalizationState;
      procedure SetMode0(MaxV, MaxW: double; MinIteration: integer);
      procedure SetMode1(MaxV, MaxW: double; MinIteration: integer; FilterCutOff: double);
  end;

  TLocHypothesis=record
    Pose: TPose2;
    State: TLocalizationState;
    Cost: double;
    ProcessLocState: TProcessLocalizationState;
  end;

  TListRefLocHypothesis=^TListLocHypothesis;
  TListLocHypothesis=record
    Current: ^TLocHypothesis;
    Next: TListRefLocHypothesis;
    Previous: TListRefLocHypothesis;
  end;


  TLocHypothesisSet=record
    LocHypothesis: array [0..MaxSeedPoints-1] of TLocHypothesis;
    NumHypothesis, LastNumHypothesis: integer;
    AuxCountNumIterations: integer;
    ListLocHypothesis: array [0..MaxSeedPoints-1] of TListLocHypothesis;
    ListLocHypothesisHead: TListLocHypothesis;
  end;

  { TGlobalLocalizationState }

  TGlobalLocalization=class
    private
      FMode: integer;
      //mode1
      FStartSeedPointsList: TSeedPointsList;
      FMode1State: (Idle, Running);
      FAdviseSpeed: TSpeed;
      FMaxV, FmaxW: double;
      FRPropIterationNumber, FRPropMinIterationNumber, FRPropMaxIterationNumber: byte;
      FMaxDist2EliminationSquared, FMaxAng2Elimination: double;
      FMaxDeltaNumHypothesis: integer;
      FMinIterationDeltaNumHypothesis: integer;
      FTimesBetter: double;
      FLocHypothesisSet: TLocHypothesisSet;
      function GetMultiHypothesisOn: boolean;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Reset;
      procedure SetMode0;
      procedure SetMode1(StartSeedPointsList: TSeedPointsList; MaxV, MaxW: double;
                         RPropMinIterationNumber, RPropMaxIterationNumber: byte;
                         MaxDist2Elimination, MaxAng2Elimination: double;
                         ProcLocMinIteration: integer;
                         FilterCutOff: double);
      procedure SetMode2(StartSeedPointsList: TSeedPointsList; MaxV, MaxW: double;
                         RPropMinIterationNumber, RPropMaxIterationNumber: byte;
                         MaxDist2Elimination, MaxAng2Elimination: double;
                         ProcLocMinIteration: integer;
                         FilterCutOff: double;
                         MaxDeltaNumHypothesis, MinIterationDeltaNumHypothesis: integer;
                         TimesBetter: double);
      function NewPoseMode0(ActualLocData: TLocalizationData): TLocalizationData;
      function NewPoseMode1(ActualLocData: TLocalizationData; OdosData: TOdosData;Compass: TCompassData; VPointList: TPointList): TLocalizationData;
      function NewPoseMode2(ActualLocData: TLocalizationData; OdosData: TOdosData;Compass: TCompassData; VPointList: TPointList): TLocalizationData;
      property LocHypothesisSet: TLocHypothesisSet read FLocHypothesisSet;
      property MultiHypothesisOn: boolean read GetMultiHypothesisOn;
  end;

  { TLostNavigation }

  TLostNavigation=class
    private
      FMode: integer;
      FNumMinClosePnt:integer;
      FMaxClosePntDist:double;
      FMinClosePntDist:double;
      FAngleClosePnt:double;
      FAdvisedNavigateV:double;
      FAdvisedNavigateW:double;
      FAngleOffset:double;
      FAdviseSpeed: TSpeed;
      FCPointList: TPointList;
      FRotState:integer;
      FActualAng:double;
      function VerifyIfClosePoint(VPointList: TPointList): TPointList;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Reset;
      procedure SetMode0(NumMinClosePnt:integer;
                         MaxClosePntDist:double; MinClosePntDist:double; AngleClosePnt:double;
                         AdvisedNavigateV:double; AdvisedNavigateW:double;
                         AngleOffset: double);
      procedure SetMode1;
      procedure SetMode2;
      procedure CalcAdvisedSpeed0(vehLocState: TLocalizationState; PointList: TPointList; dteta:double);
      property CPointList: TPointList read FCPointList;
      property AdviseSpeed: TSpeed read FAdviseSpeed;
  end;

function PropagateXYTetaOdos0(ActualPose: TPose2; OdosData: TOdosData): TPose2;
function PropagateXYTetaOdos2(ActualPose: TPose2; OdosData: TOdosData; KErrorXY, KErrorTeta: double): TPose2;
function PropagateXYTetaOdos3(ActualPose: TPose2; OdosData: TOdosData; KErrorXY, KErrorTeta: double): TPose2;
procedure LoadMap(FileName: string);
function LoadSeedsPoints(FileName: string): TSeedPointsList;

function OptimizeMatch(out FinalPose:TPose2; InitialPose:TPose2; VPointList: TPointList;
                       IterationNumber:longword; StepWidthXY:double; StepWidthTeta:double): double;
function OptimizeMatch2(out FinalPose:TPose2; InitialPose:TPose2; VPointList: TPointList; IterationNumber:longword;
                        StepWidthXY:double; StepWidthTeta:double): double;
procedure CalcMatchError(out PointErr:double; out dx:double; out dy:double; out dteta:double;
                         x, y, teta:double; VPointList: TPointList);
procedure CalcMatchError2(out PointErr:double; out dx:double; out dy:double; out dteta:double;
                          x, y, teta:double; VPointList: TPointList);
function Distance(p:TPose; var Dist:TMapDist):double;
function Gradient(p:TPose; var Grad:TMapGrad):TPose;
procedure SecondDerivate(out hdX, hdY, hdTeta: double; pose: TPose2; VPointList: TPointList);
function EstimateFusion(PoseOdos, PoseSensors: TPose2; MinVarXY, MinVarTeta: double): TPose2;
function EstimateFusion1(PoseOdos, PoseSensors: TPose2; MinVarXY, MinVarTeta: double): TPose2;

function ProcessSeedPoints(var PointList: TPointList; var SeedPointsList: TSeedPointsList;
                           IterationNumber: longword; AnglesTries: integer): TPose2; //funcao de teste
procedure ProcessSeedPoints2(var PointList: TPointList; var SeedPointsList: TSeedPointsList;
                            IterationNumber: longword; AnglesTries: integer;
                            out SeedsPointsProcessed: TSeedsPointsProcessed); //funcao de teste

function LimitMaxDist(PointsList: TPointList; MaxDist: double): TPointList;

var
  MapDist: TMapDist;
  MapGrad: TMapGrad;
  MapAreaH:integer;
  MapAreaW:integer;
  cell_size:double;
  Lc: double;  //sair daqui um dia
  LcSquared: double;
  SensorsKErrorXY: double;
  SensorsKErrorTeta: double;

implementation

uses
  Utils, math, unit_Log, dynmatrix;

{ TLowPassFilter }

constructor TLowPassFilter.Create(cutoff, smpRate: double; mode: integer);
begin
  FactualVal:=0;
  Ffc:=cutoff;
  Ftau:=1/(2*pi*Ffc);
  Fwc:=1/Ftau;
  FsampleRate:=smpRate;
  if mode=0 then begin
   Falpha:=1-exp(-FsampleRate/Ftau);
  end else begin
   Falpha:=FsampleRate/(FsampleRate+Ftau);
  end;
  FinitState:=false;
end;

function TLowPassFilter.calcLowPassVal(meassure: double): double;
var
 tempVal:double;
begin
  if not FinitState then begin
    FactualVal:=meassure*(1-Falpha)+meassure*Falpha;
    FprevVal:=FactualVal;
    FinitState:=true;
  end else begin
    tempVal:=FactualVal;
    FactualVal:=FprevVal*(1-Falpha)+meassure*Falpha;
    FprevVal:=tempVal;
  end;
  Result:=FactualVal;
end;

procedure TLowPassFilter.setFiltPar(cutoff, smpRate: double; mode: integer);
begin
  Ffc:=cutoff;
  Ftau:=1/(2*pi*Ffc);
  Fwc:=1/Ftau;
  FsampleRate:=smpRate;
  if mode=0 then begin
   Falpha:=1-exp(-FsampleRate/Ftau);
  end else begin
   Falpha:=FsampleRate/(FsampleRate+Ftau);
  end;
end;

procedure TLowPassFilter.reset;
begin
  FactualVal:=0;
end;

{ TLowPassCompass }

function TLowPassCompass.calcCompassVal(meassure: double): double;
var
 tempVal:double;
 termPrev,termMeas:double;
 x1,y1,x2,y2:double;
begin
  if not FinitState then begin
    FactualVal:=meassure*(1-Falpha)+meassure*Falpha;
    FprevVal:=FactualVal;
    FinitState:=true;
  end else begin
    tempVal:=FactualVal;
    //polares
    x1:=cos(meassure);
    y1:=sin(meassure);
    x2:=cos(FprevVal);
    y2:=sin(FprevVal);
    //
    x1:=x1*Falpha+x2*(1-Falpha);
    y1:=y1*Falpha+y2*(1-Falpha);
    //
    FactualVal:=ATan2(y1,x1);
    FprevVal:=tempVal;
  end;
  Result:=FactualVal;
end;

function OptimizeMatch(out FinalPose:TPose2; InitialPose:TPose2; VPointList: TPointList;
                       IterationNumber:longword; StepWidthXY:double; StepWidthTeta:double): double;
var
   parami: array [0..2] of double;
   gradi: array [0..2] of double;
   latest_grad: array [0..2] of double;
   PointErr:double;
   stepwidth: array [0..2] of double;
   i, j:longword;
begin
  //inicializcao de parametros e muda o referencial de sitio
  parami[0]:=InitialPose.x+((MapAreaW/2)*cell_size);
  parami[1]:=InitialPose.y+((MapAreaH/2)*cell_size);
  parami[2]:=InitialPose.teta;
  latest_grad[0]:=0;
  latest_grad[1]:=0;
  latest_grad[2]:=0;
  stepwidth[0]:=StepWidthXY;
  stepwidth[1]:=StepWidthXY;
  stepwidth[2]:=StepWidthTeta;

  //para todos os pontos visiveis efectua as n iteracoes(IterationNumber) do algoritmo RPROP
  for i:=0 to IterationNumber do begin
    //calcula o erro (e outras cenas) associado ao ponto
    CalcMatchError(PointErr,gradi[0],gradi[1], gradi[2], parami[0],parami[1],parami[2], VPointList);
    //aplica o algoritmo RPROP para cada componente da posicao, x, y e teta
    for j := 0 to 2 do
    begin
      if (gradi[j]<>0) then  begin
        if (gradi[j]*latest_grad[j]>0) then begin
          stepwidth[j]:= stepwidth[j]*1.2; //ver parametros fantasma
        end else if (gradi[j]*latest_grad[j]<0) then begin
          stepwidth[j]:= stepwidth[j]*0.5;
        end;

        if (gradi[j] > 0) then begin
          parami[j]:=parami[j]-stepwidth[j];
        end else if (gradi[j]<0) then begin
          parami[j]:=parami[j]+stepwidth[j];
        end;
      end;
      latest_grad[j]:=gradi[j];
    end;
  end;
  //actualiza parametros de saida
  FinalPose.x:=parami[0];
  FinalPose.y:=parami[1];
  FinalPose.teta:=NormalizeAngle(parami[2]);

  //calcula erro da estimativa ou segunda derivada
  SecondDerivate(FinalPose.CovX, FinalPose.CovY, FinalPose.CovTeta, FinalPose, VPointList);
  FinalPose.CovXY:=0;
  FinalPose.CovXTeta:=0;
  FinalPose.CovYTeta:=0;

  //volta a por o referencial no sitio
  FinalPose.x:=FinalPose.x-((MapAreaW/2)*cell_size);
  FinalPose.y:=FinalPose.y-((MapAreaH/2)*cell_size);
  Result:=PointErr/double(VPointList.PCount);
end;

function OptimizeMatch2(out FinalPose:TPose2; InitialPose: TPose2; VPointList: TPointList; IterationNumber: longword;
                        StepWidthXY: double; StepWidthTeta: double): double;
var
   parami: array [0..2] of double;
   gradi: array [0..2] of double;
   latest_grad: array [0..2] of double;
   PointErr:double;
   stepwidth: array [0..2] of double;
   i, j:longword;
begin
  //inicializcao de parametros e muda o referencial de sitio
  parami[0]:=InitialPose.x+((MapAreaW/2)*cell_size);
  parami[1]:=InitialPose.y+((MapAreaH/2)*cell_size);
  parami[2]:=InitialPose.teta;
  latest_grad[0]:=0;
  latest_grad[1]:=0;
  latest_grad[2]:=0;
  stepwidth[0]:=StepWidthXY;
  stepwidth[1]:=StepWidthXY;
  stepwidth[2]:=StepWidthTeta;

  //para todos os pontos visiveis efectua as n iteracoes(IterationNumber) do algoritmo RPROP
  for i:=0 to IterationNumber do begin
    //calcula o erro (e outras cenas) associado ao ponto
    CalcMatchError2(PointErr,gradi[0],gradi[1], gradi[2], parami[0],parami[1],parami[2], VPointList);
    //aplica o algoritmo RPROP para cada componente da posicao, x, y e teta
    for j := 0 to 2 do
    begin
      if (gradi[j]<>0) then  begin
        if (gradi[j]*latest_grad[j]>0) then begin
          stepwidth[j]:= stepwidth[j]*1.2; //ver parametros fantasma
        end else if (gradi[j]*latest_grad[j]<0) then begin
          stepwidth[j]:= stepwidth[j]*0.5;
        end;

        if (gradi[j] > 0) then begin
          parami[j]:=parami[j]-stepwidth[j];
        end else if (gradi[j]<0) then begin
          parami[j]:=parami[j]+stepwidth[j];
        end;
      end;
      latest_grad[j]:=gradi[j];
    end;
  end;
  //actualiza parametros de saida
  FinalPose.x:=parami[0]-((MapAreaW/2)*cell_size);
  FinalPose.y:=parami[1]-((MapAreaH/2)*cell_size);
  FinalPose.teta:=NormalizeAngle(parami[2]);
  Result:=PointErr/double(VPointList.PCount);
end;

//esta funcao calcula a funcao de erro aproximada a err:= (err + 1 - (c*c/(c*c+dist*dist)));
//ou seja, para todos os pontos da linha obtidos na visÃ£o calcula o erro, e a derivada do erro em relacao ao ponto
//tem como saida, o erro ("err") e as primeiras derivadas em ordem a x ("dx") a y ("dy") e a phi ("phi")
procedure CalcMatchError(out PointErr:double; out dx:double; out dy:double; out dteta:double;
                         x, y, teta:double; VPointList: TPointList);
var
   nPoints:longword ;
   vis_iterator: integer;
   vp, ddistdpos: TPose;
   dist, derrddist: double;
begin
  //inicializacao dos parametros
  PointErr:=0;
  dx:=0;
  dy:=0;
  dteta:=0;

  //para todos os pontos visiveis faz o somatorio
  for vis_iterator:=0 to VPointList.PCount-1 do begin
    //calculo da posicao do ponto no sistema de coordenadas global
    vp.x:=x+cos(teta)*VPointList.PList[vis_iterator].x-sin(teta)*VPointList.PList[vis_iterator].y;
    vp.y:=y+sin(teta)*VPointList.PList[vis_iterator].x+cos(teta)*VPointList.PList[vis_iterator].y;

    //distancia do ponto Ã¡ linha real do campo mais proxima do ponto
    if (vp.x >=0) and (vp.y>=0) then
      dist:=Distance(vp, MapDist);

    //somatório da funcao de erro aproximada  err:=(err+1-(c*c/(c*c+dist*dist)))
    PointErr:=PointErr+(1 -(Lc*Lc/(Lc*Lc+dist*dist)));

    //derivada do erro em ordem Ã¡ distania ("dist")
    derrddist:=(2*Lc*Lc*dist)/((Lc*Lc+dist*dist)*(Lc*Lc+dist*dist));

    //derivada da distancia em ordem Ã¡ posicao, calculo do gradiente usando o filtro de sobel
    ddistdpos:=Gradient(vp, MapGrad);

    //somatorio das derivadas do erro em ordem Ã¡ posicaoo (x,y,teta) para todos os pontos
    dx:=dx+derrddist*ddistdpos.x;
    dy:=dy+derrddist*ddistdpos.y;
    dteta:=dteta+derrddist*(ddistdpos.x*(-sin(teta)*VPointList.PList[vis_iterator].x-cos(teta)*VPointList.PList[vis_iterator].y)
                            +ddistdpos.y*(cos(teta)*VPointList.PList[vis_iterator].x-sin(teta)*VPointList.PList[vis_iterator].y));
  end;
end;

procedure CalcMatchError2(out PointErr: double; out dx: double; out dy: double;
  out dteta: double; x, y, teta: double; VPointList: TPointList);
var
   nPoints:longword ;
   vis_iterator: integer;
   vp, ddistdpos: TPose;
   dist, derrddist: double;
   CosTeta, SinTeta: double;
begin
  //inicializacao dos parametros
  PointErr:=0;
  dx:=0;
  dy:=0;
  dteta:=0;
  CosTeta:=cos(teta);
  SinTeta:=sin(teta);

  //para todos os pontos visiveis faz o somatorio
  for vis_iterator:=0 to VPointList.PCount-1 do begin
    //calculo da posicao do ponto no sistema de coordenadas global
    vp.x:=x+CosTeta*VPointList.PList[vis_iterator].x-SinTeta*VPointList.PList[vis_iterator].y;
    vp.y:=y+SinTeta*VPointList.PList[vis_iterator].x+CosTeta*VPointList.PList[vis_iterator].y;

    //distancia do ponto Ã¡ linha real do campo mais proxima do ponto
    dist:=Distance(vp, MapDist);

    //somatório da funcao de erro aproximada  err:=(err+1-(c*c/(c*c+dist*dist)))
    PointErr:=PointErr+(1 -(LcSquared/(LcSquared+dist*dist)));

    //derivada do erro em ordem Ã¡ distania ("dist")
    derrddist:=(2*LcSquared*dist)/((LcSquared+dist*dist)*(LcSquared+dist*dist));

    //derivada da distancia em ordem Ã¡ posicao, calculo do gradiente usando o filtro de sobel
    ddistdpos:=Gradient(vp, MapGrad);

    //somatorio das derivadas do erro em ordem Ã¡ posicaoo (x,y,teta) para todos os pontos
    dx:=dx+derrddist*ddistdpos.x;
    dy:=dy+derrddist*ddistdpos.y;
    dteta:=dteta+derrddist*(ddistdpos.x*(-SinTeta*VPointList.PList[vis_iterator].x-CosTeta*VPointList.PList[vis_iterator].y)
                            +ddistdpos.y*(CosTeta*VPointList.PList[vis_iterator].x-SinTeta*VPointList.PList[vis_iterator].y));
  end;
end;

//esta funcao determina a distancia minima do ponto Ã¡ linha do campo mais proxima
//basicamente lÃª o array "d", na posiÃ§Ã£o de interesse
//p Ã© em metros, necessario converter para pixeis(cÃ©lulas)
function Distance(p:TPose; var Dist:TMapDist):double;
var
  xi, yi:integer;
begin
  xi:=round(p.x/cell_size);
  yi:=round(p.y/cell_size);
  //se o ponto se encontra dentro do campo determina a distancia
  if (xi<0) then xi:=0;
  if (yi<0) then yi:=0;
  if (xi>=MapAreaW-1) then xi:=MapAreaW-1;
  if (yi>=MapAreaH-1) then yi:=MapAreaH-1;
  Result:=Dist[yi,xi];
end;

//esta funÃ§Ã£o determina a derivada da distÃ¢ncia ordem Ã¡ posiÃ§Ã£o, o gradiente
//basicamente lÃª o array "Grad", na posiÃ§Ã£o de interesse
//p Ã© em metros, necessÃ¡rio converter para pixeis(cÃ©lulas)
function Gradient(p:TPose; var Grad:TMapGrad):TPose;
var
  xi, yi:integer;
  r: TPose;
begin
  xi:=round(p.x/cell_size);
  yi:=round(p.y/cell_size);
  //se o ponto estiver fora do campo actualiza para limites do campo
  if xi<0 then xi:=0;
  if yi<0 then yi:=0;
  if xi>=MapAreaW-1 then xi:=MapAreaW-1;
  if yi>=MapAreaH-1 then yi:=MapAreaH-1;
  r.x:=Grad.x[yi,xi];
  r.y:=Grad.y[yi,xi];
  Result:=r;
end;

//esta funcao calcula a segunda derivada do erro em ordem a x,y e phi
procedure SecondDerivate(out hdX, hdY, hdTeta: double; pose: TPose2; VPointList: TPointList);
var
   teta:double;
   PointErr, dErr, ddErr, PointDist:double;
   n_points: longword;
   dDist:TPose;
   vp, dposdphi, ddposdphi2: TPose;
   vis_iterator: integer;
begin
  teta:=pose.teta;
  PointErr:=0;
  hdX:=0;
  hdY:=0;
  hdTeta:=0;
  for vis_iterator:=0 to VPointList.PCount-1 do begin
    vp.x:=pose.x+cos(teta)*VPointList.PList[vis_iterator].x-sin(teta)*VPointList.PList[vis_iterator].y;
    vp.y:=pose.y+sin(teta)*VPointList.PList[vis_iterator].x+cos(teta)*VPointList.PList[vis_iterator].y;
    PointDist:=Distance(vp, MapDist);
    PointErr:= (PointErr + ( 1 - (Lc*Lc/(Lc*Lc+PointDist*PointDist))));
    if (PointDist<2*Lc) then begin
      dErr:=PointDist/(Lc*Lc);
      dderr:=1/(Lc*Lc);
      ddist:=Gradient(vp, MapGrad);
      dposdphi.x:=-sin(teta)*VPointList.PList[vis_iterator].x-cos(teta)*VPointList.PList[vis_iterator].y;
      dposdphi.y:=cos(teta)*VPointList.PList[vis_iterator].x-sin(teta)*VPointList.PList[vis_iterator].y;
      ddposdphi2.x:=-cos(teta)*VPointList.PList[vis_iterator].x+sin(teta)*VPointList.PList[vis_iterator].y;
      ddposdphi2.y:=-sin(teta)*VPointList.PList[vis_iterator].x-cos(teta)*VPointList.PList[vis_iterator].y;

      hdX:=hdX+dderr*ddist.x*ddist.x;
      hdY:=hdY+dderr*ddist.y*ddist.y;
      hdTeta:= hdTeta+dderr*(ddist.x*dposdphi.x+ddist.y*dposdphi.y)*(ddist.x*dposdphi.x+ddist.y*dposdphi.y) + derr*(ddist.x*ddposdphi2.x+ddist.y*ddposdphi2.y);
    end;
  end;

  if hdX<>0 then begin
   hdX:=abs((1/hdX)*SensorsKErrorXY);
   if hdX>MAX_VAR then
     hdX:=MAX_VAR;
  end else begin
   hdX:=MAX_VAR;
  end;

  if hdY<>0 then begin
   hdY:=abs((1/hdY)*SensorsKErrorXY);
   if hdY>MAX_VAR then
     hdY:=MAX_VAR;
  end else begin
   hdY:=MAX_VAR;
  end;

  if hdTeta<>0 then begin
   hdTeta:=abs((1/hdTeta)*SensorsKErrorTeta); //parametro fantasma
   if hdTeta>MAX_VAR then
     hdTeta:=MAX_VAR;
  end else begin
    hdTeta:=MAX_VAR;
  end;
end;

function EstimateFusion(PoseOdos, PoseSensors: TPose2; MinVarXY, MinVarTeta: double): TPose2;
var
  x1, y1, x2, y2, x3, y3: double;
begin
  Result.x:=(PoseSensors.CovX*PoseOdos.x+PoseOdos.CovX*PoseSensors.x)/(PoseSensors.CovX+PoseOdos.CovX);
  Result.CovX:=(PoseOdos.CovX*PoseSensors.CovX)/(PoseOdos.CovX+PoseSensors.CovX);
  Result.y:=(PoseSensors.CovY*PoseOdos.y+PoseOdos.CovY*PoseSensors.y)/(PoseSensors.CovY+PoseOdos.CovY);
  Result.CovY:=(PoseOdos.CovY*PoseSensors.CovY)/(PoseOdos.CovY+PoseSensors.CovY);
  //Result.teta:=(PoseSensors.CovTeta*PoseOdos.teta+PoseOdos.CovTeta*PoseSensors.teta)/(PoseSensors.CovTeta+PoseOdos.CovTeta);
  //passar os angulos para coordenadas cartesianas de modulo 1
  x1:=cos(PoseOdos.teta);
  y1:=sin(PoseOdos.teta);
  x2:=cos(PoseSensors.teta);
  y2:=sin(PoseSensors.teta);
  x3:=(PoseSensors.CovTeta*x1+PoseOdos.CovTeta*x2)/(PoseSensors.CovTeta+PoseOdos.CovTeta);
  y3:=(PoseSensors.CovTeta*y1+PoseOdos.CovTeta*y2)/(PoseSensors.CovTeta+PoseOdos.CovTeta);
  Result.teta:=ATan2(y3, x3);
  Result.CovTeta:=(PoseOdos.CovTeta*PoseSensors.CovTeta)/(PoseOdos.CovTeta+PoseSensors.CovTeta);

  //limitar o valor minimo
  Result.CovX:=Max(Result.CovX, MinVarXY);
  Result.CovY:=Max(Result.CovY, MinVarXY);
  Result.CovTeta:=Max(Result.CovTeta, MinVarTeta);
  Result.CovXY:=0;
  Result.CovXTeta:=0;
  Result.CovYTeta:=0;
end;

function EstimateFusion1(PoseOdos, PoseSensors: TPose2; MinVarXY,
  MinVarTeta: double): TPose2;
var
  x1, y1, x2, y2, x3, y3: double;
  P,R,KGain,State,Z,Zest,Vinov:TDMatrix;
begin
  R.SetSize(3,3);
  R.setv(0,0,PoseSensors.CovX);
  R.setv(1,1,PoseSensors.CovY);
  R.setv(2,2,PoseSensors.CovTeta);
  //
  P.SetSize(3,3);
  // CovX..
  P.setv(0,0,PoseOdos.CovX);
  P.setv(0,1,PoseOdos.CovXY);
  P.setv(0,2,PoseOdos.CovXTeta);
  // CovY..
  P.setv(1,0,PoseOdos.CovXY);
  P.setv(1,1,PoseOdos.CovY);
  P.setv(1,2,PoseOdos.CovYTeta);
  // CovTeta..
  P.setv(2,0,PoseOdos.CovXTeta);
  P.setv(2,1,PoseOdos.CovYTeta);
  P.setv(2,2,PoseOdos.CovTeta);
  //
  KGain.SetSize(3,3);
  KGain:=P*Minv(P+R);
  //
  State.SetSize(3,1);
  State.setv(0,0,PoseOdos.X);
  State.setv(1,0,PoseOdos.Y);
  State.setv(2,0,PoseOdos.Teta);
  //
  Zest.SetSize(3,1);
  Zest:=State;
  //
  Z.SetSize(3,1);
  Z.setv(0,0,PoseSensors.x);
  Z.setv(1,0,PoseSensors.y);
  Z.setv(2,0,PoseSensors.teta);
  Vinov:=Z-Zest;
  Vinov.setv(2,0,NormalizeAngle(Vinov.getv(2,0)));
  State:=State+KGain*Vinov;
  P:=(Meye(3)-KGain)*P;
  //neste caso há cruzadas
  Result.CovX:=P.getv(0,0);
  Result.CovY:=P.getv(1,1);
  Result.CovTeta:=P.getv(2,2);
  Result.CovXY:=P.getv(0,1);
  Result.CovXTeta:=P.getv(0,2);
  Result.CovYTeta:=P.getv(1,2);
  //
  Result.x:=State.getv(0,0);
  Result.y:=State.getv(1,0);
  Result.teta:=NormalizeAngle(State.getv(2,0));

    //limitar o valor minimo
  Result.CovX:=Max(Result.CovX, MinVarXY);
  Result.CovY:=Max(Result.CovY, MinVarXY);
  Result.CovTeta:=Max(Result.CovTeta, MinVarTeta);
end;

function ProcessSeedPoints(var PointList: TPointList; var SeedPointsList: TSeedPointsList; IterationNumber: longword; AnglesTries: integer): TPose2;
var
  TestAngleOffset: double;
  TestAngle: double;
  i,j: integer;
  BestFit, ActualFit: double;
  TestPose: Tpose2;
begin
  if SeedPointsList.PCount<=0 then begin
    raise Exception.Create('ProcessSeedPoints, SeedPointsList empty');
  end;
  TestAngleOffset:=2*Pi/AnglesTries;

  SeedPointsList.PList[0].teta:=0;//inicialize
  BestFit:=OptimizeMatch(Result, SeedPointsList.PList[0], PointList ,IterationNumber, t1, t2);
  for j:=1 to AnglesTries-1 do begin
    TestAngle:=j*TestAngleOffset;
    SeedPointsList.PList[0].teta:=TestAngle;
    ActualFit:=OptimizeMatch(TestPose, SeedPointsList.PList[0], PointList ,IterationNumber, t1, t2);
    if ActualFit<BestFit then begin
      BestFit:=ActualFit;
      Result:=TestPose;
    end;
  end;

  for i:=1 to SeedPointsList.PCount-1 do begin
    for j:=0 to AnglesTries-1 do begin
      TestAngle:=j*TestAngleOffset;
      SeedPointsList.PList[i].teta:=TestAngle;
      ActualFit:=OptimizeMatch(TestPose, SeedPointsList.PList[i], PointList ,IterationNumber, t1, t2);
      if ActualFit<BestFit then begin
        BestFit:=ActualFit;
        Result:=TestPose;
      end;
    end;
  end;
end;

procedure ProcessSeedPoints2(var PointList: TPointList; var SeedPointsList: TSeedPointsList;
                             IterationNumber: longword; AnglesTries: integer;
                             out SeedsPointsProcessed: TSeedsPointsProcessed); //funcao de teste!!!!
const
  KMindist=2;
  KMindistAng=5;//degree
var
  i, j, k: integer;
  TestAngleOffset, TestAngle: double;
  ActualFit: double;
  TestPose: Tpose2;
  CostR, CostL: double;
  aux: integer;
  dist, distAng: double;
  x1, y1, x2, y2, teta1, teta2: double;
  Mindist: double;
  MindistAng: double;
begin
  if SeedPointsList.PCount<=0 then begin
    raise Exception.Create('ProcessSeedPoints, SeedPointsList empty')
  end;
  TestAngleOffset:=2*Pi/AnglesTries;
  SeedsPointsProcessed.ActualSeedsPoints.PCount:=0;
  for i:=0 to SeedPointsList.PCount-1 do begin
    for j:=0 to AnglesTries-1 do begin
      TestAngle:=j*TestAngleOffset;
      SeedPointsList.PList[i].teta:=TestAngle;
      ActualFit:=OptimizeMatch(TestPose, SeedPointsList.PList[i], PointList ,IterationNumber, t1, t2);
      SeedsPointsProcessed.ActualSeedsPoints.PList[SeedsPointsProcessed.ActualSeedsPoints.PCount]:=TestPose;
      SeedsPointsProcessed.ActualSeedsPointsCostValues[SeedsPointsProcessed.ActualSeedsPoints.PCount]:=ActualFit;
      inc(SeedsPointsProcessed.ActualSeedsPoints.PCount);
    end;
  end;

  //eliminar repetidos
  Mindist:=cell_size*KMindist;
  MindistAng:=pi/180*KMindistAng;
  i:=0;
  while i<SeedsPointsProcessed.ActualSeedsPoints.PCount do begin
    j:=i+1;
    while j<SeedsPointsProcessed.ActualSeedsPoints.PCount do begin
      x1:=SeedsPointsProcessed.ActualSeedsPoints.PList[i].x;
      y1:=SeedsPointsProcessed.ActualSeedsPoints.PList[i].y;
      teta1:=SeedsPointsProcessed.ActualSeedsPoints.PList[i].teta;
      x2:=SeedsPointsProcessed.ActualSeedsPoints.PList[j].x;
      y2:=SeedsPointsProcessed.ActualSeedsPoints.PList[j].y;
      teta2:=SeedsPointsProcessed.ActualSeedsPoints.PList[j].teta;
      dist:=sqrt(power(x2-x1,2)+power(y2-y1,2));
      distAng:=abs(NormalizeAngle(teta2-teta1));
      if (dist<Mindist) and (distAng<MindistAng) then begin
        //elimina j
        for k:=j to SeedsPointsProcessed.ActualSeedsPoints.PCount-2 do begin
          SeedsPointsProcessed.ActualSeedsPoints.PList[k]:=SeedsPointsProcessed.ActualSeedsPoints.PList[k+1];
          SeedsPointsProcessed.ActualSeedsPointsCostValues[k]:=SeedsPointsProcessed.ActualSeedsPointsCostValues[k+1];
        end;
        Dec(SeedsPointsProcessed.ActualSeedsPoints.PCount);
      end
      else begin
        inc(j);
      end;
    end;
    inc(i);
  end;

  //Sort, O mais reles! depois implementar um mais rapido
  for i:=0 to SeedsPointsProcessed.ActualSeedsPoints.PCount-1 do begin
    SeedsPointsProcessed.OrderedCostValues[i]:=i;
  end;

  i:=1;
  while i<SeedsPointsProcessed.ActualSeedsPoints.PCount do begin
    CostR:=SeedsPointsProcessed.ActualSeedsPointsCostValues[SeedsPointsProcessed.OrderedCostValues[i]];
    CostL:=SeedsPointsProcessed.ActualSeedsPointsCostValues[SeedsPointsProcessed.OrderedCostValues[i-1]];
    if CostL<=CostR then begin
      inc(i);
    end
    else begin
      aux:=SeedsPointsProcessed.OrderedCostValues[i];
      SeedsPointsProcessed.OrderedCostValues[i]:=SeedsPointsProcessed.OrderedCostValues[i-1];
      SeedsPointsProcessed.OrderedCostValues[i-1]:=aux;
      i:=1;
    end;
  end;
end;

function LimitMaxDist(PointsList: TPointList; MaxDist: double): TPointList;
var
  i: integer;
  distance: double;
begin
  Result.PCount:=0;
  for i:=0 to PointsList.PCount-1 do begin
  distance:=Dist(PointsList.PList[i].x,PointsList.PList[i].y);
    if distance<MaxDist then begin
      Result.PList[Result.PCount].x:=PointsList.PList[i].x;
      Result.PList[Result.PCount].y:=PointsList.PList[i].y;
      inc(Result.PCount);
    end;
  end;
end;

function PropagateXYTetaOdos0(ActualPose: TPose2; OdosData: TOdosData): TPose2;
var
  ce, se, cde, sde: double;
  deltaX, deltaY, deltaTeta: double;
begin
  if OdosData.Avaible then begin
    ce:=cos(ActualPose.teta);
    se:=sin(ActualPose.teta);
    cde:=cos(OdosData.dteta);
    sde:=sin(OdosData.dteta);
    if abs(OdosData.dteta)=0 then begin
      deltaX:=(OdosData.dv*ce-OdosData.dvn*se);
      deltaY:=(OdosData.dv*se+OdosData.dvn*ce);
      deltaTeta:=OdosData.dteta;
    end
    else begin
      deltaX:=(OdosData.dv*sde+OdosData.dvn*(cde-1))*cos(ActualPose.teta+OdosData.dteta/2)/OdosData.dteta
               -(OdosData.dv*(1-cde)+OdosData.dvn*sde)*sin(ActualPose.teta+OdosData.dteta/2)/OdosData.dteta;
      deltaY:=(OdosData.dv*sde+OdosData.dvn*(cde-1))*sin(ActualPose.teta+OdosData.dteta/2)/OdosData.dteta
               +(OdosData.dv*(1-cde)+OdosData.dvn*sde)*cos(ActualPose.teta+OdosData.dteta/2)/OdosData.dteta;
      deltaTeta:=OdosData.dteta;
    end;
    Result.x:=ActualPose.x+deltaX;
    Result.y:=ActualPose.y+deltaY;
    Result.teta:=NormalizeAngle(ActualPose.teta+deltaTeta);
  end
  else begin
    Result:=ActualPose;
  end;
end;

function PropagateXYTetaOdos2(ActualPose: TPose2; OdosData: TOdosData; KErrorXY, KErrorTeta: double): TPose2;
var
  ce, se, cde, sde: double;
  deltaX, deltaY, deltaTeta: double;
begin
  if OdosData.Avaible then begin
    ce:=cos(ActualPose.teta);
    se:=sin(ActualPose.teta);
    cde:=cos(OdosData.dteta);
    sde:=sin(OdosData.dteta);
    if abs(OdosData.dteta)=0 then begin
      deltaX:=(OdosData.dv*ce-OdosData.dvn*se);
      deltaY:=(OdosData.dv*se+OdosData.dvn*ce);
      deltaTeta:=OdosData.dteta;
    end
    else begin
      deltaX:=(OdosData.dv*sde+OdosData.dvn*(cde-1))*cos(ActualPose.teta+OdosData.dteta/2)/OdosData.dteta
               -(OdosData.dv*(1-cde)+OdosData.dvn*sde)*sin(ActualPose.teta+OdosData.dteta/2)/OdosData.dteta;
      deltaY:=(OdosData.dv*sde+OdosData.dvn*(cde-1))*sin(ActualPose.teta+OdosData.dteta/2)/OdosData.dteta
               +(OdosData.dv*(1-cde)+OdosData.dvn*sde)*cos(ActualPose.teta+OdosData.dteta/2)/OdosData.dteta;
      deltaTeta:=OdosData.dteta;
    end;
    Result.x:=ActualPose.x+deltaX;
    Result.y:=ActualPose.y+deltaY;
    Result.teta:=NormalizeAngle(ActualPose.teta+deltaTeta);
    Result.CovX:=ActualPose.CovX+(KErrorXY*abs(deltaX))*(KErrorXY*abs(deltaX));
    Result.CovY:=ActualPose.CovY+(KErrorXY*abs(deltaY))*(KErrorXY*abs(deltaY));
    Result.CovTeta:=ActualPose.CovTeta+(KErrorTeta*abs(deltaTeta))*(KErrorTeta*abs(deltaTeta));
    Result.CovXY:=0;
    Result.CovXTeta:=0;
    Result.CovYTeta:=0;
    //Result.CovX:=power(sqrt(ActualPose.CovX)+KErrorXY*abs(deltaX),2);
    //Result.CovY:=power(sqrt(ActualPose.CovY)+KErrorXY*abs(deltaY),2);
    //Result.CovTeta:=power(sqrt(ActualPose.CovTeta)+KErrorXY*abs(deltaTeta),2);
  end
  else begin
    Result:=ActualPose;
  end;
end;

function PropagateXYTetaOdos3(ActualPose: TPose2; OdosData: TOdosData;
  KErrorXY, KErrorTeta: double): TPose2;
var
  ce, se, cde, sde: double;
  deltaX, deltaY, deltaTeta: double;
  k1,k2:double;
  dx_dx,dx_dy,dx_dTeta:double;
  dy_dx,dy_dy,dy_dTeta:double;
  dTeta_dx,dTeta_dy,dTeta_dTeta:double;
  gradFx:TDMatrix;
  Q,P:TDMatrix;
begin
  if OdosData.Avaible then begin
    ce:=cos(ActualPose.teta);
    se:=sin(ActualPose.teta);
    cde:=cos(OdosData.dteta);
    sde:=sin(OdosData.dteta);
    if abs(OdosData.dteta)=0 then begin
      deltaX:=(OdosData.dv*ce-OdosData.dvn*se);
      deltaY:=(OdosData.dv*se+OdosData.dvn*ce);
      deltaTeta:=OdosData.dteta;
      //
      dx_dx:=1;
      dx_dy:=0;
      dx_dTeta:=(-OdosData.dv*se-OdosData.dvn*ce);
      dy_dx:=0;
      dy_dy:=1;
      dy_dTeta:=(+OdosData.dv*ce-OdosData.dvn*se);
      dTeta_dx:=0;
      dTeta_dy:=0;
      dTeta_dTeta:=1;
    end
    else begin
      k1:=(OdosData.dv*sde+OdosData.dvn*(cde-1))/OdosData.dteta;
      k2:=(OdosData.dv*(1-cde)+OdosData.dvn*sde)/OdosData.dteta;
      deltaX:=k1*cos(ActualPose.teta+OdosData.dteta/2)-k2*sin(ActualPose.teta+OdosData.dteta/2);
      deltaY:=k1*sin(ActualPose.teta+OdosData.dteta/2)+k2*cos(ActualPose.teta+OdosData.dteta/2);
      deltaTeta:=OdosData.dteta;
      dx_dx:=1;
      dx_dy:=0;
      dx_dTeta:=-k1*sin(ActualPose.teta+OdosData.dteta/2)-k2*cos(ActualPose.teta+OdosData.dteta/2);
      dy_dx:=0;
      dy_dy:=1;
      dy_dTeta:=+k1*cos(ActualPose.teta+OdosData.dteta/2)-k2*sin(ActualPose.teta+OdosData.dteta/2);
      dTeta_dx:=0;
      dTeta_dy:=0;
      dTeta_dTeta:=1;
      //
    end;
    //
    gradFx.SetSize(3,3);
    // dx_dX
    gradFx.setv(0,0,dx_dx);
    gradFx.setv(0,1,dx_dy);
    gradFx.setv(0,2,dx_dTeta);
    // dy_dX
    gradFx.setv(1,0,dy_dx);
    gradFx.setv(1,1,dy_dy);
    gradFx.setv(1,2,dy_dTeta);
    // dTeta_dX
    gradFx.setv(2,0,dTeta_dx);
    gradFx.setv(2,1,dTeta_dy);
    gradFx.setv(2,2,dTeta_dTeta);
    //
    Q.SetSize(3,3);
    //
    Q.setv(0,0,(KErrorXY*abs(deltaX))*(KErrorXY*abs(deltaX)));
    Q.setv(1,1,(KErrorXY*abs(deltaY))*(KErrorXY*abs(deltaY)));
    Q.setv(2,2,(KErrorTeta*abs(deltaTeta))*(KErrorTeta*abs(deltaTeta)));
    //
    P.SetSize(3,3);
    // CovX..
    P.setv(0,0,ActualPose.CovX);
    P.setv(0,1,ActualPose.CovXY);
    P.setv(0,2,ActualPose.CovXTeta);
    // CovY..
    P.setv(1,0,ActualPose.CovXY);
    P.setv(1,1,ActualPose.CovY);
    P.setv(1,2,ActualPose.CovYTeta);
    // CovTeta..
    P.setv(2,0,ActualPose.CovXTeta);
    P.setv(2,1,ActualPose.CovYTeta);
    P.setv(2,2,ActualPose.CovTeta);
    //
    P:=gradFx*P*Mtran(gradFx)+Q;
    //neste caso há cruzadas
    Result.CovX:=P.getv(0,0);
    Result.CovY:=P.getv(1,1);
    Result.CovTeta:=P.getv(2,2);
    //
    Result.CovXY:=P.getv(0,1);
    Result.CovXTeta:=P.getv(0,2);
    Result.CovYTeta:=P.getv(1,2);
    Result.x:=ActualPose.x+deltaX;
    Result.y:=ActualPose.y+deltaY;
    Result.teta:=NormalizeAngle(ActualPose.teta+deltaTeta);
  end
  else begin
    Result:=ActualPose;
  end;
end;

//map functions
procedure LoadMap(FileName: string);
var
  i,j: integer;
  lines, lin:integer;
  thisline,thisline2,thisline3: TStringList;
  fileMap,fileMap2,fileMap3: TStringList;
begin
  try
    fileMap:=TStringList.Create;
    fileMap2:=TStringList.Create;
    fileMap3:=TStringList.Create;
    thisline:=TStringList.Create;
    thisline2:=TStringList.Create;
    thisline3:=TStringList.Create;
    fileMap.LoadFromFile(FileName);
    lines:=fileMap.Count;
    fileMap2.LoadFromFile(FileName+'gx');
    fileMap3.LoadFromFile(FileName+'gy');

    ParseString(fileMap[0],',',thisline);
    cell_size:=StrToFloat(thisline[0]);
    MapAreaW:=round(StrToFloat(thisline[1])/cell_size);
    MapAreaH:=round(StrToFloat(thisline[2])/cell_size);
    for i := 1 to lines-1 do begin
      ParseString(fileMap[i],',',thisline);
      ParseString(fileMap2[i],',',thisline2);
      ParseString(fileMap3[i],',',thisline3);
      lin:=thisline.Count;
      if (thisline.Count<MapAreaW-1) or (thisline2.Count<MapAreaW-1) or (thisline3.Count<MapAreaW-1) then begin
        if thisline.Count < lin then
          lin:=thisline.Count;
        if thisline2.Count < lin then
          lin:=thisline2.Count;
        if thisline3.Count < lin then
          lin:=thisline3.Count;
      end;
      for j := 0 to lin-1 do begin
        MapDist[i-1,j]:=StrToFloat(thisline[j]);
        MapGrad.x[i-1,j]:=StrToFloat(thisline2[j]);
        MapGrad.y[i-1,j]:=StrToFloat(thisline3[j]);
      end;
    end;
  finally
    fileMap.Free;
    fileMap2.Free;
    fileMap3.Free;
  end;
end;

function LoadSeedsPoints(FileName: string): TSeedPointsList;
var
  logSeedPoints: TLogText;
  i: integer;
  NewLine: array of double;
begin
  logSeedPoints:=TLogText.Create;
  if logSeedPoints.InicializePlay(FileName) then begin
    if logSeedPoints.NColumns=2 then begin
      Result.PCount:=logSeedPoints.NLines;
      SetLength(NewLine,2);
      for i:=0 to Result.PCount-1 do begin
        logSeedPoints.GetNewLine(NewLine, i);
        Result.PList[i].x:=NewLine[0];
        Result.PList[i].y:=NewLine[1];
      end;
      SetLength(NewLine,0);
    end
    else begin
      Result.PCount:=0;
    end;
  end
  else begin
    Result.PCount:=0;
  end;
  logSeedPoints.Free;
end;

{ TProcessLocalizationState }

constructor TProcessLocalizationState.Create;
begin
  FMode:=-1;
  FLowPassFilter:=TLowPassCompass.Create(FFilterCutOff,CCycleTime/1000,CCompassMode);
  Reset;
end;

destructor TProcessLocalizationState.Destroy;
begin
  FLowPassFilter.Free;
  inherited Destroy;
end;

procedure TProcessLocalizationState.Reset;
begin
  FIterationCountInverted:=0;
  FIterationCountLost:=0;
  FErrorFiltered:=0;
  FLowPassFilter.reset;
end;

function TProcessLocalizationState.NewStateMode0(ActualState: TLocalizationState; OdosData: TOdosData; ActualPose: TPose2; CompassFiltered: TCompassData): TLocalizationState;
const
  CompassMaxError=pi/180*45;
var
  CompassError, V: double;
begin
  if FMode=0 then begin
    if OdosData.Avaible and CompassFiltered.Avaible then begin
      V:=sqrt(power(OdosData.v, 2)+power(OdosData.vn, 2));
      if (V<FMaxV) and (OdosData.w<FMaxW) then begin
        CompassError:=abs(NormalizeAngle(CompassFiltered.Angle-ActualPose.teta));
        if CompassError>CompassMaxError then begin
          if CompassError>(pi-CompassMaxError) then begin
            inc(FIterationCountInverted);
          end
          else begin
            inc(FIterationCountLost);
          end;
          if (FIterationCountInverted+FIterationCountLost)>=FMinIteration then begin
            if FIterationCountInverted>FIterationCountLost then begin
              Result:=LocInverted;
            end
            else begin
              Result:=LocLost;
            end;
            FIterationCountLost:=0;
            FIterationCountInverted:=0;
          end
          else begin
            Result:=ActualState;
          end;
        end
        else begin
          FIterationCountLost:=0;
          FIterationCountInverted:=0;
          Result:=ActualState;
        end;
      end
      else begin
        FIterationCountLost:=0;
        FIterationCountInverted:=0;
        Result:=ActualState;
      end;
    end
    else begin
      Result:=ActualState;
    end;
  end
  else begin
    raise Exception.Create('Process LocalizationState Error');
    Result:=ActualState;
  end;
end;

function TProcessLocalizationState.NewStateMode1(Actualstate: TLocalizationState; OdosData: TOdosData; ActualPose: TPose2;
                                                 CompassNotFiltered: TCompassData): TLocalizationState;
const
  CompassMaxError=pi/180*45;
var
  CompassError, CompassErrorFiltered, V: double;
begin
  if FMode=1 then begin
    if OdosData.Avaible and CompassNotFiltered.Avaible then begin
      V:=sqrt(power(OdosData.v, 2)+power(OdosData.vn, 2));
      if (V<FMaxV) and (OdosData.w<FMaxW) then begin
        CompassError:=NormalizeAngle(CompassNotFiltered.Angle-ActualPose.teta);
        FErrorFiltered:=FLowPassFilter.calcCompassVal(CompassError);
        CompassErrorFiltered:=abs(FErrorFiltered);
        if CompassErrorFiltered>CompassMaxError then begin
          if CompassErrorFiltered>(pi-CompassMaxError) then begin
            inc(FIterationCountInverted);
          end
          else begin
            inc(FIterationCountLost);
          end;
          if (FIterationCountInverted+FIterationCountLost)>=FMinIteration then begin
            if FIterationCountInverted>FIterationCountLost then begin
              Result:=LocInverted;
            end
            else begin
              Result:=LocLost;
            end;
            FLowPassFilter.reset;
            FIterationCountLost:=0;
            FIterationCountInverted:=0;
          end
          else begin
            Result:=ActualState;
          end;
        end
        else begin
          FIterationCountLost:=0;
          FIterationCountInverted:=0;
          Result:=ActualState;
        end;
      end
      else begin
        FIterationCountLost:=0;
        FIterationCountInverted:=0;
        Result:=ActualState;
      end;
    end
    else begin
      Result:=ActualState;
    end;
  end
  else begin
    Result:=ActualState;
    raise Exception.Create('Process LocalizationState Error');
  end;
end;

function TProcessLocalizationState.NewStateMode1(OdosData: TOdosData; ActualPose: TPose;
                                                 CompassNotFiltered: TCompassData): TLocalizationState;
const
  CompassMaxError=pi/180*45;
var
  CompassError, CompassErrorFiltered, V: double;
begin
  if FMode=1 then begin
    if OdosData.Avaible and CompassNotFiltered.Avaible then begin
      V:=sqrt(power(OdosData.v, 2)+power(OdosData.vn, 2));
      if (V<FMaxV) and (OdosData.w<FMaxW) then begin
        CompassError:=NormalizeAngle(CompassNotFiltered.Angle-ActualPose.teta);
        FErrorFiltered:=FLowPassFilter.calcCompassVal(CompassError);
        CompassErrorFiltered:=abs(FErrorFiltered);
        if CompassErrorFiltered>CompassMaxError then begin
          if CompassErrorFiltered>(pi-CompassMaxError) then begin
            inc(FIterationCountInverted);
          end
          else begin
            inc(FIterationCountLost);
          end;
          if (FIterationCountInverted+FIterationCountLost)>=FMinIteration then begin
            if FIterationCountInverted>FIterationCountLost then begin
              Result:=LocInverted;
            end
            else begin
              Result:=LocLost;
            end;
            FLowPassFilter.reset;
            FIterationCountLost:=0;
            FIterationCountInverted:=0;
          end
          else begin
            Result:=LocOk;
          end;
        end
        else begin
          FIterationCountLost:=0;
          FIterationCountInverted:=0;
          Result:=LocOk;
        end;
      end
      else begin
        FIterationCountLost:=0;
        FIterationCountInverted:=0;
        Result:=LocOk;
      end;
    end
    else begin
      Result:=LocOk;
    end;
  end
  else begin
    Result:=LocOk;
    raise Exception.Create('Process LocalizationState Error');
  end;
end;

procedure TProcessLocalizationState.SetMode0(MaxV, MaxW: double; MinIteration: integer);
begin
  FMode:=0;
  FMaxV:=MaxV;
  FMaxW:=MaxW;
  FMinIteration:=MinIteration;
end;

procedure TProcessLocalizationState.SetMode1(MaxV, MaxW: double;
  MinIteration: integer; FilterCutOff: double);
begin

  FMode:=1;
  FMaxV:=MaxV;
  FMaxW:=MaxW;
  FMinIteration:=MinIteration;
  FFilterCutOff:=FilterCutOff;
  FLowPassFilter.setFiltPar(FFilterCutOff,CCycleTime/1000,CCompassMode);
end;

{ TGlobalLocalizationState }

function TGlobalLocalization.GetMultiHypothesisOn: boolean;
begin
  if FMode1State=Running then begin
    Result:=true;
  end
  else begin
    Result:=false;
  end;
end;

constructor TGlobalLocalization.Create;
var
  i: integer;
begin
  FMode:=-1;
  for i:=0 to MaxSeedPoints-1 do begin
    FLocHypothesisSet.LocHypothesis[i].ProcessLocState:=TProcessLocalizationState.Create;
    FLocHypothesisSet.ListLocHypothesis[i].Current:=@FLocHypothesisSet.LocHypothesis[i];
  end;
  Reset;
end;

destructor TGlobalLocalization.Destroy;
var
  i: integer;
begin
  for i:=0 to MaxSeedPoints-1 do begin
    FLocHypothesisSet.LocHypothesis[i].ProcessLocState.Free;
  end;
  inherited Destroy;
end;

procedure TGlobalLocalization.Reset;
var
  i: integer;
begin
  FMode1State:=Idle;
  for i:=0 to MaxSeedPoints-1 do begin
    FLocHypothesisSet.LocHypothesis[i].ProcessLocState.Reset;
  end;
end;

procedure TGlobalLocalization.SetMode0;
begin
  Reset;
  FMode:=0;
end;

procedure TGlobalLocalization.SetMode1(StartSeedPointsList: TSeedPointsList;
  MaxV, MaxW: double; RPropMinIterationNumber, RPropMaxIterationNumber: byte;
  MaxDist2Elimination, MaxAng2Elimination: double;
  ProcLocMinIteration: integer; FilterCutOff: double);
var
  i: integer;
begin
  Reset;
  FMode:=1;
  FStartSeedPointsList:=StartSeedPointsList;
  FMaxV:=MaxV;
  FmaxW:=MaxW;
  FMaxDist2EliminationSquared:=power(MaxDist2Elimination,2);
  FMaxAng2Elimination:=MaxAng2Elimination;
  FRPropMinIterationNumber:=RPropMinIterationNumber;
  FRPropMaxIterationNumber:=RPropMaxIterationNumber;
  for i:=0 to MaxSeedPoints-1 do begin
    FLocHypothesisSet.LocHypothesis[i].ProcessLocState.SetMode1(FMaxV,FmaxW,ProcLocMinIteration,FilterCutOff);
  end;
end;

procedure TGlobalLocalization.SetMode2(StartSeedPointsList: TSeedPointsList;
  MaxV, MaxW: double; RPropMinIterationNumber, RPropMaxIterationNumber: byte;
  MaxDist2Elimination, MaxAng2Elimination: double;
  ProcLocMinIteration: integer; FilterCutOff: double; MaxDeltaNumHypothesis,
  MinIterationDeltaNumHypothesis: integer; TimesBetter: double);
var
  i: integer;
begin
  Reset;
  FMode:=2;
  FStartSeedPointsList:=StartSeedPointsList;
  FMaxV:=MaxV;
  FmaxW:=MaxW;
  FMaxDist2EliminationSquared:=power(MaxDist2Elimination,2);
  FMaxAng2Elimination:=MaxAng2Elimination;
  FRPropMinIterationNumber:=RPropMinIterationNumber;
  FRPropMaxIterationNumber:=RPropMaxIterationNumber;
  FMaxDeltaNumHypothesis:=MaxDeltaNumHypothesis;
  FMinIterationDeltaNumHypothesis:=MinIterationDeltaNumHypothesis;
  if TimesBetter>=1 then begin
    FTimesBetter:=TimesBetter;
  end
  else begin
    FTimesBetter:=1;
  end;
  for i:=0 to MaxSeedPoints-1 do begin
    FLocHypothesisSet.LocHypothesis[i].ProcessLocState.SetMode1(FMaxV,FmaxW,ProcLocMinIteration,FilterCutOff);
  end;
end;

function TGlobalLocalization.NewPoseMode0(ActualLocData: TLocalizationData): TLocalizationData;
begin
  if FMode=0 then begin
    Result:=ActualLocData;
    if ActualLocData.CompassFiltered.Avaible then begin
      Result.Pose.teta:=ActualLocData.CompassFiltered.Angle;
      Result.State:=LocOk;
    end;
  end
  else begin
    Result:=ActualLocData;
    Result.State:=LocLost;
    raise Exception.Create('Global Localization Error');
  end;
end;

function TGlobalLocalization.NewPoseMode1(ActualLocData: TLocalizationData; OdosData: TOdosData;
                                          Compass: TCompassData; VPointList: TPointList): TLocalizationData;
var
  V: double;
  i,j: integer;
  ListElementLocHypt: TListLocHypothesis;
  ListElementLocHypt1, ListElementLocHypt2: TListLocHypothesis;
  Pose1, Pose2: Tpose2;
  deltaDist, deltaAng: double;
  testHead, test0: TListRefLocHypothesis;
begin
  if FMode=1 then begin
    if FMode1State=Idle then begin
      FAdviseSpeed.V:=0;
      FAdviseSpeed.Vn:=0;
      FAdviseSpeed.W:=0;
      if FStartSeedPointsList.PCount<=0 then begin
        FMode:=0;
        raise Exception.Create('ProcessSeedPoints, SeedPointsList empty')
      end;
      if (OdosData.Avaible) and (ActualLocData.CompassFiltered.Avaible) then begin
        V:=sqrt(power(OdosData.v,2)+power(OdosData.vn,2));
        if (V<FMaxV) and (OdosData.w<FmaxW) then begin
          FMode1State:=Running;
          //initialize
          FRPropIterationNumber:=FRPropMinIterationNumber;
          FLocHypothesisSet.NumHypothesis:=FStartSeedPointsList.PCount;
          FLocHypothesisSet.ListLocHypothesisHead.Next:=@FLocHypothesisSet.ListLocHypothesis[0];
          for i:=0 to FStartSeedPointsList.PCount-2 do begin
            FLocHypothesisSet.LocHypothesis[i].Pose:=FStartSeedPointsList.PList[i];
            FLocHypothesisSet.LocHypothesis[i].Pose.teta:=ActualLocData.CompassFiltered.Angle;
            FLocHypothesisSet.LocHypothesis[i].State:=LocOk;
            FLocHypothesisSet.LocHypothesis[i].ProcessLocState.Reset;
            FLocHypothesisSet.ListLocHypothesis[i].Next:=@FLocHypothesisSet.ListLocHypothesis[i+1];
            FLocHypothesisSet.ListLocHypothesis[FStartSeedPointsList.PCount-i-1].Previous:=@FLocHypothesisSet.ListLocHypothesis[FStartSeedPointsList.PCount-i-2];
          end;
          FLocHypothesisSet.LocHypothesis[FStartSeedPointsList.PCount-1].Pose:=FStartSeedPointsList.PList[FStartSeedPointsList.PCount-1];
          FLocHypothesisSet.LocHypothesis[FStartSeedPointsList.PCount-1].Pose.teta:=ActualLocData.CompassFiltered.Angle;
          FLocHypothesisSet.LocHypothesis[FStartSeedPointsList.PCount-1].State:=LocOk;
          FLocHypothesisSet.LocHypothesis[FStartSeedPointsList.PCount-1].ProcessLocState.Reset;
          FLocHypothesisSet.ListLocHypothesis[FStartSeedPointsList.PCount-1].Next:=@FLocHypothesisSet.ListLocHypothesis[0];
          FLocHypothesisSet.ListLocHypothesis[0].Previous:=@FLocHypothesisSet.ListLocHypothesisHead;
        end;
      end;
    end;
    if FMode1State=Running then begin
      i:=0;
      ListElementLocHypt:=FLocHypothesisSet.ListLocHypothesisHead.Next^;
      while i<FLocHypothesisSet.NumHypothesis do begin
        with ListElementLocHypt.Current^ do begin
          if State=LocLost then begin
            if i=0 then begin //remendo ver depois!!!!
              testHead:=@FLocHypothesisSet.ListLocHypothesisHead;
              test0:=@FLocHypothesisSet.ListLocHypothesis[0];

              FLocHypothesisSet.ListLocHypothesisHead.Next:=ListElementLocHypt.Next;
              ListElementLocHypt.Next^.Previous:=ListElementLocHypt.Previous;
              dec(FLocHypothesisSet.NumHypothesis);
            end
            else begin
              ListElementLocHypt.Previous^.Next:=ListElementLocHypt.Next;
              ListElementLocHypt.Next^.Previous:=ListElementLocHypt.Previous;
              dec(FLocHypothesisSet.NumHypothesis);
            end;
          end
          else begin
            if State=LocInverted then begin
              Pose.x:=-Pose.x;
              Pose.y:=-Pose.y;
              Pose.teta:=NormalizeAngle(Pose.teta+pi);
              State:=LocOk;
            end;
            Pose:=PropagateXYTetaOdos0(Pose, OdosData);
            if VPointList.PCount>=CNumMinPoins then begin
              Cost:=OptimizeMatch2(Pose, Pose, VPointList, FRPropIterationNumber, t1, t2);
            end;
            State:=ProcessLocState.NewStateMode1(State, OdosData, Pose, Compass);
            inc(i);
          end;
          ListElementLocHypt:=ListElementLocHypt.Next^;
        end;
      end;

      i:=0;
      ListElementLocHypt1:=FLocHypothesisSet.ListLocHypothesisHead.Next^;
      while i<(FLocHypothesisSet.NumHypothesis-1) do begin
        pose1:=ListElementLocHypt1.Current^.Pose;
        j:=i+1;
        ListElementLocHypt2:=ListElementLocHypt1.Next^;
        while j<FLocHypothesisSet.NumHypothesis do begin
          pose2:=ListElementLocHypt2.Current^.Pose;
          deltaDist:=power(Pose2.x-Pose1.x,2)+power(Pose2.y-Pose1.y,2);
          deltaAng:=abs(NormalizeAngle(Pose2.teta-Pose1.teta));
          if (deltaDist<FMaxDist2EliminationSquared) and (deltaAng<FMaxAng2Elimination) then begin
            ListElementLocHypt2.Previous^.Next:=ListElementLocHypt2.Next;
            ListElementLocHypt2.Next^.Previous:=ListElementLocHypt2.Previous;
            dec(FLocHypothesisSet.NumHypothesis);
          end
          else begin
            inc(j);
          end;
          ListElementLocHypt2:=ListElementLocHypt2.Next^;
        end;
        ListElementLocHypt1:=ListElementLocHypt1.Next^;
        inc(i);
      end;

      if FLocHypothesisSet.NumHypothesis<=1 then begin
        if FLocHypothesisSet.NumHypothesis=1 then begin
          Result:=ActualLocData;
          Result.Pose.x:=FLocHypothesisSet.ListLocHypothesisHead.Next^.Current^.Pose.x;
          Result.Pose.y:=FLocHypothesisSet.ListLocHypothesisHead.Next^.Current^.Pose.y;
          Result.Pose.teta:=FLocHypothesisSet.ListLocHypothesisHead.Next^.Current^.Pose.teta;
          Result.State:=LocOk;
        end
        else begin
          Result:=ActualLocData;
          Result.State:=LocLost;
        end;
        Reset;
      end
      else begin
        Result:=ActualLocData;
        Result.State:=LocLost;
      end;
    end
    else begin
      Result:=ActualLocData;
      Result.State:=LocLost;
    end;
  end
  else begin
    Result:=ActualLocData;
    Result.State:=LocLost;
    raise Exception.Create('Global Localization Error0');
  end;
end;

function TGlobalLocalization.NewPoseMode2(ActualLocData: TLocalizationData;
  OdosData: TOdosData; Compass: TCompassData; VPointList: TPointList
  ): TLocalizationData;
var
  V: double;
  i,j: integer;
  ListElementLocHypt: TListLocHypothesis;
  ListElementLocHypt1, ListElementLocHypt2, ListElementLocHyptAux: TListLocHypothesis;
  Pose1, Pose2: Tpose2;
  deltaDist, deltaAng: double;
begin
  if FMode=2 then begin
    if FMode1State=Idle then begin
      FAdviseSpeed.V:=0;
      FAdviseSpeed.Vn:=0;
      FAdviseSpeed.W:=0;
      if FStartSeedPointsList.PCount<=0 then begin
        FMode:=0;
        raise Exception.Create('ProcessSeedPoints, SeedPointsList empty')
      end;
      if (OdosData.Avaible) and (ActualLocData.CompassFiltered.Avaible) then begin
        V:=sqrt(power(OdosData.v,2)+power(OdosData.vn,2));
        if (V<FMaxV) and (OdosData.w<FmaxW) then begin
          FMode1State:=Running;
          //initialize
          FRPropIterationNumber:=FRPropMinIterationNumber;
          FLocHypothesisSet.AuxCountNumIterations:=0;
          FLocHypothesisSet.NumHypothesis:=FStartSeedPointsList.PCount;
          FLocHypothesisSet.ListLocHypothesisHead.Next:=@FLocHypothesisSet.ListLocHypothesis[0];
          FLocHypothesisSet.ListLocHypothesisHead.Previous:=@FLocHypothesisSet.ListLocHypothesis[FStartSeedPointsList.PCount-1];
          for i:=0 to FStartSeedPointsList.PCount-2 do begin
            FLocHypothesisSet.LocHypothesis[i].Pose:=FStartSeedPointsList.PList[i];
            FLocHypothesisSet.LocHypothesis[i].Pose.teta:=ActualLocData.CompassFiltered.Angle;
            FLocHypothesisSet.LocHypothesis[i].State:=LocOk;
            FLocHypothesisSet.LocHypothesis[i].ProcessLocState.Reset;
            FLocHypothesisSet.ListLocHypothesis[i].Next:=@FLocHypothesisSet.ListLocHypothesis[i+1];
            FLocHypothesisSet.ListLocHypothesis[FStartSeedPointsList.PCount-i-1].Previous:=@FLocHypothesisSet.ListLocHypothesis[FStartSeedPointsList.PCount-i-2];
          end;
          FLocHypothesisSet.LocHypothesis[FStartSeedPointsList.PCount-1].Pose:=FStartSeedPointsList.PList[FStartSeedPointsList.PCount-1];
          FLocHypothesisSet.LocHypothesis[FStartSeedPointsList.PCount-1].Pose.teta:=ActualLocData.CompassFiltered.Angle;
          FLocHypothesisSet.LocHypothesis[FStartSeedPointsList.PCount-1].State:=LocOk;
          FLocHypothesisSet.LocHypothesis[FStartSeedPointsList.PCount-1].ProcessLocState.Reset;
          FLocHypothesisSet.ListLocHypothesis[FStartSeedPointsList.PCount-1].Next:=@FLocHypothesisSet.ListLocHypothesisHead;
          FLocHypothesisSet.ListLocHypothesis[0].Previous:=@FLocHypothesisSet.ListLocHypothesisHead;
        end;
      end;
    end;
    if FMode1State=Running then begin
      i:=0;
      ListElementLocHypt:=FLocHypothesisSet.ListLocHypothesisHead.Next^;
      while i<FLocHypothesisSet.NumHypothesis do begin
        //eliminar perdidos
        with ListElementLocHypt.Current^ do begin
          if State=LocLost then begin
            if i=0 then begin //remendo ver depois!!!!
              FLocHypothesisSet.ListLocHypothesisHead.Next:=ListElementLocHypt.Next;
              ListElementLocHypt.Next^.Previous:=ListElementLocHypt.Previous;
              dec(FLocHypothesisSet.NumHypothesis);
            end
            else begin
              ListElementLocHypt.Previous^.Next:=ListElementLocHypt.Next;
              ListElementLocHypt.Next^.Previous:=ListElementLocHypt.Previous;
              dec(FLocHypothesisSet.NumHypothesis);
            end;
          end
          else begin
            if State=LocInverted then begin
              Pose.x:=-Pose.x;
              Pose.y:=-Pose.y;
              Pose.teta:=NormalizeAngle(Pose.teta+pi);
              State:=LocOk;
            end;
            Pose:=PropagateXYTetaOdos0(Pose, OdosData);
            if VPointList.PCount>=CNumMinPoins then begin
              Cost:=OptimizeMatch2(Pose, Pose, VPointList, FRPropIterationNumber, t1, t2);
            end;
            State:=ProcessLocState.NewStateMode1(State, OdosData, Pose, Compass);
            inc(i);
          end;
          ListElementLocHypt:=ListElementLocHypt.Next^;
        end;
      end;
      //eliminar repetidos
      i:=0;
      ListElementLocHypt1:=FLocHypothesisSet.ListLocHypothesisHead.Next^;
      while i<(FLocHypothesisSet.NumHypothesis-1) do begin
        pose1:=ListElementLocHypt1.Current^.Pose;
        j:=i+1;
        ListElementLocHypt2:=ListElementLocHypt1.Next^;
        while j<FLocHypothesisSet.NumHypothesis do begin
          pose2:=ListElementLocHypt2.Current^.Pose;
          deltaDist:=power(Pose2.x-Pose1.x,2)+power(Pose2.y-Pose1.y,2);
          deltaAng:=abs(NormalizeAngle(Pose2.teta-Pose1.teta));
          if (deltaDist<FMaxDist2EliminationSquared) and (deltaAng<FMaxAng2Elimination) then begin
            ListElementLocHypt2.Previous^.Next:=ListElementLocHypt2.Next;
            ListElementLocHypt2.Next^.Previous:=ListElementLocHypt2.Previous;
            dec(FLocHypothesisSet.NumHypothesis);
          end
          else begin
            inc(j);
          end;
          ListElementLocHypt2:=ListElementLocHypt2.Next^;
        end;
        ListElementLocHypt1:=ListElementLocHypt1.Next^;
        inc(i);
      end;
      //master kill!
      if (OdosData.Avaible) and (FLocHypothesisSet.NumHypothesis>1) then begin
        V:=sqrt(power(OdosData.v,2)+power(OdosData.vn,2));
        if (V<FMaxV) and (OdosData.w<FmaxW) then begin
          if FLocHypothesisSet.AuxCountNumIterations<=FMinIterationDeltaNumHypothesis then begin
            if (FLocHypothesisSet.LastNumHypothesis-FLocHypothesisSet.NumHypothesis)<=FMaxDeltaNumHypothesis then begin
              inc(FLocHypothesisSet.AuxCountNumIterations);
            end
            else begin
              FLocHypothesisSet.AuxCountNumIterations:=0;
            end;
          end
          else begin
            i:=2;
            ListElementLocHypt:=FLocHypothesisSet.ListLocHypothesisHead.Next^;
            ListElementLocHypt1:=ListElementLocHypt;
            ListElementLocHypt:=ListElementLocHypt.Next^;
            ListElementLocHypt2:=ListElementLocHypt;
            ListElementLocHypt:=ListElementLocHypt.Next^;
            if ListElementLocHypt1.Current^.Cost>ListElementLocHypt2.Current^.Cost then begin
              ListElementLocHyptAux:=ListElementLocHypt1;
              ListElementLocHypt1:=ListElementLocHypt2;
              ListElementLocHypt2:=ListElementLocHyptAux;
            end;
            while i<FLocHypothesisSet.NumHypothesis do begin
              if (ListElementLocHypt.Current^.Cost<ListElementLocHypt2.Current^.Cost) then begin //melhorar
                ListElementLocHypt2:=ListElementLocHypt;
                if ListElementLocHypt1.Current^.Cost>ListElementLocHypt2.Current^.Cost then begin
                  ListElementLocHyptAux:=ListElementLocHypt1;
                  ListElementLocHypt1:=ListElementLocHypt2;
                  ListElementLocHypt2:=ListElementLocHyptAux;
                end;
              end;
              ListElementLocHypt:=ListElementLocHypt.Next^;
              inc(i);
            end;
            if ListElementLocHypt1.Current^.Cost*FTimesBetter<ListElementLocHypt2.Current^.Cost then begin
              FLocHypothesisSet.NumHypothesis:=1;
              FLocHypothesisSet.ListLocHypothesisHead.Next:=@ListElementLocHypt1;
            end;
          end;
        end
        else begin
          FLocHypothesisSet.AuxCountNumIterations:=0;
        end;
      end;
      FLocHypothesisSet.LastNumHypothesis:=FLocHypothesisSet.NumHypothesis;

      //selecionar soulucao
      if FLocHypothesisSet.NumHypothesis<=1 then begin
        if FLocHypothesisSet.NumHypothesis=1 then begin
          Result:=ActualLocData;
          Result.Pose.x:=FLocHypothesisSet.ListLocHypothesisHead.Next^.Current^.Pose.x;
          Result.Pose.y:=FLocHypothesisSet.ListLocHypothesisHead.Next^.Current^.Pose.y;
          Result.Pose.teta:=FLocHypothesisSet.ListLocHypothesisHead.Next^.Current^.Pose.teta;
          Result.State:=LocOk;
        end
        else begin
          Result:=ActualLocData;
          Result.State:=LocLost;
        end;
        Reset;
      end
      else begin
        Result:=ActualLocData;
        Result.State:=LocLost;
      end;
    end
    else begin
      Result:=ActualLocData;
      Result.State:=LocLost;
    end;
  end
  else begin
    Result:=ActualLocData;
    Result.State:=LocLost;
    raise Exception.Create('Global Localization Error0');
  end;
end;

{ TLostNavigation }

function TLostNavigation.VerifyIfClosePoint(VPointList: TPointList): TPointList;
var
  i,j:integer;
  pntDist:double;
begin
  Result.PCount:=0;
  j:=0;
  for i:=0 to VPointList.PCount-1 do begin
   if(abs(ATan2(VPointList.PList[i].y,VPointList.PList[i].x))<FAngleClosePnt) then begin
     pntDist:=sqrt(power(VPointList.PList[i].y,2)+power(VPointList.PList[i].x,2));
     if (pntDist<=FMaxClosePntDist) and (pntDist>=FMinClosePntDist) then begin
      Result.PCount:=Result.PCount+1;
      Result.PList[j].x:=VPointList.PList[i].x;
      Result.PList[j].y:=VPointList.PList[i].y;
      j:=j+1;
     end;
   end;
  end;
end;

constructor TLostNavigation.Create;
begin
  FMode:=-1;
end;

destructor TLostNavigation.Destroy;
begin
  inherited Destroy;
end;

procedure TLostNavigation.Reset;
begin
  FAdviseSpeed.V:=0;
  FAdviseSpeed.Vn:=0;
  FAdviseSpeed.W:=0;
  FRotState:=0;
  FCPointList.PCount:=0;
  FActualAng:=0;
end;

procedure TLostNavigation.SetMode0(NumMinClosePnt: integer;
  MaxClosePntDist: double; MinClosePntDist: double; AngleClosePnt: double;
  AdvisedNavigateV: double; AdvisedNavigateW: double; AngleOffset: double);
begin
  FMode:=0;
  FNumMinClosePnt:=NumMinClosePnt;
  FMaxClosePntDist:=MaxClosePntDist;
  FMinClosePntDist:=MinClosePntDist;
  FAngleClosePnt:=AngleClosePnt;
  FAdvisedNavigateV:=AdvisedNavigateV;
  FAdvisedNavigateW:=AdvisedNavigateW;
  FAngleOffset:=AngleOffset;
end;

procedure TLostNavigation.SetMode1;
begin
  FMode:=1;
end;

procedure TLostNavigation.SetMode2;
begin
  FMode:=2;
end;

procedure TLostNavigation.CalcAdvisedSpeed0(vehLocState: TLocalizationState; PointList: TPointList; dteta: double);
begin
  if FMode=0 then begin
    if(vehLocState=LocOk)then begin
      Reset;
    end
    else begin
      FCPointList:=VerifyIfClosePoint(PointList);
      if(FCPointList.PCount>=FNumMinClosePnt)then begin
        if(FRotState=0)then begin
          FActualAng:=0;
          FAdviseSpeed.V:=0;
          FAdviseSpeed.Vn:=0;
          FAdviseSpeed.W:=FAdvisedNavigateW;
          FRotState:=1;
      end;
      end
      else begin
        if(FRotState=1)then begin
          if (FActualAng>=(pi/2-FAngleOffset)) then begin
            FAdviseSpeed.V:=FAdvisedNavigateV;
            FAdviseSpeed.Vn:=0;
            FAdviseSpeed.W:=0;
            FRotState:=0;
          end
          else begin
            FActualAng:=FActualAng+abs(dteta);
          end;
        end
        else begin
          FAdviseSpeed.V:=FAdvisedNavigateV;
          FAdviseSpeed.Vn:=0;
          FAdviseSpeed.W:=0;
          FRotState:=0;
        end;
      end;
    end;
  end
  else begin
    raise Exception.Create('Lost Navigation error');
  end;
end;

end.

