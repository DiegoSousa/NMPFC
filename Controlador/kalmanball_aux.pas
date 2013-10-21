unit KalmanBall_Aux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dynmatrix,math;
type

 { TestBall }

 TestBall= class
 private
  FerrorOdoXY:double;
  FerrorOdoAng:double;
  FmodPred:integer;
  FsRate:double;
  FerrorRo:double;
  FerrorTeta:double;
  FerrorPowerRo:integer;
  FerrorPowerTeta:integer;
  FInitCovXY:double;
  FInitCovVXY:double;
  FInitMode:integer;
  Fstate:integer;
  FIter:integer;
  FZcamera:double;
  FZball:double;
  FminCovXY:double;
  FminCovVxy:double;
  FmodObs:integer;
  FvelPredQ:double;
  FminIter:integer;
  FqualDecay:double;
  FqualDecayLine:double;
  //matrizes de filtro de kalman
  FQ:TDMatrix;
  FR:TDMatrix;
  FQv:TDMatrix;
  // estados
  FBallState:TDMatrix;
  FBallCovState:TDMatrix;
  //pedriction
  Ff_xk:TDMatrix;
  Ff_odo:TDMatrix;
  Ftrans:TDMatrix;
  Ff:TDMatrix;
  FRot_p:TDMatrix;
  FRot:TDMatrix;
  FGradFx:TDMatrix;
  FGradFq:TDMatrix;
  FGradFqc:TDMatrix;
  FDevRot_p:TDMatrix;
  FDevRot:TDMatrix;
  FObsX:double;
  FObsY:double;
  FObsD:double;
  FObsXant:double;
  FObsYant:double;
  FObsVx:double;
  FObsVy:double;
  //Update
  FZest:TDMatrix;
  FZobs:TDMatrix;
  FH:TDMatrix;
  Finov:TDMatrix;
  FGradZr:TDMatrix;
  FK:TDMatrix;
  procedure initFilter(obsRo,obsTeta,obsX,obsY,d:double);
  procedure prediction(deltV,deltVn,deltTeta:double);
  procedure prediction1;
  procedure update(obsRo,obsTeta,obsX,obsY,d:double);
  procedure update1(obsRo,obsTeta,obsX,obsY,obsVx,obsVy,d:double);
  function GetBallCovX:double;
  function GetBallCovY:double;
  function GetBallCovXY:double;
  function GetBallCovVx:double;
  function GetBallCovVy:double;
  function GetBallCovVxy:double;
  function GetballObsX:double;
  function GetballObsY:double;
  function GetballObsVx:double;
  function GetballObsVy:double;
  procedure SetFstate(state:integer);
  function GetfilterStarted:boolean;
 public
  constructor Create;
  procedure setParameters(errorOdoXY:double;
                          errorOdoAng:double;
                          modPred:integer;
                          sRate:double;
                          errorRo:double;
                          errorTeta:double;
                          errorPowerRo:integer;
                          errorPowerTeta:integer;
                          InitCovXY:double;
                          InitCovVXY:double;
                          InitMode:integer;
                          Zcamera:double;
                          Zball:double;
                          minCovXY:double;
                          minCovVxy:double;
                          modObs:integer;
                          velPredQ:double;
                          minIter:integer;
                          qualDecay:double;
                          qualDecayLin:double);
  procedure KalmanFilterEstBall(BallStateQual:double;deltV,deltVn,deltTeta:double;obsRo,obsTeta:double;quality:integer;var x,y,Vx,Vy:double);
  procedure getBallXY(var obsX,obsY,d:double;obsRo,obsTeta:double);
  property ballCovX: double read GetBallCovX;
  property ballCovY: double read GetBallCovY;
  property ballCovXY: double read GetBallCovXY;
  property ballCovVx: double read GetBallCovVx;
  property ballCovVy: double read GetBallCovVy;
  property ballCovVxy: double read GetBallCovVxy;
  property stateFilter:integer write SetFstate;
  property ballObsX: double read GetballObsX;
  property ballObsY: double read GetballObsY;
  property ballObsVx: double read GetballObsVx;
  property ballObsVy: double read GetballObsVy;
  property filterStarted:boolean read GetfilterStarted;
  procedure calcQualityDecay(var quality:double);
 end;

implementation

{ TestBall }

procedure TestBall.initFilter(obsRo,obsTeta,obsX,obsY,d:double);
begin
  //State
  FBallState.setv(0,0,obsX);
  FBallState.setv(1,0,obsY);
  FBallState.setv(2,0,0);
  FBallState.setv(3,0,0);
  FBallCovState:=Mzeros(4,4);
  if FInitMode=0 then begin
    FR:=Mzeros(2,2);
    FR.setv(0,0,FerrorRo*power(d,FerrorPowerRo));
    FR.setv(1,1,FerrorTeta/(1+power(d,FerrorPowerTeta)));
    //grad
    FGradZr.setv(0,0,(1+power(tan(obsRo),2))*cos(obsTeta)*(FZcamera-FZball));
    FGradZr.setv(1,0,(1+power(tan(obsRo),2))*sin(obsTeta)*(FZcamera-FZball));
    FGradZr.setv(0,1,-tan(obsRo)*sin(obsTeta)*(FZcamera-FZball));
    FGradZr.setv(1,1,+tan(obsRo)*cos(obsTeta)*(FZcamera-FZball));
    FBallCovState:=MStamp(FBallCovState,FGradZr*FR*Mtran(FGradZr),0,0);
  end else begin
    FBallCovState.setv(0,0,sqrt(FInitCovXY));
    FBallCovState.setv(1,1,sqrt(FInitCovXY));
  end;
  FBallCovState.setv(2,2,sqrt(FInitCovVXY));
  FBallCovState.setv(3,3,sqrt(FInitCovVXY));
end;

procedure TestBall.prediction(deltV, deltVn, deltTeta:double);
begin
  //1
  Ff_xk:=Ftrans*FBallState;
  Ff_odo.setv(0,0,deltV);
  Ff_odo.setv(1,0,deltVn);
  Ff_odo.setv(2,0,0); // n soma V
  Ff_odo.setv(3,0,0); // n soma Vn
  Ff:=Ff_xk-Ff_odo;
  //2
  FRot_p.setv(0,0,cos(deltTeta));
  FRot_p.setv(1,1,cos(deltTeta));
  FRot_p.setv(0,1,sin(deltTeta));
  FRot_p.setv(1,0,-sin(deltTeta));
  FRot:=Mzeros(4,4);
  FRot:=MStamp(FRot,FRot_p,0,0);
  FRot:=MStamp(FRot,FRot_p,2,2);
  //3
  FBallState:=FRot*Ff;
  //4
  FGradFx:=Mzeros(4,4);
  FGradFx:=MStamp(FGradFx,FRot_p,0,0);
  FGradFx:=MStamp(FGradFx,FRot_p,2,2);
  FGradFx:=MStamp(FGradFx,FsRate*FRot_p,0,2);
  //5
  FDevRot_p.setv(0,0,-sin(deltTeta));
  FDevRot_p.setv(1,1,-sin(deltTeta));
  FDevRot_p.setv(0,1,cos(deltTeta));
  FDevRot_p.setv(1,0,-cos(deltTeta));
  FDevRot:=MStamp(FDevRot,FDevRot_p,0,0);
  FDevRot:=MStamp(FDevRot,FDevRot_p,2,2);
  //6
  FGradFqc:=FDevRot*Ff;
  //7
  FGradFq:=Mzeros(4,3);
  FGradFq:=MStamp(FGradFq,-FRot_p,0,0);
  FGradFq:=MStamp(FGradFq,FGradFqc,0,2);
  //8
  FQ:=Mzeros(3,3);
  FQ.setv(0,0,FerrorOdoXY*power(deltV,2));
  FQ.setv(1,1,FerrorOdoXY*power(deltVn,2));
  FQ.setv(2,2,FerrorOdoAng*power(deltTeta,2));
  //9
  FBallCovState:=FGradFx*FBallCovState*Mtran(FGradFx)+FGradFq*FQ*Mtran(FGradFq)+FQv;
end;

procedure TestBall.prediction1;
begin
  //1
  FBallState:=Ftrans*FBallState;
  //2
  FGradFx:=Mzeros(4,4);
  FGradFx:=Meye(4);
  FGradFx:=MStamp(FGradFx,FsRate*Meye(2),0,2);
  //3
  FQ:=Mzeros(4,4);
  FQ.setv(0,0,FerrorOdoXY*power(FsRate*FBallState.getv(2,0),2));
  FQ.setv(1,1,FerrorOdoXY*power(FsRate*FBallState.getv(3,0),2));
  //4
  FBallCovState:=FGradFx*FBallCovState*Mtran(FGradFx)+FQ+FQv;
end;

procedure TestBall.SetFstate(state: integer);
begin
  Fstate:=state;
end;

function TestBall.GetfilterStarted: boolean;
begin
  Result:=(FIter=FminIter);
end;

procedure TestBall.update(obsRo,obsTeta,obsX,obsY,d:double);
var
 dZx_dro,dZx_dteta:double;
 dZy_dro,dZy_dteta:double;
begin
 //2
 FZobs.setv(0,0,obsX);
 FZobs.setv(1,0,obsY);
 //3
 FZest.setv(0,0,FBallState.getv(0,0));
 FZest.setv(1,0,FBallState.getv(1,0));
 //4
 Finov:=FZobs-FZest;
 //5
 FR:=Mzeros(2,2);
 FR.setv(0,0,FerrorRo*power(d,FerrorPowerRo));
 //ver mais tarde
 FR.setv(1,1,FerrorTeta/(FerrorTeta+power(d,FerrorPowerTeta)));
 //6
 dZx_dro:=(1+power(tan(obsRo),2))*cos(obsTeta)*(FZcamera-FZball);
 dZy_dro:=(1+power(tan(obsRo),2))*sin(obsTeta)*(FZcamera-FZball);
 dZx_dteta:=-tan(obsRo)*sin(obsTeta)*(FZcamera-FZball);
 dZy_dteta:=+tan(obsRo)*cos(obsTeta)*(FZcamera-FZball);
 FGradZr.setv(0,0,dZx_dro);
 FGradZr.setv(1,0,dZy_dro);
 FGradZr.setv(0,1,dZx_dteta);
 FGradZr.setv(1,1,dZy_dteta);
 //7
 FK:=FBallCovState*Mtran(FH)*Minv(FH*FBallCovState*Mtran(FH)+FGradZr*FR*Mtran(FGradZr));
 //8
 FBallState:=FBallState+FK*Finov;
 //9
 FBallCovState:=(Meye(4)-FK*FH)*FBallCovState;
 // para não ficar com uma variancia abaixo de
 if FBallCovState.getv(0,0)<power(FminCovXY,2) then begin
    FBallCovState.setv(0,0,power(FminCovXY,2));
 end;
 if FBallCovState.getv(1,1)<power(FminCovXY,2) then begin
    FBallCovState.setv(1,1,power(FminCovXY,2));
 end;
 if FBallCovState.getv(2,2)<power(FminCovVxy,2) then begin
    FBallCovState.setv(2,2,power(FminCovVxy,2));
 end;
 if FBallCovState.getv(3,3)<power(FminCovVxy,2) then begin
    FBallCovState.setv(3,3,power(FminCovVxy,2));
 end;

end;

procedure TestBall.update1(obsRo,obsTeta,obsX,obsY,obsVx,obsVy,d:double);
var
 dZx_dro,dZx_dteta:double;
 dZy_dro,dZy_dteta:double;
begin
 //1
 getBallXY(obsX,obsY,d,obsRo,obsTeta);
 //2
 FZobs.setv(0,0,obsX);
 FZobs.setv(1,0,obsY);
 FZobs.setv(2,0,obsVx);
 FZobs.setv(3,0,obsVy);
 //3
 FZest.setv(0,0,FBallState.getv(0,0));
 FZest.setv(1,0,FBallState.getv(1,0));
 FZest.setv(2,0,FBallState.getv(2,0));
 FZest.setv(3,0,FBallState.getv(3,0));
 //4
 Finov:=FZobs-FZest;
 //5
 FR:=Mzeros(2,2);
 FR.setv(0,0,FerrorRo*power(d,FerrorPowerRo));
 //ver mais tarde
 FR.setv(1,1,FerrorTeta/(FerrorTeta+power(d,FerrorPowerTeta)));
 //6
 dZx_dro:=(1+power(tan(obsRo),2))*cos(obsTeta)*(FZcamera-FZball);
 dZy_dro:=(1+power(tan(obsRo),2))*sin(obsTeta)*(FZcamera-FZball);
 dZx_dteta:=-tan(obsRo)*sin(obsTeta)*(FZcamera-FZball);
 dZy_dteta:=+tan(obsRo)*cos(obsTeta)*(FZcamera-FZball);
 FGradZr.setv(0,0,dZx_dro);
 FGradZr.setv(1,0,dZy_dro);
 FGradZr.setv(2,0,dZx_dro/FsRate);
 FGradZr.setv(3,0,dZy_dro/FsRate);
 FGradZr.setv(0,1,dZx_dteta);
 FGradZr.setv(1,1,dZy_dteta);
 FGradZr.setv(2,1,dZx_dteta/FsRate);
 FGradZr.setv(3,1,dZy_dteta/FsRate);
 //7
 FK:=FBallCovState*Mtran(FH)*Minv(FH*FBallCovState*Mtran(FH)+FGradZr*FR*Mtran(FGradZr));
 //8
 FBallState:=FBallState+FK*Finov;
 //9
 FBallCovState:=(Meye(4)-FK*FH)*FBallCovState;
 if FBallCovState.getv(0,0)<power(FminCovXY,2) then begin
    FBallCovState.setv(0,0,power(FminCovXY,2));
 end;
 if FBallCovState.getv(1,1)<power(FminCovXY,2) then begin
    FBallCovState.setv(1,1,power(FminCovXY,2));
 end;
 if FBallCovState.getv(2,2)<power(FminCovVxy,2) then begin
    FBallCovState.setv(2,2,power(FminCovVxy,2));
 end;
 if FBallCovState.getv(3,3)<power(FminCovVxy,2) then begin
    FBallCovState.setv(3,3,power(FminCovVxy,2));
 end;
end;


function TestBall.GetBallCovX: double;
begin
  Result:=FBallCovState.getv(0,0);
end;

function TestBall.GetBallCovY: double;
begin
  Result:=FBallCovState.getv(1,1);
end;

function TestBall.GetBallCovXY: double;
begin
 Result:=FBallCovState.getv(0,1);
end;

function TestBall.GetBallCovVx: double;
begin
 Result:=FBallCovState.getv(2,2);
end;

function TestBall.GetBallCovVy: double;
begin
 Result:=FBallCovState.getv(3,3);
end;

function TestBall.GetBallCovVxy: double;
begin
 Result:=FBallCovState.getv(2,3);
end;

function TestBall.GetballObsX: double;
begin
  Result:=FObsX;
end;

function TestBall.GetballObsY: double;
begin
  Result:=FObsY;
end;

function TestBall.GetballObsVx: double;
begin
  Result:=FObsVx;
end;

function TestBall.GetballObsVy: double;
begin
  Result:=FObsVy;
end;


constructor TestBall.Create;
begin

end;

procedure TestBall.setParameters(errorOdoXY: double; errorOdoAng: double;
  modPred:integer; sRate: double; errorRo: double; errorTeta: double;
  errorPowerRo: integer; errorPowerTeta: integer; InitCovXY: double;
  InitCovVXY: double;InitMode:integer;Zcamera:double;Zball:double;
  minCovXY:double;minCovVxy:double;modObs:integer;velPredQ:double;
  minIter:integer;qualDecay:double;qualDecayLin:double);
begin
  FerrorOdoXY:=errorOdoXY;
  FerrorOdoAng:=errorOdoAng;
  FmodPred:=modPred;
  FsRate:=sRate;
  FerrorRo:=errorRo;
  FerrorTeta:=errorTeta;
  FerrorPowerRo:=errorPowerRo;
  FerrorPowerTeta:=errorPowerTeta;
  FInitCovXY:=InitCovXY;
  FInitCovVXY:=InitCovVXY;
  FInitMode:=InitMode;
  FZcamera:=Zcamera;
  FZball:=Zball;
  FminCovXY:=minCovXY;
  FminCovVxy:=minCovVxy;
  FmodObs:=modObs;
  FvelPredQ:=velPredQ;
  FminIter:=minIter;
  FqualDecay:=qualDecay;
  FqualDecayLine:=qualDecayLin;
  //matrizes de filtro de kalman
  if FmodPred=0 then begin
   FQ.SetSize(3,3);
  end else begin
   FQ.SetSize(4,4);
  end;
  FR.SetSize(2,2);
  // estados
  FBallState.SetSize(4,1);
  FBallCovState.SetSize(4,4);
  //pedriction
  Ff_xk.SetSize(4,1);
  Ff_odo.SetSize(4,1);
  Ftrans.SetSize(4,4);
  Ff.SetSize(4,1);
  FRot_p.SetSize(2,2);
  FRot.SetSize(4,4);
  FGradFx.SetSize(4,4);
  FGradFq.SetSize(4,3);
  FGradFqc.SetSize(4,1);
  FDevRot_p.SetSize(2,2);
  FDevRot.SetSize(4,4);
  FQv.SetSize(4,4);
  FQv:=Mzeros(4,4);
  FQv.setv(2,2,sqrt(FvelPredQ));
  FQv.setv(3,3,sqrt(FvelPredQ));
  // Ftrans
  Ftrans:=Meye(4);
  Ftrans.setv(0,2,FsRate);
  Ftrans.setv(1,3,FsRate);
  //Update
  if (FmodObs=0)  then begin
    FZest.SetSize(2,1);
    FZobs.SetSize(2,1);
    Finov.SetSize(2,1);
    FH.SetSize(2,4);
    FGradZr.SetSize(2,2);
    FK.SetSize(4,2);
    // FH
    FH:=Mzeros(2,4);
    FH:=MStamp(FH,Meye(2),0,0);
  end else begin
    FZest.SetSize(4,1);
    FZobs.SetSize(4,1);
    Finov.SetSize(4,1);
    FH.SetSize(4,4);
    FGradZr.SetSize(4,2);
    FK.SetSize(4,4);
    // FH
    FH:=Meye(4);
  end;
  //Fstate
  Fstate:=0;
end;

procedure TestBall.KalmanFilterEstBall(BallStateQual:double;deltV,deltVn,deltTeta:double;obsRo,obsTeta: double;quality:integer;var x,y,Vx,Vy:double);
begin
  FObsXant:=FObsX;
  FObsYant:=FObsY;
  // get observation
  getBallXY(FObsX,FObsY,FObsD,obsRo,obsTeta);
  FObsVx:=(FObsX-FObsXant)/FsRate;
  FObsVy:=(FObsY-FObsYant)/FsRate;
  //init cycle
  if (Fstate=0)then begin
   if(quality>0)then begin
    initFilter(obsRo,obsTeta,FObsX,FObsY,FObsD);
    Fstate:=1;
    FIter:=0;
   end;
  end else begin
   if((quality>0)or(BallStateQual>0))then begin
    //prediction
    if FmodPred=0 then begin
     prediction1;
    end else if FmodObs=1 then begin
     prediction(deltV,deltVn,deltTeta);
    end;
   end;
   if(quality>0)then begin
    //update
    if FmodObs=0 then begin
     update(obsRo,obsTeta,FObsX,FObsY,FObsD);
    end else if FmodObs=1 then begin
     update1(obsRo,obsTeta,FObsX,FObsY,FObsVx,FObsVy,FObsD);
    end;
    if FIter<FminIter then begin
     Fiter:=FIter+1;
    end;
   end else begin
    Fstate:=0;
   end;
  end;
  x:=FBallState.getv(0,0);
  y:=FBallState.getv(1,0);
  Vx:=FBallState.getv(2,0);
  Vy:=FBallState.getv(3,0);
end;

procedure TestBall.getBallXY(var obsX, obsY, d: double;obsRo,obsTeta:double);
begin
  d:=tan(obsRo)*(FZcamera-FZball);
  obsX:=cos(obsTeta)*d;
  obsY:=sin(obsTeta)*d;
end;

procedure TestBall.calcQualityDecay(var quality: double);
begin
  quality:=quality*FqualDecay-FqualDecayLine;
end;

end.

