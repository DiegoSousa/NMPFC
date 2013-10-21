// Miguel Pinto 13/May/2011
unit KalmanFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dynmatrix;
type

  { TKalmanFilter }

  TKalmanFilter=class
  // estado...covariancia
  X,{X2,}P:TDMatrix;
  // matrizes de transicao
  A,H:TDMatrix;
  //Matrizes de covariancia
  Q,R:TDMatrix;
  // entrada
  U:TDMatrix;
  //observacao
  Obs:TDMatrix;
  //observacao estimada
  Z:TDMatrix;
  // inovacao
  V:TDMatrix;
  //Kalman gain
  K:TDMatrix;
  //dimState
  dimX,dimO:integer;
  constructor Create(dimState,dimObs:integer;covQ,covR:double);
  procedure prediction;
  procedure predictionCov;
  procedure Inovation;
  procedure KalmanGain;
  procedure update;
  procedure updateCoV;
  procedure calcZEst;
  // Exemplo sistemas lineares
  procedure filterCycle(observation:TDMatrix;covR:double);
  end;

implementation

{ TKalmanFilter }

constructor TKalmanFilter.Create(dimState,dimObs:integer;covQ,covR:double);
var i,j:integer;
begin
  // prediction
  X.SetSize(dimState,1);
  P.SetSize(dimState,dimState);
  A.SetSize(dimState,dimState);
  Q.SetSize(dimState,dimState);
  //update
  H.SetSize(dimObs,dimState);
  K.SetSize(dimState,dimObs);
  Z.SetSize(dimObs,1);
  V.SetSize(dimObs,1);

  Obs.SetSize(dimObs,1);
  R.SetSize(dimObs,dimObs);

  dimX:=dimState;
  dimO:=dimObs;

  //Q
  Q.setv(0,0,0.01);
  Q.setv(0,1,0);
  Q.setv(0,2,0);
  Q.setv(0,3,0);
  Q.setv(1,0,0);
  Q.setv(1,1,0.01);
  Q.setv(1,2,0);
  Q.setv(1,3,0);
  Q.setv(2,0,0);
  Q.setv(2,1,0);
  Q.setv(2,2,0.001);
  Q.setv(2,3,0);
  Q.setv(3,0,0);
  Q.setv(3,1,0);
  Q.setv(3,2,0);
  Q.setv(3,3,0.001);

  R.setv(0,0,covR);
  R.setv(0,1,0);
  R.setv(1,0,0);
  R.setv(1,1,covR);

  // inicializar a zero

  X:=Mzeros(dimState,1);
  P:=Mzeros(dimState,dimState);

  //matrizes de transicao
  //A
  A.setv(0,0,1);
  A.setv(0,1,0);
  A.setv(0,2,0.04);
  A.setv(0,3,0);

  A.setv(1,0,0);
  A.setv(1,1,1);
  A.setv(1,2,0);
  A.setv(1,3,0.04);

  A.setv(2,0,0);
  A.setv(2,1,0);
  A.setv(2,2,1);
  A.setv(2,3,0);

  A.setv(3,0,0);
  A.setv(3,1,0);
  A.setv(3,2,0);
  A.setv(3,3,1);

  //H
  H.setv(0,0,1);
  H.setv(0,1,0);
  H.setv(0,2,0);
  H.setv(0,3,0);

  H.setv(1,0,0);
  H.setv(1,1,1);
  H.setv(1,2,0);
  H.setv(1,3,0);

end;

procedure TKalmanFilter.prediction;
begin
  //X:=A*X+U;
  X:=(A*X);
end;

procedure TKalmanFilter.predictionCov;
begin
  P:=((A*P)*Mtran(A))+Q;
end;

procedure TKalmanFilter.Inovation;
begin
  V:=Obs-Z;
end;

procedure TKalmanFilter.KalmanGain;
var i,j: integer;
begin
  K:=(P*Mtran(H))*Minv(((H*P)*Mtran(H))+R);
end;

procedure TKalmanFilter.update;
begin
  X:=X+(K*V);
  //X2:=X;
end;

procedure TKalmanFilter.updateCoV;
begin
  P:=(Meye(dimX)-(K*H))*P;
end;

procedure TKalmanFilter.calcZEst;
begin
  Z:=(H*X);
end;


procedure TKalmanFilter.filterCycle(observation:TDMatrix;covR:double);
begin
  R.setv(0,0,covR);
  R.setv(0,1,0);
  R.setv(1,0,0);
  R.setv(1,1,covR);

  ////prediction
  //prediction;
  //predictionCov;
  ////Estimativa da observacao
  //calcZEst;
  //Obs:=observation;
  //Inovation;
  //KalmanGain;
  ////update
  //update;
  //updateCoV;

  //prediction
  X:=(A*X);
  P:=((A*P)*Mtran(A))+Q;
  //Estimativa da observacao
  Z:=(H*X);
  Obs:=observation;
  V:=Obs-Z;
  K:=(P*Mtran(H))*Minv(((H*P)*Mtran(H))+R);
  //update
  X:=X+(K*V);
  P:=(Meye(dimX)-(K*H))*P;

end;

end.

