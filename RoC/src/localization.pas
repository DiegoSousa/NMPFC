unit localization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, ExtCtrls, IntfGraphics,LCLType, Graphics, FPimage, LCLIntf;
  
const
  MapW=4098;
  MapH=4098;
  
  //Verificar a validade disto???
  Lc=250;
  
  MAX_VAR=500;
  //VAR_CONST=1.4e-10;

  t1=0.01;
  t2=0.05;
  t3=0.1;
  t4=0.5;

type

  TPos = record
    x: double;
    y: double;
    teta:double;
    tipo:integer;
  end;

  PointList = array [0..200] of Tpos;

  TPointList = record
    PList: PointList;
    PCount: integer;
  end;
  
  TLocalization = record
    //global position
    x:double;
    y:double;
    teta:double;
    PointError:double;
    //variancia
    cov:TPos;
    //odos position
    odos:TPos;
    //localization Position
    loc:Tpos;
  end;
  
  TRLocState=record
    //global position
    rpos: TPos;
    //global actual speeds
    v,vn,w:double;
    //odos variation
    dodos:TPos;
    //odos position only
    odos:TPos;
    //localization Position only
    loc:Tpos;
    //variancia
    cov:TPos;
    varpos:Tpos;
    dv,dvn:double;
    Mode: integer;
  end;
  
  TPointsLine = record
    x1:double;
    y1: double;
    x2: double;
    y2: double;
  end;
  
  //array do mapa
  TMapDist = array [0..MapW-1,0..MapH-1] of double;
  pTMapDist=^TMapDist;
  //array do gradiente
  TMapGrad = record
    x: TMapDist;
    y: TMapDist;
  end;




procedure MinimizeError(out PointErr:double; out dx:double; out dy:double; out dphi:double; x, y, phi:double; vis:PointList; max_points:longword);
function Optimize(out xy:TPos;out h:double; vis:PointList; niter:longword; max_points: longword;stepwidthxy:double; stepwidthphi:double):double;
procedure SecondDerivate(out hdxy: TPos; out hdphi:double; xy:TPos; h:double; vis: PointList; max_points: longword);
function EstimatePosition(var rxy:TPos; var rphi:double; visList:PointList; contador:integer; nposes:integer): double;
function RobotLocalization(var Loc:TLocalization; ExtPoints:PointList; numPoints: integer):integer;
procedure EstimateFusion(var RobotPos: TPos; OdosDeltaPos:TPos; SensorPos:TPos; var VarPos:TPos; VarSensorPos :TPos);
function Distance(p:TPos; var Dist:TMapDist):double;
function Gradient(p:TPos; var Grad:TMapGrad):TPos;

procedure locRobot (var Robotdata:TRLocState;SensorPoints:TPointList);
procedure LoadMap(FileName: string);
procedure DrawArrayDist(var TheArray: TMapDist; SizeW, SizeH:integer;Image:TImage);

function RotatePos(p:TPos; ang:double): TPos;



//function GetTickCount: LongWord;

var
  MapDist: TMapDist;
  MapGrad: TMapGrad;
  MapLoaded: boolean=false;
  TempBitmap: TBitmap;
  imfactor: double;
  cand_pose: array[0..20] of TLocalization;
  MapAreaH:integer;
  MapAreaW:integer;
  cell_size:double;
  best_pose_index:integer;
  best_cand_pose_counter:integer;
  changepose:integer;
  varRobot:TPos;
  JumpFlag: boolean;
  jumps:integer = 0;
  VAR_CONST: double = 1e-9;
  //t1,t2,t3,t4:double;



  
  RobotActualLocalization: TLocalization;



implementation

uses Utils, LocMap;


{
--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
Funçoes do algoritmo
--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
}

//esta função calcula a função de erro aproximada a err:= (err + 1 - (c*c/(c*c+dist*dist)));
//ou seja, para todos os pontos da linha obtidos na visão calcula o erro, e a derivada do erro em relação ao ponto
//tem como saida, o erro ("err") e as primeiras derivadas em ordem a x ("dx") a y ("dy") e a phi ("phi")
procedure MinimizeError(out PointErr:double; out dx:double; out dy:double; out dphi:double; x, y, phi:double; vis:PointList; max_points:longword);
var
   nPoints:longword ;
   vis_iterator: longword;
   vp, ddistdpos: TPos;
   dist, derrddist: double;
begin
  //se o numero de pontos for menor que o máximo permitido, actualiza o numero de pontos a tratar
  //visto que todos os pontos a tratar estão guardados em um vector
  if (round(Length(vis)) < max_points) then
    nPoints:=Length(vis)
  else
    nPoints:=max_points;

  //inicialização dos parametros
  PointErr:=0;
  dx:=0;
  dy:=0;
  dphi:=0;

  //para todos os pontos visiveis faz o somatório
  for vis_iterator:=low(vis) to nPoints -1 do
  begin
    //calculo da posição do ponto no sistema de coordenadas global
    vp.x:=x + cos(phi)*vis[vis_iterator].x - sin(phi)*vis[vis_iterator].y;
    vp.y:=y + sin(phi)*vis[vis_iterator].x + cos(phi)*vis[vis_iterator].y;

    //distância do ponto á linha real do campo mais proxima do ponto, ou seja
    //lê o array ("d")que contém as distâncias minimas(precalculadas) para todas as células(pontos) do campo
    if (vp.x >=0) and (vp.y>=0) then
      dist:=Distance(vp, MapDist);

    //somatório da função de erro aproximada  err:= (err + 1 - (c*c/(c*c+dist*dist)));
    PointErr:= (PointErr + (1 - (Lc*Lc/(Lc*Lc+dist*dist))));

    //derivada do erro em ordem á distâcia ("dist")
    derrddist:=(2*Lc*Lc*dist)/((Lc*Lc+dist*dist)*(Lc*Lc+dist*dist));

    //derivada da distÂncia em ordem á posição , calculo do gradiente usando o filtro de sobel
    //este valor é lido de um array ("dgradient")que contém os valores precalculados para cada célula(ponto) do campo
    ddistdpos:=Gradient(vp, MapGrad);

    //somatório das derivadas do erro em ordem á posição (x,y,phi) para todos os pontos
    dx:=dx+derrddist*ddistdpos.x;
    dy:=dy+derrddist*ddistdpos.y;
    dphi:=dphi+derrddist*(ddistdpos.x*(-sin(phi)*vis[vis_iterator].x-cos(phi)*vis[vis_iterator].y)+ddistdpos.y*(cos(phi)*vis[vis_iterator].x-sin(phi)*vis[vis_iterator].y));
  end;
end;

//esta função faz uma optimização do erro calculado, ou seja
//aplica o algoritmo RPROP, ás três componente da posição (x,y,phi) de forma a obter uma melhor convergência desta
//para além da informação do erro, utiliza também a informação das derivadas do erro
function Optimize(out xy:TPos;out h:double; vis:PointList; niter:longword; max_points: longword;stepwidthxy:double; stepwidthphi:double):double;
var
   parami: array [0..2] of double;
   gradi: array [0..2] of double;
   latest_grad: array [0..2] of double;
   PointErr:double;
   stepwidth: array [0..2] of double;
   i, j:longword;
begin
  //inicialização de parametros
  parami[0]:=xy.x;
  parami[1]:=xy.y;
  parami[2]:=h;
  latest_grad[0]:=0;
  latest_grad[1]:=0;
  latest_grad[2]:=0;
  stepwidth[0]:=stepwidthxy;
  stepwidth[1]:=stepwidthxy;
  stepwidth[2]:=stepwidthphi;

  //para todos os pontos visiveis efectua as niter(n iterações) do algoritmo RPROP
  for i := 0 to niter do
  begin
    //calcula o erro associado ao ponto
    MinimizeError(PointErr,gradi[0],gradi[1], gradi[2], parami[0],parami[1],parami[2], vis, max_points);

    //aplica o algoritmo RPROP para cada componente da posiçã, x, y e h(phi)
    for j := 0 to 2 do
    begin
      if (gradi[j]<>0) then  begin
        if (gradi[j]*latest_grad[j]>0) then begin
          stepwidth[j]:= stepwidth[j]*1.2;
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
  xy.x:=parami[0];
  xy.y:=parami[1];
  h:=NormalizeAngle(parami[2]);
  result:=PointErr/max_points;
end;


//esta função calcula a segunda derivada do erro em ordem a x,y e phi
procedure SecondDerivate(out hdxy: TPos; out hdphi:double; xy:TPos; h:double; vis:PointList; max_points: longword);
var
   phi:double;
   PointErr, dErr, ddErr, PointDist:double;
   n_points: longword;
   dDist:TPos;
   vp, dposdphi, ddposdphi2: TPos;
   vis_iterator: longword;
begin
  phi:=h;
  PointErr:=0;
  hdxy.x:=0;
  hdxy.y:=0;
  hdphi:=0;
  if (round(max_points)> Length(vis)) then begin
    n_points:=Length(vis);
  end else begin
    n_points:=max_points;
  end;

  for vis_iterator:=low(vis) to n_points - 1 do begin
    vp.x:=xy.x + cos(phi)*vis[vis_iterator].x-sin(phi)*vis[vis_iterator].y;
    vp.y:=xy.y + sin(phi)*vis[vis_iterator].x+cos(phi)*vis[vis_iterator].y;
    PointDist:=Distance(vp, MapDist);
    PointErr:= (PointErr + ( 1 - (Lc*Lc/(Lc*Lc+PointDist*PointDist))));
    if (PointDist<2*Lc) then begin
      dErr:=PointDist/(Lc*Lc);
      dderr:=1/(Lc*Lc);
      ddist:=Gradient(vp, MapGrad);
      dposdphi.x:=-sin(phi)*vis[vis_iterator].x-cos(phi)*vis[vis_iterator].y;
      dposdphi.y:= cos(phi)*vis[vis_iterator].x-sin(phi)*vis[vis_iterator].y;
      ddposdphi2.x:=-cos(phi)*vis[vis_iterator].x+sin(phi)*vis[vis_iterator].y;
      ddposdphi2.y:=-sin(phi)*vis[vis_iterator].x-cos(phi)*vis[vis_iterator].y;

      hdxy.x:=hdxy.x + dderr*ddist.x*ddist.x;
      hdxy.y:=hdxy.y + dderr*ddist.y*ddist.y;
      hdphi:= hdphi  + dderr*(ddist.x*dposdphi.x+ddist.y*dposdphi.y)*(ddist.x*dposdphi.x+ddist.y*dposdphi.y) + derr*(ddist.x*ddposdphi2.x+ddist.y*ddposdphi2.y);
    end;
  end;
  //VAR_CONST=1e-10;

  if hdxy.x <> 0 then begin
   hdxy.x:=abs((1/hdxy.x)*VAR_CONST);
   if hdxy.x > MAX_VAR then
     hdxy.x:=MAX_VAR;
  end else begin
   hdxy.x:=MAX_VAR;
  end;

  if hdxy.y <> 0 then begin
   hdxy.y:=abs((1/hdxy.y)*VAR_CONST);
   if hdxy.y > MAX_VAR then
     hdxy.y:=MAX_VAR;
  end else begin
   hdxy.y:=MAX_VAR;
  end;

  if hdphi <> 0 then begin
   hdphi:=abs((1/hdphi)*VAR_CONST*0.01);
   if hdphi > MAX_VAR then
     hdphi:=MAX_VAR;
  end else begin
    hdphi:=MAX_VAR;
  end;
end;

function normangle(angle:double): double;
begin
  if angle > degtorad(360) then
    angle:=angle- degtorad(360);
  if angle < -degtorad(360) then
    angle:=angle+degtorad(360);
  result:=angle;
end;


//EstimateFusion

procedure EstimateFusion(var RobotPos: TPos; OdosDeltaPos:TPos; SensorPos:TPos; var VarPos:TPos; VarSensorPos :TPos);
var
  tempOdosDeltaPos: TPos;
begin

  rotatepos(OdosDeltaPos,OdosDeltaPos.teta);

  tempOdosDeltaPos.x:=min(0.1*abs(OdosDeltaPos.x),0.1);
  tempOdosDeltaPos.y:=min(0.1*abs(OdosDeltaPos.y),0.1);
  tempOdosDeltaPos.teta:=min(0.1*abs(OdosDeltaPos.teta),0.1);

  VarPos.x:=VarPos.x+tempOdosDeltaPos.x*tempOdosDeltaPos.x;
  VarPos.y:=VarPos.y+tempOdosDeltaPos.y*tempOdosDeltaPos.y;
  VarPos.teta:=VarPos.teta+tempOdosDeltaPos.teta*tempOdosDeltaPos.teta;


  //FLocMap.LVar.Caption:=format('%.8f; %.8f; %.8f  --  %.4f; %.4f; %.4f',[VarPos.x, VarPos.y, VarPos.teta, VarSensorPos.x, varSensorPos.y, varSensorPos.y]);

  RobotPos.x:=(VarSensorPos.x*RobotPos.x+VarPos.x*SensorPos.x)/(VarSensorPos.x+VarPos.x);
  VarPos.x:=(VarPos.x*VarSensorPos.x)/(VarPos.x+VarSensorPos.x);
  RobotPos.y:=(VarSensorPos.y*RobotPos.y+VarPos.y*SensorPos.y)/(VarSensorPos.y+VarPos.y);
  VarPos.y:=(VarPos.y*VarSensorPos.y)/(VarPos.y+VarSensorPos.y);
  RobotPos.teta:=NormalizeAngle((VarSensorPos.teta*RobotPos.teta+VarPos.teta*SensorPos.teta)/(VarSensorPos.teta+VarPos.teta));
  VarPos.teta:=(VarPos.teta*VarSensorPos.teta)/(VarPos.teta+VarSensorPos.teta);

end;

//Estimate position
function EstimatePosition(var rxy:TPos; var rphi:double; visList:PointList; contador:integer; nposes:integer): double;
var
  xyteta: TPos;
  i: Integer;
  err_opt, min_err, rerr: double;

begin
  //parametros ajustaveis????? preciso verificar isto  :/???????

  Randomize;

  //optimiza�ao para a posi�ao seleccionada
  err_opt:=Optimize(rxy,rphi,visList,10,contador,t1,t2);
  rerr:=err_opt;

  //numero minimo de pontos para a localiza��o global

  if FLocMap.CBGlobalLocalization.Checked then begin
    if contador > 20 then begin

      //cria lista de poses candidatas aleatoriamente
      for i:=0 to nposes-2 do begin
        cand_pose[i].x:=Random(MapAreaW)*cell_size;
        cand_pose[i].y:=Random(MapAreaH)*cell_size;
        cand_pose[i].teta:=Random(round(pi*2));
      end;

      //para todas as pose calcula o erro associado
      for i:=0 to nposes-1 do begin
        xyteta.x:=cand_pose[i].x;
        xyteta.y:=cand_pose[i].y;
        xyteta.teta:=cand_pose[i].teta;
        err_opt:=Optimize(xyteta,xyteta.teta,visList,10,contador,t3,t4);

        //se a optimizaçao der fora do campo dar erro grande para descartar ponto
        if ((xyteta.x > MapAreaW*cell_size-1) or (xyteta.y > MapAreaH*cell_size-1) or (xyteta.x < 0) or (xyteta.y<0)) then begin
          cand_pose[i].PointError:=1;
        //guarda a optimiza�ao das poses e o respectivo erro
        end else begin
          cand_pose[i].x:=xyteta.x;
          cand_pose[i].y:=xyteta.y;
          cand_pose[i].teta:=xyteta.teta;
          //se o angulo variar mais de 80 graus da erro grande para descartar ponto
          if Abs(DiffAngle(rphi,cand_pose[i].teta)) > degtorad(80) then begin
            cand_pose[i].PointError:=err_opt+1;
          end else begin
            cand_pose[i].PointError:=err_opt;
          end;
        end;
      end;

      //considera-se que no fim do vector fica sempre a pose considerada minima (este valor nunca é criado aleatóriamente)
      min_err:=cand_pose[nposes-1].PointError;
      best_pose_index:=nposes-1;

      //verifica-se se o erro das novas Poses é menor que o minimo, dentro de uma dada gama, se for substitui-se
      for i:=0 to nposes-2 do begin
        if (cand_pose[i].PointError <= min_err*1) then begin
          min_err:=cand_pose[i].PointError;
          best_pose_index:=i;
          best_cand_pose_counter:=0;
        end;
      end;

      //actualiza-se a posição referência com a posição onde o erro é minimo
      cand_pose[nposes-1].x:=cand_pose[best_pose_index].x;
      cand_pose[nposes-1].y:=cand_pose[best_pose_index].y;
      cand_pose[nposes-1].teta:=cand_pose[best_pose_index].teta;
      cand_pose[nposes-1].PointError:=cand_pose[best_pose_index].PointError;

      //se essa posição se repetir por 5 vezes o robot é auto-localiza-se nessa posição
      if cand_pose[nposes-1].PointError <= rerr*0.25 then begin
        best_cand_pose_counter:=best_cand_pose_counter+1;
      end else begin
        best_cand_pose_counter:=0;
      end;
      //actualiza a posição com a Pose candidata ganhadora se esta tiver menor erro que a posição actual
      if best_cand_pose_counter > 5 then begin
        if changepose > 100 then begin
          changepose:=0;
        end else begin
          changepose:=changepose+1;
        end;
        best_cand_pose_counter:=0;
        rxy.x:=cand_pose[nposes-1].x;
        rxy.y:=cand_pose[nposes-1].y;
        rphi:=cand_pose[nposes-1].teta;
        rerr:=cand_pose[nposes-1].PointError;
        jumps:=jumps+1;
        s:=s+' |Jump| ';
        JumpFlag:=true;
      end;
    end;
  end;
  result:=rerr;
end;

//retorna numero de pontos se tiver pontos suficientes senao retorna 0
function RobotLocalization(var Loc:TLocalization; ExtPoints:PointList; numPoints: integer):integer;
var
  rxy, RobotCovXY: TPos;
  rphi:double;
  RobotCovTeta:double;
begin
  if(numPoints > 1) then begin
    rxy.x:=Loc.x;
    rxy.y:=Loc.y;
    rphi:=Loc.teta;

    //estima a posi�ao do robot
    //retorna valores em metros ja nas coordenadas do campo
    Loc.PointError:=EstimatePosition(rxy, rphi, ExtPoints, numPoints, 5);
    //estima a covariancia do, x,y, phi para a melhor posi�ao calculada  (cov_x e cov_y multiplicadas por 100000)
    SecondDerivate(RobotCovXY, RobotCovTeta, rxy, rphi, ExtPoints, numPoints);

    Loc.x:=rxy.x;
    Loc.y:=rxy.y;
    Loc.teta:=rphi;

    Loc.cov.x:= RobotCovXY.x;
    Loc.cov.y:= RobotCovXY.y;
    Loc.cov.teta:=RobotCovTeta;

    Result:=numPoints;
  end else begin
    //se o numero de pontos �e insuficiente retorna zero
    Result:=0;
  end;
end;

//esta função determina a distância minima do ponto á linha do campo mais próxima
//basicamente lê o array "d", na posição de interesse
//p é em metros, necessário converter para pixeis(células)
function Distance(p:TPos; var Dist:TMapDist):double;
var
  xi, yi:integer;
begin
  xi:=round(p.x/cell_size);
  yi:=round(p.y/cell_size);
  //se o ponto se encontra dentro do campo determina a distância
  if (xi<0) then xi:=0;
  if (yi<0) then yi:=0;
  if (xi>=MapAreaW-1) then xi:=MapAreaW-1;
  if (yi>=MapAreaH-1) then yi:=MapAreaH-1;
  Result:=Dist[yi,xi];
end;


//esta função determina a derivada da distância ordem á posição, o gradiente
//basicamente lê o array "Grad", na posição de interesse
//p é em metros, necessário converter para pixeis(células)
function Gradient(p:TPos; var Grad:TMapGrad):TPos;
var
  xi, yi:integer;
  r: TPos;
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



procedure locRobot (var Robotdata:TRLocState;SensorPoints:TPointList);
var
  myloc:TLocalization;
begin
  //actualiza movimento doobo com a odometria
  myloc.x:=Robotdata.rpos.x+((MapAreaW/2)*cell_size);//+Robotdata.dodos.x;
  myloc.y:=Robotdata.rpos.y+((MapAreaH/2)*cell_size);//+Robotdata.dodos.y;
  myloc.teta:=Robotdata.rpos.teta;//+Robotdata.dodos.teta;

  //estimativa da localiza�ao do robo baseada nos sensores
  RobotLocalization(myloc,SensorPoints.PList, SensorPoints.PCount);

  if JumpFlag then begin
    Robotdata.rpos.x:=myloc.x-((MapAreaW/2)*cell_size);
    Robotdata.rpos.y:=myloc.y-((MapAreaH/2)*cell_size);
    Robotdata.rpos.teta:=myloc.teta;
  end;

  Robotdata.loc.x:=myloc.x-((MapAreaW/2)*cell_size);
  Robotdata.loc.y:=myloc.y-((MapAreaH/2)*cell_size);
  Robotdata.loc.teta:=myloc.teta;

  Robotdata.cov.x:=myloc.cov.x;
  Robotdata.cov.y:=myloc.cov.y;
  Robotdata.cov.teta:=myloc.cov.teta;
end;



//map functions
procedure LoadMap(FileName: string);
var
  i,j: integer;
  lines, col1, col2,col3, lin:integer;
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
      {col1:=thisline.Count;
      col2:=thisline2.Count;
      col3:=thisline3.Count;
      if (col2 < col1) then
        col1:=thisline2.Count; }
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

procedure DrawArrayDist(var TheArray: TMapDist; SizeW, SizeH:integer;Image:TImage);
var
  SrcIntfImg: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  SrcBitmap: TBitmap;
  vi, ui, gray: integer;
  yi,xi:integer;
  fpcol: TFPcolor;
  min_val, max_val:double;
begin
  TempBitmap:=TBitmap.Create;
  SrcBitmap:=TBitmap.Create;
  SrcIntfImg:=TLazIntfImage.Create(0,0);

  if SizeW/Image.Width > SizeH/Image.Height   then
     imfactor:=SizeW/Image.Width
  else
    imfactor:=SizeH/Image.Height;

  if (SizeW<Image.Width) and (SizeH<Image.Height) then
    imfactor:=1;

  SrcBitmap.Width:=Image.Width;
  SrcBitmap.Height:=Image.Height;

  SrcIntfImg.LoadFromBitmap(SrcBitmap.Handle, SrcBitmap.MaskHandle);
  max_val:=0;
  min_val:=100;
  for yi := 0 to SizeH-1 do begin
    for xi := 0 to SizeW-1 do begin
      if ((TheArray[yi,xi]>max_val)and (TheArray[yi,xi]<65535)) then
        max_val:=TheArray[yi,xi];
      if (TheArray[yi,xi]<min_val) then
        min_val:=TheArray[yi,xi];
    end;
  end;
  //min_val:=min_val+0.1;

  SrcIntfImg.FillPixels(colWhite);
  fpcol.alpha:=0;
  for vi :=0 to Image.Height-1 do begin
    for ui :=0 to Image.Width-1 do begin
      if ((vi*imfactor<SizeH-1) and (ui*imfactor<SizeW-1)) then  begin
        gray := round((TheArray[round(SizeH-1-imfactor*vi),round(imfactor*ui)]+abs(min_val))*65536/(max_val-min_val));
        if (gray > 65535 )then begin
          gray:=255*255;
          fpcol.red := gray;
          fpcol.green := gray;
          fpcol.blue := gray;
        {end else if gray<=min_val*65536/max_val then begin
          gray:=255*255;
          fpcol.red := gray;
          fpcol.green := 0;
          fpcol.blue := 0;}
        end else begin
          fpcol.red := gray;
          fpcol.green := gray;
          fpcol.blue := gray;
        end;
        SrcIntfImg.Colors[round(ui*imfactor),round(Image.Height-SizeH+vi*imfactor)] := fpcol;
      end;
    end;
  end;

  SrcIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,true);
  TempBitmap.Handle:=ImgHandle;
  //Image.Canvas.Clear;
  //Image.Canvas.Draw(0,0,TempBitmap);
  Image.Visible:=false;
  FLocMap.Canvas.Clear;
  FLocMap.Canvas.Draw(0,0,TempBitmap);
  SrcIntfImg.Free;
end;

function RotatePos(p:TPos; ang:double): TPos;
var
  pp:TPos;
  cosa, sina:double;
begin
  cosa:=cos(ang);
  sina:=sin(ang);
  pp.x:=p.x*cosa+p.y*sina;
  pp.y:=-p.x*sina + p.y*cosa;

  result:=pp;
end;



end.

