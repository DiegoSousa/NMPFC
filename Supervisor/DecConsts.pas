unit DecConsts;

{$mode objfpc}{$H+}

interface

const
  // team dimension
  MaxOponents=6;
  MaxRobots=6;

  // field dimensions
  FieldWidth=12;
  FieldLength=18;
  FieldOutsideSpace=1;
  GoalDepth=0.5;
  RSpace=0.6;


type
  TPlay = (
    playHalt, playNormal, playSearch,playFormation
    );
    
const
  CPlayString: array[low(TPlay)..High(TPlay)] of string =
    (
    'Halt', 'Normal Play', 'Search Ball','Formation'
    );
    
var

  GoalWidth: double;
  // this is actually Area Radius...
  AreaWidth: double;
  // calculated constants
  OurGoalX: double;
  OurAreaX: double;
  TheirGoalX: double;
  MaxFieldX: double;
  MaxFieldY: double;

  rob_num: integer=-1;
  // dynamic parameters
  AxialDistance: double;  // distancia entre as rodas
  Kw: double;
  SpeedMax: double=3.0;

  BallSpace: double=0.4;   // TODO : o que e' isto?
  RobotSpace: double=0.6; // dist between robot centers



implementation

end.
