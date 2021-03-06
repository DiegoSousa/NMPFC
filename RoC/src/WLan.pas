unit WLan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DecConsts, Roles;
  
type
  TPlayerRobotState = packed record
    x, y, teta: single;
    vx, vy, w: single;
    conf: single;
    Active: boolean;
    WithBall: boolean;
  end;

  TPlayerBallState = packed record
    x,y,vx,vy: single;
    x_n,y_n,vx_n,vy_n: single;
    x_next,y_next: single;
    quality: single;
  end;

  TPlayerObsState = packed record
    x, y: single;
    conf: single;
  end;

  TPlayerInfo = packed record
    Magic: LongWord;
    RobotState: TPlayerRobotState;
    ObsState: array [0..MaxRobots-1] of TPlayerObsState;
    BallState: TPlayerBallState;
    num: byte;
    Batery:double;
  end;


  TCoachRobotState = packed record
    x, y, teta: single;
//    cov_x,cov_y,cov_xy,cov_teta: single;
    vx,vy,w: single;
    conf: single;
    role: TRole;
    active: boolean;
  end;

  TCoachBallState = packed record
    x,y,vx,vy: single;
    vote:integer;
    x_n,y_n,vx_n,vy_n: single;
    x_next,y_next: single;
    coachQuality: single;
  end;
  
  TCoachObsState = packed record
    x, y: single;
    conf: single;
  end;

  TCoachInfo = packed record
    Magic: LongWord;
    RobotState: array [0..MaxRobots-1] of TCoachRobotState;
    BallState: TCoachBallState;
    ObsStates:  array [0..MaxRobots-1] of TCoachObsState;
    Play: TPlay;
  end;



implementation

end.

