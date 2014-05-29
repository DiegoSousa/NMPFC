program Watchtower;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Coach, lnetvisual, TAChartLazarusPkg, Field, Param2, Fuzzy,
  Log, dynmatrix, LogCompat, Robots, Roles, Tactic, Utils;

{$IFDEF WINDOWS}{$R Watchtower.rc}{$ENDIF}

{$R *.res}

begin
//  {$I Watchtower.lrs}
  Application.Initialize;
  Application.CreateForm(TFCoach, FCoach);
  Application.CreateForm(TFField, FField);
  Application.CreateForm(TFLog, FLog);
  Application.CreateForm(TFParam2, FParam2);
  Application.Run;
end.

