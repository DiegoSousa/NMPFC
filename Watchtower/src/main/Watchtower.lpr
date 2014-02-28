program Watchtower;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Coach, lnetvisual, TAChartLazarusPkg, Field, Param, Fuzzy,
  Log;

{$IFDEF WINDOWS}{$R Watchtower.rc}{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFCoach, FCoach);
  Application.CreateForm(TFField, FField);
  Application.CreateForm(TFLog, FLog);
  Application.CreateForm(TFParam, FParam);
  Application.Run;
end.
