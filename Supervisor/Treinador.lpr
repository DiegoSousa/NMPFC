program Treinador;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Coach, LResources, lnetvisual, TAChartLazarusPkg, Field, Param2, Fuzzy,
  Log;

{$IFDEF WINDOWS}{$R Treinador.rc}{$ENDIF}

begin
  {$I Treinador.lrs}
  Application.Initialize;
  Application.CreateForm(TFCoach, FCoach);
  Application.CreateForm(TFField, FField);
  Application.CreateForm(TFLog, FLog);
  Application.CreateForm(TFParam2, FParam2);
  Application.Run;
end.

