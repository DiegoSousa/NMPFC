program Treinador;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Coach, LResources, lnetvisual, TAChartLazarusPkg, Field, Param, Fuzzy,
  Log
  { you can add units after this };

{$IFDEF WINDOWS}{$R Treinador.rc}{$ENDIF}

begin
  {$I Treinador.lrs}
  Application.Initialize;
  Application.CreateForm(TFCoach, FCoach);
  Application.CreateForm(TFField, FField);
  Application.CreateForm(TFParam, FParam);
  Application.CreateForm(TFLog, FLog);
  Application.Run;
end.

