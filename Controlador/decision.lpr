program Decision;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, Main, lnetvisual, Field, Camera, daisybin,
  Joy, RLan, Utils, Log, Param, Robots, TAChartLazarusPkg, omni3, SdpoSerialLaz,
  kicker, Roles, Actions, Tactic, Tasks, obsavoid, LogCompat, WLan, CoachMain,
  dynmatrix, Localization, LocMap, Debug, DecConsts, astarmapimage,
  Unit_Hardware, Unit_Hardware_Configure_UDP, unit_hardware_configure,
  Unit_FlashBus, Unit_joystick, SdpoJoystickLaz, unit_localizationAux,
  unit_Localization, KalmanBall, KalmanBall_Aux, unit_obstacles, Unit_RolesAux,
  Unit1, MPC;

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TFCoachMain, FCoachMain);
  Application.CreateForm(TFField, FField);
  Application.CreateForm(TFCamera, FCamera);
  Application.CreateForm(TFJoy, FJoy);
  Application.CreateForm(TFLog, FLog);
  Application.CreateForm(TFParam, FParam);
  Application.CreateForm(TFOmni3, FOmni3);
  Application.CreateForm(TFKicker, FKicker);
  Application.CreateForm(TFLocMap, FLocMap);
  Application.CreateForm(TFormDebug, FormDebug);
  Application.CreateForm(TForm_Hardware, Form_Hardware);
  Application.CreateForm(TForm_Hardware_Configure_UDP,Form_Hardware_Configure_UDP);
  Application.CreateForm(TForm_Hardware_Configure, Form_Hardware_Configure);
  Application.CreateForm(TForm_joystick, Form_joystick);
  Application.CreateForm(TForm_Localization, Form_Localization);
  Application.CreateForm(TFBall, FBall);
  Application.CreateForm(TForm_Obstacles, Form_Obstacles);
  Application.CreateForm(TForm_Roles, Form_Roles);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormMPC, FormMPC);
  Application.Run;
end.

