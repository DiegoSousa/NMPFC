unit unit_hardware_configure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TForm_Hardware_Configure }

  TForm_Hardware_Configure = class(TForm)
    Edit_Timer_GlobalStatus: TEdit;
    Edit_Timer_Interval: TEdit;
    Label1: TLabel;
    Label2: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form_Hardware_Configure: TForm_Hardware_Configure;

implementation

initialization
  {$I unit_hardware_configure.lrs}

end.

