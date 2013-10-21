unit Unit_Hardware_Configure_UDP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  IniPropStorage, StdCtrls;

type

  { TForm_Hardware_Configure_UDP }

  TForm_Hardware_Configure_UDP = class(TForm)
    Edit_HostIP: TEdit;
    Edit_HostPort: TEdit;
    Edit_Port: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form_Hardware_Configure_UDP: TForm_Hardware_Configure_UDP;

implementation

initialization
  {$I unit_hardware_configure_udp.lrs}

end.

