unit Debug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Roles;

type

  { TFormDebug }

  TFormDebug = class(TForm)
    EditReceiver: TEdit;
    procedure EditReceiverChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormDebug: TFormDebug;

implementation

{ TFormDebug }

procedure TFormDebug.EditReceiverChange(Sender: TObject);
begin

end;

initialization
  {$I debug.lrs}

end.

