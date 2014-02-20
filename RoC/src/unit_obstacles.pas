unit unit_obstacles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs; 

type

  { TForm_Obstacles }

  TForm_Obstacles = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form_Obstacles: TForm_Obstacles;

implementation

{ TForm_Obstacles }

procedure TForm_Obstacles.FormCreate(Sender: TObject);
begin

end;

initialization
  {$I unit_obstacles.lrs}

end.

