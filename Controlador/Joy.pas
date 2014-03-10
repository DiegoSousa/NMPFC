unit Joy;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Types, Main, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BaseUnix, LResources, IniPropStorage;

const
JS_EVENT_BUTTON =        $01;    // button pressed/released
JS_EVENT_AXIS   =        $02;    // joystick moved
JS_EVENT_INIT   =        $80;    // initial state of device

type

//struct js_event
//  __u32 time;     /* event timestamp in milliseconds */
//  __s16 value;    /* value */
//  __u8 type;      /* event type */
//  __u8 number;    /* axis/button number */

  js_event = record
    time: Longword;  //event timestamp in milliseconds
    value: Smallint; // value
    etype: byte;       // event type
    number: byte;     // axis/button number
  end;

type

  { TFJoy }

  TFJoy = class(TForm)
    //FormStorage: TFormStorage;
    EditJoyFile: TEdit;
    Bgo: TButton;
    EditJoyEvent: TEdit;
    FormStorage: TIniPropStorage;
    procedure FormClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BgoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FJoy: TFJoy;
  joyfd: integer;
  JoyEvent :js_event;
  axis,buts: array[0..3] of integer;
  axisnames: array[0..3] of string=('vn','v','w','level');

implementation

uses utils,Robots;

procedure TFJoy.FormClose(Sender: TObject);
begin
  Fmain.AuxFormClosed('Joystick');
  fileclose(joyfd);
end;

procedure TFJoy.FormCreate(Sender: TObject);
begin
  FormStorage.IniFileName:=FMain.FormStorage.IniFileName;
  Fmain.InsertAuxForms(FJoy,'Joystick');

end;

procedure TFJoy.BgoClick(Sender: TObject);
var joypath: string;
begin
  joypath:=EditJoyFile.text;
  joyfd := fpOpen(pchar(joypath), O_NONBLOCK or O_RDONLY);
  if joyfd>0 then begin
    EditJoyEvent.Text:='Opened';
  end else begin
    EditJoyEvent.Text:='Failed';
  end;

end;

initialization
  {$I Joy.lrs}

end.
