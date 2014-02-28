unit RLan;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls, math;

const
  NumLanCenters=8;
  NumLanColors=5;
  NumLanEdges=32;
  NumLanRadar=64;
  NumLanOdos=4;
  NumLanRegions=16; //Não faz sentido ser <> de MaxRegions
  NumSensDists=2;
  LanImageBlockSize=980;

  UDPBufSize=1024;

const
  DAISY_OUT_MASK   = 8;

  PACK_TYPE_NOP	   = 0;
  PACK_TYPE_SPEED  = 1;
  PACK_TYPE_KICK   = 2;
  PACK_TYPE_DISPLAY      = 3;
  PACK_TYPE_WRITE_EEPROM = 4;
  PACK_TYPE_READ_EEPROM  = (0 or DAISY_OUT_MASK);
  PACK_TYPE_READ_ENC     = (1 or DAISY_OUT_MASK);

{type
//-------------------------------------------------------------------------------------
// LAN types

  TLanCenter=packed record
//    x,y: word;
    enc1,enc2: integer;
    count: word;
  end;

  pTLanCenter=^TLanCenter;

  TLanHeaderPacket= packed record
    FromRobot: byte;
    ToRobot:byte;
    PacketType: char;
    PacketSubType: char;
    SerialNumber: integer;
    FrameTime: integer;
    SendTime: integer;
  end;

  TLanHeaderPart= packed record
    PartType: char;
    PartSubType: char;
    PartSize: word;
  end;

  TBestCenters= packed array[0..NumLanCenters*NumLanColors-1] of TLanCenter;

  TLanCentersPart= packed record
    NumCenters: packed array[0..NumLanColors-1] of byte;
    BestCenters: TBestCenters;
  end;

  TLanEdge=packed record
    lineNum: byte;
    x,y: word;
    color1_2: byte;
  end;

  TBestEdges= packed array[0..NumLanEdges-1] of TLanEdge;

  TLanEdgesPart= packed record
    NumEdges: word;
    BestEdges: TBestEdges;
  end;

  TLanOdo=record
//    speed1,speed2: smallint;
    pos1,pos2,pos3: integer;
    count: integer;
  end;

//  TLanOdoOmni=packed record
//    pos1,pos2,pos3: integer;
//    count: integer;
//  end;

  TBestOdos= array[0..NumLanOdos-1] of TLanOdo;
}
type
  TSolKick=packed record
    pulse1: byte;
    pulse2: byte;
    delay: byte;
    osc_div: byte;
  end;

{//TLanRegionCount= packed array[0..NumLanRegionColors-1] of byte;

  TLanRegion=packed record
//    number: byte;
    BestColor: byte;
    BestColorPer: byte;
  end;

  TBestRegions = packed array[0..NumLanRegions-1] of TLanRegion;

  TLanRegionsPart= packed record
    NumRegions: word;
    BestRegions: TBestRegions;
  end;

  TLanNeckSpeeds=packed record
    speed: smallint;
    count: word;
  end;

  TLanImageBlock = packed array[0..LanImageBlockSize-1] of byte;

  TLanImagePart= packed record
    NumBlocks: byte;
    ActBlock: byte;
    BlockSize: word;
    ImageBlock: TLanImageBlock;
  end;

  TUDPBuffer= record
    data: array[0..UDPBufSize-1] of char;
    MessSize: integer;
    PacketNum: integer;
  end;

  TArrayofChar=array of char;
  pTArrayofChar=^TArrayofChar;

}

  TUDPBuffer= record
    data: array[0..UDPBufSize-1] of byte;
    MessSize, ReadDisp: integer;
  end;


procedure AddToUDPBuffer(var Buf: TUDPBuffer; it: pointer; size_it: integer);
procedure ClearUDPBuffer(var Buf: TUDPBuffer);

procedure NetPutBuffer(var Buf: TUDPBuffer; var data; size_it: integer);
procedure NetBufferSeek(var Buf: TUDPBuffer; disp: integer);
procedure NetGetBuffer(var Buf: TUDPBuffer; var data; size_it: integer);

procedure NetPutByte(var Buf: TUDPBuffer; value: byte);
procedure NetPutWord(var Buf: TUDPBuffer; value: word);
procedure NetPutShort(var Buf: TUDPBuffer; value: SmallInt);
procedure NetPutInt(var Buf: TUDPBuffer; value: integer);
procedure NetPutFloat(var Buf: TUDPBuffer; value: single);
procedure NetPutString(var Buf: TUDPBuffer; str: string);
procedure NetPutAngle(var Buf: TUDPBuffer; value: double);

function NetPeekByte(var Buf: TUDPBuffer): byte;
function NetGetByte(var Buf: TUDPBuffer): byte;
function NetGetWord(var Buf: TUDPBuffer): word;
function NetGetShort(var Buf: TUDPBuffer): SmallInt;
function NetGetInt(var Buf: TUDPBuffer): integer;
function NetGetFloat(var Buf: TUDPBuffer): single;
function NetGetString(var Buf: TUDPBuffer): string;
function NetGetAngle(var Buf: TUDPBuffer): double;

// Funcao inserida por Andre Scolari em 08/11/04
function TicksToSerialOmni(u1, u2, u3 ,u4: integer): string;
/////////////////////////////////////////////////

function VoltagesToSerial(speed1, speed2, count: integer): string;
function VoltagesToSerialOmni(u1,u2,u3,u4: integer; count: integer): string;
function SolKickToSerial(KickCom: TSolKick): string;
function SolKickToSerialND(KickCom: TSolKick): string;


function isValidMessage(mess: string): boolean;
function ExtractMessage(var sbuf: string): string;
procedure Hex2Raw(txt: string; var raw_buf: array of byte);

{
procedure LanPutInt(var Mess: string; value,digits: integer);
procedure LanPutDouble(var Mess: string; value: double; digits,prec: integer);
procedure LanPutString(var Mess: string; str: string);
procedure LanPutAngle(var Mess: string; value: double);

function LanGetInt(var Mess: string; digits: integer): integer;
function LanGetDouble(var Mess: string; digits,prec: integer): double;
function LanGetString(var Mess: string): string;
function LanGetAngle(var Mess: string): double;

procedure LanInitPut(var Mess: string);
function LanEndPut(var Mess: string): string;

function LanInitGet(packet: string): string;
//function LanInitGet(packet: string): integer;
}
implementation

uses Utils, ZLib, Param;

//var LanCurPacket: string;

const
  digstohexdigs: array[0..10] of integer=(0,2,2,3,4,5,6,7,7,8,8);

procedure NetPutBuffer(var Buf: TUDPBuffer; var data; size_it: integer);
begin
  if Buf.MessSize+size_it >= UDPBufSize then exit;
  copyMemory(@(Buf.data[Buf.MessSize]),@data,size_it);
  Buf.MessSize:=Buf.MessSize+size_it;
end;

procedure NetBufferSeek(var Buf: TUDPBuffer; disp: integer);
begin
  if (disp>Buf.MessSize) or (disp=-1) then disp:=Buf.MessSize;
  Buf.ReadDisp:=disp;
end;

procedure NetGetBuffer(var Buf: TUDPBuffer; var data; size_it: integer);
begin
  if Buf.ReadDisp+size_it>Buf.MessSize then exit;
  copyMemory(@data,@(Buf.data[Buf.ReadDisp]),size_it);
  Buf.ReadDisp:=Buf.ReadDisp+size_it;
end;


procedure NetPutByte(var Buf: TUDPBuffer; value: byte);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutWord(var Buf: TUDPBuffer; value: word);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutShort(var Buf: TUDPBuffer; value: SmallInt);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutInt(var Buf: TUDPBuffer; value: integer);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutFloat(var Buf: TUDPBuffer; value: single);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutString(var Buf: TUDPBuffer; str: string);
var len: word;
begin
  len:=length(str);
  if len>0 then begin
    //NetPutBuffer(Buf,len,sizeof(len));
    NetPutWord(Buf,len);
    NetPutBuffer(Buf,str[1],len);
  end;
end;

procedure NetPutAngle(var Buf: TUDPBuffer; value: double);
var tmp: word;
begin
  tmp:=round((NormalizeAngle(value)+pi)*10000);
  NetPutWord(Buf,tmp);
end;

function NetPeekByte(var Buf: TUDPBuffer): byte;
begin
  result:=0;
  if Buf.ReadDisp>Buf.MessSize then exit;
  result:=Buf.data[Buf.ReadDisp];
end;

function NetGetByte(var Buf: TUDPBuffer): byte;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetWord(var Buf: TUDPBuffer): word;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetShort(var Buf: TUDPBuffer): SmallInt;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetInt(var Buf: TUDPBuffer): integer;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetFloat(var Buf: TUDPBuffer): single;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetString(var Buf: TUDPBuffer): string;
var size: word;
begin
  result:='';
  size:=NetGetWord(Buf);
  if size=0 then exit;
  result:=stringofchar(chr(0),size);
  NetGetBuffer(Buf,result[1],size);
end;

function NetGetAngle(var Buf: TUDPBuffer): double;
begin
  result:=NormalizeAngle(NetGetWord(buf)/10000-pi);
end;


{
procedure LanPutInt(var Mess: string; value,digits: integer);
var snum: string;
    len: integer;
begin
  if digits<=0 then exit;
  if digits>10 then digits:=10;
  snum:=inttohex(value,digstohexdigs[digits]);
  len:=length(snum);
  Mess:=Mess+copy(snum,len-(digits-1),len);
end;

procedure LanPutDouble(var Mess: string; value: double; digits,prec: integer);
begin
  LanPutInt(Mess, round(value*IntPower(10,prec)),digits+prec);
end;

procedure LanPutString(var Mess: string; str: string);
begin
  Mess:=Mess+Format('%4d%s',[length(str),str]);
end;

procedure LanPutAngle(var Mess: string; value: double);
begin
  LanPutDouble(Mess,NormalizeAngle(value),1,3);
end;


function LanGetInt(var Mess: string; digits: integer): integer;
var hd: integer;
begin
  result:=0;
  if digits<=0 then exit;
  if digits>10 then digits:=10;
  hd:=digstohexdigs[digits];
  result:=strtoint('$'+copy(Mess,1,hd));
  if (hd<8) and ((result and (1 shl (hd*4-1)))<>0) then begin
    result:=strtoint('$'+stringofchar('F',8-hd)+copy(Mess,1,hd));
  end;
  Mess:=copy(Mess,hd+1,length(Mess));
end;

function LanGetDouble(var Mess: string; digits,prec: integer): double;
begin
  result:=LanGetInt(Mess,digits+prec)/IntPower(10,prec);
end;

function LanGetAngle(var Mess: string): double;
begin
  result:=LanGetDouble(Mess,1,3);
end;

function LanGetString(var Mess: string): string;
var len: integer;
begin
  len:=strtoint(copy(Mess,1,4));
  result:=copy(Mess,5,len);
  Mess:=copy(Mess,5+len,length(Mess));
end;

procedure LanInitPut(var Mess: string);
begin
  Mess:='';
end;

function LanEndPut(var Mess: string): string;
begin
  //result:='5dpo'+CompressString(Mess);
  result:='5dpo'+Mess;
end;

function LanInitGet(packet: string): string;
begin
  if copy(packet,1,4)<>'5dpo' then begin
    result:='';
//    LanCurPacket:='';
//    result:=-1;
  end else begin
    //result:=DecompressString(copy(packet,5,length(packet)));
    result:=copy(packet,5,length(packet));
//    LanCurPacket:=DecompressString(copy(packet,5,length(packet)));
//    result:=0;
  end;
end;

}


function ExtractMessage(var sbuf: string): string;
var i: integer;
begin
  result:='';
  while (sbuf<>'') do begin
    if sbuf[1]<>'<' then begin
      sbuf:=copy(sbuf,2,length(sbuf));
    end else begin
      break;
    end;
  end;

  if (sbuf<>'') and (sbuf[1]='<') then begin
    i:=2;
    while i<=length(sbuf) do begin
      if sbuf[i]='<' then begin
        sbuf:=copy(sbuf,i,length(sbuf));
        i:=2;
      end else if sbuf[i]='>' then begin
        result:=copy(sbuf,1,i);
        sbuf:=copy(sbuf,i+1,length(sbuf));
        exit;
      end else begin
        inc(i);
      end;
    end;
  end;

end;

function isValidMessage(mess: string): boolean;
var i: integer;
begin
  result:=false;
  if mess='' then exit;
  if mess[1]<>'<' then exit;
  if mess[length(mess)]<>'>' then exit;
  for i:=2 to length(mess)-1 do begin
    if not (mess[i] in ['0'..'9','A'..'F','a'..'f']) then exit;
  end;
  result:=true;
end;

procedure Hex2Raw(txt: string; var raw_buf: array of byte);
var i: integer;
begin
  for i:=0 to (length(txt) div 2)-1 do begin
    raw_buf[i]:=StrToInt('$'+copy(txt,i*2+1,2));
  end;
end;

{
procedure ComPortRxData(Sender: TObject);
var txt: string;
    IR1,IR2: double;
    t: Dword;
    raw_buf: array[0..255] of byte;
begin
  txt:=Comport.ReadData; //ReadStr(txt,count);
  SerialInBuf:=SerialInBuf+txt;
  MemoLog.Lines.Add(txt);
  while MemoLog.Lines.Count>20 do begin
    MemoLog.Lines.Delete(0);
  end;
  while true do begin
    txt:=ExtractMessage(SerialInBuf);
    if txt='' then break;
    if isValidMessage(txt) then begin
      txt:=copy(txt,2,length(txt)-2);
      Hex2Raw(txt,raw_buf);

      case (raw_buf[0] and $F) of
        PACK_TYPE_READ_EEPROM: begin
        end;

        PACK_TYPE_READ_ENC: begin
          EditMess.Text:=txt;
          IR1:=raw_buf[5]*20;
          IR2:=raw_buf[11]*20;
          IMot1:=0.95*Imot1+0.05*IR1;
          IMot2:=0.95*Imot2+0.05*IR2;
          EditIMot.text:=format('%.1f,%.1f',[IMot1,IMot2]);
          inc(MessCount);
          t:=GetTickCount();
          if LastTickCount+100<t then begin
            EditMessCount.text:=format('%d',[MessCount]);
            LastTickCount:=LastTickCount+100;
            MessCount:=0;
          end;

          if raw_buf[6]<>0 then begin
            inc(ResetCount);
            EditResetCount.text:=inttostr(ResetCount);
          end;
        end;
      end;

      if txt[2]+txt[3]='EE' then begin //EEPROM read
        EditEEAddress.text:=txt[4]+txt[5];
        EditEEValue.text:=txt[6]+txt[7];
      end;
      if txt[2]+txt[3]='1D' then begin //REset or ID notification
        inc(ResetCount);
        EditResetCount.text:=inttostr(ResetCount);
      end;
    end;
  end;
end;
}



procedure ClearUDPBuffer(var Buf:TUDPBuffer);
begin
  zeroMemory(@(Buf.data[0]),UDPBufSize);
  Buf.MessSize:=0;
end;

procedure AddToUDPBuffer(var Buf:TUDPBuffer; it: pointer; size_it: integer);
begin
  if Buf.MessSize+size_it >= UDPBufSize then exit;
  copyMemory(@(Buf.data[Buf.MessSize]),it,size_it);
  Buf.MessSize:=Buf.MessSize+size_it;
end;

{
procedure TFMain.Timer100Timer(Sender: TObject);
var sig1,spd1,sig2,spd2: string;
begin
// General Packet format
// '<' NT [n_id1] [n_id2] '>'
// N - Node number
// T - packet type
// n_idX - n data bytes for driver X

  sig1:='00';
  if VMot1<0 then sig1:='01';
  spd1:=inttohex(VMot1 and $FF ,2);

  sig2:='00';
  if VMot2<0 then sig2:='01';
  spd2:=inttohex(VMot2 and $FF ,2);

  Comport.WriteData('<01'+sig1+spd1+sig2+spd2+sig1+spd1+sig2+spd2+'>');
  Comport.WriteData('<090000000000000000000000000000000000000000>');
end;

procedure TFMain.BEEReadClick(Sender: TObject);
var cs: string;
    addr: integer;
begin
  addr:=strtoint('$'+EditEEAddress.text);
  cs:='C5';
  Comport.WriteData('*0005'+inttohex(addr,2)+cs+';');
end;

procedure TFMain.BEEWriteClick(Sender: TObject);
var cs: string;
    addr,val: integer;
begin
  addr:=strtoint('$'+EditEEAddress.text);
  val:=strtoint('$'+EditEEvalue.text);
  cs:=inttohex(addr,2)+inttohex(val,2);
  Comport.WriteData('<04'+cs+cs+cs+cs+'>');
end;

procedure Hex2Raw(txt: string; var raw_buf: array of byte);
var i: integer;
begin
  for i:=0 to (length(txt) div 2)-1 do begin
    raw_buf[i]:=StrToInt('$'+copy(txt,i*2+1,2));
  end;
end;


procedure TFMain.ComPortRxData(Sender: TObject);
var txt: string;
    IR1,IR2: double;
    raw_buf: array[0..255] of byte;
begin
  txt:=Comport.ReadData; //ReadStr(txt,count);
  SerialInBuf:=SerialInBuf+txt;
  MemoLog.Lines.Add(txt);
  while MemoLog.Lines.Count>20 do begin
    MemoLog.Lines.Delete(0);
  end;
  while true do begin
    txt:=ExtractMessage(SerialInBuf);
    if txt='' then break;
    if isValidMessage(txt) then begin
      txt:=copy(txt,2,length(txt)-2);
      Hex2Raw(txt,raw_buf);

      case (raw_buf[0] and $F) of
        PACK_TYPE_READ_EEPROM: begin
        end;

        PACK_TYPE_READ_ENC: begin
          EditMess.Text:=txt;
          IR1:=raw_buf[5]*20;
          IR2:=raw_buf[11]*20;
          IMot1:=0.95*Imot1+0.05*IR1;
          IMot2:=0.95*Imot2+0.05*IR2;
          EditIMot.text:=format('%.1f,%.1f',[IMot1,IMot2]);
          inc(MessCount);
          EditMessCount.text:=format('%d',[MessCount]);

          if raw_buf[6]<>0 then begin
            inc(ResetCount);
            EditResetCount.text:=inttostr(ResetCount);
          end;
        end;
      end;

      if txt[2]+txt[3]='EE' then begin //EEPROM read
        EditEEAddress.text:=txt[4]+txt[5];
        EditEEValue.text:=txt[6]+txt[7];
      end;
      if txt[2]+txt[3]='1D' then begin //REset or ID notification
        inc(ResetCount);
        EditResetCount.text:=inttostr(ResetCount);
      end;
    end;
  end;
end;


procedure TFMain.BSetPulseClick(Sender: TObject);
var period,on_pulse: string;
begin
  period:=inttohex(strtointdef(EditPulsePeriod.text,400),4);
  on_pulse:=inttohex(strtointdef(EditPulseOn.text,400),4);

  Comport.WriteData('<05'+period+on_pulse+period+on_pulse+period+on_pulse+period+on_pulse+'>');
end;

}

// Formato do pacote para os motores
// AD, 4E, 01, m1.l, m1.h, m2.l, m2.h, count.l, count.h
function VoltagesToSerial(speed1, speed2, count: integer): string;
begin
  result:=#$AD+#$4E+#$01
     +char(speed1 and $FF)+char((speed1 shr 8) and $FF)
     +char(speed2 and $FF)+char((speed2 shr 8) and $FF)
     +char(count and $FF)+char((count shr 8) and $FF);
end;


// Formato do pacote para os motores omni
// General Packet format
// '<' NT [n_id1] [n_id2] '>'
// N - Node number
// T - packet type
// n_idX - n data bytes for driver X
function VoltagesToSerialOmni(u1, u2, u3 ,u4: integer; count: integer): string;
var u_char: string;
    u: array[0..3] of integer;
    i: integer;
begin

  u[0]:=u1;
  u[1]:=u2;
  u[2]:=u3;
  u[3]:=u4;

  result:='<0C300F';


  for i:=0 to 3 do begin
    result := result + '01';
    if u[i]>=0 then u_char:='00'
    else u_char:='01';
    u_char:=u_char+inttohex(u[i] and $FF ,2);
    result:=result+u_char;
  end;
  result:=result+'>';
end;



function TicksToSerialOmni(u1, u2, u3 ,u4: integer): string;
var s: string;
    u: array[0..3] of integer;
    i: integer;
begin

  u[0]:=u1;
  u[1]:=u2;
  u[2]:=u3;
  u[3]:=u4;

  s:='<0C300F';       // packet type

  for i:=0 to 3 do
  begin
    s:=s+'06';
    s:=s+inttohex(u[i] and $FF,2)+inttohex((u[i] shr 8) and $FF,2);
  end;

  s:=s+'>';
  result := s;


end;




// Formato do pacote para o Kicker
// <02 pulse1 (x 5ms), pulse2 (x 5ms), delay (x 5ms), osc_div (10-100) (x4)>
function SolKickToSerialND(KickCom: TSolKick): string;
var pulse1, pulse2, pdelay, osc_div: integer;
begin
  pulse1:=KickCom.pulse1 and $FF;
  pulse2:=KickCom.pulse2 and $FF;
  pdelay:=KickCom.delay and $FF;
  osc_div:=KickCom.osc_div and $FF;
  result:=inttohex(pulse1 and $FF ,2)+inttohex(pulse2 and $FF ,2)+
          inttohex(pdelay and $FF ,2)+inttohex(osc_div and $FF ,2);
  result:='<02'+ result+ result+ result+ result+'>';
//  result:='<02'+ result+'>';
end;


// Formato do pacote para o Kicker
// AD, 4E, 03, pulse1 (x 5ms), pulse2 (x 5ms), delay (x 5ms), osc_div (10-100)
function SolKickToSerial(KickCom: TSolKick): string;
begin
  result:=#$AD+#$4E+#$03
     +char(KickCom.pulse1 and $FF)
     +char(KickCom.pulse2 and $FF)
     +char(KickCom.delay and $FF)
     +char(KickCom.osc_div and $FF);
end;

{
// Formato do pacote para o motor passo a passo
// AD, 4E, 05, xx, xx, m.l, m.h, count.l, count.h
function NeckSpeedToSerial(ns, count: integer): string;
begin
//  if (v1>-100) and (v1<100) then v1:=0;
//  if (v2>-100) and (v2<100) then v2:=0;
  result:=#$AD+#$4E+#$05+#00+#00
     +char(ns and $FF)+char((ns shr 8) and $FF)
     +char(count and $FF)+char((count shr 8) and $FF);
end;}


end.
