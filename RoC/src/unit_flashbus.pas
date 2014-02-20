unit Unit_FlashBus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  FBcharComWrite='w'; //escrita sem confirmação
  FBcharComRead='r'; //escrita
  FBcharComBroadCastRead='R';
  FBcharComBroadCastWrite='W';
  FBcharComReadFailed='l'; //erro timeout
  FBcharComBufferOverFlow='b'; //erro buffer over flow

  NumDrivers=5;
  Motor1=1;
  Motor2=2;
  Motor3=3;
  Kicker=4;
  Sensors=5;
  CTablesAddrsOffsetsOutputs: array[1..NumDrivers] of integer=(0, 0, 0, 99, 104);
  CTablesAddrsOffsetsInputs: array[1..NumDrivers] of integer=(5, 5, 5, 100, 99);
  CTablesAddrsOffsetsParameters: array[1..NumDrivers] of integer=(20, 20, 20, 104, 0);

type

  {TFlashBusState}

  TFlashBusState=(WaitingResponse, Idle, FailResponse, OverFlowResponse);

  { TFlashBusDriverTable }

  TFlashBusDriverTable=class
    private
      FInputs: array of PByte;
      FInputsSize: integer;
      FOutputs: array of PByte;
      FOutputsSize: integer;
      FParameters: array of PByte;
      FParametersSize: integer;
      function GetInputs(i: byte): PByte;
      function GetOutputs(i: byte): PByte;
      function GetParameters(i: byte): PByte;
      procedure FlashBusDriverTableException(str: string);
    public
      procedure AddInputPointer(Pointer: PByte);
      property InputsSize: integer read FInputsSize;
      procedure AddOutputsPointer(Pointer: PByte);
      property OutputsSize: integer read FOutputsSize;
      procedure AddParametersPointer(Pointer: PByte);
      property ParametersSize: integer read FParametersSize;
      property Parameters [i: byte]: PByte read GetParameters;
      property Inputs [i: byte]: PByte read GetInputs;
      property Outputs [i: byte]: PByte read GetOutputs;
  end;

  { TFlashBus }

  TFlashBus=class
    private
      FParametersState: array [1..NumDrivers] of TFlashBusState;
      FParametersChanged: array[1..NumDrivers] of boolean;
      FDriverEnable: array[1..NumDrivers] of boolean;
      FInputsState: array [1..NumDrivers] of TFlashBusState;
      FInputsChanged: array[1..NumDrivers] of boolean;
      procedure FlashBusException(str: string);
      function GetDriverEnable(i: byte): boolean;
      function GetInputsChanged(i: byte): boolean;
      function GetInputsState(i: byte): TFlashBusState;
      function GetParametersChanged(i: byte): boolean;
      function GetParametersState(i: byte): TFlashBusState;
      procedure SetDriverEnable(i: byte; const AValue: boolean);
      procedure SetInputsChanged(i: byte; const AValue: boolean);
      procedure SetParametersChanged(i: byte; const AValue: boolean);
    public
      Table: array[1..NumDrivers] of TFlashBusDriverTable;
      constructor Create;
      destructor Destroy; override;
      function RequestParameters(DriverNumber: integer): string;
      function RequestInputs: string;
      function WriteParameters(DriverNumber: integer): string;
      function WriteOutputs: string;
      property ParametersState [i: byte]: TFlashBusState read GetParametersState;
      property ParametersChanged [i: byte]: boolean read GetParametersChanged write SetParametersChanged;
      property DriverEnable[i: byte]: boolean read GetDriverEnable write SetDriverEnable;
      property InputsState [i: byte]: TFlashBusState read GetInputsState;
      property InputsChanged [i: byte]: boolean read GetInputsChanged write SetInputsChanged;
      procedure ReadMessage(data: string);
  end;

implementation

{ TFlashBusDriverTable }

function TFlashBusDriverTable.GetParameters(i: byte): PByte;
begin
  if i<FParametersSize then begin
    Result:=FParameters[i];
  end
  else begin
    FlashBusDriverTableException('Addr out of range');
  end;
end;

function TFlashBusDriverTable.GetInputs(i: byte): PByte;
begin
  if i<FInputsSize then begin
    Result:=FInputs[i];
  end
  else begin
    FlashBusDriverTableException('Addr out of range');
  end;
end;

function TFlashBusDriverTable.GetOutputs(i: byte): PByte;
begin
  if i<FOutputsSize then begin
    Result:=FOutputs[i];
  end
  else begin
    FlashBusDriverTableException('Addr out of range');
  end;
end;

procedure TFlashBusDriverTable.FlashBusDriverTableException(str: string);
begin
  raise Exception.Create('FlashBus table error: '+str);
end;

procedure TFlashBusDriverTable.AddInputPointer(Pointer: PByte);
begin
  SetLength(FInputs, Length(FInputs)+1);
  FInputs[Length(FInputs)-1]:=Pointer;
  FInputsSize:=Length(FInputs);
end;

procedure TFlashBusDriverTable.AddOutputsPointer(Pointer: PByte);
begin
  SetLength(FOutputs, Length(FOutputs)+1);
  FOutputs[Length(FOutputs)-1]:=Pointer;
  FOutputsSize:=Length(FOutputs);
end;

procedure TFlashBusDriverTable.AddParametersPointer(Pointer: PByte);
begin
  SetLength(FParameters, Length(FParameters)+1);
  FParameters[Length(FParameters)-1]:=Pointer;
  FParametersSize:=Length(FParameters);
end;

{ TFlashBus }

procedure TFlashBus.FlashBusException(str: string);
begin
  raise Exception.Create('FlashBus error: '+str);
end;

function TFlashBus.GetDriverEnable(i: byte): boolean;
begin
  if i<=NumDrivers then begin
    Result:=FDriverEnable[i];
  end
  else begin
    FlashBusException('Invalid Driver Number');
  end;
end;

function TFlashBus.GetInputsChanged(i: byte): boolean;
begin
  if i<=NumDrivers then begin
    Result:=FInputsChanged[i];
  end
  else begin
    FlashBusException('Invalid Driver Number');
  end;
end;

function TFlashBus.GetInputsState(i: byte): TFlashBusState;
begin
  if i<=NumDrivers then begin
    Result:=FInputsState[i];
  end
  else begin
    FlashBusException('Invalid Driver Number');
  end;
end;

function TFlashBus.GetParametersChanged(i: byte): boolean;
begin
  if i<=NumDrivers then begin
    Result:=FParametersChanged[i];
  end
  else begin
    FlashBusException('Invalid Driver Number');
  end;
end;

function TFlashBus.GetParametersState(i: byte): TFlashBusState;
begin
  if i<=NumDrivers then begin
    Result:=FParametersState[i];
  end
  else begin
    FlashBusException('Invalid Driver Number');
  end;
end;

procedure TFlashBus.SetDriverEnable(i: byte; const AValue: boolean);
begin
  if i<=NumDrivers then begin
    FDriverEnable[i]:=AValue;
  end
  else begin
    FlashBusException('Invalid Driver Number');
  end;
end;

procedure TFlashBus.SetInputsChanged(i: byte; const AValue: boolean);
begin
  if i<=NumDrivers then begin
    FInputsChanged[i]:=AValue;
  end
  else begin
    FlashBusException('Invalid Driver Number');
  end;
end;

procedure TFlashBus.SetParametersChanged(i: byte; const AValue: boolean);
begin
  if i<=NumDrivers then begin
    FParametersChanged[i]:=AValue;
  end
  else begin
    FlashBusException('Invalid Driver Number');
  end;
end;

constructor TFlashBus.Create;
var
  i: integer;
begin
  for i:=1 to NumDrivers do begin
    Table[i]:=TFlashBusDriverTable.Create;
  end;
end;

destructor TFlashBus.Destroy;
var
  i: integer;
begin
  for i:=1 to NumDrivers do begin
    Table[i].Free;
  end;
  inherited Destroy;
end;

function TFlashBus.RequestParameters(DriverNumber: integer): string;
begin
  if (DriverNumber<=0) and (DriverNumber>NumDrivers) then begin
    FlashBusException('Invalid driver Number');
  end;
  Result:=chr(DriverNumber)+FBcharComRead+
          chr(CTablesAddrsOffsetsParameters[DriverNumber])+
          chr(Table[DriverNumber].ParametersSize);
end;

function TFlashBus.RequestInputs: string;
var
  data2send: string;
  i: integer;
begin
  data2send:='';
  for i:=1 to NumDrivers do begin
    if FDriverEnable[i] then begin
      FInputsState[i]:=WaitingResponse;
      data2send:=data2send+chr(i)+chr(CTablesAddrsOffsetsInputs[i])+chr(Table[i].InputsSize);
    end
    else begin
       FInputsState[i]:=Idle;
    end;
  end;
  if Length(data2send)>0 then begin
    Result:=chr(0)+FBcharComBroadCastRead+data2send;
  end;
end;

function TFlashBus.WriteParameters(DriverNumber: integer): string;
var
  i: integer;
  auxchar: char;
begin
  if (DriverNumber<=0) and (DriverNumber>NumDrivers) then begin
    FlashBusException('Invalid driver Number');
  end;
  Result:=chr(DriverNumber)+FBcharComWrite+
          chr(CTablesAddrsOffsetsParameters[DriverNumber]);
  for i:=0 to Table[DriverNumber].ParametersSize-1 do begin
    auxchar:=chr(Table[DriverNumber].Parameters[i]^);
    Result:=Result+auxchar;
  end;
end;

function TFlashBus.WriteOutputs: string;
var
  data2send: string;
  i,j: integer;
begin
  data2send:='';
  for i:=1 to NumDrivers do begin
    if FDriverEnable[i] then begin
      data2send:=data2send+chr(i)+chr(CTablesAddrsOffsetsOutputs[i])
                          +chr(Table[i].OutputsSize);
      for j:=0 to Table[i].OutputsSize-1 do begin
        data2send:=data2send+chr(Table[i].Outputs[j]^);
      end;
    end;
  end;
  if Length(data2send)>0 then begin
    Result:=chr(0)+FBcharComBroadCastWrite+data2send;
  end;
end;

procedure TFlashBus.ReadMessage(data: string);
var
  DriverNumber: integer;
  Command: char;
  addr: integer;
  dataIn: string;
  i: integer;
begin
  if Length(data)<4 then begin
    FlashBusException('Message too short!');
  end;
  DriverNumber:=ord(data[1]);
  Command:=data[2];
  addr:=ord(data[3]);
  dataIn:=Copy(data, 4, Length(data)-3);

  if DriverNumber>NumDrivers then begin
    FlashBusException('Invalid driver Number');
  end;

  if DriverNumber=0 then begin
    case Command of
      FBcharComBufferOverFlow: begin
        for i:=1 to NumDrivers do begin
          FInputsChanged[i]:=true;
          FInputsState[i]:=OverFlowResponse;
        end;
      end;
      else begin
        FlashBusException('Invalid command');
      end;
    end;
  end
  else begin
    if addr=CTablesAddrsOffsetsInputs[DriverNumber] then begin //read Inputs
      FInputsChanged[DriverNumber]:=true;
      case Command of
          FBcharComBroadCastRead: begin
            FInputsState[DriverNumber]:=Idle;
            if Length(dataIn)=Table[DriverNumber].FInputsSize then begin
              for i:=0 to Table[DriverNumber].FInputsSize-1 do begin
                Table[DriverNumber].Inputs[i]^:=ord(dataIn[i+1])
              end;
            end
            else begin
              FlashBusException('Invalid Data size');
            end;
          end;
          FBcharComReadFailed: begin
            FInputsState[DriverNumber]:=FailResponse;
          end;
          FBcharComBufferOverFlow: begin //isto nunca acontece!
            FInputsState[DriverNumber]:=OverFlowResponse;
          end;
          else begin
            FlashBusException('Invalid command');
          end;
        end;
    end
    else begin
      if addr=CTablesAddrsOffsetsParameters[DriverNumber] then begin //read Parameters
        FParametersChanged[DriverNumber]:=true;
        case Command of
          FBcharComRead: begin
            FParametersState[DriverNumber]:=Idle;
            if Length(dataIn)=Table[DriverNumber].FParametersSize then begin
              for i:=0 to Table[DriverNumber].FParametersSize-1 do begin
                Table[DriverNumber].Parameters[i]^:=ord(dataIn[i+1])
              end;
            end
            else begin
              FlashBusException('Invalid Data size');
            end;
          end;
          FBcharComReadFailed: begin
            FParametersState[DriverNumber]:=FailResponse;
          end;
          FBcharComBufferOverFlow: begin
            FParametersState[DriverNumber]:=OverFlowResponse;
          end;
          else begin
            FlashBusException('Invalid command');
          end;
        end;
      end
      else begin
        FlashBusException('Invalid address');
      end;
    end;
  end;
end;

end.

