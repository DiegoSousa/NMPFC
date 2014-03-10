unit unit_Log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogBuffer=Array of array of double;

type

  { TLogText }

  TLogText=class
    private
      FIterator: Longword;
      FLogText: TStringList;
      FReadyToRecord: boolean;
      FReadyToPlay: boolean;
      FLogBuffer: TLogBuffer;
      FFileName: string;
      FNLines: Longword;
      FNColumns: Longword;
      FHeadString: string;
      procedure AlocateBufferSpace;
      procedure LogBufferToFile;
      function LogBufferFromFile: boolean;
    public
      constructor Create;
      destructor Destroy; override;
      property HeadString: string read FHeadString write FHeadString;
      property NLines: Longword read FNLines;
      property NColumns: Longword read FNColumns;
      function InicializeRecord(FileName: string; Lines, Columns: Longword): boolean;
      function InicializePlay(FileName: string): boolean;
      function AddNewLine(NewLine: array of double): boolean;
      function GetNewLine(var NewLine: array of double; Iterator: longword): boolean;
      procedure SaveToFile;
      procedure ResetAll;
  end;


implementation

uses
  Dialogs;

{ TLogText }

procedure TLogText.AlocateBufferSpace;
begin
  SetLength(FLogBuffer, FNLines, FNColumns);
end;

procedure TLogText.LogBufferToFile;
var
  i, j: Longword;
  NewValue: double;
  NewLine: string;
begin
  FLogText.Clear;
  FLogText.Append(FHeadString);
  for i:=0 to FNLines-1 do begin
    NewValue:=FLogBuffer[i,0];
    NewLine:=FloatToStr(NewValue);
    for j:=1 to FNColumns-1 do begin
      NewValue:=FLogBuffer[i,j];
      NewLine:=NewLine+','+FloatToStr(NewValue);
    end;
    FLogText.Append(NewLine);
  end;
  FLogText.SaveToFile(FFileName);
end;

function TLogText.LogBufferFromFile: boolean;
var
  i, j: longword;
  NewLineString: string;
  NewLineList: TStringList;
begin
  FLogText.LoadFromFile(FFileName);
  if FLogText.Count<2 then begin
    Result:=false;
    raise Exception.Create('Invalide size Log File!');
  end
  else begin
    FHeadString:=FLogText[0];
    FNLines:=FLogText.Count-1;
    NewLineList:=TStringList.Create;
    NewLineString:=FLogText[1];
    NewLineList.Delimiter:=',';
    NewLineList.DelimitedText:=NewLineString;
    if NewLineList.Count<=0 then begin
      Result:=false;
      raise Exception.Create('Invalide size Log File!');
    end
    else begin
      FNColumns:=NewLineList.Count;
      AlocateBufferSpace;
      for i:=0 to FNLines-1 do begin
        NewLineList.Clear;
        NewLineString:=FLogText[i+1];
        NewLineList.Delimiter:=',';
        NewLineList.DelimitedText:=NewLineString;
        if longword(NewLineList.Count)<>FNColumns then begin
          Result:=false;
          raise Exception.Create('Invalide size Log File!');
        end
        else begin
          for j:=0 to FNColumns-1 do begin
            if AnsiCompareText(NewLineList[j], 'nan')=0 then begin
              FLogBuffer[i,j]:=0/0;
            end
            else begin
              FLogBuffer[i,j]:=StrToFloat(NewLineList[j]);
            end;
          end;
        end;
      end;
      Result:=true;
    end;
  end;
end;

constructor TLogText.Create;
begin
  FLogText:=TStringList.Create;
  ResetAll;
end;

destructor TLogText.Destroy;
begin
  ResetAll;
  FLogText.Free;
  inherited Destroy;
end;

function TLogText.InicializeRecord(FileName: string; Lines, Columns: Longword): boolean;
begin
  ResetAll;
  try
    if (FileName='') or (Lines<=0) or (Columns<=0) then begin
      ResetAll;
      Result:=false;
      ShowMessage('Invalide Log Parameters');
    end
    else begin
      FFileName:=FileName;
      FNLines:=Lines;
      FNColumns:=Columns;
      FIterator:=0;
      AlocateBufferSpace;
      LogBufferToFile;
      FReadyToRecord:=true;
      FReadyToPlay:=false;
      Result:=true;
    end;
  except
    on E: Exception do begin
      ResetAll;
      Result:=false;
      ShowMessage(E.Message);
    end;
  end;
end;

function TLogText.InicializePlay(FileName: string): boolean;
begin
  ResetAll;
  try
    FFileName:=FileName;
    LogBufferFromFile;
    FIterator:=0;
    FReadyToPlay:=true;
    FReadyToRecord:=false;
    Result:=true;
  except
    on E: Exception do begin
      ResetAll;
      Result:=false;
      ShowMessage(E.Message);
    end;
  end;
end;

function TLogText.AddNewLine(NewLine: array of double): boolean;
var
  i: longword;
begin
  if FReadyToRecord then begin
     if longword(Length(NewLine))=FNColumns then begin //ver o tipo de retorno da funcao lenght
       if FIterator<FNLines then begin
         for i:=0 to FNColumns-1 do begin
           FLogBuffer[FIterator, i]:=NewLine[i];
         end;
         inc(FIterator);
         Result:=true;
       end
       else begin
         Result:=false;
         LogBufferToFile;
         ResetAll;
       end;
     end
     else begin
       Result:=false;
       ShowMessage('Invalide new data!');
     end;
  end
  else begin
    Result:=false;
    ShowMessage('Log not ready!');
  end;
end;

function TLogText.GetNewLine(var NewLine: array of double; Iterator: longword): boolean;
var
  i: integer;
begin
  if (FReadyToPlay) and (Iterator>=0) and (Iterator<FNLines) and (Length(NewLine)=FNColumns) then begin
    for i:=0 to FNColumns-1 do begin
      NewLine[i]:=FLogBuffer[Iterator, i];
    end;
    Result:=true;
  end
  else begin
    Result:=false;
  end;
end;

procedure TLogText.SaveToFile;
begin
  if FReadyToRecord then begin
    if FIterator>0 then begin
      FNLines:=FIterator;
      LogBufferToFile;
      ResetAll;
    end;
  end;
end;

procedure TLogText.ResetAll;
begin
  FReadyToRecord:=false;
  FReadyToPlay:=false;
  FIterator:=0;
  SetLength(FLogBuffer,0,0);
  FLogText.Clear;
  FFileName:='';
  FNLines:=0;
  FNColumns:=0;
  FHeadString:='';
end;

end.

