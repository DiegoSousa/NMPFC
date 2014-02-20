unit astarmapimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, obsavoid, StdCtrls, IntfGraphics, LCLType, FPimage, Spin;

type

  { TFAstarMap }

  TFAstarMap = class(TForm)
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Image1: TImage;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FAstarMap: TFAstarMap;

  procedure DrawAstarMap(var map: TAStarMap; SizeW, SizeH:integer;Image:TImage);

implementation



procedure DrawAstarMap(var map: TAStarMap; SizeW, SizeH:integer;Image:TImage);
{var
  SrcIntfImg: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  SrcBitmap: TBitmap;
  vi, ui, gray: integer;
  yi,xi:integer;
  fpcol: TFPcolor;
  min_val, max_val:double;
  imfactor: double;
  TempBitmap: TBitmap;
begin
  //TempBitmap:=TBitmap.Create;
  SrcBitmap:=TBitmap.Create;
  SrcIntfImg:=TLazIntfImage.Create(0,0);

  {if SizeW/Image.Width > SizeH/Image.Height   then
     imfactor:=SizeW/Image.Width
  else
    imfactor:=SizeH/Image.Height;

  if (SizeW<Image.Width) and (SizeH<Image.Height) then
    imfactor:=1;  }

  SrcBitmap.Width:=Image.Width;
  SrcBitmap.Height:=Image.Height;

  SrcIntfImg.LoadFromBitmap(SrcBitmap.Handle, SrcBitmap.MaskHandle);
  {max_val:=0;
  min_val:=100;
  for yi := 0 to SizeH-1 do begin
    for xi := 0 to SizeW-1 do begin
      if ((TheArray[yi,xi]>max_val)and (TheArray[yi,xi]<65535)) then
        max_val:=TheArray[yi,xi];
      if (TheArray[yi,xi]<min_val) then
        min_val:=TheArray[yi,xi];
    end;
  end;  }
  //min_val:=min_val+0.1;

  SrcIntfImg.FillPixels(colBlue);
  fpcol.alpha:=0;
  {for vi :=0 to Image.Height-1 do begin
    for ui :=0 to Image.Width-1 do begin
      //if ((vi*imfactor<SizeH-1) and (ui*imfactor<SizeW-1)) then  begin
        //gray := round((TheArray[round(SizeH-1-imfactor*vi),round(imfactor*ui)]+abs(min_val))*65536/(max_val-min_val));
      gray:=255*255;
      case map.GridState[vi,ui] of
        0: begin
          fpcol.red := gray;
          fpcol.green := gray;
          fpcol.blue := gray;
        end;
        1: begin
          fpcol.red := 0;
          fpcol.green := 0;
          fpcol.blue := 0;
        end;
        2: begin
          fpcol.red := gray;
          fpcol.green := 0;
          fpcol.blue := 0;
        end;
        3: begin
          fpcol.red :=0;
          fpcol.green := gray;
          fpcol.blue := 0;
        end;
      end;
        SrcIntfImg.Colors[round(ui),round(vi)] := fpcol;
      end;
    end;  }

  SrcIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,true);
  TempBitmap.Handle:=ImgHandle;
  TempBitmap.MaskHandle:=ImgMaskHandle;
  //Image.Canvas.Clear;
  Image.Canvas.Draw(0,0,TempBitmap);
  //Image.Visible:=false;
  //FLocMap.Canvas.Clear;
  //FLocMap.Canvas.Draw(0,0,TempBitmap);
  SrcIntfImg.Free;
  TempBitmap.Free;
  SrcBitmap.Free;  }
var
  SrcIntfImg, TempIntfImg: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  FadeStep: Integer;
  px, py, gray: Integer;
  fpcol: TFPColor;
  TempBitmap, ABitmap: TBitmap;
begin
  SizeW:=SizeW;
  SizeH:=SizeH;
  Image.Width:=SizeW;
  Image.Height:=SizeH;
  Image.Canvas.Clear;
  ABitmap:=TBitmap.Create;
  ABitmap.Width:=SizeW;
  ABitmap.Height:=SizeH;
  SrcIntfImg:=TLazIntfImage.Create(0,0);
  SrcIntfImg.Width:=Image.Width;
  SrcIntfImg.Height:=Image.Height;
  SrcIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
  TempIntfImg:=TLazIntfImage.Create(0,0);
  TempIntfImg.Width:=Image.Width;
  TempIntfImg.Height:=Image.Height;
  TempIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
  TempBitmap:=TBitmap.Create;
  TempBitmap.Width:=SizeW;
  TempBitmap.Height:=SizeH;
  if FAstarMap.SpinEdit1.Value = FAstarMap.SpinEdit1.MaxValue then
    FAstarMap.SpinEdit1.Value := 0;

  FAstarMap.SpinEdit1.Value := FAstarMap.SpinEdit1.Value + 1;


  for py:=0 to round(SizeH)-1 do begin
    for px:=0 to round(SizeW)-1 do begin
      gray:=255*255;
      case map.GridState[round(px),round(py)] of
        0: begin
          fpcol.red := gray;
          fpcol.green := gray;
          fpcol.blue := gray;
        end;
        1: begin
          fpcol.red := 0;
          fpcol.green := 0;
          fpcol.blue := 0;
        end;
        2: begin
          fpcol.red := gray;
          fpcol.green := 0;
          fpcol.blue := 0;
        end;
        3: begin
          fpcol.red :=0;
          fpcol.green := gray;
          fpcol.blue := 0;
        end;
         4: begin
          fpcol.red :=0;
          fpcol.green := 0 ;
          fpcol.blue := gray;
        end;
        else begin
          fpcol.red :=0;
          fpcol.green := 0;
          fpcol.blue := gray;
        end;

      end;
      TempIntfImg.Colors[px,SizeH-1-py]:=fpcol;
    end;
  end;
 // TempIntfImg.FillPixels(colBlue);

  TempIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
  TempBitmap.Handle:=ImgHandle;
  TempBitmap.MaskHandle:=ImgMaskHandle;
  Image.Canvas.Draw(0,0,TempBitmap);
  SrcIntfImg.Free;
  TempIntfImg.Free;
  TempBitmap.Free;


end;

{ TFAstarMap }


procedure TFAstarMap.Timer1Timer(Sender: TObject);
begin
  if CheckBox1.Checked then begin
   // DrawAstarMap(AStarMap,AStarGridXSize,AStarGridYSize, Image1);
  end;

end;

initialization
  {$I astarmapimage.lrs}

end.

