{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BitmapPostProcessingConfigStatic;

interface

uses
  GR32,
  i_BitmapPostProcessingConfig;

type
  TBitmapPostProcessingConfigStatic = class(TInterfacedObject, IBitmapPostProcessingConfigStatic)
  private
    FInvertColor: boolean;
    FGammaN: Integer;
    FContrastN: Integer;
  protected
    function GetInvertColor: boolean;
    function GetGammaN: Integer;
    function GetContrastN: Integer;

    procedure ProcessBitmap(Bitmap: TCustomBitmap32);
  public
    constructor Create(
      AInvertColor: boolean;
      AGammaN: Integer;
      AContrastN: Integer
    );
  end;

implementation

uses
  GR32_Filters;

{ TBitmapPostProcessingConfigStatic }

constructor TBitmapPostProcessingConfigStatic.Create(AInvertColor: boolean;
  AGammaN, AContrastN: Integer);
begin
  FInvertColor := AInvertColor;
  FContrastN := AContrastN;
  FGammaN := AGammaN;
end;

function TBitmapPostProcessingConfigStatic.GetContrastN: Integer;
begin
  Result := FContrastN;
end;

function TBitmapPostProcessingConfigStatic.GetGammaN: Integer;
begin
  Result := FGammaN;
end;

function TBitmapPostProcessingConfigStatic.GetInvertColor: boolean;
begin
  Result := FInvertColor;
end;

procedure Contrast(Bitmap: TCustomBitmap32; Value: double);
 function BLimit(B:Integer):Byte;
  begin
   if B<0 then Result:=0
          else if B>255 then Result:=255
                        else Result:=B;
  end;
var Dest: PColor32;
    y,mr,i:Integer;
    ContrastTable:array [0..255] of byte;
    vd: Double;
begin
  if Value=0 then Exit;
  Value:=Value/10;
  mR:=128;
  if Value>0 then vd:=1+(Value/10)
             else vd:=1-(Sqrt(-Value)/10);
  for i:=0 to 255 do begin
    ContrastTable[i]:=BLimit(mR+Trunc((i-mR)*vd));
  end;

  Dest:=@Bitmap.Bits[0];
  for y:=0 to Bitmap.Width*Bitmap.Height-1 do
   begin
      Dest^:=GR32.Color32(ContrastTable[RedComponent(dest^)],
                          ContrastTable[GreenComponent(dest^)],
                          ContrastTable[BlueComponent(dest^)],AlphaComponent(dest^));
      Inc(Dest);
   end;
end;

procedure TBitmapPostProcessingConfigStatic.ProcessBitmap(
  Bitmap: TCustomBitmap32);
  function Power(Base, Exponent: Extended): Extended;
  begin
    Result := Exp(Exponent * Ln(Base));
  end;
var Dest: PColor32;
    X,Y: integer;
    GammaTable:array[0..255] of byte;
    L:Double;
begin
  Contrast(Bitmap, FContrastN);
  if FInvertColor then InvertRGB(Bitmap,Bitmap);;
  if FGammaN<>50 then
   begin
    if FGammaN<50 then L:=1/((FGammaN*2)/100)
                 else L:=1/((FGammaN-40)/10);
    GammaTable[0]:=0;
    for X := 1 to 255 do GammaTable[X]:=round(255*Power(X/255,L));
    Dest:=@Bitmap.Bits[0];
    for Y := 0 to Bitmap.Height*Bitmap.Width-1 do
     begin
      Dest^:= GR32.Color32(GammaTable[RedComponent(dest^)],GammaTable[GreenComponent(dest^)],GammaTable[BlueComponent(dest^)],AlphaComponent(dest^));
      Inc(Dest);
     end;
   end;
end;

end.
