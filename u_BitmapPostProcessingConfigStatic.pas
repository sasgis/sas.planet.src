{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BitmapPostProcessingConfigStatic;

interface

uses
  GR32,
  i_Bitmap32Static,
  i_BitmapPostProcessingConfig;

type
  TBitmapPostProcessingConfigStatic = class(TInterfacedObject, IBitmapPostProcessing)
  private
    FInvertColor: boolean;
    FGammaN: Integer;
    FContrastN: Integer;
    procedure ProcessBitmap(const Bitmap: TCustomBitmap32);
  private
    function Process(const ABitmap: IBitmap32Static): IBitmap32Static;
  public
    constructor Create(
      AInvertColor: boolean;
      AGammaN: Integer;
      AContrastN: Integer
    );
  end;

implementation

uses
  GR32_Filters,
  u_Bitmap32Static;

{ TBitmapPostProcessingConfigStatic }

constructor TBitmapPostProcessingConfigStatic.Create(
  AInvertColor: boolean;
  AGammaN, AContrastN: Integer
);
begin
  inherited Create;
  FInvertColor := AInvertColor;
  FContrastN := AContrastN;
  FGammaN := AGammaN;
end;

procedure Contrast(
  const Bitmap: TCustomBitmap32;
  Value: double
);
  function BLimit(B: Integer): Byte;
  begin
    if B < 0 then begin
      Result := 0;
    end else if B > 255 then begin
      Result := 255;
    end else begin
      Result := B;
    end;
  end;

var
  VDest: PColor32;
  y, mr, i: Integer;
  VContrastTable: array [0..255] of byte;
  vd: Double;
begin
  if Value = 0 then begin
    Exit;
  end;
  mr := 128;
  if Value > 0 then begin
    vd := 1 + (Value / 100);
  end else begin
    vd := 1 - (Sqrt(-Value / 1000));
  end;
  for i := 0 to 255 do begin
    VContrastTable[i] := BLimit(mr + Trunc((i - mr) * vd));
  end;

  VDest := @Bitmap.Bits[0];
  for y := 0 to Bitmap.Width * Bitmap.Height - 1 do begin
    VDest^ :=
      GR32.Color32(
        VContrastTable[RedComponent(VDest^)],
        VContrastTable[GreenComponent(VDest^)],
        VContrastTable[BlueComponent(VDest^)],
        AlphaComponent(VDest^)
      );
    Inc(VDest);
  end;
end;

function TBitmapPostProcessingConfigStatic.Process(
  const ABitmap: IBitmap32Static
): IBitmap32Static;
var
  VBitmap: TCustomBitmap32;
begin
  if ABitmap = nil then begin
    Result := nil;
  end else if (FContrastN = 0) and (not FInvertColor) and (FGammaN = 50) then begin
    Result := ABitmap;
  end else begin
    VBitmap := TCustomBitmap32.Create;
    try
      VBitmap.Assign(ABitmap.Bitmap);
      ProcessBitmap(VBitmap);
      Result := TBitmap32Static.CreateWithOwn(VBitmap);
      VBitmap := nil;
    finally
      VBitmap.Free;
    end;
  end;
end;

procedure TBitmapPostProcessingConfigStatic.ProcessBitmap(
  const Bitmap: TCustomBitmap32);
  function Power(Base, Exponent: Extended): Extended;
  begin
    Result := Exp(Exponent * Ln(Base));
  end;

var
  VDest: PColor32;
  X, Y: integer;
  VGammaTable: array[0..255] of byte;
  L: Double;
begin
  Contrast(Bitmap, FContrastN);
  if FInvertColor then begin
    InvertRGB(Bitmap, Bitmap);
  end;

  if FGammaN <> 50 then begin
    if FGammaN < 50 then begin
      L := 1 / ((FGammaN * 2) / 100);
    end else begin
      L := 1 / ((FGammaN - 40) / 10);
    end;
    VGammaTable[0] := 0;
    for X := 1 to 255 do begin
      VGammaTable[X] := round(255 * Power(X / 255, L));
    end;
    VDest := @Bitmap.Bits[0];
    for Y := 0 to Bitmap.Height * Bitmap.Width - 1 do begin
      VDest^ :=
        GR32.Color32(
          VGammaTable[RedComponent(VDest^)],
          VGammaTable[GreenComponent(VDest^)],
          VGammaTable[BlueComponent(VDest^)],
          AlphaComponent(VDest^)
        );
      Inc(VDest);
    end;
  end;
end;

end.
