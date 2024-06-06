{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MarkerDrawableCenterScale;

interface

uses
  i_BitmapMarker,
  i_Bitmap32BufferFactory,
  u_MarkerDrawableByBitmapMarker;

type
  TMarkerDrawableCenterScale = class(TMarkerDrawableByBitmapMarker)
  private
    function CreateBitmapMarker(
      const ABitmapFactory: IBitmap32StaticFactory
    ): IBitmapMarker;
  public
    constructor Create(const ABitmapFactory: IBitmap32StaticFactory);
  end;

implementation

uses
  Types,
  SysUtils,
  GR32,
  t_GeoTypes,
  i_Bitmap32Static,
  u_BitmapMarker,
  u_GeoFunc;

{ TMarkerDrawableCenterScale }

constructor TMarkerDrawableCenterScale.Create(
  const ABitmapFactory: IBitmap32StaticFactory
);
begin
  inherited Create(CreateBitmapMarker(ABitmapFactory));
end;

function TMarkerDrawableCenterScale.CreateBitmapMarker(
  const ABitmapFactory: IBitmap32StaticFactory
): IBitmapMarker;
const
  CDegreeSymbol = #176;
var
  I: Integer;
  R: Double;
  X, Y, X1, Y1: Integer;
  VBitmap: TBitmap32;
  VHalfSize: TPoint;
  VSize: TPoint;
  VText: string;
  VTextWdth: Integer;
  VRadius: Integer;
  VFontSize: Integer;
  VDigitsOffset: Integer;
  VBitmapStatic: IBitmap32Static;
begin
  Result := nil;
  VBitmap := TBitmap32.Create;
  try
    VRadius := 115;
    VDigitsOffset := 20;
    VFontSize := 12;

    VBitmap.Font.Name := 'Arial';
    VBitmap.Font.Size := VFontSize;

    VTextWdth := VBitmap.TextWidth('270' + CDegreeSymbol);

    VSize.X := (VRadius * 2) + (VDigitsOffset * 2) + (VTextWdth * 2);
    VSize.Y := VSize.X;

    VHalfSize := Types.Point(VSize.X div 2, VSize.Y div 2);

    VBitmap.SetSize(VSize.X, VSize.Y);
    VBitmap.Clear(0);
    VBitmap.Font.Size := VFontSize - 3;

    I := 0;
    while I < 360 do begin
      VBitmap.Font.Size := VFontSize - 3;
      if (I mod 90) = 0 then begin
        R := 0;
        VBitmap.Font.Size := VFontSize;
      end else if (I mod 45) = 0 then begin
        R := VRadius - 40;
        VBitmap.Font.Size := VFontSize - 1;
      end else begin
        R := VRadius - 10;
      end;

      X := Round(VHalfSize.X + VRadius * Cos(I * (Pi / 180)));
      Y := Round(VHalfSize.Y + VRadius * Sin(I * (Pi / 180)));
      X1 := Round(VHalfSize.X + R * Cos(I * (Pi / 180)));
      Y1 := Round(VHalfSize.Y + R * Sin(I * (Pi / 180)));

      VBitmap.LineFS(X, Y, X1, Y1, SetAlpha(clRed32, 180));

      if (I mod 15) = 0 then begin
        VText := IntToStr((I + 90) mod 360) + CDegreeSymbol;
        X1 := Round(VHalfSize.X + (VRadius + VDigitsOffset) * Cos(I * (Pi / 180))) - VBitmap.TextWidth(VText) div 2;
        Y1 := Round(VHalfSize.X + (VRadius + VDigitsOffset) * Sin(I * (Pi / 180))) - 2 - VBitmap.Font.Size div 2;
        VBitmap.RenderText(X1 + 1, Y1 + 1, VText, SetAlpha(clWhite32, 150), True);
        VBitmap.RenderText(X1, Y1, VText, SetAlpha(clBlue32, 210), True);
      end;
      Inc(I, 5);
    end;

    VBitmapStatic := ABitmapFactory.Build(VSize, VBitmap.Bits);
    Result := TBitmapMarker.Create(VBitmapStatic, DoublePoint(VHalfSize));
  finally
    VBitmap.Free;
  end;
end;

end.
