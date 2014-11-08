{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_Bitmap32To8ConverterByFreeImage;

interface

uses
  Windows,
  i_Bitmap8Static,
  i_Bitmap32Static,
  i_Bitmap32To8Converter,
  u_BaseInterfacedObject;

type
  TBitmap32To8ConverterByFreeImage = class(TBaseInterfacedObject, IBitmap32To8Converter)
  private
    function Convert(const ABitmap32: IBitmap32Static): IBitmap8Static;
  end;

implementation

uses
  SysUtils,
  FreeImage,
  FreeBitmap,
  t_Bitmap32,
  u_Bitmap8Static;

type
  EBitmap32To8ConverterByFreeImage = class(Exception);

{ TBitmap32To8ConverterByFreeImage }

function TBitmap32To8ConverterByFreeImage.Convert(
  const ABitmap32: IBitmap32Static
): IBitmap8Static;
var
  I: Integer;
  VRowSize: Integer;
  VFreeBitmap: TFreeWinBitmap;
  VPalette: PRGBQUAD;
  VPaletteSize: Integer;
  VData: PColor32Array;
  VIndexedColor: PByte;
  VPtr: PByte;
  VWidth, VHeight: Integer;
begin
  Assert(ABitmap32 <> nil);

  Result := nil;

  with ABitmap32.Size do begin
    VWidth := X;
    VHeight := Y;
  end;

  // create empty bitmap
  VFreeBitmap := TFreeWinBitmap.Create(FIT_BITMAP, VWidth, VHeight, 32);
  try
    VRowSize := VFreeBitmap.GetScanWidth;
    Assert(VRowSize = VWidth * 4);
    // initialize bitmap with image data
    VData := ABitmap32.Data;
    for I := 0 to VHeight - 1 do begin
      Move(
        VData[I * VWidth],
        VFreeBitmap.GetScanLine(VHeight - 1 - I)^,
        VRowSize
      );
    end;

    // convert
    if VFreeBitmap.ConvertTo24Bits then begin
      if VFreeBitmap.ColorQuantize(FIQ_WUQUANT) then begin
        // Restore transparent background
        VPalette := VFreeBitmap.GetPalette;
        VPaletteSize := VFreeBitmap.GetPaletteSize div SizeOf(PRGBQUAD);
        if (VPalette <> nil) and (VPaletteSize <= 256) then begin
          for I := 0 to VPaletteSize - 1 do begin
            if (VPalette.rgbGreen >= $FE) and
               (VPalette.rgbBlue = $00) and
               (VPalette.rgbRed = $00)
            then begin
              VPalette.rgbReserved := $00;
            end else begin
              VPalette.rgbReserved := $FF;
            end;
            Inc(VPalette);
          end;

          VIndexedColor := GetMemory(VWidth * VHeight);
          try
            VPtr := VIndexedColor;
            VRowSize := VWidth;
            for I := 0 to VHeight - 1 do begin
              Move(
                VFreeBitmap.GetScanLine(VHeight - 1 - I)^,
                VPtr^,
                VRowSize
              );
              Inc(VPtr, VRowSize);
            end;

            Result := TBitmap8Static.CreateWithOwn(
              VIndexedColor,
              ABitmap32.Size,
              PColor32Array(VFreeBitmap.GetPalette),
              VFreeBitmap.GetPaletteSize div SizeOf(PRGBQUAD)
            );
          finally
            FreeMemory(VIndexedColor);
          end;
        end else begin
          raise EBitmap32To8ConverterByFreeImage.Create(
            'FreeBitmap.Palette ERROR!'
          );
        end;
      end else begin
        raise EBitmap32To8ConverterByFreeImage.Create(
          'FreeBitmap.ColorQuantize FAIL!'
        );
      end;
    end else begin
      raise EBitmap32To8ConverterByFreeImage.Create(
        'Palette FreeBitmap.ConvertTo24Bits FAIL!'
      );
    end;
  finally
    VFreeBitmap.Free;
  end;
end;

end.
