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

unit u_Bitmap32To8ConverterByLibImageQuant;

interface

{.$DEFINE DISABLE_FP_EXCEPTION}
{.$DEFINE DO_BGRA_TO_RGBA_CONVERTIONS}

uses
  i_Bitmap8Static,
  i_Bitmap32Static,
  i_Bitmap32To8Converter,
  u_BaseInterfacedObject;

type
  TBitmap32To8ConverterByLibImageQuant = class(TBaseInterfacedObject, IBitmap32To8Converter)
  private
    { IBitmap32To8Converter }
    function Convert(const ABitmap32: IBitmap32Static): IBitmap8Static;
  public
    constructor Create;
  end;

implementation

uses
  Types,
  SysUtils,
  libimagequant,
  t_Bitmap32,
  u_GlobalDllName,
  u_Bitmap8Static;

{$IFDEF DO_BGRA_TO_RGBA_CONVERTIONS}
type
  TBGRAColor = packed record
    B, G, R, A: Byte;
  end;
  PBGRAColor = ^TBGRAColor;

procedure bgra_to_rgba_callback(
  row_out: liq_color_ptr;
  row, width: Integer;
  user_info: pointer
); cdecl;
var
  I: Integer;
  VData: PColor32Array;
  VBGRAColor: TBGRAColor;
  row_out_p: liq_color_ptr;
begin
  VData := PColor32Array(user_info);
  row_out_p := row_out;
  for I := 0 to width - 1 do begin
    VBGRAColor := TBGRAColor(VData[I + row * width]);
    // BGRA to RGBA convert
    row_out_p.r := VBGRAColor.R;
    row_out_p.g := VBGRAColor.G;
    row_out_p.b := VBGRAColor.B;
    row_out_p.a := VBGRAColor.A;
    Inc(row_out_p);
  end;
end;
{$ENDIF}

{ TBitmap32To8ConverterByLibImageQuant }

constructor TBitmap32To8ConverterByLibImageQuant.Create;
begin
  inherited Create;

  InitLibImageQuant(GDllName.ImageQuant);
end;

function TBitmap32To8ConverterByLibImageQuant.Convert(
  const ABitmap32: IBitmap32Static
): IBitmap8Static;
var
  {$IFDEF DO_BGRA_TO_RGBA_CONVERTIONS}
  I: Integer;
  VBGRAPalette: array [0..255] of TBGRAColor;
  {$ENDIF}
  VAttr: liq_attr_ptr;
  VImage: liq_image_ptr;
  VRes: liq_result_ptr;
  VPalette: liq_palette_ptr;
  VWidth, VHeight: Integer;
  VBitmap: PByte;
  VBitmapSize: size_t;
  VRet: liq_error;
  {$IFDEF DISABLE_FP_EXCEPTION}
  CW: Word;
  {$ENDIF}
begin
  Assert(ABitmap32 <> nil);

  Result := nil;

  {$IFDEF DISABLE_FP_EXCEPTION}
  CW := Get8087CW;
  Set8087CW(CW or $003F); // Disable "Floating point division by zero" exception
  try
  {$ENDIF}
    with ABitmap32.Size do begin
      VWidth := X;
      VHeight := Y;
    end;

    VAttr := liq_attr_create();
    try
      Assert(VAttr <> nil);

      VRet := liq_set_min_opacity(VAttr, 0);
      if VRet <> LIQ_OK then begin
        liq_ret_code_check(VRet); // raise Error
      end;

      {$IFDEF DO_BGRA_TO_RGBA_CONVERTIONS}
      VImage := liq_image_create_custom(VAttr, @bgra_to_rgba_callback, ABitmap32.Data, VWidth, VHeight, 0);
      {$ELSE}
      VImage := liq_image_create_rgba(VAttr, ABitmap32.Data, VWidth, VHeight, 0);
      {$ENDIF}
      try
        Assert(VImage <> nil);

        VRes := liq_quantize_image(VAttr, VImage);
        try
          Assert(VRes <> nil);

          VBitmapSize := VWidth * VHeight; // 1 byte per pixel
          VBitmap := GetMemory(VBitmapSize);
          try
            VRet := liq_write_remapped_image(VRes, VImage, VBitmap, VBitmapSize);
            if VRet = LIQ_OK then begin
              VPalette := liq_get_palette(VRes);
              {$IFDEF DO_BGRA_TO_RGBA_CONVERTIONS}
              for I := 0 to VPalette.count - 1 do begin
                // RGBA to BGRA convert
                VBGRAPalette[I].B := VPalette.entries[I].b;
                VBGRAPalette[I].G := VPalette.entries[I].g;
                VBGRAPalette[I].R := VPalette.entries[I].r;
                VBGRAPalette[I].A := VPalette.entries[I].a;
              end;
              Result := TBitmap8Static.CreateWithOwn(VBitmap, Point(VWidth, VHeight), @VBGRAPalette[0], VPalette.count);
              {$ELSE}
              Result := TBitmap8Static.CreateWithOwn(VBitmap, Point(VWidth, VHeight), @VPalette.entries[0], VPalette.count);
              {$ENDIF}
            end else begin
              liq_ret_code_check(VRet); // raise Error
            end;
          finally
            FreeMemory(VBitmap);
          end;
        finally
          liq_result_destroy(VRes);
        end;
      finally
        liq_image_destroy(VImage);
      end;
    finally
      liq_attr_destroy(VAttr);
    end;
  {$IFDEF DISABLE_FP_EXCEPTION}
  finally
    Set8087CW(CW); // Enable "Floating point division by zero" exception
  end;
  {$ENDIF}
end;

end.
