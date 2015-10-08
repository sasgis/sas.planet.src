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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_Bitmap32To8ConverterByLibImageQuant;

interface

uses
  Windows,
  i_Bitmap8Static,
  i_Bitmap32Static,
  i_Bitmap32To8Converter,
  u_BaseInterfacedObject;

type
  { libimagequant API }
  alloc_mem_t = function(size: Integer): Pointer;
  free_mem_t = procedure(p: pointer);

  size_t = Integer;

  {$IFDEF DEBUG}
  liq_attr = record
    magic_header: PAnsiChar;
    alloc: alloc_mem_t;
    free: free_mem_t;
    target_mse, max_mse, voronoi_iteration_limit: double;
    min_opaque_val: single;
    max_colors, max_histogram_entries: integer;
    min_posterization_output, min_posterization_input: Integer;
    voronoi_iterations, feedback_loop_trials: integer;
    last_index_transparent, use_contrast_maps, use_dither_map, fast_palette: Boolean;
    log_callback: pointer;
    log_callback_user_info: pointer;
    log_flush_callback: pointer;
    log_flush_callback_user_info: pointer;
  end;
  {$ELSE}
  liq_attr = record
  end;
  {$ENDIF}

  liq_attr_ptr = ^liq_attr;

  {$IFDEF DEBUG}
  liq_image = record
    magic_header: PAnsiChar;
    alloc: alloc_mem_t;
    free: free_mem_t;
    f_pixels: pointer;
    rows: PPointer;
    gamma: double;
    width, height: integer;
    noise, edges, dither_map: PByte;
    pixels, temp_row: pointer;
    temp_f_row: pointer;
    row_callback: pointer;
    row_callback_user_info: pointer;
    min_opaque_val: single;
    free_rows, free_pixels: boolean;
  end;
  {$ELSE}
  liq_image = record
  end;
  {$ENDIF}

  liq_image_ptr = ^liq_image;

  liq_result_ptr = Pointer;

  liq_error = (
    LIQ_OK = 0,
    LIQ_VALUE_OUT_OF_RANGE = 100,
    LIQ_OUT_OF_MEMORY,
    LIQ_NOT_READY,
    LIQ_BITMAP_NOT_AVAILABLE,
    LIQ_BUFFER_TOO_SMALL,
    LIQ_INVALID_POINTER
    );

  liq_color = record
    r, g, b, a: Byte;
  end;
  liq_color_ptr = ^liq_color;

  liq_palette = record
    count: Cardinal;
    entries: array [0..255] of liq_color;
  end;
  liq_palette_ptr = ^liq_palette;
  liq_attr_create_t = function(): liq_attr_ptr; cdecl;
  liq_attr_create_with_allocator_t = function(
      alloc: alloc_mem_t;
      free: free_mem_t
    ): liq_attr_ptr; cdecl;

  liq_set_min_opacity_t = function(
      attr: liq_attr_ptr;
      min: integer
    ): liq_error; cdecl;

  liq_image_create_rgba_t = function(
      attr: liq_attr_ptr;
      bitmap: pointer;
      width, height: Integer;
      gamma: Double
    ): liq_image_ptr; cdecl;
  liq_image_get_rgba_row_callback_t = procedure(
      row_out: liq_color_ptr;
      row, width: Integer;
      user_info: pointer
    ); cdecl;
  liq_image_create_custom_t = function(
      attr: liq_attr_ptr;
      row_callback: liq_image_get_rgba_row_callback_t;
      user_info: pointer;
      width, height: Integer;
      gamma: double
    ): liq_image_ptr; cdecl;

  liq_quantize_image_t = function(
      options: liq_attr_ptr;
      input_image: liq_image_ptr
    ): liq_result_ptr; cdecl;

  liq_write_remapped_image_t = function(
      result: liq_result_ptr;
      input_image: liq_image_ptr;
      buffer: pointer;
      buffer_size: size_t
    ): liq_error; cdecl;

  liq_get_palette_t = function(result: liq_result_ptr): liq_palette_ptr; cdecl;

  liq_attr_destroy_t = procedure(attr: liq_attr_ptr); cdecl;
  liq_image_destroy_t = procedure(img: liq_image_ptr); cdecl;
  liq_result_destroy_t = procedure(result: liq_result_ptr); cdecl;

type
  TBitmap32To8ConverterByLibImageQuant = class(TBaseInterfacedObject, IBitmap32To8Converter)
  private
    libimagequant_dll: THandle;
  private
    liq_attr_create: liq_attr_create_t;
    liq_attr_create_with_allocator: liq_attr_create_with_allocator_t;
    liq_set_min_opacity: liq_set_min_opacity_t;
    liq_image_create_rgba: liq_image_create_rgba_t;
    liq_image_create_custom: liq_image_create_custom_t;
    liq_quantize_image: liq_quantize_image_t;
    liq_write_remapped_image: liq_write_remapped_image_t;
    liq_get_palette: liq_get_palette_t;
    liq_attr_destroy: liq_attr_destroy_t;
    liq_image_destroy: liq_image_destroy_t;
    liq_result_destroy: liq_result_destroy_t;
  private
    { IBitmap32To8Converter }
    function Convert(const ABitmap32: IBitmap32Static): IBitmap8Static;
  public
    constructor Create(const AQuietErrors: Boolean = False);
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  SysUtils,
  u_Bitmap8Static;

{$DEFINE DISABLE_FP_EXCEPTION}
{.$DEFINE DO_BGRA_TO_RGBA_CONVERTIONS}

const
  libimagequant_lib = 'libimagequant.dll';

type
  TBGRAColor = packed record
    B, G, R, A: Byte;
  end;
  PBGRAColor = ^TBGRAColor;

  ELibImageQuantError = class(Exception);

procedure liq_ret_code_check(const ret: liq_error);
begin
  case ret of
    LIQ_VALUE_OUT_OF_RANGE: begin
      raise ELibImageQuantError.Create('LIQ_VALUE_OUT_OF_RANGE');
    end;
    LIQ_OUT_OF_MEMORY: begin
      raise ELibImageQuantError.Create('LIQ_OUT_OF_MEMORY');
    end;
    LIQ_NOT_READY: begin
      raise ELibImageQuantError.Create('LIQ_NOT_READY');
    end;
    LIQ_BITMAP_NOT_AVAILABLE: begin
      raise ELibImageQuantError.Create('LIQ_BITMAP_NOT_AVAILABLE');
    end;
    LIQ_BUFFER_TOO_SMALL: begin
      raise ELibImageQuantError.Create('LIQ_BUFFER_TOO_SMALL');
    end;
    LIQ_INVALID_POINTER: begin
      raise ELibImageQuantError.Create('LIQ_INVALID_POINTER');
    end;
  end;
end;

{ TBitmap32To8ConverterByLibImageQuant }

constructor TBitmap32To8ConverterByLibImageQuant.Create(const AQuietErrors: Boolean);

  procedure RaiseLastOSError;
  begin
    if AQuietErrors then begin
      Abort;
    end else begin
      SysUtils.RaiseLastOSError;
    end;
  end;

begin
  inherited Create;
  libimagequant_dll := LoadLibrary(PChar(libimagequant_lib));
  if (libimagequant_dll <> 0) then begin
    try
      liq_attr_create := GetProcAddress(libimagequant_dll, 'liq_attr_create');
      if Addr(liq_attr_create) = nil then begin
        RaiseLastOSError;
      end;
      liq_attr_create_with_allocator := GetProcAddress(libimagequant_dll, 'liq_attr_create_with_allocator');
      if Addr(liq_attr_create_with_allocator) = nil then begin
        RaiseLastOSError;
      end;
      liq_set_min_opacity := GetProcAddress(libimagequant_dll, 'liq_set_min_opacity');
      if Addr(liq_set_min_opacity) = nil then begin
        RaiseLastOSError;
      end;
      liq_image_create_rgba := GetProcAddress(libimagequant_dll, 'liq_image_create_rgba');
      if Addr(liq_image_create_rgba) = nil then begin
        RaiseLastOSError;
      end;
      liq_image_create_custom := GetProcAddress(libimagequant_dll, 'liq_image_create_custom');
      if Addr(liq_image_create_custom) = nil then begin
        RaiseLastOSError;
      end;
      liq_quantize_image := GetProcAddress(libimagequant_dll, 'liq_quantize_image');
      if Addr(liq_quantize_image) = nil then begin
        RaiseLastOSError;
      end;
      liq_write_remapped_image := GetProcAddress(libimagequant_dll, 'liq_write_remapped_image');
      if Addr(liq_write_remapped_image) = nil then begin
        RaiseLastOSError;
      end;
      liq_get_palette := GetProcAddress(libimagequant_dll, 'liq_get_palette');
      if Addr(liq_get_palette) = nil then begin
        RaiseLastOSError;
      end;
      liq_attr_destroy := GetProcAddress(libimagequant_dll, 'liq_attr_destroy');
      if Addr(liq_attr_destroy) = nil then begin
        RaiseLastOSError;
      end;
      liq_image_destroy := GetProcAddress(libimagequant_dll, 'liq_image_destroy');
      if Addr(liq_image_destroy) = nil then begin
        RaiseLastOSError;
      end;
      liq_result_destroy := GetProcAddress(libimagequant_dll, 'liq_result_destroy');
      if Addr(liq_result_destroy) = nil then begin
        RaiseLastOSError;
      end;
    except
      FreeLibrary(libimagequant_dll);
      libimagequant_dll := 0;
      raise;
    end;
  end else begin
    RaiseLastOSError;
  end;
end;

destructor TBitmap32To8ConverterByLibImageQuant.Destroy;
begin
  if (libimagequant_dll <> 0) then begin
    FreeLibrary(libimagequant_dll);
    libimagequant_dll := 0;
  end;
  inherited;
end;

{$IFDEF DO_BGRA_TO_RGBA_CONVERTIONS}
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

function TBitmap32To8ConverterByLibImageQuant.Convert(
  const ABitmap32: IBitmap32Static
): IBitmap8Static;
var
  attr: liq_attr_ptr;
  image: liq_image_ptr;
  res: liq_result_ptr;
  pal: liq_palette_ptr;
  width, height: Integer;
  bitmap: PByte;
  bitmap_size: size_t;
  ret: liq_error;
  {$IFDEF DO_BGRA_TO_RGBA_CONVERTIONS}
  I: Integer;
  VBGRAPalette: array [0..255] of TBGRAColor;
  {$ENDIF}
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
      width := X;
      height := Y;
    end;

    attr := liq_attr_create_with_allocator(@GetMemory, @FreeMemory);
    try
      Assert(attr <> nil);

      ret := liq_set_min_opacity(attr, 0);
      if not (ret = LIQ_OK) then begin
        liq_ret_code_check(ret); // raise Error
      end;

      {$IFDEF DO_BGRA_TO_RGBA_CONVERTIONS}
      image := liq_image_create_custom(attr, @bgra_to_rgba_callback, ABitmap32.Data, width, height, 0);
      {$ELSE}
      image := liq_image_create_rgba(attr, ABitmap32.Data, width, height, 0);
      {$ENDIF}
      try
        Assert(image <> nil);

        res := liq_quantize_image(attr, image);
        try
          Assert(res <> nil);

          bitmap_size := width * height; // 1 byte per pixel
          bitmap := GetMemory(bitmap_size);
          try
            ret := liq_write_remapped_image(res, image, bitmap, bitmap_size);
            if ret = LIQ_OK then begin
              pal := liq_get_palette(res);
              {$IFDEF DO_BGRA_TO_RGBA_CONVERTIONS}
              for I := 0 to pal.count - 1 do begin
                // RGBA to BGRA convert
                VBGRAPalette[I].B := pal.entries[I].b;
                VBGRAPalette[I].G := pal.entries[I].g;
                VBGRAPalette[I].R := pal.entries[I].r;
                VBGRAPalette[I].A := pal.entries[I].a;
              end;
              Result := TBitmap8Static.CreateWithOwn(bitmap, Point(width, height), @VBGRAPalette[0], pal.count);
              {$ELSE}
              Result := TBitmap8Static.CreateWithOwn(bitmap, Point(width, height), @pal.entries[0], pal.count);
              {$ENDIF}
            end else begin
              liq_ret_code_check(ret); // raise Error
            end;
          finally
            FreeMemory(bitmap);
          end;
        finally
          liq_result_destroy(res);
        end;
      finally
        liq_image_destroy(image);
      end;
    finally
      liq_attr_destroy(attr);
    end;
  {$IFDEF DISABLE_FP_EXCEPTION}
  finally
    Set8087CW(CW); // Enable "Floating point division by zero" exception
  end;
  {$ENDIF}
end;

end.
