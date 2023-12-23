unit libimagequant;

interface

uses
  SysUtils;

const
  libimagequant_dll = 'imagequant.dll';

type
  size_t = NativeUInt;

  liq_attr_ptr = Pointer;
  liq_image_ptr = Pointer;
  liq_result_ptr = Pointer;

  liq_error = (
    LIQ_OK = 0,
    LIQ_QUALITY_TOO_LOW = 99,
    LIQ_VALUE_OUT_OF_RANGE = 100,
    LIQ_ABORTED,
    LIQ_BITMAP_NOT_AVAILABLE,
    LIQ_BUFFER_TOO_SMALL,
    LIQ_INVALID_POINTER,
    LIQ_UNSUPPORTED
  );

  liq_color = packed record
    r, g, b, a: Byte;
  end;
  liq_color_ptr = ^liq_color;

  liq_palette = packed record
    count: Cardinal;
    entries: array [0..255] of liq_color;
  end;
  liq_palette_ptr = ^liq_palette;

  liq_attr_create_t = function(): liq_attr_ptr; cdecl;

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

var
  liq_attr_create: liq_attr_create_t;
  liq_set_min_opacity: liq_set_min_opacity_t;
  liq_image_create_rgba: liq_image_create_rgba_t;
  liq_image_create_custom: liq_image_create_custom_t;
  liq_quantize_image: liq_quantize_image_t;
  liq_write_remapped_image: liq_write_remapped_image_t;
  liq_get_palette: liq_get_palette_t;
  liq_attr_destroy: liq_attr_destroy_t;
  liq_image_destroy: liq_image_destroy_t;
  liq_result_destroy: liq_result_destroy_t;

procedure liq_ret_code_check(const ret: liq_error);
procedure InitLibImageQuant(const ALibName: string = libimagequant_dll);

implementation

uses
  Windows,
  SyncObjs;

type
  ELibImageQuantError = class(Exception);

var
  GHandle: THandle = 0;
  GLock: TCriticalSection = nil;
  GIsInitialized: Boolean = False;

procedure liq_ret_code_check(const ret: liq_error);
begin
  case ret of
    LIQ_QUALITY_TOO_LOW: begin
      raise ELibImageQuantError.Create('LIQ_QUALITY_TOO_LOW');
    end;
    LIQ_VALUE_OUT_OF_RANGE: begin
      raise ELibImageQuantError.Create('LIQ_VALUE_OUT_OF_RANGE');
    end;
    LIQ_ABORTED: begin
      raise ELibImageQuantError.Create('LIQ_ABORTED');
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
    LIQ_UNSUPPORTED: begin
      raise ELibImageQuantError.Create('LIQ_UNSUPPORTED');
    end;
  end;
end;

procedure InitLibImageQuant(const ALibName: string);

  function GetProcAddr(const AProcName: string): Pointer;
  begin
    Result := GetProcAddress(GHandle, PChar(AProcName));
    if Result = nil then begin
      RaiseLastOSError(GetLastError, ': ' + AProcName + #13#10 + ALibName);
    end;
  end;

begin
  if GIsInitialized then begin
    Exit;
  end;

  GLock.Acquire;
  try
    if GHandle = 0 then begin
      GHandle := LoadLibrary(PChar(ALibName));
      if GHandle = 0 then begin
        RaiseLastOSError(GetLastError, ': ' + ALibName);
      end;
    end;

    try
      liq_attr_create := GetProcAddr('liq_attr_create');
      liq_set_min_opacity := GetProcAddr('liq_set_min_opacity');
      liq_image_create_rgba := GetProcAddr('liq_image_create_rgba');
      liq_image_create_custom := GetProcAddr('liq_image_create_custom');
      liq_quantize_image := GetProcAddr('liq_quantize_image');
      liq_write_remapped_image := GetProcAddr('liq_write_remapped_image');
      liq_get_palette := GetProcAddr('liq_get_palette');
      liq_attr_destroy := GetProcAddr('liq_attr_destroy');
      liq_image_destroy := GetProcAddr('liq_image_destroy');
      liq_result_destroy := GetProcAddr('liq_result_destroy');

      GIsInitialized := True;
    except
      FreeLibrary(GHandle);
      GHandle := 0;
      raise;
    end;
  finally
    GLock.Release;
  end;
end;

procedure FinLibImageQuant;
begin
  if GHandle = 0 then begin
    Exit;
  end;

  GLock.Acquire;
  try
    if GHandle <> 0 then begin
      FreeLibrary(GHandle);
      GHandle := 0;
    end;

    liq_attr_create := nil;
    liq_set_min_opacity := nil;
    liq_image_create_rgba := nil;
    liq_image_create_custom := nil;
    liq_quantize_image := nil;
    liq_write_remapped_image := nil;
    liq_get_palette := nil;
    liq_attr_destroy := nil;
    liq_image_destroy := nil;
    liq_result_destroy := nil;

    GIsInitialized := False;
  finally
    GLock.Release;
  end;
end;

initialization
  GLock := TCriticalSection.Create;

finalization
  FinLibImageQuant;
  FreeAndNil(GLock);

end.