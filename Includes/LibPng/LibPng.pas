unit LibPng;

{$ALIGN ON}

interface

const
  PNG_LIBPNG_VER_STRING = '1.5.7';
  PNG_HEADER_VERSION_STRING = 'libpng version 1.5.7 - December 15, 2011';
  PNG_LIBPNG_VER = 10507; // 1.5.7

  // These describe the color_type field in png_info.
  // color type masks
  PNG_COLOR_MASK_PALETTE = 1;
  PNG_COLOR_MASK_COLOR   = 2;
  PNG_COLOR_MASK_ALPHA   = 4;

  // color types.  Note that not all combinations are legal
  PNG_COLOR_TYPE_GRAY       = 0;
  PNG_COLOR_TYPE_PALETTE    = PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_PALETTE;
  PNG_COLOR_TYPE_RGB        = PNG_COLOR_MASK_COLOR;
  PNG_COLOR_TYPE_RGB_ALPHA  = PNG_COLOR_MASK_COLOR or  PNG_COLOR_MASK_ALPHA;
  PNG_COLOR_TYPE_GRAY_ALPHA = PNG_COLOR_MASK_ALPHA;
  // aliases
  PNG_COLOR_TYPE_RGBA       = PNG_COLOR_TYPE_RGB_ALPHA;
  PNG_COLOR_TYPE_GA         = PNG_COLOR_TYPE_GRAY_ALPHA;

  // This is for compression type. PNG 1.0-1.2 only define the single type.
  PNG_COMPRESSION_TYPE_BASE    = 0; // Deflate method 8, 32K window
  PNG_COMPRESSION_TYPE_DEFAULT = PNG_COMPRESSION_TYPE_BASE;

  // This is for filter type. PNG 1.0-1.2 only define the single type.
  PNG_FILTER_TYPE_BASE        = 0; // Single row per-byte filtering
  PNG_INTRAPIXEL_DIFFERENCING = 64; // Used only in MNG datastreams
  PNG_FILTER_TYPE_DEFAULT     = PNG_FILTER_TYPE_BASE;

  // These are for the interlacing type.  These values should NOT be changed.
  PNG_INTERLACE_NONE  = 0; // Non-interlaced image
  PNG_INTERLACE_ADAM7 = 1; // Adam7 interlacing
  PNG_INTERLACE_LAST  = 2; // Not a valid value

  (* Filter values (not flags) - used in pngwrite.c, pngwutil.c for now.
   * These defines should NOT be changed.
   *)
  PNG_FILTER_VALUE_NONE  = 0;
  PNG_FILTER_VALUE_SUB   = 1;
  PNG_FILTER_VALUE_UP    = 2;
  PNG_FILTER_VALUE_AVG   = 3;
  PNG_FILTER_VALUE_PAETH = 4;
  PNG_FILTER_VALUE_LAST  = 5;

  // flags for png_ptr->free_me and info_ptr->free_me 
  PNG_FREE_ALL = $7FFF;

type
  int = Integer;
  png_uint_16 = Word;
  png_uint_32 = Cardinal;
  png_int_32 = Longint;

  png_size_t = png_uint_32;
  
  png_bytepp = ^png_bytep;
  png_bytep = ^png_byte;
  png_const_bytep = png_bytep;
  png_byte = Byte;
  
  png_charpp = ^png_charp;
  png_charp = PAnsiChar;

  png_infopp = ^png_infop;
  png_infop = ^png_info;

  png_structpp = ^png_structp;
  png_structp = ^png_struct_def;

  jmp_buf = array [0..15] of int;
  
  png_longjmp_ptr = Pointer;
  user_error_ptr = Pointer;

  png_error_ptrp = ^png_error_ptr;
  png_error_ptr = procedure(png_ptr: png_structp; msg: png_charp); cdecl;

  png_voidp = Pointer;

  png_rw_ptrp = ^png_rw_ptr;
  png_rw_ptr = procedure(png_ptr: png_structp; data: png_bytep; data_length: png_size_t); cdecl;

  png_flush_ptrp = ^png_flush_ptr;
  png_flush_ptr = procedure(png_ptr: png_structp); cdecl;

  (* Three color definitions.  The order of the red, green, and blue, (and the
   * exact size) is not important, although the size of the fields need to
   * be png_byte or png_uint_16 (as defined below).
   *)
  png_color = packed record
    red: png_byte;
    green: png_byte;
    blue: png_byte;
  end;
  png_colorp = ^png_color;
  png_const_colorp = png_colorp;

  png_color_16 = packed record
    index: png_byte;    // used for palette files
    red: png_uint_16;   // for use in red green blue files
    green: png_uint_16;
    blue: png_uint_16;
    gray: png_uint_16;  // for use in grayscale files
  end;
  png_color_16p = ^png_color_16;
  png_const_color_16p = png_color_16p;

  png_info = packed record
    // pnginfo.h 

    width: png_uint_32;  // width of image in pixels (from IHDR)
    height: png_uint_32; // height of image in pixels (from IHDR)        

    // ...  
  end;

  png_struct_def = packed record
    // pngstruct.h    

    longjmp_buffer: jmp_buf;    // used in png_error
    longjmp_fn: png_longjmp_ptr;// setjmp non-local goto function. 
    error_fn: png_error_ptr;    // function for printing errors and aborting
    warning_fn: png_error_ptr;  // function for printing warnings 
    error_ptr: png_voidp;       // user supplied struct for error functions
    write_data_fn: png_rw_ptr;  // function for writing output data 
    read_data_fn: png_rw_ptr;   // function for reading input data
    io_ptr: png_voidp;          // ptr to application struct for I/O functions
    
    // ...
  end;

var

(* The following return the library version as a short string in the
 * format 1.0.0 through 99.99.99zz.  To get the version of *.h files
 * used with your application, print out PNG_LIBPNG_VER_STRING, which
 * is defined in png.h.
 * Note: now there is no difference between png_get_libpng_ver() and
 * png_get_header_ver().  Due to the version_nn_nn_nn typedef guard,
 * it is guaranteed that png.c uses the correct version of png.h.
 *)
png_get_libpng_ver: function(png_ptr: png_structp): png_charp; cdecl;

// Allocate and initialize png_ptr struct for writing, and any other memory
png_create_write_struct: function(user_png_ver: png_charp;
  error_ptr: user_error_ptr; error_fn: png_error_ptr;
  warn_fn: png_error_ptr): png_structp;  cdecl;

(* Allocate the memory for an info_struct for the application.  We don't
 * really need the png_ptr, but it could potentially be useful in the
 * future.  This should be used in favour of malloc(png_sizeof(png_info))
 * and png_info_init() so that applications that want to use a shared
 * libpng don't have to be recompiled if png_info changes size.
 *)
png_create_info_struct: function(png_ptr: png_structp ): png_infop; cdecl;

(* Replace the default data output functions with a user supplied one(s).
 * If buffered output is not used, then output_flush_fn can be set to NULL.
 * If PNG_WRITE_FLUSH_SUPPORTED is not defined at libpng compile time
 * output_flush_fn will be ignored (and thus can be NULL).
 * It is probably a mistake to use NULL for output_flush_fn if
 * write_data_fn is not also NULL unless you have built libpng with
 * PNG_WRITE_FLUSH_SUPPORTED undefined, because in this case libpng's
 * default flush function, which uses the standard *FILE structure, will
 * be used.
 *)
png_set_write_fn: procedure(png_ptr: png_structp; io_ptr: png_voidp;
  write_data_fn: png_rw_ptr; output_flush_fn: png_flush_ptr);  cdecl;

png_set_IHDR: procedure(png_ptr: png_structp; info_ptr: png_infop;
  width, height: png_uint_32; bit_depth, color_type, interlace_type,
  compression_type, filter_type: int); cdecl;

// Writes all the PNG information before the image.
png_write_info: procedure(png_ptr: png_structp; info_ptr: png_infop); cdecl;

// Write a row of image data
png_write_row: procedure(png_ptr: png_structp; row: png_bytep); cdecl;

// Writes the end of the PNG file.
png_write_end: procedure(png_ptr: png_structp; info_ptr: png_infop); cdecl;

// Free data that was allocated internally
png_free_data: procedure(png_ptr: png_structp; info_ptr: png_infop; num: int); cdecl;

// Free any memory associated with the png_struct and the png_info_structs
png_destroy_write_struct: procedure(png_ptr_ptr: png_structpp;
  info_ptr_ptr: png_infopp); cdecl;

(* Set the library compression level.  Currently, valid values range from
 * 0 - 9, corresponding directly to the zlib compression levels 0 - 9
 * (0 - no compression, 9 - "maximal" compression).  Note that tests have
 * shown that zlib compression levels 3-6 usually perform as well as level 9
 * for PNG images, and do considerably fewer caclulations.  In the future,
 * these values may not correspond directly to the zlib compression levels.
 *)
png_set_compression_level: procedure(png_ptr: png_structp; level: int); cdecl;

// Use 1 byte per pixel in 1, 2, or 4-bit depth files.
png_set_packing: procedure(png_ptr: png_structp); cdecl;

png_set_gAMA: procedure(png_ptr: png_structp; info_ptr: png_infop; file_gamma: double); cdecl;

png_set_sRGB: procedure(png_ptr: png_structp; info_ptr: png_infop; srgb_intent: int); cdecl;

(* Set the filtering method(s) used by libpng.  Currently, the only valid
 * value for "method" is 0.
 *)
png_set_filter: procedure(png_ptr: png_structp; method, filters: int); cdecl;

(* png_set_PLTE() shall set the array of color values used as palette for image
* to "palette". The palette shall include "num_palette" entries.
*) 
png_set_PLTE: procedure(png_ptr: png_structp; info_ptr: png_infop;
  palette: png_const_colorp; num_palette: int); cdecl;

(* png_set_tRNS() shall set the transparency data for paletted images and image
* types that don't need a full alpha channel. For a paletted image,
* png_set_tRNS() shall set the array of transparency values for the palette
* colors to "trans_alpha". The number of transparency entries is given by "num_trans".
* For non-paletted images, png_set_tRNS() shall set the single color value or
* graylevel to "trans_color"
*)
png_set_tRNS: procedure(png_ptr: png_structp; info_ptr: png_infop;
    trans_alpha: png_const_bytep; num_trans: int;
    trans_color: png_const_color_16p); cdecl;

const
  libpng_dll = 'libpng15.dll';

function InitLibPng(const ALibName: string = libpng_dll): Boolean;

implementation

uses
  Windows,
  SysUtils,
  SyncObjs;

var
  gHandle: THandle = 0;
  gLock: TCriticalSection = nil;
  gIsInitialized: Boolean = False;

function GetProcAddr(const AProcName: PChar): Pointer;
begin
  GetProcAddr := GetProcAddress(gHandle, AProcName);
end;

function InitLibPng(const ALibName: string): Boolean;
begin
  if gIsInitialized then begin
    Result := True;
    Exit;
  end;

  gLock.Acquire;
  try
    if gIsInitialized then begin
      Result := True;
      Exit;
    end;

    if gHandle = 0 then begin
      gHandle := LoadLibrary(PChar(ALibName));
    end;

    if gHandle <> 0 then begin
      png_get_libpng_ver := GetProcAddr('png_get_libpng_ver');
      png_create_write_struct := GetProcAddr('png_create_write_struct');
      png_create_info_struct := GetProcAddr('png_create_info_struct');
      png_set_write_fn := GetProcAddr('png_set_write_fn');
      png_set_IHDR := GetProcAddr('png_set_IHDR');
      png_write_info := GetProcAddr('png_write_info');
      png_write_row := GetProcAddr('png_write_row');
      png_write_end := GetProcAddr('png_write_end');
      png_free_data := GetProcAddr('png_free_data');
      png_destroy_write_struct := GetProcAddr('png_destroy_write_struct');
      png_set_compression_level := GetProcAddr('png_set_compression_level');
      png_set_packing := GetProcAddr('png_set_packing');
      png_set_gAMA := GetProcAddr('png_set_gAMA');
      png_set_sRGB := GetProcAddr('png_set_sRGB');
      png_set_filter := GetProcAddr('png_set_filter');
      png_set_PLTE := GetProcAddr('png_set_PLTE');
      png_set_tRNS := GetProcAddr('png_set_tRNS');
    end;

    gIsInitialized :=
      (gHandle <> 0) and
      (Addr(png_get_libpng_ver) <> nil) and
      (Addr(png_create_write_struct) <> nil) and
      (Addr(png_create_info_struct) <> nil) and
      (Addr(png_set_write_fn) <> nil) and
      (Addr(png_set_IHDR) <> nil) and
      (Addr(png_write_info) <> nil) and
      (Addr(png_write_row) <> nil) and
      (Addr(png_write_end) <> nil) and
      (Addr(png_free_data) <> nil) and
      (Addr(png_set_compression_level) <> nil) and
      (Addr(png_set_packing) <> nil) and
      (Addr(png_set_gAMA) <> nil) and
      (Addr(png_set_sRGB) <> nil) and
      (Addr(png_set_filter) <> nil) and
      (Addr(png_set_PLTE) <> nil) and
      (Addr(png_set_tRNS) <> nil) and
      (Addr(png_destroy_write_struct) <> nil);

    Result := gIsInitialized;
  finally
    gLock.Release;
  end;
end;

procedure FinLibPng;
begin
  gLock.Acquire;
  try
    gIsInitialized := False;

    if gHandle <> 0 then begin
      FreeLibrary(gHandle);
      gHandle := 0;
    end;

    png_get_libpng_ver := nil;
    png_create_write_struct := nil;
    png_create_info_struct := nil;
    png_set_write_fn := nil;
    png_set_IHDR := nil;
    png_write_info := nil;
    png_write_row := nil;
    png_write_end := nil;
    png_free_data := nil;
    png_destroy_write_struct := nil;
    png_set_compression_level := nil;
    png_set_packing := nil;
    png_set_gAMA := nil;
    png_set_sRGB := nil;
    png_set_filter := nil;
    png_set_PLTE := nil;
    png_set_tRNS := nil;
  finally
    gLock.Release;
  end;
end;

initialization
  gLock := TCriticalSection.Create;

finalization
  FinLibPng;
  FreeAndNil(gLock);

end.

