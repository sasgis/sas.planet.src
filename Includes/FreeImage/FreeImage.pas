unit FreeImage;

// ==========================================================
// Delphi wrapper for FreeImage 3
//
// Design and implementation by
// - Simon Beavis
// - Peter Byström
// - Anatoliy Pulyaevskiy (xvel84@rambler.ru)
//
// Contributors:
// - Lorenzo Monti (LM)  lomo74@gmail.com
// - Maurício (MAU)      mauricio_box@yahoo.com - see also http://sourceforge.net/projects/tcycomponents/
//
// Revision history
// When        Who   What
// ----------- ----- -----------------------------------------------------------
// 2010-07-14  LM    Fixed some C->Delphi translation errors,
//                   updated to 3.13.1, made RAD2010 compliant (unicode)
// 2010-07-29  LM    Added Free Pascal / Lazarus 32 bit support
// 2010-11-12  LM    Updated to 3.14.1
// 2011-02-15  LM    Updated to 3.15.0
// 2011-03-04  JMB   Modifications to compile on Free Pascal / Lazarus 64 bits (tested on Windows 7 and linux OpenSuse) :
//                   - in 64 bits, the names of the exported function are different :
//                      e.g. : _FreeImage_AcquireMemory@12 in 32 bits and FreeImage_AcquireMemory in 64 bits
//                      so the define WIN32 will allow to distinguish 32 and 64 bits in the calls to the freeimage library
//                   - in 64 bits, the Boolean type is not correctly converted to freeimage BOOL type (integer 32 bits)
//                    ==> replace Boolean with LongBool in the calls to the freeimage library
//                   - as linux sees the difference between uppercase and lowercase :
//                    ==> replace FreeImage_GetMetaData with FreeImage_GetMetadata in the call to the freeimage library
// 2012-06-04  LM    Updated to 3.15.3
// 2012-12-08  LM    Updated to 3.15.4
// 2013-05-06  MAU   Corrected calls definition to MAC OSX library
// 2013-11-25  MAU   Added type FreeImageAnsiString for handling accents on MAC OSX filenames/path
// 2014-05-05  LM    Updated to 3.16.1
// 2015-09-17  LM    Updated to 3.17.0
// 2016-01-06  LM    Updated to 3.18.0
//

//
// This file is part of FreeImage 3
//
// COVERED CODE IS PROVIDED UNDER THIS LICENSE ON AN "AS IS" BASIS, WITHOUT WARRANTY
// OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTIES
// THAT THE COVERED CODE IS FREE OF DEFECTS, MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE
// OR NON-INFRINGING. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE COVERED
// CODE IS WITH YOU. SHOULD ANY COVERED CODE PROVE DEFECTIVE IN ANY RESPECT, YOU (NOT
// THE INITIAL DEVELOPER OR ANY OTHER CONTRIBUTOR) ASSUME THE COST OF ANY NECESSARY
// SERVICING, REPAIR OR CORRECTION. THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL
// PART OF THIS LICENSE. NO USE OF ANY COVERED CODE IS AUTHORIZED HEREUNDER EXCEPT UNDER
// THIS DISCLAIMER.
//
// Use at your own risk!
// ==========================================================

interface

{$MINENUMSIZE 4} // Make sure enums are stored as an integer to be compatible with C/C++

{$I 'Version.inc'}

{$IFDEF MSWINDOWS}

{.$DEFINE USE_MINGW_DLL}
{.$DEFINE USE_DELAYLOAD}

{.$DEFINE FREEIMAGE_STATIC_LINK}

uses
  Windows;

const
  CNamePrefixDefault = '_';
  CNamePrefixMingwDll = '';
  CNamePrefix = {$IFDEF USE_MINGW_DLL} CNamePrefixMingwDll {$ELSE} CNamePrefixDefault {$ENDIF};

type
  FreeImageAnsiString = AnsiString;

{$ELSE}
type
  FreeImageAnsiString = UTF8String;

  LONG = LongInt;
  DWORD = Cardinal;

  PDWORD = ^DWORD;

  BITMAPINFOHEADER = packed record
    biSize : DWORD;
    biWidth : LONG;
    biHeight : LONG;
    biPlanes : WORD;
    biBitCount : WORD;
    biCompression : DWORD;
    biSizeImage : DWORD;
    biXPelsPerMeter : LONG;
    biYPelsPerMeter : LONG;
    biClrUsed : DWORD;
    biClrImportant : DWORD;
  end;
  LPBITMAPINFOHEADER = ^BITMAPINFOHEADER;
  TBITMAPINFOHEADER = BITMAPINFOHEADER;
  PBITMAPINFOHEADER = ^BITMAPINFOHEADER;

  RGBQUAD = packed record
    rgbBlue : BYTE;
    rgbGreen : BYTE;
    rgbRed : BYTE;
    rgbReserved : BYTE;
  end;
  tagRGBQUAD = RGBQUAD;
  TRGBQUAD = RGBQUAD;
  PRGBQUAD = ^RGBQUAD;

  BITMAPINFO = packed record
    bmiHeader : BITMAPINFOHEADER;
    bmiColors : array[0..0] of RGBQUAD;
  end;
  LPBITMAPINFO = ^BITMAPINFO;
  PBITMAPINFO = ^BITMAPINFO;
  TBITMAPINFO = BITMAPINFO;
// modif JMB NOVAXEL
  HBITMAP = type LongWord;
  HWND = type LongWord;
  HDC = type LongWord;
// end of modif JMB NOVAXEL
{$ENDIF}

const
  FIDLL = {$IFDEF MSWINDOWS}
            {$IFDEF USE_MINGW_DLL}'libfreeimage-3.dll'{$ELSE}'FreeImage.dll';{$ENDIF}
          {$ENDIF}
          {$IFDEF LINUX}'libfreeimage.so';{$ENDIF}
          {$IFDEF MACOS}'libfreeimage.dylib';{$ENDIF}

const
  // Version information
  FREEIMAGE_MAJOR_VERSION  = 3;
  FREEIMAGE_MINOR_VERSION  = 18;
  FREEIMAGE_RELEASE_SERIAL = 0;
  // This really only affects 24 and 32 bit formats, the rest are always RGB order.
  FREEIMAGE_COLORORDER_BGR = 0;
  FREEIMAGE_COLORORDER_RGB = 1;
  FREEIMAGE_COLORORDER = FREEIMAGE_COLORORDER_BGR;

// --------------------------------------------------------------------------
// Bitmap types -------------------------------------------------------------
// --------------------------------------------------------------------------

type
  FIBITMAP = record
    data: Pointer;
  end;
  PFIBITMAP = ^FIBITMAP;

  FIMULTIBITMAP = record
    data: Pointer;
  end;
  PFIMULTIBITMAP = ^FIMULTIBITMAP;

// --------------------------------------------------------------------------
// Types used in the library (specific to FreeImage) ------------------------
// --------------------------------------------------------------------------

type
  {* 48-bit RGB }
  tagFIRGB16 = packed record
    red: WORD;
    green: WORD;
    blue: WORD;
  end;
  FIRGB16 = tagFIRGB16;

  {* 64-bit RGBA }
  tagFIRGBA16 = packed record
    red: WORD;
    green: WORD;
    blue: WORD;
    alpha: WORD;
  end;
  FIRGBA16 = tagFIRGBA16;

  {* 96-bit RGB Float }
  tagFIRGBF = packed record
    red: Single;
    green: Single;
    blue: Single;
  end;
  FIRGBF = tagFIRGBF;

  {* 128-bit RGBA Float }
  tagFIRGBAF = packed record
    red: Single;
    green: Single;
    blue: Single;
    alpha: Single;
  end;
  FIRGBAF = tagFIRGBAF;

  {* Data structure for COMPLEX type (complex number) }
  tagFICOMPLEX = packed record
    /// real part
    r: Double;
    /// imaginary part
    i: Double;
  end;
  FICOMPLEX = tagFICOMPLEX;

// --------------------------------------------------------------------------
// Indexes for byte arrays, masks and shifts for treating pixels as words ---
// These coincide with the order of RGBQUAD and RGBTRIPLE -------------------
// Little Endian (x86 / MS Windows, Linux) : BGR(A) order -------------------
// --------------------------------------------------------------------------

const
  FI_RGBA_RED         = 2;
  FI_RGBA_GREEN       = 1;
  FI_RGBA_BLUE        = 0;
  FI_RGBA_ALPHA       = 3;
  FI_RGBA_RED_MASK    = $00FF0000;
  FI_RGBA_GREEN_MASK  = $0000FF00;
  FI_RGBA_BLUE_MASK   = $000000FF;
  FI_RGBA_ALPHA_MASK  = $FF000000;
  FI_RGBA_RED_SHIFT   = 16;
  FI_RGBA_GREEN_SHIFT = 8;
  FI_RGBA_BLUE_SHIFT  = 0;
  FI_RGBA_ALPHA_SHIFT = 24;

  FI_RGBA_RGB_MASK = FI_RGBA_RED_MASK or FI_RGBA_GREEN_MASK or FI_RGBA_BLUE_MASK;

// --------------------------------------------------------------------------
// The 16bit macros only include masks and shifts, --------------------------
// since each color element is not byte aligned -----------------------------
// --------------------------------------------------------------------------

const
  FI16_555_RED_MASK    = $7C00;
  FI16_555_GREEN_MASK  = $03E0;
  FI16_555_BLUE_MASK   = $001F;
  FI16_555_RED_SHIFT   = 10;
  FI16_555_GREEN_SHIFT = 5;
  FI16_555_BLUE_SHIFT  = 0;
  FI16_565_RED_MASK    = $F800;
  FI16_565_GREEN_MASK  = $07E0;
  FI16_565_BLUE_MASK   = $001F;
  FI16_565_RED_SHIFT   = 11;
  FI16_565_GREEN_SHIFT = 5;
  FI16_565_BLUE_SHIFT  = 0;

// --------------------------------------------------------------------------
// ICC profile support ------------------------------------------------------
// --------------------------------------------------------------------------

const
  FIICC_DEFAULT = $0;
  FIICC_COLOR_IS_CMYK = $1;

type
  FIICCPROFILE = packed record
    flags: WORD;   // info flag
    size: DWORD;   // profile's size measured in bytes
    data: Pointer; // points to a block of contiguous memory containing the profile
  end;
  PFIICCPROFILE = ^FIICCPROFILE;

// --------------------------------------------------------------------------
// Important enums ----------------------------------------------------------
// --------------------------------------------------------------------------

type
  FREE_IMAGE_FORMAT         = type Integer;
  FREE_IMAGE_TYPE           = type Integer;
  FREE_IMAGE_COLOR_TYPE     = type Integer;
  FREE_IMAGE_QUANTIZE       = type Integer;
  FREE_IMAGE_DITHER         = type Integer;
  FREE_IMAGE_FILTER         = type Integer;
  FREE_IMAGE_COLOR_CHANNEL  = type Integer;
  FREE_IMAGE_MDTYPE         = type Integer;
  FREE_IMAGE_MDMODEL        = type Integer;
  FREE_IMAGE_JPEG_OPERATION = type Integer;
  FREE_IMAGE_TMO            = type Integer;

const
  // I/O image format identifiers.
  FIF_UNKNOWN = FREE_IMAGE_FORMAT(-1);
  FIF_BMP     = FREE_IMAGE_FORMAT(0);
  FIF_ICO     = FREE_IMAGE_FORMAT(1);
  FIF_JPEG    = FREE_IMAGE_FORMAT(2);
  FIF_JNG     = FREE_IMAGE_FORMAT(3);
  FIF_KOALA   = FREE_IMAGE_FORMAT(4);
  FIF_LBM     = FREE_IMAGE_FORMAT(5);
  FIF_IFF     = FIF_LBM;
  FIF_MNG     = FREE_IMAGE_FORMAT(6);
  FIF_PBM     = FREE_IMAGE_FORMAT(7);
  FIF_PBMRAW  = FREE_IMAGE_FORMAT(8);
  FIF_PCD     = FREE_IMAGE_FORMAT(9);
  FIF_PCX     = FREE_IMAGE_FORMAT(10);
  FIF_PGM     = FREE_IMAGE_FORMAT(11);
  FIF_PGMRAW  = FREE_IMAGE_FORMAT(12);
  FIF_PNG     = FREE_IMAGE_FORMAT(13);
  FIF_PPM     = FREE_IMAGE_FORMAT(14);
  FIF_PPMRAW  = FREE_IMAGE_FORMAT(15);
  FIF_RAS     = FREE_IMAGE_FORMAT(16);
  FIF_TARGA   = FREE_IMAGE_FORMAT(17);
  FIF_TIFF    = FREE_IMAGE_FORMAT(18);
  FIF_WBMP    = FREE_IMAGE_FORMAT(19);
  FIF_PSD     = FREE_IMAGE_FORMAT(20);
  FIF_CUT     = FREE_IMAGE_FORMAT(21);
  FIF_XBM     = FREE_IMAGE_FORMAT(22);
  FIF_XPM     = FREE_IMAGE_FORMAT(23);
  FIF_DDS     = FREE_IMAGE_FORMAT(24);
  FIF_GIF     = FREE_IMAGE_FORMAT(25);
  FIF_HDR     = FREE_IMAGE_FORMAT(26);
  FIF_FAXG3   = FREE_IMAGE_FORMAT(27);
  FIF_SGI     = FREE_IMAGE_FORMAT(28);
  FIF_EXR     = FREE_IMAGE_FORMAT(29);
  FIF_J2K     = FREE_IMAGE_FORMAT(30);
  FIF_JP2     = FREE_IMAGE_FORMAT(31);
  FIF_PFM     = FREE_IMAGE_FORMAT(32);
  FIF_PICT    = FREE_IMAGE_FORMAT(33);
  FIF_RAW     = FREE_IMAGE_FORMAT(34);
  FIF_WEBP    = FREE_IMAGE_FORMAT(35);
  FIF_JXR     = FREE_IMAGE_FORMAT(36);

  // Image type used in FreeImage.
  FIT_UNKNOWN = FREE_IMAGE_TYPE(0);  // unknown type
  FIT_BITMAP  = FREE_IMAGE_TYPE(1);  // standard image: 1-, 4-, 8-, 16-, 24-, 32-bit
  FIT_UINT16  = FREE_IMAGE_TYPE(2);  // array of unsigned short: unsigned 16-bit
  FIT_INT16   = FREE_IMAGE_TYPE(3);  // array of short: signed 16-bit
  FIT_UINT32  = FREE_IMAGE_TYPE(4);  // array of unsigned long: unsigned 32-bit
  FIT_INT32   = FREE_IMAGE_TYPE(5);  // array of long: signed 32-bit
  FIT_FLOAT   = FREE_IMAGE_TYPE(6);  // array of float: 32-bit IEEE floating point
  FIT_DOUBLE  = FREE_IMAGE_TYPE(7);  // array of double: 64-bit IEEE floating point
  FIT_COMPLEX = FREE_IMAGE_TYPE(8);  // array of FICOMPLEX: 2 x 64-bit IEEE floating point
  FIT_RGB16   = FREE_IMAGE_TYPE(9);  // 48-bit RGB image: 3 x 16-bit
  FIT_RGBA16  = FREE_IMAGE_TYPE(10); // 64-bit RGBA image: 4 x 16-bit
  FIT_RGBF    = FREE_IMAGE_TYPE(11); // 96-bit RGB float image: 3 x 32-bit IEEE floating point
  FIT_RGBAF   = FREE_IMAGE_TYPE(12); // 128-bit RGBA float image: 4 x 32-bit IEEE floating point

  // Image color type used in FreeImage.
  FIC_MINISWHITE = FREE_IMAGE_COLOR_TYPE(0); // min value is white
  FIC_MINISBLACK = FREE_IMAGE_COLOR_TYPE(1); // min value is black
  FIC_RGB        = FREE_IMAGE_COLOR_TYPE(2); // RGB color model
  FIC_PALETTE    = FREE_IMAGE_COLOR_TYPE(3); // color map indexed
  FIC_RGBALPHA   = FREE_IMAGE_COLOR_TYPE(4); // RGB color model with alpha channel
  FIC_CMYK       = FREE_IMAGE_COLOR_TYPE(5); // CMYK color model

  // Color quantization algorithms. Constants used in FreeImage_ColorQuantize.
  FIQ_WUQUANT = FREE_IMAGE_QUANTIZE(0); // Xiaolin Wu color quantization algorithm
  FIQ_NNQUANT = FREE_IMAGE_QUANTIZE(1); // NeuQuant neural-net quantization algorithm by Anthony Dekker
  FIQ_LFPQUANT = FREE_IMAGE_QUANTIZE(2); // Lossless Fast Pseudo-Quantization Algorithm by Carsten Klein

  // Dithering algorithms. Constants used FreeImage_Dither.
  FID_FS            = FREE_IMAGE_DITHER(0); // Floyd & Steinberg error diffusion
  FID_BAYER4x4      = FREE_IMAGE_DITHER(1); // Bayer ordered dispersed dot dithering (order 2 dithering matrix)
  FID_BAYER8x8      = FREE_IMAGE_DITHER(2); // Bayer ordered dispersed dot dithering (order 3 dithering matrix)
  FID_CLUSTER6x6    = FREE_IMAGE_DITHER(3); // Ordered clustered dot dithering (order 3 - 6x6 matrix)
  FID_CLUSTER8x8    = FREE_IMAGE_DITHER(4); // Ordered clustered dot dithering (order 4 - 8x8 matrix)
  FID_CLUSTER16x16  = FREE_IMAGE_DITHER(5); // Ordered clustered dot dithering (order 8 - 16x16 matrix)
  FID_BAYER16x16    = FREE_IMAGE_DITHER(6); // Bayer ordered dispersed dot dithering (order 4 dithering matrix)

  // Lossless JPEG transformations Constants used in FreeImage_JPEGTransform
  FIJPEG_OP_NONE        = FREE_IMAGE_JPEG_OPERATION(0); // no transformation
  FIJPEG_OP_FLIP_H      = FREE_IMAGE_JPEG_OPERATION(1); // horizontal flip
  FIJPEG_OP_FLIP_V      = FREE_IMAGE_JPEG_OPERATION(2); // vertical flip
  FIJPEG_OP_TRANSPOSE   = FREE_IMAGE_JPEG_OPERATION(3); // transpose across UL-to-LR axis
  FIJPEG_OP_TRANSVERSE  = FREE_IMAGE_JPEG_OPERATION(4); // transpose across UR-to-LL axis
  FIJPEG_OP_ROTATE_90   = FREE_IMAGE_JPEG_OPERATION(5); // 90-degree clockwise rotation
  FIJPEG_OP_ROTATE_180  = FREE_IMAGE_JPEG_OPERATION(6); // 180-degree rotation
  FIJPEG_OP_ROTATE_270  = FREE_IMAGE_JPEG_OPERATION(7); // 270-degree clockwise (or 90 ccw)

  // Tone mapping operators. Constants used in FreeImage_ToneMapping.
  FITMO_DRAGO03    = FREE_IMAGE_TMO(0); // Adaptive logarithmic mapping (F. Drago, 2003)
  FITMO_REINHARD05 = FREE_IMAGE_TMO(1); // Dynamic range reduction inspired by photoreceptor physiology (E. Reinhard, 2005)
  FITMO_FATTAL02   = FREE_IMAGE_TMO(2); // Gradient domain high dynamic range compression (R. Fattal, 2002)

  // Upsampling / downsampling filters. Constants used in FreeImage_Rescale.
  FILTER_BOX        = FREE_IMAGE_FILTER(0); // Box, pulse, Fourier window, 1st order (constant) b-spline
  FILTER_BICUBIC    = FREE_IMAGE_FILTER(1); // Mitchell & Netravali's two-param cubic filter
  FILTER_BILINEAR   = FREE_IMAGE_FILTER(2); // Bilinear filter
  FILTER_BSPLINE    = FREE_IMAGE_FILTER(3); // 4th order (cubic) b-spline
  FILTER_CATMULLROM = FREE_IMAGE_FILTER(4); // Catmull-Rom spline, Overhauser spline
  FILTER_LANCZOS3   = FREE_IMAGE_FILTER(5); // Lanczos3 filter

  // Color channels. Constants used in color manipulation routines.
  FICC_RGB   = FREE_IMAGE_COLOR_CHANNEL(0); // Use red, green and blue channels
  FICC_RED   = FREE_IMAGE_COLOR_CHANNEL(1); // Use red channel
  FICC_GREEN = FREE_IMAGE_COLOR_CHANNEL(2); // Use green channel
  FICC_BLUE  = FREE_IMAGE_COLOR_CHANNEL(3); // Use blue channel
  FICC_ALPHA = FREE_IMAGE_COLOR_CHANNEL(4); // Use alpha channel
  FICC_BLACK = FREE_IMAGE_COLOR_CHANNEL(5); // Use black channel
  FICC_REAL  = FREE_IMAGE_COLOR_CHANNEL(6); // Complex images: use real part
  FICC_IMAG  = FREE_IMAGE_COLOR_CHANNEL(7); // Complex images: use imaginary part
  FICC_MAG   = FREE_IMAGE_COLOR_CHANNEL(8); // Complex images: use magnitude
  FICC_PHASE = FREE_IMAGE_COLOR_CHANNEL(9); // Complex images: use phase

  // Tag data type information (based on TIFF specifications)
  FIDT_NOTYPE    = FREE_IMAGE_MDTYPE(0);  // placeholder
  FIDT_BYTE      = FREE_IMAGE_MDTYPE(1);  // 8-bit unsigned integer
  FIDT_ASCII     = FREE_IMAGE_MDTYPE(2);  // 8-bit bytes w/ last byte null
  FIDT_SHORT     = FREE_IMAGE_MDTYPE(3);  // 16-bit unsigned integer
  FIDT_LONG      = FREE_IMAGE_MDTYPE(4);  // 32-bit unsigned integer
  FIDT_RATIONAL  = FREE_IMAGE_MDTYPE(5);  // 64-bit unsigned fraction
  FIDT_SBYTE     = FREE_IMAGE_MDTYPE(6);  // 8-bit signed integer
  FIDT_UNDEFINED = FREE_IMAGE_MDTYPE(7);  // 8-bit untyped data
  FIDT_SSHORT    = FREE_IMAGE_MDTYPE(8);  // 16-bit signed integer
  FIDT_SLONG     = FREE_IMAGE_MDTYPE(9);  // 32-bit signed integer
  FIDT_SRATIONAL = FREE_IMAGE_MDTYPE(10); // 64-bit signed fraction
  FIDT_FLOAT     = FREE_IMAGE_MDTYPE(11); // 32-bit IEEE floating point
  FIDT_DOUBLE    = FREE_IMAGE_MDTYPE(12); // 64-bit IEEE floating point
  FIDT_IFD       = FREE_IMAGE_MDTYPE(13); // 32-bit unsigned integer (offset)
  FIDT_PALETTE   = FREE_IMAGE_MDTYPE(14); // 32-bit RGBQUAD
  FIDT_LONG8     = FREE_IMAGE_MDTYPE(16); // 64-bit unsigned integer
  FIDT_SLONG8    = FREE_IMAGE_MDTYPE(17); // 64-bit signed integer
  FIDT_IFD8      = FREE_IMAGE_MDTYPE(18); // 64-bit unsigned integer (offset)

  // Metadata models supported by FreeImage
  FIMD_NODATA         = FREE_IMAGE_MDMODEL(-1);
  FIMD_COMMENTS       = FREE_IMAGE_MDMODEL(0);  // single comment or keywords
  FIMD_EXIF_MAIN      = FREE_IMAGE_MDMODEL(1);  // Exif-TIFF metadata
  FIMD_EXIF_EXIF      = FREE_IMAGE_MDMODEL(2);  // Exif-specific metadata
  FIMD_EXIF_GPS       = FREE_IMAGE_MDMODEL(3);  // Exif GPS metadata
  FIMD_EXIF_MAKERNOTE = FREE_IMAGE_MDMODEL(4);  // Exif maker note metadata
  FIMD_EXIF_INTEROP   = FREE_IMAGE_MDMODEL(5);  // Exif interoperability metadata
  FIMD_IPTC           = FREE_IMAGE_MDMODEL(6);  // IPTC/NAA metadata
  FIMD_XMP            = FREE_IMAGE_MDMODEL(7);  // Abobe XMP metadata
  FIMD_GEOTIFF        = FREE_IMAGE_MDMODEL(8);  // GeoTIFF metadata (to be implemented)
  FIMD_ANIMATION      = FREE_IMAGE_MDMODEL(9);  // Animation metadata
  FIMD_CUSTOM         = FREE_IMAGE_MDMODEL(10); // Used to attach other metadata types to a dib
  FIMD_EXIF_RAW       = FREE_IMAGE_MDMODEL(11); // Exif metadata as a raw buffer

type
  // Handle to a metadata model
  FIMETADATA = record
    data: Pointer;
  end;
  PFIMETADATA = ^FIMETADATA;

  // Handle to a metadata tag
  FITAG = record
    data: Pointer;
  end;
  PFITAG = ^FITAG;

// --------------------------------------------------------------------------
// File IO routines ---------------------------------------------------------
// --------------------------------------------------------------------------

type
  fi_handle = Pointer;

  FI_ReadProc = function(buffer: Pointer; size, count: Cardinal;
    handle: fi_handle): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_WriteProc = function(buffer: Pointer; size, count: Cardinal;
    handle: fi_handle): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_SeekProc = function(handle: fi_handle; offset: LongInt;
    origin: Integer): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_TellProc = function(handle: fi_handle): LongInt; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  FreeImageIO = packed record
    read_proc : FI_ReadProc;     // pointer to the function used to read data
    write_proc: FI_WriteProc;    // pointer to the function used to write data
    seek_proc : FI_SeekProc;     // pointer to the function used to seek
    tell_proc : FI_TellProc;     // pointer to the function used to aquire the current position
  end;
  PFreeImageIO = ^FreeImageIO;

  // Handle to a memory I/O stream
  FIMEMORY = record
    data: Pointer;
  end;
  PFIMEMORY = ^FIMEMORY;

const
  // constants used in FreeImage_Seek for Origin parameter
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;

//type
  // define portable types for 32-bit / 64-bit OS
  //FIINT64 = Int64;
  //FIUINT64 = UInt64;

// --------------------------------------------------------------------------
// Plugin routines ----------------------------------------------------------
// --------------------------------------------------------------------------

type
  PPlugin = ^Plugin;

  FI_FormatProc = function: PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_DescriptionProc = function: PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_ExtensionListProc = function: PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_RegExprProc = function: PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_OpenProc = function(io: PFreeImageIO; handle: fi_handle;
    read: LongBool): Pointer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_CloseProc = procedure(io: PFreeImageIO; handle: fi_handle;
    data: Pointer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_PageCountProc = function(io: PFreeImageIO; handle: fi_handle;
    data: Pointer): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_PageCapabilityProc = function(io: PFreeImageIO; handle: fi_handle;
    data: Pointer): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_LoadProc = function(io: PFreeImageIO; handle: fi_handle; page, flags: Integer;
    data: Pointer): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_SaveProc = function(io: PFreeImageIO; dib: PFIBITMAP; handle: fi_handle;
    page, flags: Integer; data: Pointer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_ValidateProc = function(io: PFreeImageIO; handle: fi_handle): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_MimeProc = function: PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_SupportsExportBPPProc = function(bpp: integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_SupportsExportTypeProc = function(_type: FREE_IMAGE_TYPE): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_SupportsICCProfilesProc = function: LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FI_SupportsNoPixelsProc = function: LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  Plugin = packed record
    format_proc: FI_FormatProc;
    description_proc: FI_DescriptionProc;
    extension_proc: FI_ExtensionListProc;
    regexpr_proc: FI_RegExprProc;
    open_proc: FI_OpenProc;
    close_proc: FI_CloseProc;
    pagecount_proc: FI_PageCountProc;
    pagecapability_proc: FI_PageCapabilityProc;
    load_proc: FI_LoadProc;
    save_proc: FI_SaveProc;
    validate_proc: FI_ValidateProc;
    mime_proc: FI_MimeProc;
    supports_export_bpp_proc: FI_SupportsExportBPPProc;
    supports_export_type_proc: FI_SupportsExportTypeProc;
    supports_icc_profiles_proc: FI_SupportsICCProfilesProc;
    supports_no_pixels_proc: FI_SupportsNoPixelsProc;
  end;

  FI_InitProc = procedure(aplugin: PPlugin; format_id: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// --------------------------------------------------------------------------
// Load/Save flag constants -------------------------------------------------
// --------------------------------------------------------------------------

const
  FIF_LOAD_NOPIXELS   = $8000;  //! loading: load the image header only (not supported by all plugins, default to full loading)
  BMP_DEFAULT         = 0;
  BMP_SAVE_RLE        = 1;
  CUT_DEFAULT         = 0;
  DDS_DEFAULT         = 0;
  EXR_DEFAULT         = 0;      //! save data as half with piz-based wavelet compression
  EXR_FLOAT           = $0001;  //! save data as float instead of as half (not recommended)
  EXR_NONE            = $0002;  //! save with no compression
  EXR_ZIP             = $0004;  //! save with zlib compression, in blocks of 16 scan lines
  EXR_PIZ             = $0008;  //! save with piz-based wavelet compression
  EXR_PXR24           = $0010;  //! save with lossy 24-bit float compression
  EXR_B44             = $0020;  //! save with lossy 44% float compression - goes to 22% when combined with EXR_LC
  EXR_LC              = $0040;  //! save images with one luminance and two chroma channels, rather than as RGB (lossy compression)
  FAXG3_DEFAULT       = 0;
  GIF_DEFAULT         = 0;
  GIF_LOAD256         = 1;     //! Load the image as a 256 color image with ununsed palette entries, if it's 16 or 2 color
  GIF_PLAYBACK        = 2;     //! 'Play' the GIF to generate each frame (as 32bpp) instead of returning raw frame data when loading
  HDR_DEFAULT         = 0;
  ICO_DEFAULT         = 0;
  ICO_MAKEALPHA       = 1;     //! convert to 32bpp and create an alpha channel from the AND-mask when loading
  IFF_DEFAULT         = 0;
  J2K_DEFAULT         = 0;     //! save with a 16:1 rate
  JP2_DEFAULT         = 0;     //! save with a 16:1 rate
  JPEG_DEFAULT        = 0;     //! loading (see JPEG_FAST); saving (see JPEG_QUALITYGOOD|JPEG_SUBSAMPLING_420)
  JPEG_FAST           = 1;     //! load the file as fast as possible, sacrificing some quality
  JPEG_ACCURATE       = 2;     //! load the file with the best quality, sacrificing some speed
  JPEG_CMYK           = $0004; //! load separated CMYK "as is" (use | to combine with other flags)
  JPEG_EXIFROTATE     = $0008; //! load and rotate according to Exif 'Orientation' tag if available
  JPEG_GREYSCALE      = $0010; //! load and convert to a 8-bit greyscale image
  JPEG_QUALITYSUPERB  = $0080; //! save with superb quality (100:1)
  JPEG_QUALITYGOOD    = $0100; //! save with good quality (75:1)
  JPEG_QUALITYNORMAL  = $0200; //! save with normal quality (50:1)
  JPEG_QUALITYAVERAGE = $0400; //! save with average quality (25:1)
  JPEG_QUALITYBAD     = $0800; //! save with bad quality (10:1)
  JPEG_PROGRESSIVE    = $2000; //! save as a progressive-JPEG (use | to combine with other save flags)
  JPEG_SUBSAMPLING_411 = $1000;  //! save with high 4x1 chroma subsampling (4:1:1)
  JPEG_SUBSAMPLING_420 = $4000;  //! save with medium 2x2 medium chroma subsampling (4:2:0) - default value
  JPEG_SUBSAMPLING_422 = $8000;  //! save with low 2x1 chroma subsampling (4:2:2)
  JPEG_SUBSAMPLING_444 = $10000; //! save with no chroma subsampling (4:4:4)
  JPEG_OPTIMIZE       = $20000; //! on saving, compute optimal Huffman coding tables (can reduce a few percent of file size)
  JPEG_BASELINE       = $40000; //! save basic JPEG, without metadata or any markers
  KOALA_DEFAULT       = 0;
  LBM_DEFAULT         = 0;
  MNG_DEFAULT         = 0;
  PCD_DEFAULT         = 0;
  PCD_BASE            = 1;     //! load the bitmap sized 768 x 512
  PCD_BASEDIV4        = 2;     //! load the bitmap sized 384 x 256
  PCD_BASEDIV16       = 3;     //! load the bitmap sized 192 x 128
  PCX_DEFAULT         = 0;
  PFM_DEFAULT         = 0;
  PICT_DEFAULT        = 0;
  PNG_DEFAULT         = 0;
  PNG_IGNOREGAMMA     = 1;     //! loading: avoid gamma correction
  PNG_Z_BEST_SPEED          = $0001; //! save using ZLib level 1 compression flag (default value is 6)
  PNG_Z_DEFAULT_COMPRESSION = $0006; //! save using ZLib level 6 compression flag (default recommended value)
  PNG_Z_BEST_COMPRESSION    = $0009; //! save using ZLib level 9 compression flag (default value is 6)
  PNG_Z_NO_COMPRESSION      = $0100; //! save without ZLib compression
  PNG_INTERLACED            = $0200; //! save using Adam7 interlacing (use | to combine with other save flags)
  PNM_DEFAULT         = 0;
  PNM_SAVE_RAW        = 0;     //! if set the writer saves in RAW format (i.e. P4, P5 or P6)
  PNM_SAVE_ASCII      = 1;     //! if set the writer saves in ASCII format (i.e. P1, P2 or P3)
  PSD_DEFAULT         = 0;
  PSD_CMYK            = 1; //! reads tags for separated CMYK (default is conversion to RGB)
  PSD_LAB             = 2; //! reads tags for CIELab (default is conversion to RGB)
  PSD_NONE            = $0100; //! save without any compression
  PSD_RLE             = $0200; //! save using RLE compression
  PSD_PSB             = $2000; //! save using Adobe Large Document Format (use | to combine with other save flags)
  RAS_DEFAULT         = 0;
  RAW_DEFAULT         = 0; //! load the file as linear RGB 48-bit
  RAW_PREVIEW         = 1; //! try to load the embedded JPEG preview with included Exif Data or default to RGB 24-bit
  RAW_DISPLAY         = 2; //! load the file as RGB 24-bit
  RAW_HALFSIZE        = 4; //! output a half-size color image
  RAW_UNPROCESSED     = 8; //! output a FIT_UINT16 raw Bayer image
  SGI_DEFAULT         = 0;
  TARGA_DEFAULT       = 0;
  TARGA_LOAD_RGB888   = 1;     //! if set the loader converts RGB555 and ARGB8888 -> RGB888.
  TARGA_SAVE_RLE      = 2;     //! if set, the writer saves with RLE compression
  TIFF_DEFAULT        = 0;
  TIFF_CMYK           = $0001;  //! reads/stores tags for separated CMYK (use | to combine with compression flags)
  TIFF_PACKBITS       = $0100;  //! save using PACKBITS compression
  TIFF_DEFLATE        = $0200;  //! save using DEFLATE compression
  TIFF_ADOBE_DEFLATE  = $0400;  //! save using ADOBE DEFLATE compression
  TIFF_NONE           = $0800;  //! save without any compression
  TIFF_CCITTFAX3      = $1000;  //! save using CCITT Group 3 fax encoding
  TIFF_CCITTFAX4      = $2000;  //! save using CCITT Group 4 fax encoding
  TIFF_LZW            = $4000;  //! save using LZW compression
  TIFF_JPEG           = $8000;  //! save using JPEG compression
  TIFF_LOGLUV         = $10000; //! save using LogLuv compression
  WBMP_DEFAULT        = 0;
  XBM_DEFAULT         = 0;
  XPM_DEFAULT         = 0;
  WEBP_DEFAULT        = 0;      //! save with good quality (75:1)
  WEBP_LOSSLESS       = $100;   //! save in lossless mode
  JXR_DEFAULT         = 0;      //! save with quality 80 and no chroma subsampling (4:4:4)
  JXR_LOSSLESS        = $0064;  //! save lossless
  JXR_PROGRESSIVE     = $2000;  //! save as a progressive-JXR (use | to combine with other save flags)

// --------------------------------------------------------------------------
// Background filling options -----------------------------------------------
// Constants used in FreeImage_FillBackground and FreeImage_EnlargeCanvas
// --------------------------------------------------------------------------

const
  FI_COLOR_IS_RGB_COLOR         = $00; // RGBQUAD color is a RGB color (contains no valid alpha channel)
  FI_COLOR_IS_RGBA_COLOR        = $01; // RGBQUAD color is a RGBA color (contains a valid alpha channel)
  FI_COLOR_FIND_EQUAL_COLOR     = $02; // For palettized images: lookup equal RGB color from palette
  FI_COLOR_ALPHA_IS_INDEX       = $04; // The color's rgbReserved member (alpha) contains the palette index to be used
  FI_COLOR_PALETTE_SEARCH_MASK  = FI_COLOR_FIND_EQUAL_COLOR or FI_COLOR_ALPHA_IS_INDEX; // No color lookup is performed

// RescaleEx options ---------------------------------------------------------
// Constants used in FreeImage_RescaleEx

  FI_RESCALE_DEFAULT            = $00;  //! default options; none of the following other options apply
  FI_RESCALE_TRUE_COLOR         = $01;  //! for non-transparent greyscale images, convert to 24-bit if src bitdepth <= 8 (default is a 8-bit greyscale image).
  FI_RESCALE_OMIT_METADATA      = $02;  //! do not copy metadata to the rescaled image

{$IFDEF FREEIMAGE_STATIC_LINK}
// --------------------------------------------------------------------------
// Init/Error routines ------------------------------------------------------
// --------------------------------------------------------------------------

procedure FreeImage_Initialise(load_local_plugins_only: LongBool = False); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Initialise@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Initialise'{$ENDIF};
procedure FreeImage_DeInitialise; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_DeInitialise@0'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_DeInitialise'{$ENDIF};

// --------------------------------------------------------------------------
// Version routines ---------------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetVersion: PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetVersion@0'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetVersion'{$ENDIF};
function FreeImage_GetCopyrightMessage: PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetCopyrightMessage@0'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetCopyrightMessage'{$ENDIF};

// --------------------------------------------------------------------------
// Message output functions -------------------------------------------------
// --------------------------------------------------------------------------

type
  FreeImage_OutputMessageFunction = procedure(fif: FREE_IMAGE_FORMAT;
    msg: PAnsiChar); cdecl;
  FreeImage_OutputMessageFunctionStdCall = procedure(fif: FREE_IMAGE_FORMAT;
    msg: PAnsiChar); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

procedure FreeImage_SetOutputMessageStdCall(omf: FreeImage_OutputMessageFunctionStdCall); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetOutputMessageStdCall@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetOutputMessageStdCall'{$ENDIF};
procedure FreeImage_SetOutputMessage(omf: FreeImage_OutputMessageFunction); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetOutputMessage@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetOutputMessage'{$ENDIF};

{$IFDEF DELPHI6}
//this is declared stdcall in the C header but it is actually cdecl.
//with varargs functions, clearing the stack is caller's responsibility
//(since the callee doesn't know how many parameters were passed).
//cdecl is the right convention here, not stdcall
procedure FreeImage_OutputMessageProc(fif: Integer; fmt: PAnsiChar); cdecl; varargs;
  external FIDLL;
{$ELSE}
//older Delphi versions (<6) do not support varargs.
//we provide a wrapper that uses open arrays instead
procedure FreeImage_OutputMessageProc(fif: Integer; fmt: PAnsiChar; args: array of const);
{$ENDIF}

// --------------------------------------------------------------------------
// Allocate / Clone / Unload routines ---------------------------------------
// --------------------------------------------------------------------------

function FreeImage_Allocate(width, height, bpp: Integer; red_mask: Cardinal = 0;
  green_mask: Cardinal = 0; blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Allocate@24'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Allocate'{$ENDIF};
function FreeImage_AllocateT(_type: FREE_IMAGE_TYPE; width, height: Integer;
  bpp: Integer = 8; red_mask: Cardinal = 0; green_mask: Cardinal = 0;
  blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AllocateT@28'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AllocateT'{$ENDIF};
function FreeImage_Clone(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Clone@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Clone'{$ENDIF};
procedure FreeImage_Unload(dib: PFIBITMAP); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Unload@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Unload'{$ENDIF};

// --------------------------------------------------------------------------
// Header loading routines
// --------------------------------------------------------------------------
function FreeImage_HasPixels(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_HasPixels@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_HasPixels'{$ENDIF};

// --------------------------------------------------------------------------
// Load / Save routines -----------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_Load(fif: FREE_IMAGE_FORMAT; filename: PAnsiChar;
  flags: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Load@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Load'{$ENDIF};
function FreeImage_LoadU(fif: FREE_IMAGE_FORMAT; filename: PWideChar;
  flags: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_LoadU@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_LoadU'{$ENDIF};
function FreeImage_LoadFromHandle(fif: FREE_IMAGE_FORMAT; io: PFreeImageIO;
  handle: fi_handle; flags: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_LoadFromHandle@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_LoadFromHandle'{$ENDIF};
function FreeImage_Save(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP; filename: PAnsiChar;
  flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Save@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Save'{$ENDIF};
function FreeImage_SaveU(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP; filename: PWideChar;
  flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SaveU@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SaveU'{$ENDIF};
function FreeImage_SaveToHandle(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP;
  io: PFreeImageIO; handle: fi_handle; flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SaveToHandle@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SaveToHandle'{$ENDIF};

// --------------------------------------------------------------------------
// Memory I/O stream routines -----------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_OpenMemory(data: PByte = nil; size_in_bytes: DWORD = 0): PFIMEMORY; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_OpenMemory@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_OpenMemory'{$ENDIF};
procedure FreeImage_CloseMemory(stream: PFIMEMORY); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_CloseMemory@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_CloseMemory'{$ENDIF};
function FreeImage_LoadFromMemory(fif: FREE_IMAGE_FORMAT; stream: PFIMEMORY;
  flags: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_LoadFromMemory@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_LoadFromMemory'{$ENDIF};
function FreeImage_SaveToMemory(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP;
  stream: PFIMEMORY; flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SaveToMemory@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SaveToMemory'{$ENDIF};
function FreeImage_TellMemory(stream: PFIMEMORY): LongInt; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_TellMemory@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_TellMemory'{$ENDIF};
function FreeImage_SeekMemory(stream: PFIMEMORY; offset: LongInt;
  origin: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SeekMemory@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SeekMemory'{$ENDIF};
function FreeImage_AcquireMemory(stream: PFIMEMORY; var data: PByte;
  var size_in_bytes: DWORD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AcquireMemory@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AcquireMemory'{$ENDIF};
function FreeImage_ReadMemory(buffer: Pointer; size, count: Cardinal;
  stream: PFIMEMORY): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ReadMemory@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ReadMemory'{$ENDIF};
function FreeImage_WriteMemory(buffer: Pointer; size, count: Cardinal;
  stream: PFIMEMORY): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_WriteMemory@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_WriteMemory'{$ENDIF};
function FreeImage_LoadMultiBitmapFromMemory(fif: FREE_IMAGE_FORMAT; stream: PFIMEMORY;
  flags: Integer = 0): PFIMULTIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_LoadMultiBitmapFromMemory@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_LoadMultiBitmapFromMemory'{$ENDIF};
function FreeImage_SaveMultiBitmapToMemory(fif: FREE_IMAGE_FORMAT; bitmap: PFIMULTIBITMAP;
  stream: PFIMEMORY; flags: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SaveMultiBitmapToMemory@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SaveMultiBitmapToMemory'{$ENDIF};

// --------------------------------------------------------------------------
// Plugin Interface ---------------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_RegisterLocalPlugin(proc_address: FI_InitProc; format: PAnsiChar = nil;
  description: PAnsiChar = nil; extension: PAnsiChar = nil;
  regexpr: PAnsiChar = nil): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_RegisterLocalPlugin@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_RegisterLocalPlugin'{$ENDIF};
function FreeImage_RegisterExternalPlugin(path: PAnsiChar; format: PAnsiChar = nil;
  description: PAnsiChar = nil; extension: PAnsiChar = nil;
  regexpr: PAnsiChar = nil): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_RegisterExternalPlugin@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_RegisterExternalPlugin'{$ENDIF};
function FreeImage_GetFIFCount: Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFIFCount@0'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFIFCount'{$ENDIF};
procedure FreeImage_SetPluginEnabled(fif: FREE_IMAGE_FORMAT; enable: LongBool); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetPluginEnabled@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetPluginEnabled'{$ENDIF};
function FreeImage_IsPluginEnabled(fif: FREE_IMAGE_FORMAT): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_IsPluginEnabled@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_IsPluginEnabled'{$ENDIF};
function FreeImage_GetFIFFromFormat(format: PAnsiChar): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFIFFromFormat@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFIFFromFormat'{$ENDIF};
function FreeImage_GetFIFFromMime(mime: PAnsiChar): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFIFFromMime@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFIFFromMime'{$ENDIF};
function FreeImage_GetFormatFromFIF(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFormatFromFIF@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFormatFromFIF'{$ENDIF};
function FreeImage_GetFIFExtensionList(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFIFExtensionList@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFIFExtensionList'{$ENDIF};
function FreeImage_GetFIFDescription(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFIFDescription@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFIFDescription'{$ENDIF};
function FreeImage_GetFIFRegExpr(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFIFRegExpr@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFIFRegExpr'{$ENDIF};
function FreeImage_GetFIFMimeType(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFIFMimeType@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFIFMimeType'{$ENDIF};
function FreeImage_GetFIFFromFilename(filename: PAnsiChar): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFIFFromFilename@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFIFFromFilename'{$ENDIF};
function FreeImage_GetFIFFromFilenameU(filename: PWideChar): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFIFFromFilenameU@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFIFFromFilenameU'{$ENDIF};
function FreeImage_FIFSupportsReading(fif: FREE_IMAGE_FORMAT): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FIFSupportsReading@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FIFSupportsReading'{$ENDIF};
function FreeImage_FIFSupportsWriting(fif: FREE_IMAGE_FORMAT): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FIFSupportsWriting@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FIFSupportsWriting'{$ENDIF};
function FreeImage_FIFSupportsExportBPP(fif: FREE_IMAGE_FORMAT;
  bpp: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FIFSupportsExportBPP@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FIFSupportsExportBPP'{$ENDIF};
function FreeImage_FIFSupportsExportType(fif: FREE_IMAGE_FORMAT;
  _type: FREE_IMAGE_TYPE): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FIFSupportsExportType@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FIFSupportsExportType'{$ENDIF};
function FreeImage_FIFSupportsICCProfiles(fif: FREE_IMAGE_FORMAT): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FIFSupportsICCProfiles@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FIFSupportsICCProfiles'{$ENDIF};
function FreeImage_FIFSupportsNoPixels(fif: FREE_IMAGE_FORMAT): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FIFSupportsNoPixels@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FIFSupportsNoPixels'{$ENDIF};

// --------------------------------------------------------------------------
// Multipaging interface ----------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_OpenMultiBitmap(fif: FREE_IMAGE_FORMAT; filename: PAnsiChar;
  create_new, read_only: LongBool; keep_cache_in_memory: LongBool = False;
  flags: Integer = 0): PFIMULTIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_OpenMultiBitmap@24'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_OpenMultiBitmap'{$ENDIF};
function FreeImage_OpenMultiBitmapFromHandle(fif: FREE_IMAGE_FORMAT; io: PFreeImageIO;
  handle: fi_handle; flags: Integer = 0): PFIMULTIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_OpenMultiBitmapFromHandle@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_OpenMultiBitmapFromHandle'{$ENDIF};
function FreeImage_SaveMultiBitmapToHandle(fif: FREE_IMAGE_FORMAT; bitmap: PFIMULTIBITMAP;
  io: PFreeImageIO; handle: fi_handle; flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SaveMultiBitmapToHandle@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SaveMultiBitmapToHandle'{$ENDIF};
function FreeImage_CloseMultiBitmap(bitmap: PFIMULTIBITMAP;
  flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_CloseMultiBitmap@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_CloseMultiBitmap'{$ENDIF};
function FreeImage_GetPageCount(bitmap: PFIMULTIBITMAP): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetPageCount@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetPageCount'{$ENDIF};
procedure FreeImage_AppendPage(bitmap: PFIMULTIBITMAP; data: PFIBITMAP); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AppendPage@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AppendPage'{$ENDIF};
procedure FreeImage_InsertPage(bitmap: PFIMULTIBITMAP; page: Integer;
  data: PFIBITMAP); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_InsertPage@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_InsertPage'{$ENDIF};
procedure FreeImage_DeletePage(bitmap: PFIMULTIBITMAP; page: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_DeletePage@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_DeletePage'{$ENDIF};
function FreeImage_LockPage(bitmap: PFIMULTIBITMAP; page: Integer): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_LockPage@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_LockPage'{$ENDIF};
procedure FreeImage_UnlockPage(bitmap: PFIMULTIBITMAP; data: PFIBITMAP;
  changed: LongBool); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_UnlockPage@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_UnlockPage'{$ENDIF};
function FreeImage_MovePage(bitmap: PFIMULTIBITMAP; target, source: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_MovePage@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_MovePage'{$ENDIF};
function FreeImage_GetLockedPageNumbers(bitmap: PFIMULTIBITMAP; var pages: Integer;
  var count: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetLockedPageNumbers@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetLockedPageNumbers'{$ENDIF};

// --------------------------------------------------------------------------
// Filetype request routines ------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetFileType(filename: PAnsiChar;
  size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFileType@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFileType'{$ENDIF};
function FreeImage_GetFileTypeU(filename: PWideChar;
  size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFileTypeU@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFileTypeU'{$ENDIF};
function FreeImage_GetFileTypeFromHandle(io: PFreeImageIO; handle: FI_Handle;
  size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFileTypeFromHandle@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFileTypeFromHandle'{$ENDIF};
function FreeImage_GetFileTypeFromMemory(stream: PFIMEMORY;
  size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetFileTypeFromMemory@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetFileTypeFromMemory'{$ENDIF};

// --------------------------------------------------------------------------
// ImageType request routine ------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetImageType(dib: PFIBITMAP): FREE_IMAGE_TYPE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetImageType@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetImageType'{$ENDIF};

// --------------------------------------------------------------------------
// FreeImage helper routines ------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_IsLittleEndian: LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_IsLittleEndian@0'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_IsLittleEndian'{$ENDIF};
function FreeImage_LookupX11Color(szColor: PAnsiChar; var nRed, nGreen, nBlue: Byte): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_LookupX11Color@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_LookupX11Color'{$ENDIF};
function FreeImage_LookupSVGColor(szColor: PAnsiChar; var nRed, nGreen, nBlue: Byte): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_LookupSVGColor@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_LookupSVGColor'{$ENDIF};

// --------------------------------------------------------------------------
// Pixels access routines ---------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetBits(dib: PFIBITMAP): PByte; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetBits@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetBits'{$ENDIF};
function FreeImage_GetScanLine(dib: PFIBITMAP; scanline: Integer): PByte; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetScanLine@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetScanLine'{$ENDIF};

function FreeImage_GetPixelIndex(dib: PFIBITMAP; x, y: Cardinal; var value: Byte): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetPixelIndex@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetPixelIndex'{$ENDIF};
function FreeImage_GetPixelColor(dib: PFIBITMAP; x, y: Cardinal; var value: RGBQUAD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetPixelColor@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetPixelColor'{$ENDIF};
function FreeImage_SetPixelIndex(dib: PFIBITMAP; x, y: Cardinal; var value: Byte): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetPixelIndex@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetPixelIndex'{$ENDIF};
function FreeImage_SetPixelColor(dib: PFIBITMAP; x, y: Cardinal; var value: RGBQUAD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetPixelColor@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetPixelColor'{$ENDIF};

// --------------------------------------------------------------------------
// DIB info routines --------------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetColorsUsed(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetColorsUsed@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetColorsUsed'{$ENDIF};
function FreeImage_GetBPP(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetBPP@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetBPP'{$ENDIF};
function FreeImage_GetWidth(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetWidth@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetWidth'{$ENDIF};
function FreeImage_GetHeight(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetHeight@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetHeight'{$ENDIF};
function FreeImage_GetLine(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetLine@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetLine'{$ENDIF};
function FreeImage_GetPitch(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetPitch@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetPitch'{$ENDIF};
function FreeImage_GetDIBSize(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetDIBSize@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetDIBSize'{$ENDIF};
function FreeImage_GetMemorySize(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetMemorySize@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetMemorySize'{$ENDIF};
function FreeImage_GetPalette(dib: PFIBITMAP): PRGBQuad; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetPalette@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetPalette'{$ENDIF};

function FreeImage_GetDotsPerMeterX(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetDotsPerMeterX@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetDotsPerMeterX'{$ENDIF};
function FreeImage_GetDotsPerMeterY(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetDotsPerMeterY@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetDotsPerMeterY'{$ENDIF};
procedure FreeImage_SetDotsPerMeterX(dib: PFIBITMAP; res: Cardinal); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetDotsPerMeterX@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetDotsPerMeterX'{$ENDIF};
procedure FreeImage_SetDotsPerMeterY(dib: PFIBITMAP; res: Cardinal); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetDotsPerMeterY@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetDotsPerMeterY'{$ENDIF};

function FreeImage_GetInfoHeader(dib: PFIBITMAP): PBITMAPINFOHEADER; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetInfoHeader@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetInfoHeader'{$ENDIF};
function FreeImage_GetInfo(dib: PFIBITMAP): PBITMAPINFO; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetInfo@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetInfo'{$ENDIF};
function FreeImage_GetColorType(dib: PFIBITMAP): FREE_IMAGE_COLOR_TYPE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetColorType@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetColorType'{$ENDIF};

function FreeImage_GetRedMask(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetRedMask@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetRedMask'{$ENDIF};
function FreeImage_GetGreenMask(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetGreenMask@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetGreenMask'{$ENDIF};
function FreeImage_GetBlueMask(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetBlueMask@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetBlueMask'{$ENDIF};

function FreeImage_GetTransparencyCount(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTransparencyCount@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTransparencyCount'{$ENDIF};
function FreeImage_GetTransparencyTable(dib: PFIBITMAP): PByte; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTransparencyTable@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTransparencyTable'{$ENDIF};
procedure FreeImage_SetTransparent(dib: PFIBITMAP; enabled: LongBool); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTransparent@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTransparent'{$ENDIF};
procedure FreeImage_SetTransparencyTable(dib: PFIBITMAP; table: PByte;
  count: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTransparencyTable@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTransparencyTable'{$ENDIF};
function FreeImage_IsTransparent(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_IsTransparent@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_IsTransparent'{$ENDIF};
procedure FreeImage_SetTransparentIndex(dib: PFIBITMAP; index: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTransparentIndex@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTransparentIndex'{$ENDIF};
function FreeImage_GetTransparentIndex(dib: PFIBITMAP): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTransparentIndex@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTransparentIndex'{$ENDIF};

function FreeImage_HasBackgroundColor(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_HasBackgroundColor@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_HasBackgroundColor'{$ENDIF};
function FreeImage_GetBackgroundColor(dib: PFIBITMAP; var bkcolor: RGBQUAD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetBackgroundColor@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetBackgroundColor'{$ENDIF};
function FreeImage_SetBackgroundColor(dib: PFIBITMAP; bkcolor: PRGBQuad): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetBackgroundColor@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetBackgroundColor'{$ENDIF};

function FreeImage_GetThumbnail(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetThumbnail@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetThumbnail'{$ENDIF};
function FreeImage_SetThumbnail(dib, thumbnail: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetThumbnail@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetThumbnail'{$ENDIF};

// --------------------------------------------------------------------------
// ICC profile routines -----------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetICCProfile(dib: PFIBITMAP): PFIICCPROFILE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetICCProfile@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetICCProfile'{$ENDIF};
function FreeImage_CreateICCProfile(dib: PFIBITMAP; data: Pointer;
  size: LongInt): PFIICCPROFILE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name 'FreeImage_CreateICCProfile@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name 'FreeImage_CreateICCProfile'{$ENDIF};
procedure FreeImage_DestroyICCProfile(dib: PFIBITMAP); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name 'FreeImage_DestroyICCProfile@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name 'FreeImage_DestroyICCProfile'{$ENDIF};

// --------------------------------------------------------------------------
// Line conversion routines -------------------------------------------------
// --------------------------------------------------------------------------

procedure FreeImage_ConvertLine1To4(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine1To4@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine1To4'{$ENDIF};
procedure FreeImage_ConvertLine8To4(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine8To4@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine8To4'{$ENDIF};
procedure FreeImage_ConvertLine16To4_555(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16To4_555@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16To4_555'{$ENDIF};
procedure FreeImage_ConvertLine16To4_565(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16To4_565@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16To4_565'{$ENDIF};
procedure FreeImage_ConvertLine24To4(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine24To4@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine24To4'{$ENDIF};
procedure FreeImage_ConvertLine32To4(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine32To4@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine32To4'{$ENDIF};

procedure FreeImage_ConvertLine1To8(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine1To8@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine1To8'{$ENDIF};
procedure FreeImage_ConvertLine4To8(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine4To8@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine4To8'{$ENDIF};
procedure FreeImage_ConvertLine16To8_555(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16To8_555@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16To8_555'{$ENDIF};
procedure FreeImage_ConvertLine16To8_565(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16To8_565@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16To8_565'{$ENDIF};
procedure FreeImage_ConvertLine24To8(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine24To8@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine24To8'{$ENDIF};
procedure FreeImage_ConvertLine32To8(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine32To8@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine32To8'{$ENDIF};

procedure FreeImage_ConvertLine1To16_555(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine1To16_555@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine1To16_555'{$ENDIF};
procedure FreeImage_ConvertLine4To16_555(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine4To16_555@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine4To16_555'{$ENDIF};
procedure FreeImage_ConvertLine8To16_555(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine8To16_555@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine8To16_555'{$ENDIF};
procedure FreeImage_ConvertLine16_565_To16_555(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16_565_To16_555@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16_565_To16_555'{$ENDIF};
procedure FreeImage_ConvertLine24To16_555(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine24To16_555@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine24To16_555'{$ENDIF};
procedure FreeImage_ConvertLine32To16_555(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine32To16_555@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine32To16_555'{$ENDIF};

procedure FreeImage_ConvertLine1To16_565(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine1To16_565@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine1To16_565'{$ENDIF};
procedure FreeImage_ConvertLine4To16_565(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine4To16_565@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine4To16_565'{$ENDIF};
procedure FreeImage_ConvertLine8To16_565(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine8To16_565@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine8To16_565'{$ENDIF};
procedure FreeImage_ConvertLine16_555_To16_565(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16_555_To16_565@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16_555_To16_565'{$ENDIF};
procedure FreeImage_ConvertLine24To16_565(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine24To16_565@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine24To16_565'{$ENDIF};
procedure FreeImage_ConvertLine32To16_565(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine32To16_565@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine32To16_565'{$ENDIF};

procedure FreeImage_ConvertLine1To24(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine1To24@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine1To24'{$ENDIF};
procedure FreeImage_ConvertLine4To24(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine4To24@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine4To24'{$ENDIF};
procedure FreeImage_ConvertLine8To24(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine8To24@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine8To24'{$ENDIF};
procedure FreeImage_ConvertLine16To24_555(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16To24_555@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16To24_555'{$ENDIF};
procedure FreeImage_ConvertLine16To24_565(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16To24_565@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16To24_565'{$ENDIF};
procedure FreeImage_ConvertLine32To24(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine32To24@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine32To24'{$ENDIF};

procedure FreeImage_ConvertLine1To32(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine1To32@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine1To32'{$ENDIF};
procedure FreeImage_ConvertLine1To32MapTransparency(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad; table: PByte; transparent_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine1To32MapTransparency@24'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine1To32MapTransparency'{$ENDIF};
procedure FreeImage_ConvertLine4To32(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine4To32@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine4To32'{$ENDIF};
procedure FreeImage_ConvertLine4To32MapTransparency(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad; table: PByte; transparent_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine4To32MapTransparency@24'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine4To32MapTransparency'{$ENDIF};
procedure FreeImage_ConvertLine8To32(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine8To32@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine8To32'{$ENDIF};
procedure FreeImage_ConvertLine8To32MapTransparency(target, source: PByte; width_in_pixels: Integer;
  palette: PRGBQuad; table: PByte; transparent_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine8To32MapTransparency@24'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine8To32MapTransparency'{$ENDIF};
procedure FreeImage_ConvertLine16To32_555(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16To32_555@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16To32_555'{$ENDIF};
procedure FreeImage_ConvertLine16To32_565(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine16To32_565@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine16To32_565'{$ENDIF};
procedure FreeImage_ConvertLine24To32(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertLine24To32@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertLine24To32'{$ENDIF};

// --------------------------------------------------------------------------
// Smart conversion routines ------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_ConvertTo4Bits(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertTo4Bits@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertTo4Bits'{$ENDIF};
function FreeImage_ConvertTo8Bits(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertTo8Bits@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertTo8Bits'{$ENDIF};
function FreeImage_ConvertToGreyscale(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToGreyscale@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToGreyscale'{$ENDIF};
function FreeImage_ConvertTo16Bits555(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertTo16Bits555@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertTo16Bits555'{$ENDIF};
function FreeImage_ConvertTo16Bits565(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertTo16Bits565@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertTo16Bits565'{$ENDIF};
function FreeImage_ConvertTo24Bits(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertTo24Bits@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertTo24Bits'{$ENDIF};
function FreeImage_ConvertTo32Bits(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertTo32Bits@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertTo32Bits'{$ENDIF};
function FreeImage_ColorQuantize(dib: PFIBITMAP; quantize: FREE_IMAGE_QUANTIZE): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ColorQuantize@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ColorQuantize'{$ENDIF};
function FreeImage_ColorQuantizeEx(dib: PFIBITMAP; quantize: FREE_IMAGE_QUANTIZE = FIQ_WUQUANT;
  PaletteSize: Integer = 256; ReserveSize: Integer = 0;
  ReservePalette: PRGBQuad = nil): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ColorQuantizeEx@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ColorQuantizeEx'{$ENDIF};
function FreeImage_Threshold(dib: PFIBITMAP; T: Byte): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Threshold@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Threshold'{$ENDIF};
function FreeImage_Dither(dib: PFIBITMAP; algorithm: FREE_IMAGE_DITHER): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Dither@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Dither'{$ENDIF};

function FreeImage_ConvertFromRawBits(bits: PByte; width, height, pitch: Integer;
  bpp, red_mask, green_mask, blue_mask: Cardinal; topdown: LongBool = False): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertFromRawBits@36'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertFromRawBits'{$ENDIF};
function FreeImage_ConvertFromRawBitsEx(copySource: LongBool; bits: PByte; _type: FREE_IMAGE_TYPE;
  width, height, pitch: Integer; bpp, red_mask, green_mask, blue_mask: Cardinal;
  topdown: LongBool = False): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertFromRawBitsEx@44'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertFromRawBitsEx'{$ENDIF};
procedure FreeImage_ConvertToRawBits(bits: PByte; dib: PFIBITMAP; pitch: Integer;
  bpp, red_mask, green_mask, blue_mask: Cardinal; topdown: LongBool = False); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToRawBits@32'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToRawBits'{$ENDIF};

function FreeImage_ConvertToFloat(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToFloat@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToFloat'{$ENDIF};
function FreeImage_ConvertToRGBF(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToRGBF@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToRGBF'{$ENDIF};
function FreeImage_ConvertToRGBAF(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToRGBAF@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToRGBAF'{$ENDIF};
function FreeImage_ConvertToUINT16(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToUINT16@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToUINT16'{$ENDIF};
function FreeImage_ConvertToRGB16(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToRGB16@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToRGB16'{$ENDIF};
function FreeImage_ConvertToRGBA16(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToRGBA16@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToRGBA16'{$ENDIF};

function FreeImage_ConvertToStandardType(src: PFIBITMAP;
  scale_linear: LongBool = True): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToStandardType@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToStandardType'{$ENDIF};
function FreeImage_ConvertToType(src: PFIBITMAP; dst_type: FREE_IMAGE_TYPE;
  scale_linear: LongBool = True): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ConvertToType@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ConvertToType'{$ENDIF};

// Tone mapping operators ---------------------------------------------------
function FreeImage_ToneMapping(dib: PFIBITMAP; tmo: FREE_IMAGE_TMO;
  first_param: Double = 0; second_param: Double = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ToneMapping@24'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ToneMapping'{$ENDIF};
function FreeImage_TmoDrago03(src: PFIBITMAP; gamma: Double = 2.2;
  exposure: Double = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_TmoDrago03@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_TmoDrago03'{$ENDIF};
function FreeImage_TmoReinhard05(src: PFIBITMAP; intensity: Double = 0;
  contrast: Double = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_TmoReinhard05@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_TmoReinhard05'{$ENDIF};
function FreeImage_TmoReinhard05Ex(src: PFIBITMAP; intensity: Double = 0;
  contrast: Double = 0; adaptation: Double = 1; color_correction: Double = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_TmoReinhard05Ex@36'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_TmoReinhard05Ex'{$ENDIF};

function FreeImage_TmoFattal02(src: PFIBITMAP; color_saturation: Double = 0.5;
  attenuation: Double = 0.85): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_TmoFattal02@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_TmoFattal02'{$ENDIF};

// --------------------------------------------------------------------------
// ZLib interface -----------------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_ZLibCompress(target: PByte; target_size: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ZLibCompress@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ZLibCompress'{$ENDIF};
function FreeImage_ZLibUncompress(target: PByte; target_size: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ZLibUncompress@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ZLibUncompress'{$ENDIF};
function FreeImage_ZLibGZip(target: PByte; target_size: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ZLibGZip@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ZLibGZip'{$ENDIF};
function FreeImage_ZLibGUnzip(target: PByte; target_size: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ZLibGUnzip@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ZLibGUnzip'{$ENDIF};
function FreeImage_ZLibCRC32(crc: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ZLibCRC32@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ZLibCRC32'{$ENDIF};

// --------------------------------------------------------------------------
// Metadata routines
// --------------------------------------------------------------------------

// tag creation / destruction
function FreeImage_CreateTag: PFITAG; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_CreateTag@0'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_CreateTag'{$ENDIF};
procedure FreeImage_DeleteTag(tag: PFITAG); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_DeleteTag@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_DeleteTag'{$ENDIF};
function FreeImage_CloneTag(tag: PFITAG): PFITAG; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_CloneTag@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_CloneTag'{$ENDIF};

// tag getters and setters
function FreeImage_GetTagKey(tag: PFITAG): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTagKey@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTagKey'{$ENDIF};
function FreeImage_GetTagDescription(tag: PFITAG): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTagDescription@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTagDescription'{$ENDIF};
function FreeImage_GetTagID(tag: PFITAG): Word; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTagID@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTagID'{$ENDIF};
function FreeImage_GetTagType(tag: PFITAG): FREE_IMAGE_MDTYPE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTagType@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTagType'{$ENDIF};
function FreeImage_GetTagCount(tag: PFITAG): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTagCount@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTagCount'{$ENDIF};
function FreeImage_GetTagLength(tag: PFITAG): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTagLength@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTagLength'{$ENDIF};
function FreeImage_GetTagValue(tag: PFITAG): Pointer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetTagValue@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetTagValue'{$ENDIF};

function FreeImage_SetTagKey(tag: PFITAG; key: PAnsiChar): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTagKey@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTagKey'{$ENDIF};
function FreeImage_SetTagDescription(tag: PFITAG; description: PAnsiChar): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTagDescription@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTagDescription'{$ENDIF};
function FreeImage_SetTagID(tag: PFITAG; id: Word): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTagID@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTagID'{$ENDIF};
function FreeImage_SetTagType(tag: PFITAG; _type: FREE_IMAGE_MDTYPE): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTagType@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTagType'{$ENDIF};
function FreeImage_SetTagCount(tag: PFITAG; count: DWORD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTagCount@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTagCount'{$ENDIF};
function FreeImage_SetTagLength(tag: PFITAG; length: DWORD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTagLength@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTagLength'{$ENDIF};
function FreeImage_SetTagValue(tag: PFITAG; value: Pointer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetTagValue@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetTagValue'{$ENDIF};

// iterator
function FreeImage_FindFirstMetadata(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP;
  var tag: PFITAG): PFIMETADATA; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FindFirstMetadata@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FindFirstMetadata'{$ENDIF};
function FreeImage_FindNextMetadata(mdhandle: PFIMETADATA; var tag: PFITAG): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FindNextMetadata@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FindNextMetadata'{$ENDIF};
procedure FreeImage_FindCloseMetadata(mdhandle: PFIMETADATA); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FindCloseMetadata@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FindCloseMetadata'{$ENDIF};

// metadata setter and getter
function FreeImage_SetMetadata(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP;
  key: PAnsiChar; tag: PFITAG): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetMetadata@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetMetadata'{$ENDIF};
function FreeImage_GetMetadata(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP;
  key: PAnsiChar; var tag: PFITAG): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetMetadata@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetMetadata'{$ENDIF};
function FreeImage_SetMetadataKeyValue(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP;
  key, value: PAnsiChar): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetMetadataKeyValue@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetMetadataKeyValue'{$ENDIF};

// helpers
function FreeImage_GetMetadataCount(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetMetadataCount@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetMetadataCount'{$ENDIF};
function FreeImage_CloneMetadata(dst, src: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_CloneMetadata@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_CloneMetadata'{$ENDIF};

// tag to C string conversion
function FreeImage_TagToString(model: FREE_IMAGE_MDMODEL; tag: PFITAG;
  Make: PAnsiChar = nil): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_TagToString@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_TagToString'{$ENDIF};

// --------------------------------------------------------------------------
// JPEG lossless transformation routines
// --------------------------------------------------------------------------

function FreeImage_JPEGTransform(src_file, dst_file: PAnsiChar; operation: FREE_IMAGE_JPEG_OPERATION;
  perfect: LongBool = False): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_JPEGTransform@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_JPEGTransform'{$ENDIF};
function FreeImage_JPEGTransformU(src_file, dst_file: PWideChar; operation: FREE_IMAGE_JPEG_OPERATION;
  perfect: LongBool = False): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_JPEGTransformU@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_JPEGTransformU'{$ENDIF};
function FreeImage_JPEGCrop(src_file, dst_file: PAnsiChar;
  left, top, right, bottom: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_JPEGCrop@24'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_JPEGCrop'{$ENDIF};
function FreeImage_JPEGCropU(src_file, dst_file: PWideChar;
  left, top, right, bottom: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_JPEGCropU@24'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_JPEGCropU'{$ENDIF};
function FreeImage_JPEGTransformFromHandle(src_io: PFreeImageIO; src_handle: fi_handle; dst_io: PFreeImageIO;
  dst_handle: fi_handle; operation: FREE_IMAGE_JPEG_OPERATION; var left, top, right, bottom: Integer;
  perfect: LongBool = True): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_JPEGTransformFromHandle@40'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_JPEGTransformFromHandle'{$ENDIF};
function FreeImage_JPEGTransformCombined(src_file, dst_file: PAnsiChar; operation: FREE_IMAGE_JPEG_OPERATION;
  var left, top, right, bottom: Integer; perfect: LongBool = True): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_JPEGTransformCombined@32'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_JPEGTransformCombined'{$ENDIF};
function FreeImage_JPEGTransformCombinedU(src_file, dst_file: PWideChar; operation: FREE_IMAGE_JPEG_OPERATION;
  var left, top, right, bottom: Integer; perfect: LongBool = True): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_JPEGTransformCombinedU@32'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_JPEGTransformCombinedU'{$ENDIF};
function FreeImage_JPEGTransformCombinedFromMemory(src_stream, dst_stream: PFIMEMORY; operation: FREE_IMAGE_JPEG_OPERATION;
  var left, top, right, bottom: Integer; perfect: LongBool = True): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_JPEGTransformCombinedFromMemory@32'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_JPEGTransformCombinedFromMemory'{$ENDIF};

// --------------------------------------------------------------------------
// Image manipulation toolkit
// --------------------------------------------------------------------------

// rotation and flipping
// modif JMB : FreeImage_RotateClassic : deprecated function, call to DeprecationManager in 64 bits crashes freeimage.dll
//function FreeImage_RotateClassic(dib: PFIBITMAP; angle: Double): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
//  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_RotateClassic@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
//  {$IFDEF MACOS}name '_FreeImage_RotateClassic'{$ENDIF};
function FreeImage_Rotate(dib: PFIBITMAP; angle: Double; bkcolor: Pointer = nil): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Rotate@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Rotate'{$ENDIF};
function FreeImage_RotateEx(dib: PFIBITMAP; angle, x_shift, y_shift, x_origin, y_origin: Double;
  use_mask: LongBool): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_RotateEx@48'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_RotateEx'{$ENDIF};
function FreeImage_FlipHorizontal(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FlipHorizontal@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FlipHorizontal'{$ENDIF};
function FreeImage_FlipVertical(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FlipVertical@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FlipVertical'{$ENDIF};

// upsampling / downsampling
function FreeImage_Rescale(dib: PFIBITMAP; dst_width, dst_height: Integer;
  filter: FREE_IMAGE_FILTER = FILTER_CATMULLROM): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Rescale@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Rescale'{$ENDIF};
function FreeImage_MakeThumbnail(dib: PFIBITMAP; max_pixel_size: Integer; convert: LongBool = True): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_MakeThumbnail@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_MakeThumbnail'{$ENDIF};
function FreeImage_RescaleRect(dib: PFIBITMAP; dst_width, dst_height, left, top, right, bottom: Integer;
  filter: FREE_IMAGE_FILTER = FILTER_CATMULLROM; flags: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_RescaleRect@36'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_RescaleRect'{$ENDIF};

// color manipulation routines (point operations)
function FreeImage_AdjustCurve(dib: PFIBITMAP; LUT: PByte;
  channel: FREE_IMAGE_COLOR_CHANNEL): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AdjustCurve@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AdjustCurve'{$ENDIF};
function FreeImage_AdjustGamma(dib: PFIBITMAP; gamma: Double): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AdjustGamma@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AdjustGamma'{$ENDIF};
function FreeImage_AdjustBrightness(dib: PFIBITMAP; percentage: Double): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AdjustBrightness@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AdjustBrightness'{$ENDIF};
function FreeImage_AdjustContrast(dib: PFIBITMAP; percentage: Double): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AdjustContrast@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AdjustContrast'{$ENDIF};
function FreeImage_Invert(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Invert@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Invert'{$ENDIF};
function FreeImage_GetHistogram(dib: PFIBITMAP; histo: PDWORD;
  channel: FREE_IMAGE_COLOR_CHANNEL = FICC_BLACK): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetHistogram@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetHistogram'{$ENDIF};
function FreeImage_GetAdjustColorsLookupTable(LUT: PByte; brightness, contrast, gamma: Double;
  invert: LongBool): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetAdjustColorsLookupTable@32'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetAdjustColorsLookupTable'{$ENDIF};
function FreeImage_AdjustColors(dib: PFIBITMAP; brightness, contrast, gamma: Double;
  invert: LongBool = False): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AdjustColors@32'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AdjustColors'{$ENDIF};
function FreeImage_ApplyColorMapping(dib: PFIBITMAP; srccolors, dstcolors: PRGBQuad;
  count: Cardinal; ignore_alpha, swap: LongBool): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ApplyColorMapping@24'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ApplyColorMapping'{$ENDIF};
function FreeImage_SwapColors(dib: PFIBITMAP; color_a, color_b: PRGBQuad;
  ignore_alpha: LongBool): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SwapColors@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SwapColors'{$ENDIF};
function FreeImage_ApplyPaletteIndexMapping(dib: PFIBITMAP; srcindices, dstindices: PByte;
  count: Cardinal; swap: LongBool): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_ApplyPaletteIndexMapping@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_ApplyPaletteIndexMapping'{$ENDIF};
function FreeImage_SwapPaletteIndices(dib: PFIBITMAP; index_a, index_b: PByte): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SwapPaletteIndices@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SwapPaletteIndices'{$ENDIF};

// channel processing routines
function FreeImage_GetChannel(dib: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetChannel@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetChannel'{$ENDIF};
function FreeImage_SetChannel(dst, src: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetChannel@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetChannel'{$ENDIF};
function FreeImage_GetComplexChannel(src: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_GetComplexChannel@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_GetComplexChannel'{$ENDIF};
function FreeImage_SetComplexChannel(dst, src: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_SetComplexChannel@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_SetComplexChannel'{$ENDIF};

// copy / paste / composite routines

function FreeImage_Copy(dib: PFIBITMAP; left, top, right, bottom: Integer): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Copy@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Copy'{$ENDIF};
function FreeImage_Paste(dst, src: PFIBITMAP; left, top, alpha: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Paste@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Paste'{$ENDIF};
function FreeImage_CreateView(dib: PFIBITMAP; left, top, right, bottom: Cardinal): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_CreateView@20'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_CreateView'{$ENDIF};

function FreeImage_Composite(fg: PFIBITMAP; useFileBkg: LongBool = False;
  appBkColor: PRGBQuad = nil; bg: PFIBITMAP = nil): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_Composite@16'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_Composite'{$ENDIF};
function FreeImage_PreMultiplyWithAlpha(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_PreMultiplyWithAlpha@4'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_PreMultiplyWithAlpha'{$ENDIF};

// background filling routines
function FreeImage_FillBackground(dib: PFIBITMAP; color: Pointer;
  options: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_FillBackground@12'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_FillBackground'{$ENDIF};
function FreeImage_EnlargeCanvas(src: PFIBITMAP; left, top, right, bottom: Integer;
  color: Pointer; options: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_EnlargeCanvas@28'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_EnlargeCanvas'{$ENDIF};
function FreeImage_AllocateEx(width, height, bpp: Integer; color: PRGBQuad;
  options: Integer = 0; palette: PRGBQuad = nil; red_mask: Cardinal = 0;
  green_mask: Cardinal = 0; blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AllocateEx@36'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AllocateEx'{$ENDIF};
function FreeImage_AllocateExT(_type: FREE_IMAGE_TYPE; width, height, bpp: Integer;
  color: Pointer; options: Integer = 0; palette: PRGBQuad = nil; red_mask: Cardinal = 0;
  green_mask: Cardinal = 0; blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_AllocateExT@40'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_AllocateExT'{$ENDIF};

// miscellaneous algorithms
function FreeImage_MultigridPoissonSolver(Laplacian: PFIBITMAP;
  ncycle: Integer = 3): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  external FIDLL {$IFDEF WIN32}name CNamePrefix + 'FreeImage_MultigridPoissonSolver@8'{$ENDIF} {$IFDEF USE_DELAYLOAD}delayed{$ENDIF}
  {$IFDEF MACOS}name '_FreeImage_MultigridPoissonSolver'{$ENDIF};

{$ELSE}

type
  FreeImage_OutputMessageFunction = procedure(fif: FREE_IMAGE_FORMAT;
    msg: PAnsiChar); cdecl;
  FreeImage_OutputMessageFunctionStdCall = procedure(fif: FREE_IMAGE_FORMAT;
    msg: PAnsiChar); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

var
  FreeImage_Initialise: procedure(load_local_plugins_only: LongBool = False); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_DeInitialise: procedure(); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetVersion: function(): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetCopyrightMessage: function(): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetOutputMessageStdCall: procedure(omf: FreeImage_OutputMessageFunctionStdCall); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetOutputMessage: procedure(omf: FreeImage_OutputMessageFunction); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  //FreeImage_OutputMessageProc: procedure(fif: Integer; fmt: PAnsiChar); cdecl; varargs;

  FreeImage_Allocate: function(width, height, bpp: Integer; red_mask: Cardinal = 0;
    green_mask: Cardinal = 0; blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AllocateT: function(_type: FREE_IMAGE_TYPE; width, height: Integer;
    bpp: Integer = 8; red_mask: Cardinal = 0; green_mask: Cardinal = 0;
    blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Clone: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Unload: procedure(dib: PFIBITMAP); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_HasPixels: function(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Load: function(fif: FREE_IMAGE_FORMAT; filename: PAnsiChar;
    flags: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_LoadU: function(fif: FREE_IMAGE_FORMAT; filename: PWideChar;
    flags: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_LoadFromHandle: function(fif: FREE_IMAGE_FORMAT; io: PFreeImageIO;
    handle: fi_handle; flags: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Save: function(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP; filename: PAnsiChar;
    flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SaveU: function(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP; filename: PWideChar;
    flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SaveToHandle: function(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP;
    io: PFreeImageIO; handle: fi_handle; flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_OpenMemory: function(data: PByte = nil; size_in_bytes: DWORD = 0): PFIMEMORY; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_CloseMemory: procedure(stream: PFIMEMORY); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_LoadFromMemory: function(fif: FREE_IMAGE_FORMAT; stream: PFIMEMORY;
    flags: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SaveToMemory: function(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP;
    stream: PFIMEMORY; flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_TellMemory: function(stream: PFIMEMORY): LongInt; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SeekMemory: function(stream: PFIMEMORY; offset: LongInt;
    origin: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AcquireMemory: function(stream: PFIMEMORY; var data: PByte;
    var size_in_bytes: DWORD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ReadMemory: function(buffer: Pointer; size, count: Cardinal;
    stream: PFIMEMORY): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_WriteMemory: function(buffer: Pointer; size, count: Cardinal;
    stream: PFIMEMORY): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_LoadMultiBitmapFromMemory: function(fif: FREE_IMAGE_FORMAT; stream: PFIMEMORY;
    flags: Integer = 0): PFIMULTIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SaveMultiBitmapToMemory: function(fif: FREE_IMAGE_FORMAT; bitmap: PFIMULTIBITMAP;
    stream: PFIMEMORY; flags: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_RegisterLocalPlugin: function(proc_address: FI_InitProc; format: PAnsiChar = nil;
    description: PAnsiChar = nil; extension: PAnsiChar = nil;
    regexpr: PAnsiChar = nil): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_RegisterExternalPlugin: function(path: PAnsiChar; format: PAnsiChar = nil;
    description: PAnsiChar = nil; extension: PAnsiChar = nil;
    regexpr: PAnsiChar = nil): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFIFCount: function(): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetPluginEnabled: procedure(fif: FREE_IMAGE_FORMAT; enable: LongBool); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_IsPluginEnabled: function(fif: FREE_IMAGE_FORMAT): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFIFFromFormat: function(format: PAnsiChar): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFIFFromMime: function(mime: PAnsiChar): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFormatFromFIF: function(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFIFExtensionList: function(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFIFDescription: function(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFIFRegExpr: function(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFIFMimeType: function(fif: FREE_IMAGE_FORMAT): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFIFFromFilename: function(filename: PAnsiChar): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFIFFromFilenameU: function(filename: PWideChar): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FIFSupportsReading: function(fif: FREE_IMAGE_FORMAT): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FIFSupportsWriting: function(fif: FREE_IMAGE_FORMAT): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FIFSupportsExportBPP: function(fif: FREE_IMAGE_FORMAT;
    bpp: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FIFSupportsExportType: function(fif: FREE_IMAGE_FORMAT;
    _type: FREE_IMAGE_TYPE): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FIFSupportsICCProfiles: function(fif: FREE_IMAGE_FORMAT): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FIFSupportsNoPixels: function(fif: FREE_IMAGE_FORMAT): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_OpenMultiBitmap: function(fif: FREE_IMAGE_FORMAT; filename: PAnsiChar;
    create_new, read_only: LongBool; keep_cache_in_memory: LongBool = False;
    flags: Integer = 0): PFIMULTIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_OpenMultiBitmapFromHandle: function(fif: FREE_IMAGE_FORMAT; io: PFreeImageIO;
    handle: fi_handle; flags: Integer = 0): PFIMULTIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SaveMultiBitmapToHandle: function(fif: FREE_IMAGE_FORMAT; bitmap: PFIMULTIBITMAP;
    io: PFreeImageIO; handle: fi_handle; flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_CloseMultiBitmap: function(bitmap: PFIMULTIBITMAP;
    flags: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetPageCount: function(bitmap: PFIMULTIBITMAP): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AppendPage: procedure(bitmap: PFIMULTIBITMAP; data: PFIBITMAP); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_InsertPage: procedure(bitmap: PFIMULTIBITMAP; page: Integer;
    data: PFIBITMAP); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_DeletePage: procedure(bitmap: PFIMULTIBITMAP; page: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_LockPage: function(bitmap: PFIMULTIBITMAP; page: Integer): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_UnlockPage: procedure(bitmap: PFIMULTIBITMAP; data: PFIBITMAP;
    changed: LongBool); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_MovePage: function(bitmap: PFIMULTIBITMAP; target, source: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetLockedPageNumbers: function(bitmap: PFIMULTIBITMAP; var pages: Integer;
    var count: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFileType: function(filename: PAnsiChar;
    size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFileTypeU: function(filename: PWideChar;
    size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFileTypeFromHandle: function(io: PFreeImageIO; handle: FI_Handle;
    size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetFileTypeFromMemory: function(stream: PFIMEMORY;
    size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetImageType: function(dib: PFIBITMAP): FREE_IMAGE_TYPE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_IsLittleEndian: function(): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_LookupX11Color: function(szColor: PAnsiChar; var nRed, nGreen, nBlue: Byte): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_LookupSVGColor: function(szColor: PAnsiChar; var nRed, nGreen, nBlue: Byte): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetBits: function(dib: PFIBITMAP): PByte; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetScanLine: function(dib: PFIBITMAP; scanline: Integer): PByte; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetPixelIndex: function(dib: PFIBITMAP; x, y: Cardinal; var value: Byte): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetPixelColor: function(dib: PFIBITMAP; x, y: Cardinal; var value: RGBQUAD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetPixelIndex: function(dib: PFIBITMAP; x, y: Cardinal; var value: Byte): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetPixelColor: function(dib: PFIBITMAP; x, y: Cardinal; var value: RGBQUAD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetColorsUsed: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetBPP: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetWidth: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetHeight: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetLine: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetPitch: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetDIBSize: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetMemorySize: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetPalette: function(dib: PFIBITMAP): PRGBQuad; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetDotsPerMeterX: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetDotsPerMeterY: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetDotsPerMeterX: procedure(dib: PFIBITMAP; res: Cardinal); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetDotsPerMeterY: procedure(dib: PFIBITMAP; res: Cardinal); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetInfoHeader: function(dib: PFIBITMAP): PBITMAPINFOHEADER; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetInfo: function(dib: PFIBITMAP): PBITMAPINFO; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetColorType: function(dib: PFIBITMAP): FREE_IMAGE_COLOR_TYPE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetRedMask: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetGreenMask: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetBlueMask: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTransparencyCount: function(dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTransparencyTable: function(dib: PFIBITMAP): PByte; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTransparent: procedure(dib: PFIBITMAP; enabled: LongBool); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTransparencyTable: procedure(dib: PFIBITMAP; table: PByte;
    count: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_IsTransparent: function(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTransparentIndex: procedure(dib: PFIBITMAP; index: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTransparentIndex: function(dib: PFIBITMAP): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_HasBackgroundColor: function(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetBackgroundColor: function(dib: PFIBITMAP; var bkcolor: RGBQUAD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetBackgroundColor: function(dib: PFIBITMAP; bkcolor: PRGBQuad): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetThumbnail: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetThumbnail: function(dib, thumbnail: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetICCProfile: function(dib: PFIBITMAP): PFIICCPROFILE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_CreateICCProfile: function(dib: PFIBITMAP; data: Pointer;
    size: LongInt): PFIICCPROFILE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_DestroyICCProfile: procedure(dib: PFIBITMAP); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine1To4: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine8To4: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16To4_555: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16To4_565: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine24To4: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine32To4: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine1To8: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine4To8: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16To8_555: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16To8_565: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine24To8: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine32To8: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine1To16_555: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine4To16_555: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine8To16_555: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16_565_To16_555: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine24To16_555: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine32To16_555: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine1To16_565: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine4To16_565: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine8To16_565: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16_555_To16_565: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine24To16_565: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine32To16_565: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine1To24: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine4To24: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine8To24: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16To24_555: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16To24_565: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine32To24: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine1To32: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine1To32MapTransparency: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad; table: PByte; transparent_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine4To32: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine4To32MapTransparency: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad; table: PByte; transparent_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine8To32: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine8To32MapTransparency: procedure(target, source: PByte; width_in_pixels: Integer;
    palette: PRGBQuad; table: PByte; transparent_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16To32_555: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine16To32_565: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertLine24To32: procedure(target, source: PByte; width_in_pixels: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertTo4Bits: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertTo8Bits: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToGreyscale: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertTo16Bits555: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertTo16Bits565: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertTo24Bits: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertTo32Bits: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ColorQuantize: function(dib: PFIBITMAP; quantize: FREE_IMAGE_QUANTIZE): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ColorQuantizeEx: function(dib: PFIBITMAP; quantize: FREE_IMAGE_QUANTIZE = FIQ_WUQUANT;
    PaletteSize: Integer = 256; ReserveSize: Integer = 0;
    ReservePalette: PRGBQuad = nil): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Threshold: function(dib: PFIBITMAP; T: Byte): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Dither: function(dib: PFIBITMAP; algorithm: FREE_IMAGE_DITHER): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertFromRawBits: function(bits: PByte; width, height, pitch: Integer;
    bpp, red_mask, green_mask, blue_mask: Cardinal; topdown: LongBool = False): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertFromRawBitsEx: function(copySource: LongBool; bits: PByte; _type: FREE_IMAGE_TYPE;
    width, height, pitch: Integer; bpp, red_mask, green_mask, blue_mask: Cardinal;
    topdown: LongBool = False): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToRawBits: procedure(bits: PByte; dib: PFIBITMAP; pitch: Integer;
    bpp, red_mask, green_mask, blue_mask: Cardinal; topdown: LongBool = False); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToFloat: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToRGBF: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToRGBAF: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToUINT16: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToRGB16: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToRGBA16: function(dib: PFIBITMAP): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToStandardType: function(src: PFIBITMAP;
    scale_linear: LongBool = True): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ConvertToType: function(src: PFIBITMAP; dst_type: FREE_IMAGE_TYPE;
    scale_linear: LongBool = True): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ToneMapping: function(dib: PFIBITMAP; tmo: FREE_IMAGE_TMO;
    first_param: Double = 0; second_param: Double = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_TmoDrago03: function(src: PFIBITMAP; gamma: Double = 2.2;
    exposure: Double = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_TmoReinhard05: function(src: PFIBITMAP; intensity: Double = 0;
    contrast: Double = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_TmoReinhard05Ex: function(src: PFIBITMAP; intensity: Double = 0;
    contrast: Double = 0; adaptation: Double = 1; color_correction: Double = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_TmoFattal02: function(src: PFIBITMAP; color_saturation: Double = 0.5;
    attenuation: Double = 0.85): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ZLibCompress: function(target: PByte; target_size: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ZLibUncompress: function(target: PByte; target_size: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ZLibGZip: function(target: PByte; target_size: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ZLibGUnzip: function(target: PByte; target_size: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ZLibCRC32: function(crc: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_CreateTag: function(): PFITAG; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_DeleteTag: procedure(tag: PFITAG); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_CloneTag: function(tag: PFITAG): PFITAG; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTagKey: function(tag: PFITAG): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTagDescription: function(tag: PFITAG): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTagID: function(tag: PFITAG): Word; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTagType: function(tag: PFITAG): FREE_IMAGE_MDTYPE; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTagCount: function(tag: PFITAG): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTagLength: function(tag: PFITAG): DWORD; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetTagValue: function(tag: PFITAG): Pointer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTagKey: function(tag: PFITAG; key: PAnsiChar): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTagDescription: function(tag: PFITAG; description: PAnsiChar): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTagID: function(tag: PFITAG; id: Word): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTagType: function(tag: PFITAG; _type: FREE_IMAGE_MDTYPE): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTagCount: function(tag: PFITAG; count: DWORD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTagLength: function(tag: PFITAG; length: DWORD): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetTagValue: function(tag: PFITAG; value: Pointer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FindFirstMetadata: function(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP;
    var tag: PFITAG): PFIMETADATA; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FindNextMetadata: function(mdhandle: PFIMETADATA; var tag: PFITAG): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FindCloseMetadata: procedure(mdhandle: PFIMETADATA); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetMetadata: function(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP;
    key: PAnsiChar; tag: PFITAG): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetMetadata: function(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP;
    key: PAnsiChar; var tag: PFITAG): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetMetadataKeyValue: function(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP;
    key, value: PAnsiChar): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetMetadataCount: function(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_CloneMetadata: function(dst, src: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_TagToString: function(model: FREE_IMAGE_MDMODEL; tag: PFITAG;
    Make: PAnsiChar = nil): PAnsiChar; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  //FreeImage_JPEGTransform: function(src_file, dst_file: PAnsiChar; operation: FREE_IMAGE_JPEG_OPERATION;
  //  perfect: LongBool = False): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  //FreeImage_JPEGTransformU: function(src_file, dst_file: PWideChar; operation: FREE_IMAGE_JPEG_OPERATION;
  //  perfect: LongBool = False): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  //FreeImage_JPEGCrop: function(src_file, dst_file: PAnsiChar;
  //  left, top, right, bottom: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  //FreeImage_JPEGCropU: function(src_file, dst_file: PWideChar;
  //  left, top, right, bottom: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  //FreeImage_JPEGTransformFromHandle: function(src_io: PFreeImageIO; src_handle: fi_handle; dst_io: PFreeImageIO;
  //  dst_handle: fi_handle; operation: FREE_IMAGE_JPEG_OPERATION; var left, top, right, bottom: Integer;
  //  perfect: LongBool = True): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  //FreeImage_JPEGTransformCombined: function(src_file, dst_file: PAnsiChar; operation: FREE_IMAGE_JPEG_OPERATION;
  //  var left, top, right, bottom: Integer; perfect: LongBool = True): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  //FreeImage_JPEGTransformCombinedU: function(src_file, dst_file: PWideChar; operation: FREE_IMAGE_JPEG_OPERATION;
  //  var left, top, right, bottom: Integer; perfect: LongBool = True): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  //FreeImage_JPEGTransformCombinedFromMemory: function(src_stream, dst_stream: PFIMEMORY; operation: FREE_IMAGE_JPEG_OPERATION;
  //  var left, top, right, bottom: Integer; perfect: LongBool = True): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  //FreeImage_RotateClassic: function(dib: PFIBITMAP; angle: Double): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  FreeImage_Rotate: function(dib: PFIBITMAP; angle: Double; bkcolor: Pointer = nil): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_RotateEx: function(dib: PFIBITMAP; angle, x_shift, y_shift, x_origin, y_origin: Double;
    use_mask: LongBool): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FlipHorizontal: function(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FlipVertical: function(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Rescale: function(dib: PFIBITMAP; dst_width, dst_height: Integer;
    filter: FREE_IMAGE_FILTER = FILTER_CATMULLROM): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_MakeThumbnail: function(dib: PFIBITMAP; max_pixel_size: Integer; convert: LongBool = True): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_RescaleRect: function(dib: PFIBITMAP; dst_width, dst_height, left, top, right, bottom: Integer;
    filter: FREE_IMAGE_FILTER = FILTER_CATMULLROM; flags: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AdjustCurve: function(dib: PFIBITMAP; LUT: PByte;
    channel: FREE_IMAGE_COLOR_CHANNEL): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AdjustGamma: function(dib: PFIBITMAP; gamma: Double): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AdjustBrightness: function(dib: PFIBITMAP; percentage: Double): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AdjustContrast: function(dib: PFIBITMAP; percentage: Double): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Invert: function(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetHistogram: function(dib: PFIBITMAP; histo: PDWORD;
    channel: FREE_IMAGE_COLOR_CHANNEL = FICC_BLACK): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetAdjustColorsLookupTable: function(LUT: PByte; brightness, contrast, gamma: Double;
    invert: LongBool): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AdjustColors: function(dib: PFIBITMAP; brightness, contrast, gamma: Double;
    invert: LongBool = False): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ApplyColorMapping: function(dib: PFIBITMAP; srccolors, dstcolors: PRGBQuad;
    count: Cardinal; ignore_alpha, swap: LongBool): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SwapColors: function(dib: PFIBITMAP; color_a, color_b: PRGBQuad;
    ignore_alpha: LongBool): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_ApplyPaletteIndexMapping: function(dib: PFIBITMAP; srcindices, dstindices: PByte;
    count: Cardinal; swap: LongBool): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SwapPaletteIndices: function(dib: PFIBITMAP; index_a, index_b: PByte): Cardinal; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetChannel: function(dib: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetChannel: function(dst, src: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_GetComplexChannel: function(src: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_SetComplexChannel: function(dst, src: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Copy: function(dib: PFIBITMAP; left, top, right, bottom: Integer): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Paste: function(dst, src: PFIBITMAP; left, top, alpha: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_CreateView: function(dib: PFIBITMAP; left, top, right, bottom: Cardinal): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_Composite: function(fg: PFIBITMAP; useFileBkg: LongBool = False;
    appBkColor: PRGBQuad = nil; bg: PFIBITMAP = nil): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_PreMultiplyWithAlpha: function(dib: PFIBITMAP): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_FillBackground: function(dib: PFIBITMAP; color: Pointer;
    options: Integer = 0): LongBool; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_EnlargeCanvas: function(src: PFIBITMAP; left, top, right, bottom: Integer;
    color: Pointer; options: Integer = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AllocateEx: function(width, height, bpp: Integer; color: PRGBQuad;
    options: Integer = 0; palette: PRGBQuad = nil; red_mask: Cardinal = 0;
    green_mask: Cardinal = 0; blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_AllocateExT: function(_type: FREE_IMAGE_TYPE; width, height, bpp: Integer;
    color: Pointer; options: Integer = 0; palette: PRGBQuad = nil; red_mask: Cardinal = 0;
    green_mask: Cardinal = 0; blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  FreeImage_MultigridPoissonSolver: function(Laplacian: PFIBITMAP;
    ncycle: Integer = 3): PFIBITMAP; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
{$ENDIF}

procedure InitFreeImageLib(const ALibName: string); {$IFDEF FREEIMAGE_STATIC_LINK}inline;{$ENDIF}

implementation

{$IFNDEF DELPHI6}
uses SysUtils;

//we provide a wrapper since we haven't varargs in older versions of Delphi
procedure __FreeImage_OutputMessageProc; cdecl;
  external FIDLL name 'FreeImage_OutputMessageProc';

procedure FreeImage_OutputMessageProc(fif: Integer; fmt: PAnsiChar; args: array of const);
  function ArrayToBuffer(Args: array of const;
    var Argv: Pointer; Buffer: Pointer; Size: Cardinal): Integer;
  var
    i: Integer;
    temp: AnsiString;
    parg: Pointer;
    psrc, pbuf: PAnsiChar;
    len: Cardinal;
  begin
    Result := High(Args) + 1;
    if Result = 0 then
      Exit;
    //array of pointers to push on stack
    GetMem(Argv, Result * SizeOf(Pointer));
    //pointer to current string in buffer
    pbuf := Buffer;
    //pointer to current arg
    parg := Argv;
    //for each const...
    for i := 0 to Result - 1 do begin
      case Args[i].VType of
        vtInteger: begin
          //integer
          psrc := nil;
          len := 0;
          Integer(parg^) := Args[i].VInteger;
        end;
        vtString: begin
          //short string
          psrc := PAnsiChar(Cardinal(Args[i].VString) + SizeOf(Byte));
          len := PByte(Args[i].VString)^;
          PAnsiChar(parg^) := pbuf;
        end;
        vtPChar: begin
          //NULL terminated MBCS string
          psrc := nil;
          len := 0;
          PAnsiChar(parg^) := Args[i].VPChar;
        end;
        vtPWideChar: begin
          //NULL terminated Unicode string
          temp := AnsiString(Args[i].VPWideChar);
          psrc := PAnsiChar(temp);
          len := Length(temp);
          PAnsiChar(parg^) := pbuf;
        end;
        vtAnsiString: begin
          //ANSI string
          psrc := PAnsiChar(Args[i].VAnsiString);
          len := StrLen(psrc);
          PAnsiChar(parg^) := pbuf;
        end;
        vtWideString: begin
          //Wide string (OLE)
          temp := AnsiString(PWideChar(Args[i].VWideString));
          psrc := PAnsiChar(temp);
          len := Length(temp);
          PAnsiChar(parg^) := pbuf;
        end;
        else raise Exception.Create('Unsupported argument type');
      end;
      if (psrc <> nil) and (len <> 0) then begin
        //enough space to hold string?
        if Size < (len + 1) then
          raise Exception.Create('Buffer overflow');
        //copy string
        Move(psrc^, pbuf^, len);
        //NULL terminator
        PAnsiChar(Cardinal(pbuf) + len)^ := #0;
        //shift pointer...
        Inc(pbuf, len + 1);
        //...and decrease space left
        Dec(Size, len + 1);
      end;
      Cardinal(parg) := Cardinal(parg) + SizeOf(Pointer);
    end;
  end;

  procedure DoVarargsCall(fif: Integer; fmt: PAnsiChar; Argv: Pointer; Argc: Integer);
  {
  fif     -> EAX
  fmt     -> EDX
  Argv    -> ECX
  Argc    -> [EBP+$08]
  }
  asm
      PUSH    EAX                      //remember fif
      PUSH    ECX                      //make room for ESP backup

      MOV     DWORD PTR [EBP-$08], ESP //backup stack pointer

      MOV     EAX, DWORD PTR [EBP+$08] //store Argc

      TEST    EAX, EAX                 //Argc <= 0?
      JLE     @Call

    @Loop:
      PUSH    DWORD PTR [ECX+EAX*$04-$04] //push Argv in right to left order
      DEC     EAX
      JNZ     @Loop

    @Call:
      PUSH    EDX                      //push fmt
      PUSH    DWORD PTR [EBP-$04]      //push fif
      CALL    __FreeImage_OutputMessageProc

      MOV     ESP, DWORD PTR [EBP-$08] //restore stack pointer

      POP     ECX                      //clean stack
      POP     EAX
  end;
var
  Argc: Integer;
  Argv: Pointer;
  //buffer to hold strings - FreeImage allocates 512 bytes, we needn't more...
  Buffer: array[1..512] of Byte;
begin
  Argv := nil;
  //build array of pointers from array of const
  Argc := ArrayToBuffer(args, Argv, @Buffer, SizeOf(Buffer));
  try
    //mimic cdecl call with varargs
    DoVarargsCall(fif, fmt, Argv, Argc);
  finally
    //cleanup
    FreeMem(Argv);
  end;
end;
{$ENDIF}

{$IFDEF FREEIMAGE_STATIC_LINK}
procedure InitFreeImageLib(const ALibName: string);
begin
  // nothing to do
end;
{$ELSE}
uses
  SyncObjs,
  SysUtils;

var
  GHandle: THandle = 0;
  GLock: TCriticalSection = nil;
  GIsInitialized: Boolean = False;

procedure InitFreeImageLib(const ALibName: string);
var
  VNamePrefix: string;
  VRetryNamePrefix: Boolean;

  function GetProcAddr(const AProcName: string): Pointer;
  var
    VName: string;
  begin
    VName := VNamePrefix + AProcName;
    Result := GetProcAddress(GHandle, PChar(VName));
    if Result = nil then begin
      if VRetryNamePrefix then begin
        VRetryNamePrefix := False;
        if VNamePrefix = CNamePrefixDefault then begin
          VNamePrefix := CNamePrefixMingwDll;
        end else begin
          VNamePrefix := CNamePrefixDefault;
        end;
        Result := GetProcAddr(AProcName);
      end else begin
        RaiseLastOSError(GetLastError, ': ' + VName + #13#10 + ALibName);
      end;
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

    VNamePrefix := CNamePrefix;
    VRetryNamePrefix := True;

    FreeImage_Initialise := GetProcAddr('FreeImage_Initialise@4');
    FreeImage_DeInitialise := GetProcAddr('FreeImage_DeInitialise@0');
    FreeImage_GetVersion := GetProcAddr('FreeImage_GetVersion@0');
    FreeImage_GetCopyrightMessage := GetProcAddr('FreeImage_GetCopyrightMessage@0');
    FreeImage_SetOutputMessageStdCall := GetProcAddr('FreeImage_SetOutputMessageStdCall@4');
    FreeImage_SetOutputMessage := GetProcAddr('FreeImage_SetOutputMessage@4');
    FreeImage_Allocate := GetProcAddr('FreeImage_Allocate@24');
    FreeImage_AllocateT := GetProcAddr('FreeImage_AllocateT@28');
    FreeImage_Clone := GetProcAddr('FreeImage_Clone@4');
    FreeImage_Unload := GetProcAddr('FreeImage_Unload@4');
    FreeImage_HasPixels := GetProcAddr('FreeImage_HasPixels@4');
    FreeImage_Load := GetProcAddr('FreeImage_Load@12');
    FreeImage_LoadU := GetProcAddr('FreeImage_LoadU@12');
    FreeImage_LoadFromHandle := GetProcAddr('FreeImage_LoadFromHandle@16');
    FreeImage_Save := GetProcAddr('FreeImage_Save@16');
    FreeImage_SaveU := GetProcAddr('FreeImage_SaveU@16');
    FreeImage_SaveToHandle := GetProcAddr('FreeImage_SaveToHandle@20');
    FreeImage_OpenMemory := GetProcAddr('FreeImage_OpenMemory@8');
    FreeImage_CloseMemory := GetProcAddr('FreeImage_CloseMemory@4');
    FreeImage_LoadFromMemory := GetProcAddr('FreeImage_LoadFromMemory@12');
    FreeImage_SaveToMemory := GetProcAddr('FreeImage_SaveToMemory@16');
    FreeImage_TellMemory := GetProcAddr('FreeImage_TellMemory@4');
    FreeImage_SeekMemory := GetProcAddr('FreeImage_SeekMemory@12');
    FreeImage_AcquireMemory := GetProcAddr('FreeImage_AcquireMemory@12');
    FreeImage_ReadMemory := GetProcAddr('FreeImage_ReadMemory@16');
    FreeImage_WriteMemory := GetProcAddr('FreeImage_WriteMemory@16');
    FreeImage_LoadMultiBitmapFromMemory := GetProcAddr('FreeImage_LoadMultiBitmapFromMemory@12');
    FreeImage_SaveMultiBitmapToMemory := GetProcAddr('FreeImage_SaveMultiBitmapToMemory@16');
    FreeImage_RegisterLocalPlugin := GetProcAddr('FreeImage_RegisterLocalPlugin@20');
    FreeImage_RegisterExternalPlugin := GetProcAddr('FreeImage_RegisterExternalPlugin@20');
    FreeImage_GetFIFCount := GetProcAddr('FreeImage_GetFIFCount@0');
    FreeImage_SetPluginEnabled := GetProcAddr('FreeImage_SetPluginEnabled@8');
    FreeImage_IsPluginEnabled := GetProcAddr('FreeImage_IsPluginEnabled@4');
    FreeImage_GetFIFFromFormat := GetProcAddr('FreeImage_GetFIFFromFormat@4');
    FreeImage_GetFIFFromMime := GetProcAddr('FreeImage_GetFIFFromMime@4');
    FreeImage_GetFormatFromFIF := GetProcAddr('FreeImage_GetFormatFromFIF@4');
    FreeImage_GetFIFExtensionList := GetProcAddr('FreeImage_GetFIFExtensionList@4');
    FreeImage_GetFIFDescription := GetProcAddr('FreeImage_GetFIFDescription@4');
    FreeImage_GetFIFRegExpr := GetProcAddr('FreeImage_GetFIFRegExpr@4');
    FreeImage_GetFIFMimeType := GetProcAddr('FreeImage_GetFIFMimeType@4');
    FreeImage_GetFIFFromFilename := GetProcAddr('FreeImage_GetFIFFromFilename@4');
    FreeImage_GetFIFFromFilenameU := GetProcAddr('FreeImage_GetFIFFromFilenameU@4');
    FreeImage_FIFSupportsReading := GetProcAddr('FreeImage_FIFSupportsReading@4');
    FreeImage_FIFSupportsWriting := GetProcAddr('FreeImage_FIFSupportsWriting@4');
    FreeImage_FIFSupportsExportBPP := GetProcAddr('FreeImage_FIFSupportsExportBPP@8');
    FreeImage_FIFSupportsExportType := GetProcAddr('FreeImage_FIFSupportsExportType@8');
    FreeImage_FIFSupportsICCProfiles := GetProcAddr('FreeImage_FIFSupportsICCProfiles@4');
    FreeImage_FIFSupportsNoPixels := GetProcAddr('FreeImage_FIFSupportsNoPixels@4');
    FreeImage_OpenMultiBitmap := GetProcAddr('FreeImage_OpenMultiBitmap@24');
    FreeImage_OpenMultiBitmapFromHandle := GetProcAddr('FreeImage_OpenMultiBitmapFromHandle@16');
    FreeImage_SaveMultiBitmapToHandle := GetProcAddr('FreeImage_SaveMultiBitmapToHandle@20');
    FreeImage_CloseMultiBitmap := GetProcAddr('FreeImage_CloseMultiBitmap@8');
    FreeImage_GetPageCount := GetProcAddr('FreeImage_GetPageCount@4');
    FreeImage_AppendPage := GetProcAddr('FreeImage_AppendPage@8');
    FreeImage_InsertPage := GetProcAddr('FreeImage_InsertPage@12');
    FreeImage_DeletePage := GetProcAddr('FreeImage_DeletePage@8');
    FreeImage_LockPage := GetProcAddr('FreeImage_LockPage@8');
    FreeImage_UnlockPage := GetProcAddr('FreeImage_UnlockPage@12');
    FreeImage_MovePage := GetProcAddr('FreeImage_MovePage@12');
    FreeImage_GetLockedPageNumbers := GetProcAddr('FreeImage_GetLockedPageNumbers@12');
    FreeImage_GetFileType := GetProcAddr('FreeImage_GetFileType@8');
    FreeImage_GetFileTypeU := GetProcAddr('FreeImage_GetFileTypeU@8');
    FreeImage_GetFileTypeFromHandle := GetProcAddr('FreeImage_GetFileTypeFromHandle@12');
    FreeImage_GetFileTypeFromMemory := GetProcAddr('FreeImage_GetFileTypeFromMemory@8');
    FreeImage_GetImageType := GetProcAddr('FreeImage_GetImageType@4');
    FreeImage_IsLittleEndian := GetProcAddr('FreeImage_IsLittleEndian@0');
    FreeImage_LookupX11Color := GetProcAddr('FreeImage_LookupX11Color@16');
    FreeImage_LookupSVGColor := GetProcAddr('FreeImage_LookupSVGColor@16');
    FreeImage_GetBits := GetProcAddr('FreeImage_GetBits@4');
    FreeImage_GetScanLine := GetProcAddr('FreeImage_GetScanLine@8');
    FreeImage_GetPixelIndex := GetProcAddr('FreeImage_GetPixelIndex@16');
    FreeImage_GetPixelColor := GetProcAddr('FreeImage_GetPixelColor@16');
    FreeImage_SetPixelIndex := GetProcAddr('FreeImage_SetPixelIndex@16');
    FreeImage_SetPixelColor := GetProcAddr('FreeImage_SetPixelColor@16');
    FreeImage_GetColorsUsed := GetProcAddr('FreeImage_GetColorsUsed@4');
    FreeImage_GetBPP := GetProcAddr('FreeImage_GetBPP@4');
    FreeImage_GetWidth := GetProcAddr('FreeImage_GetWidth@4');
    FreeImage_GetHeight := GetProcAddr('FreeImage_GetHeight@4');
    FreeImage_GetLine := GetProcAddr('FreeImage_GetLine@4');
    FreeImage_GetPitch := GetProcAddr('FreeImage_GetPitch@4');
    FreeImage_GetDIBSize := GetProcAddr('FreeImage_GetDIBSize@4');
    FreeImage_GetMemorySize := GetProcAddr('FreeImage_GetMemorySize@4');
    FreeImage_GetPalette := GetProcAddr('FreeImage_GetPalette@4');
    FreeImage_GetDotsPerMeterX := GetProcAddr('FreeImage_GetDotsPerMeterX@4');
    FreeImage_GetDotsPerMeterY := GetProcAddr('FreeImage_GetDotsPerMeterY@4');
    FreeImage_SetDotsPerMeterX := GetProcAddr('FreeImage_SetDotsPerMeterX@8');
    FreeImage_SetDotsPerMeterY := GetProcAddr('FreeImage_SetDotsPerMeterY@8');
    FreeImage_GetInfoHeader := GetProcAddr('FreeImage_GetInfoHeader@4');
    FreeImage_GetInfo := GetProcAddr('FreeImage_GetInfo@4');
    FreeImage_GetColorType := GetProcAddr('FreeImage_GetColorType@4');
    FreeImage_GetRedMask := GetProcAddr('FreeImage_GetRedMask@4');
    FreeImage_GetGreenMask := GetProcAddr('FreeImage_GetGreenMask@4');
    FreeImage_GetBlueMask := GetProcAddr('FreeImage_GetBlueMask@4');
    FreeImage_GetTransparencyCount := GetProcAddr('FreeImage_GetTransparencyCount@4');
    FreeImage_GetTransparencyTable := GetProcAddr('FreeImage_GetTransparencyTable@4');
    FreeImage_SetTransparent := GetProcAddr('FreeImage_SetTransparent@8');
    FreeImage_SetTransparencyTable := GetProcAddr('FreeImage_SetTransparencyTable@12');
    FreeImage_IsTransparent := GetProcAddr('FreeImage_IsTransparent@4');
    FreeImage_SetTransparentIndex := GetProcAddr('FreeImage_SetTransparentIndex@8');
    FreeImage_GetTransparentIndex := GetProcAddr('FreeImage_GetTransparentIndex@4');
    FreeImage_HasBackgroundColor := GetProcAddr('FreeImage_HasBackgroundColor@4');
    FreeImage_GetBackgroundColor := GetProcAddr('FreeImage_GetBackgroundColor@8');
    FreeImage_SetBackgroundColor := GetProcAddr('FreeImage_SetBackgroundColor@8');
    FreeImage_GetThumbnail := GetProcAddr('FreeImage_GetThumbnail@4');
    FreeImage_SetThumbnail := GetProcAddr('FreeImage_SetThumbnail@8');
    FreeImage_GetICCProfile := GetProcAddr('FreeImage_GetICCProfile@4');
    FreeImage_ConvertLine1To4 := GetProcAddr('FreeImage_ConvertLine1To4@12');
    FreeImage_ConvertLine8To4 := GetProcAddr('FreeImage_ConvertLine8To4@16');
    FreeImage_ConvertLine16To4_555 := GetProcAddr('FreeImage_ConvertLine16To4_555@12');
    FreeImage_ConvertLine16To4_565 := GetProcAddr('FreeImage_ConvertLine16To4_565@12');
    FreeImage_ConvertLine24To4 := GetProcAddr('FreeImage_ConvertLine24To4@12');
    FreeImage_ConvertLine32To4 := GetProcAddr('FreeImage_ConvertLine32To4@12');
    FreeImage_ConvertLine1To8 := GetProcAddr('FreeImage_ConvertLine1To8@12');
    FreeImage_ConvertLine4To8 := GetProcAddr('FreeImage_ConvertLine4To8@12');
    FreeImage_ConvertLine16To8_555 := GetProcAddr('FreeImage_ConvertLine16To8_555@12');
    FreeImage_ConvertLine16To8_565 := GetProcAddr('FreeImage_ConvertLine16To8_565@12');
    FreeImage_ConvertLine24To8 := GetProcAddr('FreeImage_ConvertLine24To8@12');
    FreeImage_ConvertLine32To8 := GetProcAddr('FreeImage_ConvertLine32To8@12');
    FreeImage_ConvertLine1To16_555 := GetProcAddr('FreeImage_ConvertLine1To16_555@16');
    FreeImage_ConvertLine4To16_555 := GetProcAddr('FreeImage_ConvertLine4To16_555@16');
    FreeImage_ConvertLine8To16_555 := GetProcAddr('FreeImage_ConvertLine8To16_555@16');
    FreeImage_ConvertLine16_565_To16_555 := GetProcAddr('FreeImage_ConvertLine16_565_To16_555@12');
    FreeImage_ConvertLine24To16_555 := GetProcAddr('FreeImage_ConvertLine24To16_555@12');
    FreeImage_ConvertLine32To16_555 := GetProcAddr('FreeImage_ConvertLine32To16_555@12');
    FreeImage_ConvertLine1To16_565 := GetProcAddr('FreeImage_ConvertLine1To16_565@16');
    FreeImage_ConvertLine4To16_565 := GetProcAddr('FreeImage_ConvertLine4To16_565@16');
    FreeImage_ConvertLine8To16_565 := GetProcAddr('FreeImage_ConvertLine8To16_565@16');
    FreeImage_ConvertLine16_555_To16_565 := GetProcAddr('FreeImage_ConvertLine16_555_To16_565@12');
    FreeImage_ConvertLine24To16_565 := GetProcAddr('FreeImage_ConvertLine24To16_565@12');
    FreeImage_ConvertLine32To16_565 := GetProcAddr('FreeImage_ConvertLine32To16_565@12');
    FreeImage_ConvertLine1To24 := GetProcAddr('FreeImage_ConvertLine1To24@16');
    FreeImage_ConvertLine4To24 := GetProcAddr('FreeImage_ConvertLine4To24@16');
    FreeImage_ConvertLine8To24 := GetProcAddr('FreeImage_ConvertLine8To24@16');
    FreeImage_ConvertLine16To24_555 := GetProcAddr('FreeImage_ConvertLine16To24_555@12');
    FreeImage_ConvertLine16To24_565 := GetProcAddr('FreeImage_ConvertLine16To24_565@12');
    FreeImage_ConvertLine32To24 := GetProcAddr('FreeImage_ConvertLine32To24@12');
    FreeImage_ConvertLine1To32 := GetProcAddr('FreeImage_ConvertLine1To32@16');
    FreeImage_ConvertLine1To32MapTransparency := GetProcAddr('FreeImage_ConvertLine1To32MapTransparency@24');
    FreeImage_ConvertLine4To32 := GetProcAddr('FreeImage_ConvertLine4To32@16');
    FreeImage_ConvertLine4To32MapTransparency := GetProcAddr('FreeImage_ConvertLine4To32MapTransparency@24');
    FreeImage_ConvertLine8To32 := GetProcAddr('FreeImage_ConvertLine8To32@16');
    FreeImage_ConvertLine8To32MapTransparency := GetProcAddr('FreeImage_ConvertLine8To32MapTransparency@24');
    FreeImage_ConvertLine16To32_555 := GetProcAddr('FreeImage_ConvertLine16To32_555@12');
    FreeImage_ConvertLine16To32_565 := GetProcAddr('FreeImage_ConvertLine16To32_565@12');
    FreeImage_ConvertLine24To32 := GetProcAddr('FreeImage_ConvertLine24To32@12');
    FreeImage_ConvertTo4Bits := GetProcAddr('FreeImage_ConvertTo4Bits@4');
    FreeImage_ConvertTo8Bits := GetProcAddr('FreeImage_ConvertTo8Bits@4');
    FreeImage_ConvertToGreyscale := GetProcAddr('FreeImage_ConvertToGreyscale@4');
    FreeImage_ConvertTo16Bits555 := GetProcAddr('FreeImage_ConvertTo16Bits555@4');
    FreeImage_ConvertTo16Bits565 := GetProcAddr('FreeImage_ConvertTo16Bits565@4');
    FreeImage_ConvertTo24Bits := GetProcAddr('FreeImage_ConvertTo24Bits@4');
    FreeImage_ConvertTo32Bits := GetProcAddr('FreeImage_ConvertTo32Bits@4');
    FreeImage_ColorQuantize := GetProcAddr('FreeImage_ColorQuantize@8');
    FreeImage_ColorQuantizeEx := GetProcAddr('FreeImage_ColorQuantizeEx@20');
    FreeImage_Threshold := GetProcAddr('FreeImage_Threshold@8');
    FreeImage_Dither := GetProcAddr('FreeImage_Dither@8');
    FreeImage_ConvertFromRawBits := GetProcAddr('FreeImage_ConvertFromRawBits@36');
    FreeImage_ConvertFromRawBitsEx := GetProcAddr('FreeImage_ConvertFromRawBitsEx@44');
    FreeImage_ConvertToRawBits := GetProcAddr('FreeImage_ConvertToRawBits@32');
    FreeImage_ConvertToFloat := GetProcAddr('FreeImage_ConvertToFloat@4');
    FreeImage_ConvertToRGBF := GetProcAddr('FreeImage_ConvertToRGBF@4');
    FreeImage_ConvertToRGBAF := GetProcAddr('FreeImage_ConvertToRGBAF@4');
    FreeImage_ConvertToUINT16 := GetProcAddr('FreeImage_ConvertToUINT16@4');
    FreeImage_ConvertToRGB16 := GetProcAddr('FreeImage_ConvertToRGB16@4');
    FreeImage_ConvertToRGBA16 := GetProcAddr('FreeImage_ConvertToRGBA16@4');
    FreeImage_ConvertToStandardType := GetProcAddr('FreeImage_ConvertToStandardType@8');
    FreeImage_ConvertToType := GetProcAddr('FreeImage_ConvertToType@12');
    FreeImage_ToneMapping := GetProcAddr('FreeImage_ToneMapping@24');
    FreeImage_TmoDrago03 := GetProcAddr('FreeImage_TmoDrago03@20');
    FreeImage_TmoReinhard05 := GetProcAddr('FreeImage_TmoReinhard05@20');
    FreeImage_TmoReinhard05Ex := GetProcAddr('FreeImage_TmoReinhard05Ex@36');
    FreeImage_TmoFattal02 := GetProcAddr('FreeImage_TmoFattal02@20');
    FreeImage_ZLibCompress := GetProcAddr('FreeImage_ZLibCompress@16');
    FreeImage_ZLibUncompress := GetProcAddr('FreeImage_ZLibUncompress@16');
    FreeImage_ZLibGZip := GetProcAddr('FreeImage_ZLibGZip@16');
    FreeImage_ZLibGUnzip := GetProcAddr('FreeImage_ZLibGUnzip@16');
    FreeImage_ZLibCRC32 := GetProcAddr('FreeImage_ZLibCRC32@12');
    FreeImage_CreateTag := GetProcAddr('FreeImage_CreateTag@0');
    FreeImage_DeleteTag := GetProcAddr('FreeImage_DeleteTag@4');
    FreeImage_CloneTag := GetProcAddr('FreeImage_CloneTag@4');
    FreeImage_GetTagKey := GetProcAddr('FreeImage_GetTagKey@4');
    FreeImage_GetTagDescription := GetProcAddr('FreeImage_GetTagDescription@4');
    FreeImage_GetTagID := GetProcAddr('FreeImage_GetTagID@4');
    FreeImage_GetTagType := GetProcAddr('FreeImage_GetTagType@4');
    FreeImage_GetTagCount := GetProcAddr('FreeImage_GetTagCount@4');
    FreeImage_GetTagLength := GetProcAddr('FreeImage_GetTagLength@4');
    FreeImage_GetTagValue := GetProcAddr('FreeImage_GetTagValue@4');
    FreeImage_SetTagKey := GetProcAddr('FreeImage_SetTagKey@8');
    FreeImage_SetTagDescription := GetProcAddr('FreeImage_SetTagDescription@8');
    FreeImage_SetTagID := GetProcAddr('FreeImage_SetTagID@8');
    FreeImage_SetTagType := GetProcAddr('FreeImage_SetTagType@8');
    FreeImage_SetTagCount := GetProcAddr('FreeImage_SetTagCount@8');
    FreeImage_SetTagLength := GetProcAddr('FreeImage_SetTagLength@8');
    FreeImage_SetTagValue := GetProcAddr('FreeImage_SetTagValue@8');
    FreeImage_FindFirstMetadata := GetProcAddr('FreeImage_FindFirstMetadata@12');
    FreeImage_FindNextMetadata := GetProcAddr('FreeImage_FindNextMetadata@8');
    FreeImage_FindCloseMetadata := GetProcAddr('FreeImage_FindCloseMetadata@4');
    FreeImage_SetMetadata := GetProcAddr('FreeImage_SetMetadata@16');
    FreeImage_GetMetadata := GetProcAddr('FreeImage_GetMetadata@16');
    FreeImage_SetMetadataKeyValue := GetProcAddr('FreeImage_SetMetadataKeyValue@16');
    FreeImage_GetMetadataCount := GetProcAddr('FreeImage_GetMetadataCount@8');
    FreeImage_CloneMetadata := GetProcAddr('FreeImage_CloneMetadata@8');
    FreeImage_TagToString := GetProcAddr('FreeImage_TagToString@12');

//    FreeImage_JPEGTransform := GetProcAddr('FreeImage_JPEGTransform@16');
//    FreeImage_JPEGTransformU := GetProcAddr('FreeImage_JPEGTransformU@16');
//    FreeImage_JPEGCrop := GetProcAddr('FreeImage_JPEGCrop@24');
//    FreeImage_JPEGCropU := GetProcAddr('FreeImage_JPEGCropU@24');
//    FreeImage_JPEGTransformFromHandle := GetProcAddr('FreeImage_JPEGTransformFromHandle@40');
//    FreeImage_JPEGTransformCombined := GetProcAddr('FreeImage_JPEGTransformCombined@32');
//    FreeImage_JPEGTransformCombinedU := GetProcAddr('FreeImage_JPEGTransformCombinedU@32');
//    FreeImage_JPEGTransformCombinedFromMemory := GetProcAddr('FreeImage_JPEGTransformCombinedFromMemory@32');

//    FreeImage_RotateClassic := GetProcAddr('FreeImage_RotateClassic@12');

    FreeImage_Rotate := GetProcAddr('FreeImage_Rotate@16');
    FreeImage_RotateEx := GetProcAddr('FreeImage_RotateEx@48');
    FreeImage_FlipHorizontal := GetProcAddr('FreeImage_FlipHorizontal@4');
    FreeImage_FlipVertical := GetProcAddr('FreeImage_FlipVertical@4');
    FreeImage_Rescale := GetProcAddr('FreeImage_Rescale@16');
    FreeImage_MakeThumbnail := GetProcAddr('FreeImage_MakeThumbnail@12');
    FreeImage_RescaleRect := GetProcAddr('FreeImage_RescaleRect@36');
    FreeImage_AdjustCurve := GetProcAddr('FreeImage_AdjustCurve@12');
    FreeImage_AdjustGamma := GetProcAddr('FreeImage_AdjustGamma@12');
    FreeImage_AdjustBrightness := GetProcAddr('FreeImage_AdjustBrightness@12');
    FreeImage_AdjustContrast := GetProcAddr('FreeImage_AdjustContrast@12');
    FreeImage_Invert := GetProcAddr('FreeImage_Invert@4');
    FreeImage_GetHistogram := GetProcAddr('FreeImage_GetHistogram@12');
    FreeImage_GetAdjustColorsLookupTable := GetProcAddr('FreeImage_GetAdjustColorsLookupTable@32');
    FreeImage_AdjustColors := GetProcAddr('FreeImage_AdjustColors@32');
    FreeImage_ApplyColorMapping := GetProcAddr('FreeImage_ApplyColorMapping@24');
    FreeImage_SwapColors := GetProcAddr('FreeImage_SwapColors@16');
    FreeImage_ApplyPaletteIndexMapping := GetProcAddr('FreeImage_ApplyPaletteIndexMapping@20');
    FreeImage_SwapPaletteIndices := GetProcAddr('FreeImage_SwapPaletteIndices@12');
    FreeImage_GetChannel := GetProcAddr('FreeImage_GetChannel@8');
    FreeImage_SetChannel := GetProcAddr('FreeImage_SetChannel@12');
    FreeImage_GetComplexChannel := GetProcAddr('FreeImage_GetComplexChannel@8');
    FreeImage_SetComplexChannel := GetProcAddr('FreeImage_SetComplexChannel@12');
    FreeImage_Copy := GetProcAddr('FreeImage_Copy@20');
    FreeImage_Paste := GetProcAddr('FreeImage_Paste@20');
    FreeImage_CreateView := GetProcAddr('FreeImage_CreateView@20');
    FreeImage_Composite := GetProcAddr('FreeImage_Composite@16');
    FreeImage_PreMultiplyWithAlpha := GetProcAddr('FreeImage_PreMultiplyWithAlpha@4');
    FreeImage_FillBackground := GetProcAddr('FreeImage_FillBackground@12');
    FreeImage_EnlargeCanvas := GetProcAddr('FreeImage_EnlargeCanvas@28');
    FreeImage_AllocateEx := GetProcAddr('FreeImage_AllocateEx@36');
    FreeImage_AllocateExT := GetProcAddr('FreeImage_AllocateExT@40');
    FreeImage_MultigridPoissonSolver := GetProcAddr('FreeImage_MultigridPoissonSolver@8');

    GIsInitialized := True;
  finally
    GLock.Release;
  end;
end;

procedure FinFreeImageLib;
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

    FreeImage_Initialise := nil;
    FreeImage_DeInitialise := nil;
    FreeImage_GetVersion := nil;
    FreeImage_GetCopyrightMessage := nil;
    FreeImage_SetOutputMessageStdCall := nil;
    FreeImage_SetOutputMessage := nil;
    FreeImage_Allocate := nil;
    FreeImage_AllocateT := nil;
    FreeImage_Clone := nil;
    FreeImage_Unload := nil;
    FreeImage_HasPixels := nil;
    FreeImage_Load := nil;
    FreeImage_LoadU := nil;
    FreeImage_LoadFromHandle := nil;
    FreeImage_Save := nil;
    FreeImage_SaveU := nil;
    FreeImage_SaveToHandle := nil;
    FreeImage_OpenMemory := nil;
    FreeImage_CloseMemory := nil;
    FreeImage_LoadFromMemory := nil;
    FreeImage_SaveToMemory := nil;
    FreeImage_TellMemory := nil;
    FreeImage_SeekMemory := nil;
    FreeImage_AcquireMemory := nil;
    FreeImage_ReadMemory := nil;
    FreeImage_WriteMemory := nil;
    FreeImage_LoadMultiBitmapFromMemory := nil;
    FreeImage_SaveMultiBitmapToMemory := nil;
    FreeImage_RegisterLocalPlugin := nil;
    FreeImage_RegisterExternalPlugin := nil;
    FreeImage_GetFIFCount := nil;
    FreeImage_SetPluginEnabled := nil;
    FreeImage_IsPluginEnabled := nil;
    FreeImage_GetFIFFromFormat := nil;
    FreeImage_GetFIFFromMime := nil;
    FreeImage_GetFormatFromFIF := nil;
    FreeImage_GetFIFExtensionList := nil;
    FreeImage_GetFIFDescription := nil;
    FreeImage_GetFIFRegExpr := nil;
    FreeImage_GetFIFMimeType := nil;
    FreeImage_GetFIFFromFilename := nil;
    FreeImage_GetFIFFromFilenameU := nil;
    FreeImage_FIFSupportsReading := nil;
    FreeImage_FIFSupportsWriting := nil;
    FreeImage_FIFSupportsExportBPP := nil;
    FreeImage_FIFSupportsExportType := nil;
    FreeImage_FIFSupportsICCProfiles := nil;
    FreeImage_FIFSupportsNoPixels := nil;
    FreeImage_OpenMultiBitmap := nil;
    FreeImage_OpenMultiBitmapFromHandle := nil;
    FreeImage_SaveMultiBitmapToHandle := nil;
    FreeImage_CloseMultiBitmap := nil;
    FreeImage_GetPageCount := nil;
    FreeImage_AppendPage := nil;
    FreeImage_InsertPage := nil;
    FreeImage_DeletePage := nil;
    FreeImage_LockPage := nil;
    FreeImage_UnlockPage := nil;
    FreeImage_MovePage := nil;
    FreeImage_GetLockedPageNumbers := nil;
    FreeImage_GetFileType := nil;
    FreeImage_GetFileTypeU := nil;
    FreeImage_GetFileTypeFromHandle := nil;
    FreeImage_GetFileTypeFromMemory := nil;
    FreeImage_GetImageType := nil;
    FreeImage_IsLittleEndian := nil;
    FreeImage_LookupX11Color := nil;
    FreeImage_LookupSVGColor := nil;
    FreeImage_GetBits := nil;
    FreeImage_GetScanLine := nil;
    FreeImage_GetPixelIndex := nil;
    FreeImage_GetPixelColor := nil;
    FreeImage_SetPixelIndex := nil;
    FreeImage_SetPixelColor := nil;
    FreeImage_GetColorsUsed := nil;
    FreeImage_GetBPP := nil;
    FreeImage_GetWidth := nil;
    FreeImage_GetHeight := nil;
    FreeImage_GetLine := nil;
    FreeImage_GetPitch := nil;
    FreeImage_GetDIBSize := nil;
    FreeImage_GetMemorySize := nil;
    FreeImage_GetPalette := nil;
    FreeImage_GetDotsPerMeterX := nil;
    FreeImage_GetDotsPerMeterY := nil;
    FreeImage_SetDotsPerMeterX := nil;
    FreeImage_SetDotsPerMeterY := nil;
    FreeImage_GetInfoHeader := nil;
    FreeImage_GetInfo := nil;
    FreeImage_GetColorType := nil;
    FreeImage_GetRedMask := nil;
    FreeImage_GetGreenMask := nil;
    FreeImage_GetBlueMask := nil;
    FreeImage_GetTransparencyCount := nil;
    FreeImage_GetTransparencyTable := nil;
    FreeImage_SetTransparent := nil;
    FreeImage_SetTransparencyTable := nil;
    FreeImage_IsTransparent := nil;
    FreeImage_SetTransparentIndex := nil;
    FreeImage_GetTransparentIndex := nil;
    FreeImage_HasBackgroundColor := nil;
    FreeImage_GetBackgroundColor := nil;
    FreeImage_SetBackgroundColor := nil;
    FreeImage_GetThumbnail := nil;
    FreeImage_SetThumbnail := nil;
    FreeImage_GetICCProfile := nil;
    FreeImage_ConvertLine1To4 := nil;
    FreeImage_ConvertLine8To4 := nil;
    FreeImage_ConvertLine16To4_555 := nil;
    FreeImage_ConvertLine16To4_565 := nil;
    FreeImage_ConvertLine24To4 := nil;
    FreeImage_ConvertLine32To4 := nil;
    FreeImage_ConvertLine1To8 := nil;
    FreeImage_ConvertLine4To8 := nil;
    FreeImage_ConvertLine16To8_555 := nil;
    FreeImage_ConvertLine16To8_565 := nil;
    FreeImage_ConvertLine24To8 := nil;
    FreeImage_ConvertLine32To8 := nil;
    FreeImage_ConvertLine1To16_555 := nil;
    FreeImage_ConvertLine4To16_555 := nil;
    FreeImage_ConvertLine8To16_555 := nil;
    FreeImage_ConvertLine16_565_To16_555 := nil;
    FreeImage_ConvertLine24To16_555 := nil;
    FreeImage_ConvertLine32To16_555 := nil;
    FreeImage_ConvertLine1To16_565 := nil;
    FreeImage_ConvertLine4To16_565 := nil;
    FreeImage_ConvertLine8To16_565 := nil;
    FreeImage_ConvertLine16_555_To16_565 := nil;
    FreeImage_ConvertLine24To16_565 := nil;
    FreeImage_ConvertLine32To16_565 := nil;
    FreeImage_ConvertLine1To24 := nil;
    FreeImage_ConvertLine4To24 := nil;
    FreeImage_ConvertLine8To24 := nil;
    FreeImage_ConvertLine16To24_555 := nil;
    FreeImage_ConvertLine16To24_565 := nil;
    FreeImage_ConvertLine32To24 := nil;
    FreeImage_ConvertLine1To32 := nil;
    FreeImage_ConvertLine1To32MapTransparency := nil;
    FreeImage_ConvertLine4To32 := nil;
    FreeImage_ConvertLine4To32MapTransparency := nil;
    FreeImage_ConvertLine8To32 := nil;
    FreeImage_ConvertLine8To32MapTransparency := nil;
    FreeImage_ConvertLine16To32_555 := nil;
    FreeImage_ConvertLine16To32_565 := nil;
    FreeImage_ConvertLine24To32 := nil;
    FreeImage_ConvertTo4Bits := nil;
    FreeImage_ConvertTo8Bits := nil;
    FreeImage_ConvertToGreyscale := nil;
    FreeImage_ConvertTo16Bits555 := nil;
    FreeImage_ConvertTo16Bits565 := nil;
    FreeImage_ConvertTo24Bits := nil;
    FreeImage_ConvertTo32Bits := nil;
    FreeImage_ColorQuantize := nil;
    FreeImage_ColorQuantizeEx := nil;
    FreeImage_Threshold := nil;
    FreeImage_Dither := nil;
    FreeImage_ConvertFromRawBits := nil;
    FreeImage_ConvertFromRawBitsEx := nil;
    FreeImage_ConvertToRawBits := nil;
    FreeImage_ConvertToFloat := nil;
    FreeImage_ConvertToRGBF := nil;
    FreeImage_ConvertToRGBAF := nil;
    FreeImage_ConvertToUINT16 := nil;
    FreeImage_ConvertToRGB16 := nil;
    FreeImage_ConvertToRGBA16 := nil;
    FreeImage_ConvertToStandardType := nil;
    FreeImage_ConvertToType := nil;
    FreeImage_ToneMapping := nil;
    FreeImage_TmoDrago03 := nil;
    FreeImage_TmoReinhard05 := nil;
    FreeImage_TmoReinhard05Ex := nil;
    FreeImage_TmoFattal02 := nil;
    FreeImage_ZLibCompress := nil;
    FreeImage_ZLibUncompress := nil;
    FreeImage_ZLibGZip := nil;
    FreeImage_ZLibGUnzip := nil;
    FreeImage_ZLibCRC32 := nil;
    FreeImage_CreateTag := nil;
    FreeImage_DeleteTag := nil;
    FreeImage_CloneTag := nil;
    FreeImage_GetTagKey := nil;
    FreeImage_GetTagDescription := nil;
    FreeImage_GetTagID := nil;
    FreeImage_GetTagType := nil;
    FreeImage_GetTagCount := nil;
    FreeImage_GetTagLength := nil;
    FreeImage_GetTagValue := nil;
    FreeImage_SetTagKey := nil;
    FreeImage_SetTagDescription := nil;
    FreeImage_SetTagID := nil;
    FreeImage_SetTagType := nil;
    FreeImage_SetTagCount := nil;
    FreeImage_SetTagLength := nil;
    FreeImage_SetTagValue := nil;
    FreeImage_FindFirstMetadata := nil;
    FreeImage_FindNextMetadata := nil;
    FreeImage_FindCloseMetadata := nil;
    FreeImage_SetMetadata := nil;
    FreeImage_GetMetadata := nil;
    FreeImage_SetMetadataKeyValue := nil;
    FreeImage_GetMetadataCount := nil;
    FreeImage_CloneMetadata := nil;
    FreeImage_TagToString := nil;

//    FreeImage_JPEGTransform := nil;
//    FreeImage_JPEGTransformU := nil;
//    FreeImage_JPEGCrop := nil;
//    FreeImage_JPEGCropU := nil;
//    FreeImage_JPEGTransformFromHandle := nil;
//    FreeImage_JPEGTransformCombined := nil;
//    FreeImage_JPEGTransformCombinedU := nil;
//    FreeImage_JPEGTransformCombinedFromMemory := nil;

//    FreeImage_RotateClassic := nil;

    FreeImage_Rotate := nil;
    FreeImage_RotateEx := nil;
    FreeImage_FlipHorizontal := nil;
    FreeImage_FlipVertical := nil;
    FreeImage_Rescale := nil;
    FreeImage_MakeThumbnail := nil;
    FreeImage_RescaleRect := nil;
    FreeImage_AdjustCurve := nil;
    FreeImage_AdjustGamma := nil;
    FreeImage_AdjustBrightness := nil;
    FreeImage_AdjustContrast := nil;
    FreeImage_Invert := nil;
    FreeImage_GetHistogram := nil;
    FreeImage_GetAdjustColorsLookupTable := nil;
    FreeImage_AdjustColors := nil;
    FreeImage_ApplyColorMapping := nil;
    FreeImage_SwapColors := nil;
    FreeImage_ApplyPaletteIndexMapping := nil;
    FreeImage_SwapPaletteIndices := nil;
    FreeImage_GetChannel := nil;
    FreeImage_SetChannel := nil;
    FreeImage_GetComplexChannel := nil;
    FreeImage_SetComplexChannel := nil;
    FreeImage_Copy := nil;
    FreeImage_Paste := nil;
    FreeImage_CreateView := nil;
    FreeImage_Composite := nil;
    FreeImage_PreMultiplyWithAlpha := nil;
    FreeImage_FillBackground := nil;
    FreeImage_EnlargeCanvas := nil;
    FreeImage_AllocateEx := nil;
    FreeImage_AllocateExT := nil;
    FreeImage_MultigridPoissonSolver := nil;

    GIsInitialized := False;
  finally
    GLock.Release;
  end;
end;

initialization
  GLock := TCriticalSection.Create;

finalization
  FinFreeImageLib;
  FreeAndNil(GLock);
{$ENDIF}

end.
