unit LibJpeg8;
{
/*
 * jpeglib.h
 *
 * Copyright (C) 1991-1998, Thomas G. Lane.
 * Modified 2002-2011 by Guido Vollbeding.
 * This file is part of the Independent JPEG Group's software.
 * For conditions of distribution and use, see the accompanying README file.
 *
 * This file defines the application interface for the JPEG library.
 * Most applications using the library need only include this file,
 * and perhaps jerror.h if they want to know the exact error codes.
 */
}
interface

{$INCLUDE LibJpeg.inc}

{$IFDEF FPC}
  {$MODE Delphi}

  {$IFDEF CPUI386}
    {$DEFINE CPU386}
    {$ASMMODE INTEL}
  {$ENDIF}

  {$IFNDEF WIN32}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}

{$EXTENDEDSYNTAX ON}
{$ALIGN 8}
{$MINENUMSIZE 4}

const
  {$ifdef win32}
    LIB_JPEG_NAME = 'jpeg8.dll';
  {$else}
    LIB_JPEG_NAME = 'libjpeg.so.8';
  {$endif}

type
  UINT8 = byte;
  UINT16 = word;
  JDIMENSION = cardinal;
  JCOEF = smallint;
  JOCTET = byte;
  JSAMPLE = byte;
  
  JOCTET_ptr = ^JOCTET;
  JSAMPLE_ptr = ^JSAMPLE;

{$ifdef LINUX}
  // Under linux it's looks like that the the booleans are allways 4 byte large.
  JBOOL = Cardinal;
{$else}
  // Under windows they are only 1 byte large.
  JBOOL = Byte;
{$endif}


const
  JPEG_LIB_VERSION = 80;  { Version 8d }
  JPEG_LIB_VERSION_MAJOR = 8;
  JPEG_LIB_VERSION_MINOR = 4;
  
  DCTSIZE	= 8;  { The basic DCT block is 8x8 samples }
  DCTSIZE2 = 64;  { DCTSIZE squared; # of elements in a block }
  NUM_QUANT_TBLS = 4;  { Quantization tables are numbered 0..3 }
  NUM_HUFF_TBLS = 4;  { Huffman tables are numbered 0..3 }
  NUM_ARITH_TBLS = 16;  { Arith-coding tables are numbered 0..15 }
  MAX_COMPS_IN_SCAN = 4;  { JPEG limit on # of components in one scan }
  MAX_SAMP_FACTOR = 4;  { JPEG limit on sampling factors }
  
{ Unfortunately, some bozo at Adobe saw no reason to be bound by the standard;
  the PostScript DCT filter can emit files with many more than 10 blocks/MCU.
  If you happen to run across such a file, you can up D_MAX_BLOCKS_IN_MCU
  to handle it.  We even let you do this from the jconfig.h file.  However,
  we strongly discourage changing C_MAX_BLOCKS_IN_MCU; just because Adobe
  sometimes emits noncompliant files doesn't mean you should too. }
  C_MAX_BLOCKS_IN_MCU = 10;  { compressor's limit on blocks per MCU }
  D_MAX_BLOCKS_IN_MCU = 10;  { decompressor's limit on blocks per MCU }
  
type
{ Data structures for images (arrays of samples and of DCT coefficients).
  On 80x86 machines, the image arrays are too big for near pointers,
  but the pointer arrays can fit in near memory. }
 
  JSAMPROW = ^JSAMPLE;  { ptr to one image row of pixel samples. }
  JSAMPARRAY = ^JSAMPROW;  { ptr to some rows (a 2-D sample array) }
  JSAMPIMAGE = ^JSAMPARRAY;  { a 3-D sample array: top index is color }

  JBLOCK = array[1..DCTSIZE2] of JCOEF;  { one block of coefficients }
  JBLOCKROW = ^JBLOCK;  { pointer to one row of coefficient blocks }
  JBLOCKARRAY = ^JBLOCKROW;  { a 2-D array of coefficient blocks }
  JBLOCKIMAGE = ^JBLOCKARRAY;  { a 3-D array of coefficient blocks }

  JCOEFPTR = ^JCOEF;  { useful in a couple of places }

  JCOEFBITSPTR = ^JCOEFBITS;
  JCOEFBITS = array [0..DCTSIZE2 -1] of integer;
  
{ Types for JPEG compression parameters and working tables. }

{ DCT coefficient quantization tables. }
JQUANT_TBL = record 
  { This array gives the coefficient quantizers in natural array order
    (not the zigzag order in which they are stored in a JPEG DQT marker).
    CAUTION: IJG versions prior to v6a kept this array in zigzag order. }
  quantval: array [0..DCTSIZE2 -1] of UINT16;  { quantization step for each coefficient }

  { This field is used only during compression.  It's initialized FALSE when
    the table is created, and set TRUE when it's been output to the file.
    You could suppress output of a table by setting this to TRUE.
    (See jpeg_suppress_tables for an example.) }
  sent_table: JBOOL;     { TRUE when table has been output }
end;
JQUANT_TBL_ptr = ^JQUANT_TBL;


{ Huffman coding tables. }
JHUFF_TBL = record
  { These two fields directly represent the contents of a JPEG DHT marker }
  bits: array [0..16] of UINT8;  { bits[k] = # of symbols with codes of }
                    { length k bits; bits[0] is unused }
  huffval: array [0..255] of UINT8;  { The symbols, in order of incr code length }
  { This field is used only during compression.  It's initialized FALSE when
    the table is created, and set TRUE when it's been output to the file.
    You could suppress output of a table by setting this to TRUE.
    (See jpeg_suppress_tables for an example.) }
  sent_table: JBOOL;  { TRUE when table has been output }
end;
JHUFF_TBL_ptr = ^JHUFF_TBL;


{ Basic info about one component (color channel). }
jpeg_component_info_ptr = ^jpeg_component_info;
jpeg_component_info = record 
  { These values are fixed over the whole image. }
  { For compression, they must be supplied by parameter setup; }
  { for decompression, they are read from the SOF marker. }
  component_id: integer;    { identifier for this component (0..255) }
  component_index: integer; { its index in SOF or cinfo->comp_info[] }
  h_samp_factor: integer;   { horizontal sampling factor (1..4) }
  v_samp_factor: integer;   { vertical sampling factor (1..4) }
  quant_tbl_no: integer;    { quantization table selector (0..3) }
  { These values may vary between scans. }
  { For compression, they must be supplied by parameter setup; }
  { for decompression, they are read from the SOS marker. }
  { The decompressor output side may not use these variables. }
  dc_tbl_no: integer;  { DC entropy table selector (0..3) }
  ac_tbl_no: integer;  { AC entropy table selector (0..3) }
  
  { Remaining fields should be treated as private by applications. }
  
  { These values are computed during compression or decompression startup: }
  { Component's size in DCT blocks.
   Any dummy blocks added to complete an MCU are not counted; therefore
   these values do not depend on whether a scan is interleaved or not.  }
  width_in_blocks: JDIMENSION;
  height_in_blocks: JDIMENSION;
  { Size of a DCT block in samples.  Always DCTSIZE for compression.
    For decompression this is the size of the output from one DCT block,
    reflecting any scaling we choose to apply during the IDCT step.
    Values of 1,2,4,8 are likely to be supported.  Note that different
    components may receive different IDCT scalings.  }
  DCT_h_scaled_size: integer;
  DCT_v_scaled_size: integer;
  { The downsampled dimensions are the component's actual, unpadded number
    of samples at the main buffer (preprocessing/compression interface), thus
    downsampled_width = ceil(image_width * Hi/Hmax)
    and similarly for height.  For decompression, IDCT scaling is included, so
    downsampled_width = ceil(image_width * Hi/Hmax * DCT_scaled_size/DCTSIZE) }
  downsampled_width: JDIMENSION;   { actual width in samples }
  downsampled_height: JDIMENSION;  { actual height in samples }
  { This flag is used only for decompression.  In cases where some of the
    components will be ignored (eg grayscale output from YCbCr image),
    we can skip most computations for the unused components.  }
  component_needed: JBOOL;  { do we need the value of this component? }

  { These values are computed before starting a scan of the component. }
  { The decompressor output side may not use these variables. }
  MCU_width: integer;        { number of blocks per MCU, horizontally }
  MCU_height: integer;       { number of blocks per MCU, vertically }
  MCU_blocks: integer;	     { MCU_width * MCU_height }
  MCU_sample_width: integer; { MCU width in samples, MCU_width*DCT_scaled_size }
  last_col_width: integer;   { # of non-dummy blocks across in last MCU }
  last_row_height: integer;  { # of non-dummy blocks down in last MCU }

  { Saved quantization table for component; NULL if none yet saved.
    See jdinput.c comments about the need for this information.
    This field is currently used only for decompression.  }
  quant_table: JQUANT_TBL_ptr;

  { Private per-component storage for DCT or IDCT subsystem. }
  dct_table: pointer;
end;


{ The script for encoding a multiple-scan file is an array of these: }
jpeg_scan_info_ptr = ^jpeg_scan_info;
jpeg_scan_info = record 
  comps_in_scan: integer;  { number of components encoded in this scan }
  component_index: array [0..MAX_COMPS_IN_SCAN -1] of integer; { their SOF/comp_info[] indexes }
  Ss: integer;
  Se: integer;             { progressive JPEG spectral selection parms }
  Ah: integer;
  Al: integer;             { progressive JPEG successive approx. parms }
end;


{ The decompressor can save APPn and COM markers in a list of these: }
jpeg_saved_marker_ptr = ^jpeg_marker_struct;
jpeg_marker_struct = record
  next: jpeg_saved_marker_ptr;  { next in list, or NULL *}
  marker: UINT8;                { marker code: JPEG_COM, or JPEG_APP0+n }
  original_length: cardinal;    { # bytes of data in the file }
  data_length: cardinal;        { # bytes of data saved at data[] }
  data: JOCTET_ptr;             { the data contained in the marker }
  { the marker length word is not counted in data_length or original_length }
end;


{ Known color spaces. }
J_COLOR_SPACE = (
  JCS_UNKNOWN,    { error/unspecified }
  JCS_GRAYSCALE,  { monochrome }
  JCS_RGB,        { red/green/blue }
  JCS_YCbCr,      { Y/Cb/Cr (also known as YUV) }
  JCS_CMYK,       { C/M/Y/K }
  JCS_YCCK        { Y/Cb/Cr/K }
    
{$IFDEF LIB_JPEG_8_TURBO_JCS_EXTENSIONS}

  { libjpeg-turbo includes extensions that allow JPEG 
    images to be compressed directly from (and decompressed 
    directly to) buffers that use BGR, BGRX, RGBX, XBGR, and 
    XRGB pixel ordering. }
  ,
  JCS_EXT_RGB,  { red/green/blue }
  JCS_EXT_RGBX,  { red/green/blue/x }
  JCS_EXT_BGR,  { blue/green/red }
  JCS_EXT_BGRX,  { blue/green/red/x }
  JCS_EXT_XBGR,  { x/blue/green/red }
  JCS_EXT_XRGB  { x/red/green/blue }
  
  {$IFDEF LIB_JPEG_8_TURBO_JCS_ALPHA_EXTENSIONS}
  
    { When out_color_space it set to JCS_EXT_RGBX, JCS_EXT_BGRX,
    JCS_EXT_XBGR, or JCS_EXT_XRGB during decompression, the X byte is
    undefined, and in order to ensure the best performance,
    libjpeg-turbo can set that byte to whatever value it wishes.  Use
    the following colorspace constants to ensure that the X byte is set
    to 0xFF, so that it can be interpreted as an opaque alpha
    channel. }
    ,
  JCS_EXT_RGBA,    { red/green/blue/alpha }
  JCS_EXT_BGRA,    { blue/green/red/alpha }
  JCS_EXT_ABGR,    { alpha/blue/green/red }
  JCS_EXT_ARGB    { alpha/red/green/blue }

  {$ENDIF} // LIB_JPEG_8_TURBO_JCS_ALPHA_EXTENSIONS
{$ENDIF} // LIB_JPEG_8_TURBO_JCS_EXTENSIONS
);


{ DCT/IDCT algorithm options. }
J_DCT_METHOD = (
	JDCT_ISLOW,  { slow but accurate integer algorithm }
	JDCT_IFAST,  { faster, less accurate integer method }
	JDCT_FLOAT   { floating-point: accurate, fast on fast HW }
);

const
  JDCT_DEFAULT = JDCT_ISLOW;
  JDCT_FASTEST = JDCT_IFAST;

type
{ Dithering options for decompression. }
J_DITHER_MODE = (
	JDITHER_NONE,     { no dithering }
	JDITHER_ORDERED,  { simple ordered dither }
	JDITHER_FS        { Floyd-Steinberg error diffusion dither }
);

const  
  JPOOL_PERMANENT = 0;  { lasts until master record is destroyed }
  JPOOL_IMAGE = 1;  { lasts until done with image/datastream }
  JPOOL_NUMPOOLS = 2;

{ Values of global_state field (jdapi.c has some dependencies on ordering!) }
  CSTATE_START        = 100;    { after create_compress }
  CSTATE_SCANNING     = 101;    { start_compress done, write_scanlines OK }
  CSTATE_RAW_OK       = 102;    { start_compress done, write_raw_data OK }
  CSTATE_WRCOEFS      = 103;    { jpeg_write_coefficients done }
  DSTATE_START        = 200;    { after create_decompress }
  DSTATE_INHEADER     = 201;    { reading header markers, no SOS yet }
  DSTATE_READY        = 202;    { found SOS, ready for start_decompress }
  DSTATE_PRELOAD      = 203;    { reading multiscan file in start_decompress}
  DSTATE_PRESCAN      = 204;    { performing dummy pass for 2-pass quant }
  DSTATE_SCANNING     = 205;    { start_decompress done, read_scanlines OK }
  DSTATE_RAW_OK       = 206;    { start_decompress done, read_raw_data OK }
  DSTATE_BUFIMAGE     = 207;    { expecting jpeg_start_output }
  DSTATE_BUFPOST      = 208;    { looking for SOS/EOI in jpeg_finish_output }
  DSTATE_RDCOEFS      = 209;    { reading file in jpeg_read_coefficients }
  DSTATE_STOPPING     = 210;    { looking for EOI in jpeg_finish_decompress }


type
// pointer forward
jpeg_error_mgr_ptr = ^jpeg_error_mgr;
jpeg_memory_mgr_ptr = ^jpeg_memory_mgr;
jpeg_progress_mgr_ptr = ^jpeg_progress_mgr;
jpeg_destination_mgr_ptr = ^jpeg_destination_mgr;
jpeg_source_mgr_ptr = ^jpeg_source_mgr;

// dummy types
jvirt_sarray_control = record dummy: longint; end;
jvirt_sarray_ptr = ^jvirt_sarray_control;
jvirt_sarray_ptr_ptr = ^jvirt_sarray_ptr;

jvirt_barray_control = record dummy: longint; end;
jvirt_barray_ptr = ^jvirt_barray_control;
jvirt_barray_ptr_ptr = ^jvirt_barray_ptr;

jpeg_comp_master = record dummy: longint; end;
jpeg_comp_master_ptr = ^jpeg_comp_master;

jpeg_c_main_controller = record dummy: longint; end;
jpeg_c_main_controller_ptr = ^jpeg_c_main_controller;

jpeg_c_prep_controller = record dummy: longint; end;
jpeg_c_prep_controller_ptr = ^jpeg_c_prep_controller;

jpeg_c_coef_controller = record dummy: longint; end;
jpeg_c_coef_controller_ptr = ^jpeg_c_coef_controller;

jpeg_marker_writer = record dummy: longint; end;
jpeg_marker_writer_ptr = ^jpeg_marker_writer;

jpeg_color_converter = record dummy: longint; end;
jpeg_color_converter_ptr = ^jpeg_color_converter;

jpeg_downsampler = record dummy: longint; end;
jpeg_downsampler_ptr = ^jpeg_downsampler;

jpeg_forward_dct = record dummy: longint; end;
jpeg_forward_dct_ptr = ^jpeg_forward_dct;

jpeg_entropy_encoder = record dummy: longint; end;
jpeg_entropy_encoder_ptr = ^jpeg_entropy_encoder;

jpeg_decomp_master = record dummy: longint; end;
jpeg_decomp_master_ptr = ^jpeg_decomp_master;

jpeg_d_main_controller = record dummy: longint; end;
jpeg_d_main_controller_ptr = ^jpeg_d_main_controller;

jpeg_d_coef_controller = record dummy: longint; end;
jpeg_d_coef_controller_ptr = ^jpeg_d_coef_controller;

jpeg_d_post_controller = record dummy: longint; end;
jpeg_d_post_controller_ptr = ^jpeg_d_post_controller;

jpeg_input_controller = record dummy: longint; end;
jpeg_input_controller_ptr = ^jpeg_input_controller;

jpeg_marker_reader = record dummy: longint; end;
jpeg_marker_reader_ptr = ^jpeg_marker_reader;

jpeg_entropy_decoder = record dummy: longint; end;
jpeg_entropy_decoder_ptr = ^jpeg_entropy_decoder;

jpeg_inverse_dct = record dummy: longint; end;
jpeg_inverse_dct_ptr = ^jpeg_inverse_dct;

jpeg_upsampler = record dummy: longint; end;
jpeg_upsampler_ptr = ^jpeg_upsampler;

jpeg_color_deconverter = record dummy: longint; end;
jpeg_color_deconverter_ptr = ^jpeg_color_deconverter;

jpeg_color_quantizer = record dummy: longint; end;
jpeg_color_quantizer_ptr = ^jpeg_color_quantizer;



{ Routines that are to be used by both halves of the library are declared
  to receive a pointer to this structure.  There are no actual instances of
  jpeg_common_struct, only of jpeg_compress_struct and jpeg_decompress_struct. }
jpeg_common_struct = record 
  err: jpeg_error_mgr_ptr;          { Error handler module }
  mem: jpeg_memory_mgr_ptr;	        { Memory manager module }
  progress: jpeg_progress_mgr_ptr;  { Progress monitor, or NULL if none }
  client_data: pointer;             { Available for use by application }
  is_decompressor: JBOOL;           { So common code can tell which is which }
  global_state: integer;            { For checking call sequence validity }
  { Additional fields follow in an actual jpeg_compress_struct or
    jpeg_decompress_struct.  All three structs must agree on these
    initial fields!  (This would be a lot cleaner in C++.) }
end;


j_common_ptr = ^jpeg_common_struct;
j_compress_ptr = ^jpeg_compress_struct;
j_decompress_ptr = ^jpeg_decompress_struct;


{ Master record for a compression instance }
jpeg_compress_struct = record 
  err: jpeg_error_mgr_ptr;          { Error handler module }
  mem: jpeg_memory_mgr_ptr;	        { Memory manager module }
  progress: jpeg_progress_mgr_ptr;  { Progress monitor, or NULL if none }
  client_data: pointer;             { Available for use by application }
  is_decompressor: JBOOL;           { So common code can tell which is which }
  global_state: integer;            { For checking call sequence validity }

  { Destination for compressed data }
  dest: jpeg_destination_mgr_ptr;

  { Description of source image --- these fields must be filled in by
    outer application before starting compression.  in_color_space must
    be correct before you can even call jpeg_set_defaults().  }

  image_width: JDIMENSION;          { input image width }
  image_height: JDIMENSION;         { input image height }
  input_components: integer;        { # of color components in input image }
  in_color_space: J_COLOR_SPACE;    { colorspace of input image }

  input_gamma: double;              { image gamma of input image }

  { Compression parameters --- these fields must be set before calling
    jpeg_start_compress().  We recommend calling jpeg_set_defaults() to
    initialize everything to reasonable defaults, then changing anything
    the application specifically wants to change.  That way you won't get
    burnt when new parameters are added.  Also note that there are several
    helper routines to simplify changing parameters. }
  
  scale_num, scale_denom: Cardinal; { fraction by which to scale image }

  jpeg_width: JDIMENSION;	{ scaled JPEG image width }
  jpeg_height: JDIMENSION;	{ scaled JPEG image height }
  
  { Dimensions of actual JPEG image that will be written to file,
    derived from input dimensions by scaling factors above.
    These fields are computed by jpeg_start_compress().
    You can also use jpeg_calc_jpeg_dimensions() to determine these values
    in advance of calling jpeg_start_compress().  }
    
  data_precision: integer;          { bits of precision in image data }

  num_components: integer;          { # of color components in JPEG image }
  jpeg_color_space: J_COLOR_SPACE;  { colorspace of JPEG image }

  comp_info: jpeg_component_info_ptr;
  { comp_info[i] describes component that appears i'th in SOF }

  quant_tbl_ptrs: array [0..NUM_QUANT_TBLS -1] of JQUANT_TBL_ptr;
  q_scale_factor: array [0..NUM_QUANT_TBLS -1] of integer;
  { ptrs to coefficient quantization tables, or NULL if not defined,
    and corresponding scale factors (percentage, initialized 100). }

  dc_huff_tbl_ptrs: array [0..NUM_HUFF_TBLS -1] of JHUFF_TBL_ptr;
  ac_huff_tbl_ptrs: array [0..NUM_HUFF_TBLS -1] of JHUFF_TBL_ptr;
  { ptrs to Huffman coding tables, or NULL if not defined }

  arith_dc_L: array [0..NUM_ARITH_TBLS -1] of UINT8;  { L values for DC arith-coding tables }
  arith_dc_U: array [0..NUM_ARITH_TBLS -1] of UINT8;  { U values for DC arith-coding tables }
  arith_ac_K: array [0..NUM_ARITH_TBLS -1] of UINT8;  { Kx values for AC arith-coding tables }

  num_scans: integer;               { # of entries in scan_info array }
  scan_info: jpeg_scan_info_ptr;    { script for multi-scan file, or NULL }
  { The default value of scan_info is NULL, which causes a single-scan
    sequential JPEG file to be emitted.  To create a multi-scan file,
    set num_scans and scan_info to point to an array of scan definitions.  }

  raw_data_in: JBOOL;         { TRUE=caller supplies downsampled data }
  arith_code: JBOOL;          { TRUE=arithmetic coding, FALSE=Huffman }
  optimize_coding: JBOOL;     { TRUE=optimize entropy encoding parms }
  CCIR601_sampling: JBOOL;    { TRUE=first samples are cosited }
  smoothing_factor: integer;  { 1..100, or 0 for no input smoothing }
  dct_method: J_DCT_METHOD;   { DCT algorithm selector }

  { The restart interval can be specified in absolute MCUs by setting
    restart_interval, or in MCU rows by setting restart_in_rows
    (in which case the correct restart_interval will be figured
    for each scan). }
  restart_interval: cardinal; { MCUs per restart, or 0 for no restart }
  restart_in_rows: integer;   { if > 0, MCU rows per restart interval }

  { Parameters controlling emission of special markers. }

  write_JFIF_header: JBOOL;   { should a JFIF marker be written? }
  JFIF_major_version: UINT8;  { What to write for the JFIF version number }
  JFIF_minor_version: UINT8;
  { These three values are not used by the JPEG code, merely copied }
  { into the JFIF APP0 marker.  density_unit can be 0 for unknown, }
  { 1 for dots/inch, or 2 for dots/cm.  Note that the pixel aspect }
  { ratio is defined by X_density/Y_density even when density_unit=0. }
  density_unit: UINT8;        { JFIF code for pixel size units }
  X_density: UINT16;          { Horizontal pixel density }
  Y_density: UINT16;          { Vertical pixel density }
  write_Adobe_marker: JBOOL;  { should an Adobe marker be written? }

  { State variable: index of next scanline to be written to
    jpeg_write_scanlines().  Application may use this to control its
    processing loop, e.g., "while (next_scanline < image_height)".  }

  next_scanline: JDIMENSION;  { 0 .. image_height-1  }

  { Remaining fields are known throughout compressor, but generally
    should not be touched by a surrounding application.  }

  { These fields are computed during compression startup }
  progressive_mode: JBOOL;    { TRUE if scan script uses progressive mode }
  max_h_samp_factor: integer; { largest h_samp_factor }
  max_v_samp_factor: integer; { largest v_samp_factor }

  min_DCT_h_scaled_size: integer;	{ smallest DCT_h_scaled_size of any component }
  min_DCT_v_scaled_size: integer;	{ smallest DCT_v_scaled_size of any component }

  total_iMCU_rows: JDIMENSION; { # of iMCU rows to be input to coef ctlr }
  { The coefficient controller receives data in units of MCU rows as defined
    for fully interleaved scans (whether the JPEG file is interleaved or not).
    There are v_samp_factor * DCTSIZE sample rows of each component in an
    "iMCU" (interleaved MCU) row.  }
  
  { These fields are valid during any one scan.
    They describe the components and MCUs actually appearing in the scan.  }
  comps_in_scan: integer;     { # of JPEG components in this scan }
  cur_comp_info: array [0..MAX_COMPS_IN_SCAN -1] of jpeg_component_info_ptr;
  { *cur_comp_info[i] describes component that appears i'th in SOS }

  MCUs_per_row: JDIMENSION;   { # of MCUs across the image }
  MCU_rows_in_scan: JDIMENSION;  { # of MCU rows in the image }

  blocks_in_MCU: integer;     { # of DCT blocks per MCU }
  MCU_membership: array [0..C_MAX_BLOCKS_IN_MCU -1] of integer;
  { MCU_membership[i] is index in cur_comp_info of component owning }
  { i'th block in an MCU }

  Ss: integer;
  Se: integer;
  Ah: integer;
  Al: integer;		{ progressive JPEG parameters for scan }

  block_size: integer;		{ the basic DCT block size: 1..16 }
  natural_order: pointer;	{ natural-order position array }
  im_Se: integer;			    { min( Se, DCTSIZE2-1 ) }
  
  { Links to compression subobjects (methods and private variables of modules) }
  master: jpeg_comp_master_ptr;
  main: jpeg_c_main_controller_ptr;
  prep: jpeg_c_prep_controller_ptr;
  coef: jpeg_c_coef_controller_ptr;
  marker: jpeg_marker_writer_ptr;
  cconvert: jpeg_color_converter_ptr;
  downsample: jpeg_downsampler_ptr;
  fdct: jpeg_forward_dct_ptr;
  entropy: jpeg_entropy_encoder_ptr;

  script_space: jpeg_scan_info_ptr; { workspace for jpeg_simple_progression }
  script_space_size: integer;
end;

{ Master record for a decompression instance }
jpeg_decompress_struct = record 
  err: jpeg_error_mgr_ptr;          { Error handler module }
  mem: jpeg_memory_mgr_ptr;	        { Memory manager module }
  progress: jpeg_progress_mgr_ptr;  { Progress monitor, or NULL if none }
  client_data: pointer;             { Available for use by application }
  is_decompressor: JBOOL;           { So common code can tell which is which }
  global_state: Integer;            { For checking call sequence validity }

  { Source of compressed data }
  src: jpeg_source_mgr_ptr;

  { Basic description of image --- filled in by jpeg_read_header(). }
  { Application may inspect these values to decide how to process image. }

  image_width: JDIMENSION;    { nominal image width (from SOF marker) }
  image_height: JDIMENSION;   { nominal image height }
  num_components: integer;    { # of color components in JPEG image }
  jpeg_color_space: J_COLOR_SPACE;  { colorspace of JPEG image }

  { Decompression processing parameters --- these fields must be set before
    calling jpeg_start_decompress().  Note that jpeg_read_header() initializes
    them to default values.  }

  out_color_space: J_COLOR_SPACE; { colorspace for output }

  scale_num: cardinal;
  scale_denom: cardinal;  { fraction by which to scale image }

  output_gamma: double;   { image gamma wanted in output }

  buffered_image: JBOOL;      { TRUE=multiple output passes }
  raw_data_out: JBOOL;        { TRUE=downsampled data wanted }

  dct_method: J_DCT_METHOD;  { IDCT algorithm selector }
  do_fancy_upsampling: JBOOL; { TRUE=apply fancy upsampling }
  do_block_smoothing: JBOOL;  { TRUE=apply interblock smoothing }

  quantize_colors: JBOOL;     { TRUE=colormapped output wanted }
  { the following are ignored if not quantize_colors: }
  dither_mode: J_DITHER_MODE; { type of color dithering to use }
  two_pass_quantize: JBOOL;   { TRUE=use two-pass color quantization }
  desired_number_of_colors: Integer; { max # colors to use in created colormap }
  { these are significant only in buffered-image mode: }
  enable_1pass_quant: JBOOL;  { enable future use of 1-pass quantizer }
  enable_external_quant: JBOOL; { enable future use of external colormap }
  enable_2pass_quant: JBOOL;  { enable future use of 2-pass quantizer }
  { Description of actual output image that will be returned to application.
    These fields are computed by jpeg_start_decompress().
    You can also use jpeg_calc_output_dimensions() to determine these values
    in advance of calling jpeg_start_decompress(). }

  output_width: JDIMENSION;   { scaled image width }
  output_height: JDIMENSION;  { scaled image height }
  out_color_components: integer; { # of color components in out_color_space }
  output_components: integer; { # of color components returned }
  { output_components is 1 (a colormap index) when quantizing colors;
    otherwise it equals out_color_components. }
    
  rec_outbuf_height: integer;	{ min recommended height of scanline buffer }
  { If the buffer passed to jpeg_read_scanlines() is less than this many rows
    high, space and time will be wasted due to unnecessary data copying.
    Usually rec_outbuf_height will be 1 or 2, at most 4. }

  { When quantizing colors, the output colormap is described by these fields.
    The application can supply a colormap by setting colormap non-NULL before
    calling jpeg_start_decompress; otherwise a colormap is created during
    jpeg_start_decompress or jpeg_start_output.
    The map has out_color_components rows and actual_number_of_colors columns.  }
  actual_number_of_colors: integer;	{ number of entries in use }
  colormap: JSAMPARRAY;       { The color map as a 2-D pixel array }

  { State variables: these variables indicate the progress of decompression.
    The application may examine these but must not modify them.  }

  { Row index of next scanline to be read from jpeg_read_scanlines().
    Application may use this to control its processing loop, e.g.,
    "while (output_scanline < output_height)".  }
  output_scanline: JDIMENSION;  { 0 .. output_height-1  }

  { Current input scan number and number of iMCU rows completed in scan.
    These indicate the progress of the decompressor input side.  }
  input_scan_number: integer; { Number of SOS markers seen so far }
  input_iMCU_row: JDIMENSION; { Number of iMCU rows completed }

  { The "output scan number" is the notional scan being displayed by the
    output side.  The decompressor will not allow output scan/row number
    to get ahead of input scan/row, but it can fall arbitrarily far behind. }
  output_scan_number: integer;  { Nominal scan number being displayed }
  output_iMCU_row: JDIMENSION;  { Number of iMCU rows read }

  { Current progression status.  coef_bits[c][i] indicates the precision
    with which component c's DCT coefficient i (in zigzag order) is known.
    It is -1 when no data has yet been received, otherwise it is the point
    transform (shift) value for the most recent scan of the coefficient
    (thus, 0 at completion of the progression).
    This pointer is NULL when reading a non-progressive file.  }
  coef_bits: JCOEFBITSPTR;	  { -1 or current Al value for each coef }

  { Internal JPEG parameters --- the application usually need not look at
    these fields.  Note that the decompressor output side may not use
    any parameters that can change between scans. }

  { Quantization and Huffman tables are carried forward across input
    datastreams when processing abbreviated JPEG datastreams. }

  quant_tbl_ptrs: array [0..NUM_QUANT_TBLS -1] of JQUANT_TBL_ptr;
  { ptrs to coefficient quantization tables, or NULL if not defined }

  dc_huff_tbl_ptrs: array [0..NUM_HUFF_TBLS -1] of JHUFF_TBL_ptr;
  ac_huff_tbl_ptrs: array [0..NUM_HUFF_TBLS -1] of JHUFF_TBL_ptr;
  { ptrs to Huffman coding tables, or NULL if not defined }

  { These parameters are never carried across datastreams, since they
    are given in SOF/SOS markers or defined to be reset by SOI.  }

  data_precision: integer;		{ bits of precision in image data }

  comp_info: jpeg_component_info_ptr;
  { comp_info[i] describes component that appears i'th in SOF }

  is_baseline: JBOOL;		      { TRUE if Baseline SOF0 encountered }
  progressive_mode: JBOOL;    { TRUE if SOFn specifies progressive mode }
  arith_code: JBOOL;          { TRUE=arithmetic coding, FALSE=Huffman }

  arith_dc_L: array [0..NUM_ARITH_TBLS -1] of UINT8; { L values for DC arith-coding tables }
  arith_dc_U: array [0..NUM_ARITH_TBLS -1] of UINT8; { U values for DC arith-coding tables }
  arith_ac_K: array [0..NUM_ARITH_TBLS -1] of UINT8; { Kx values for AC arith-coding tables }

  restart_interval: cardinal; { MCUs per restart interval, or 0 for no restart }

  { These fields record data obtained from optional markers recognized by
    the JPEG library. }
  saw_JFIF_marker: JBOOL;     { TRUE iff a JFIF APP0 marker was found }
  { Data copied from JFIF marker; only valid if saw_JFIF_marker is TRUE: }
  JFIF_major_version: UINT8;  { JFIF version number }
  JFIF_minor_version: UINT8;
  density_unit: UINT8;        { JFIF code for pixel size units }
  X_density: UINT16;          { Horizontal pixel density }
  Y_density: UINT16;          { Vertical pixel density }
  saw_Adobe_marker: JBOOL;    { TRUE iff an Adobe APP14 marker was found }
  Adobe_transform: UINT8;     { Color transform code from Adobe marker }

  CCIR601_sampling: JBOOL;    { TRUE=first samples are cosited }

  { Aside from the specific data retained from APPn markers known to the
    library, the uninterpreted contents of any or all APPn and COM markers
    can be saved in a list for examination by the application.  }
  marker_list: jpeg_saved_marker_ptr;  { Head of list of saved markers }

  { Remaining fields are known throughout decompressor, but generally
    should not be touched by a surrounding application. }

  { These fields are computed during decompression startup }
  max_h_samp_factor: integer;   { largest h_samp_factor }
  max_v_samp_factor: integer;   { largest v_samp_factor }

  min_DCT_h_scaled_size: integer;	{ smallest DCT_h_scaled_size of any component }
  min_DCT_v_scaled_size: integer;	{ smallest DCT_v_scaled_size of any component }

  total_iMCU_rows: JDIMENSION;  { # of iMCU rows in image }
  { The coefficient controller's input and output progress is measured in
    units of "iMCU" (interleaved MCU) rows.  These are the same as MCU rows
    in fully interleaved JPEG scans, but are used whether the scan is
    interleaved or not.  We define an iMCU row as v_samp_factor DCT block
    rows of each component.  Therefore, the IDCT output contains
    v_samp_factor*DCT_scaled_size sample rows of a component per iMCU row. }

  sample_range_limit: JSAMPLE_ptr;  { table for fast range-limiting }

  { These fields are valid during any one scan.
    They describe the components and MCUs actually appearing in the scan.
    Note that the decompressor output side must not use these fields. }
  comps_in_scan: integer;       { # of JPEG components in this scan }
  cur_comp_info: array [0..MAX_COMPS_IN_SCAN -1] of jpeg_component_info_ptr;
  { *cur_comp_info[i] describes component that appears i'th in SOS }

  MCUs_per_row: JDIMENSION;     { # of MCUs across the image }
  MCU_rows_in_scan: JDIMENSION; { # of MCU rows in the image }

  blocks_in_MCU: integer;	      { # of DCT blocks per MCU }
  MCU_membership: array [0..D_MAX_BLOCKS_IN_MCU -1] of integer;
  { MCU_membership[i] is index in cur_comp_info of component owning }
  { i'th block in an MCU }

  Ss: integer;
  Se: integer;
  Ah: integer;
  Al: integer;		{ progressive JPEG parameters for scan }

  { These fields are derived from Se of first SOS marker. }
  block_size: integer;		{ the basic DCT block size: 1..16 }
  natural_order: pointer; { natural-order position array for entropy decode }
  lim_Se: integer;			  { min( Se, DCTSIZE2-1 ) for entropy decode }
  
  { This field is shared between entropy decoder and marker parser.
    It is either zero or the code of a JPEG marker that has been
    read from the data source, but has not yet been processed. }
  unread_marker: integer;

  { Links to decompression subobjects (methods, private variables of modules) }
  master: jpeg_decomp_master_ptr;
  main: jpeg_d_main_controller_ptr;
  coef: jpeg_d_coef_controller_ptr;
  post: jpeg_d_post_controller_ptr;
  inputctl: jpeg_input_controller_ptr;
  marker: jpeg_marker_reader_ptr;
  entropy: jpeg_entropy_decoder_ptr;
  idct: jpeg_inverse_dct_ptr;
  upsample: jpeg_upsampler_ptr;
  cconvert: jpeg_color_deconverter_ptr;
  cquantize: jpeg_color_quantizer_ptr;
end;


{ "Object" declarations for JPEG modules that may be supplied or called
  directly by the surrounding application.
  As with all objects in the JPEG library, these structs only define the
  publicly visible methods and state variables of a module.  Additional
  private fields may exist after the public ones. }


{ Error handler object }
jpeg_error_mgr = record
  { Error exit handler: does not return to caller }
  error_exit: procedure(cinfo: j_common_ptr); cdecl;
  { Conditionally emit a trace or warning message }
  emit_message: procedure(cinfo: j_common_ptr; msg_level: integer); cdecl;
  { Routine that actually outputs a trace or error message }
  output_message: procedure(cinfo: j_common_ptr); cdecl;
  { Format a message string for the most recent JPEG error or message }
  format_message: procedure(cinfo: j_common_ptr; buffer: PAnsiChar); cdecl;
//#define JMSG_LENGTH_MAX  200	{ recommended size of format_message buffer }
  { Reset error state variables at start of a new image }
  reset_error_mgr: procedure(cinfo: j_common_ptr); cdecl;
  
  { The message ID code and any parameters are saved here.
    A message can have one string parameter or up to 8 int parameters. }
  msg_code: integer;
  msg_parm: record
    case integer of
      1: (i: array [0..7] of integer);
      2: (s: array [0..79] of AnsiChar);
    end;
  
  { Standard state variables for error facility }
  
  trace_level: integer;   { max msg_level that will be displayed }
  
  { For recoverable corrupt-data errors, we emit a warning message,
    but keep going unless emit_message chooses to abort.  emit_message
    should count warnings in num_warnings.  The surrounding application
    can check for bad data by seeing if num_warnings is nonzero at the
    end of processing.  }
  num_warnings: longint;    { number of corrupt-data warnings }

  { These fields point to the table(s) of error message strings.
   * An application can change the table pointer to switch to a different
   * message list (typically, to change the language in which errors are
   * reported).  Some applications may wish to add additional error codes
   * that will be handled by the JPEG library error mechanism; the second
   * table pointer is used for this purpose.
   *
   * First table includes all errors generated by JPEG library itself.
   * Error code 0 is reserved for a "no such error string" message.
   }
//  const char * const * jpeg_message_table; { Library errors }
  jpeg_message_table: pointer;  { Library errors }
  last_jpeg_message: integer;   { Table contains strings 0..last_jpeg_message }
  { Second table can be added by application (see cjpeg/djpeg for example).
   * It contains strings numbered first_addon_message..last_addon_message.
   }
//  const char * const * addon_message_table; { Non-library errors }
  addon_message_table: pointer; { Non-library errors }
  first_addon_message: integer; { code for first string in addon table }
  last_addon_message: integer;	{ code for last string in addon table }
end;


{ Progress monitor object }
jpeg_progress_mgr = record
  progress_monitor: procedure(cinfo: j_common_ptr); cdecl;

  pass_counter: longint;      { work units completed in this pass }
  pass_limit: longint;        { total number of work units in this pass }
  completed_passes: integer;  { passes completed so far }
  total_passes: integer;      { total number of passes expected }
end;


{ Data destination object for compression }
jpeg_destination_mgr = record 
  next_output_byte: JOCTET_ptr;   { => next byte to write in buffer }
  free_in_buffer: cardinal;       { # of byte spaces remaining in buffer }

  init_destination: procedure(cinfo: j_compress_ptr); cdecl;
  empty_output_buffer: function(cinfo: j_compress_ptr): boolean; cdecl;
  term_destination: procedure(cinfo: j_compress_ptr); cdecl;
end;


{ Data source object for decompression }
jpeg_source_mgr = record
  next_input_byte: JOCTET_ptr;  { => next byte to read from buffer }
  bytes_in_buffer: Integer;    { # of bytes remaining in buffer }

  init_source: procedure(cinfo: j_decompress_ptr); cdecl;
  fill_input_buffer: function(cinfo: j_decompress_ptr): boolean; cdecl;
  skip_input_data: procedure(cinfo: j_decompress_ptr; num_bytes: longint); cdecl;
  resync_to_restart: function(cinfo: j_decompress_ptr; desired: integer): boolean; cdecl;
  term_source: procedure(cinfo: j_decompress_ptr); cdecl;
end;


{ Memory manager object.
  Allocates "small" objects (a few K total), "large" objects (tens of K),
  and "really big" objects (virtual arrays with backing store if needed).
  The memory manager does not allow individual objects to be freed; rather,
  each created object is assigned to a pool, and whole pools can be freed
  at once.  This is faster and more convenient than remembering exactly what
  to free, especially where malloc()/free() are not too speedy.
  NB: alloc routines never return NULL.  They exit to error_exit if not
  successful. }

jpeg_memory_mgr = record
  { Method pointers }
  alloc_small: function(cinfo: j_common_ptr; pool_id: integer; sizeofobject: cardinal): pointer; cdecl;
  alloc_large: function(cinfo: j_common_ptr; pool_id: integer; sizeofobject: cardinal): pointer; cdecl;
  alloc_sarray: function(cinfo: j_common_ptr; pool_id: integer; samplesperrow: JDIMENSION; numrows: JDIMENSION): JSAMPARRAY; cdecl;
  alloc_barray: function(cinfo: j_common_ptr; pool_id: integer; blocksperrow: JDIMENSION; numrows: JDIMENSION): JBLOCKARRAY; cdecl;
  request_virt_sarray: function(cinfo: j_common_ptr; pool_id: integer; pre_zero: boolean; samplesperrow: JDIMENSION; numrows: JDIMENSION; maxaccess: JDIMENSION) : jvirt_sarray_ptr; cdecl;
  request_virt_barray: function(cinfo: j_common_ptr; pool_id: integer; pre_zero: boolean; blocksperrow: JDIMENSION; numrows: JDIMENSION; maxaccess: JDIMENSION): jvirt_barray_ptr; cdecl;
  realize_virt_arrays: procedure(cinfo: j_common_ptr); cdecl;
  access_virt_sarray: function(cinfo: j_common_ptr; ptr: jvirt_sarray_ptr; start_row: JDIMENSION; num_rows: JDIMENSION; writable: boolean): JSAMPARRAY; cdecl;
  access_virt_barray: function(cinfo: j_common_ptr; ptr: jvirt_barray_ptr; start_row: JDIMENSION; num_rows: JDIMENSION; writable: boolean): JBLOCKARRAY; cdecl;
  free_pool: procedure(cinfo: j_common_ptr; pool_id: integer); cdecl;
  self_destruct: procedure(cinfo: j_common_ptr); cdecl;

  { Limit on memory allocation for this JPEG object.  (Note that this is
    merely advisory, not a guaranteed maximum; it only affects the space
    used for virtual-array buffers.)  May be changed by outer application
    after creating the JPEG object. }
  max_memory_to_use: longint;

  { Maximum allocation request accepted by alloc_large. }
  max_alloc_chunk: longint;
end;

type
{ Routine signature for application-supplied marker processing methods.
  Need not pass marker code since it is stored in cinfo->unread_marker. }
  jpeg_marker_parser_method = function(cinfo: j_decompress_ptr): boolean; cdecl;

const
{ Return value is one of: }
  JPEG_SUSPENDED    = 0;  { Suspended due to lack of input data }
  JPEG_HEADER_OK    = 1;  { Found valid image datastream }
  JPEG_HEADER_TABLES_ONLY = 2;  { Found valid table-specs-only datastream }
{ If you pass require_image = TRUE (normal case), you need not check for
  a TABLES_ONLY return code; an abbreviated file will cause an error exit.
  JPEG_SUSPENDED is only possible if you use a data source module that can
  give a suspension return (the stdio source module doesn't).  }

{ Return value is one of: }
{ #define JPEG_SUSPENDED	0    Suspended due to lack of input data }
  JPEG_REACHED_SOS  = 1; { Reached start of new scan }
  JPEG_REACHED_EOI  = 2; { Reached end of image }
  JPEG_ROW_COMPLETED  = 3; { Completed one iMCU row }
  JPEG_SCAN_COMPLETED = 4; { Completed last iMCU row of a scan }

{ These marker codes are exported since applications and data source modules
  are likely to want to use them. }
  JPEG_RST0 = $D0;  { RST0 marker code }
  JPEG_EOI  = $D9;  { EOI marker code }
  JPEG_APP0 = $E0;  { APP0 marker code }
  JPEG_COM  = $FE;  { COM marker code }

{$IFNDEF LIB_JPEG_8_STATIC_LINK}
var
{ Default error-management setup }
  jpeg_std_error: function(err: jpeg_error_mgr_ptr): jpeg_error_mgr_ptr; cdecl;

{ Initialization of JPEG compression objects.
  jpeg_create_compress() and jpeg_create_decompress() are the exported
  names that applications should call.  These expand to calls on
  jpeg_CreateCompress and jpeg_CreateDecompress with additional information
  passed for version mismatch checking.
  NB: you must set up the error-manager BEFORE calling jpeg_create_xxx. }
  jpeg_CreateCompress: procedure(cinfo: j_compress_ptr; version: integer; structsize: cardinal); cdecl;
  jpeg_CreateDecompress: procedure(cinfo: j_decompress_ptr; version: integer; structsize: cardinal); cdecl;

{ Destruction of JPEG compression objects }
  jpeg_destroy_compress: procedure(cinfo: j_compress_ptr); cdecl;
  jpeg_destroy_decompress: procedure(cinfo: j_decompress_ptr); cdecl;

{ Standard data source and destination managers: stdio streams. }
{ Caller is responsible for opening the file before and closing after. }
//  jpeg_stdio_dest: procedure(cinfo: j_compress_ptr; FILE * outfile); cdecl;
//  jpeg_stdio_src: procedure(cinfo: j_decompress_ptr; FILE * infile); cdecl;

{ Default parameter setup for compression }
  jpeg_set_defaults: procedure(cinfo: j_compress_ptr); cdecl;

{ Compression parameter setup aids }
  jpeg_set_colorspace: procedure(cinfo: j_common_ptr; colorspace: J_COLOR_SPACE); cdecl;
  jpeg_default_colorspace: procedure(cinfo: j_common_ptr); cdecl;
  jpeg_set_quality: procedure(cinfo: j_common_ptr; quality: integer; force_baseline: boolean); cdecl;
  jpeg_set_linear_quality: procedure(cinfo: j_common_ptr; scale_factor: integer; force_baseline: boolean); cdecl;
  jpeg_add_quant_table: procedure(cinfo: j_common_ptr; which_tbl: integer; const basic_table: pcardinal; scale_factor: integer; force_baseline: boolean); cdecl;
  jpeg_quality_scaling: function(quality: integer): integer; cdecl;
  jpeg_simple_progression: procedure(cinfo: j_common_ptr); cdecl;
  jpeg_suppress_tables: procedure(cinfo: j_common_ptr; suppress: boolean); cdecl;
  jpeg_alloc_quant_table: function(cinfo: j_common_ptr): JQUANT_TBL_ptr; cdecl;
  jpeg_alloc_huff_table: function(cinfo: j_common_ptr): JHUFF_TBL_ptr; cdecl;

{ Main entry points for compression }
  jpeg_start_compress: procedure(cinfo: j_compress_ptr; write_all_tables: boolean); cdecl;
  jpeg_write_scanlines: function(cinfo: j_compress_ptr; scanlines: JSAMPARRAY; num_lines: JDIMENSION): JDIMENSION; cdecl;
  jpeg_finish_compress: procedure(cinfo: j_compress_ptr); cdecl;

{ Replaces jpeg_write_scanlines when writing raw downsampled data. }
  jpeg_write_raw_data: function(cinfo: j_compress_ptr; data: JSAMPIMAGE; num_lines: JDIMENSION): JDIMENSION; cdecl;

{ Write a special marker.  See libjpeg.doc concerning safe usage. }
  jpeg_write_marker: procedure(cinfo: j_compress_ptr; marker: integer; const dataptr: JOCTET_ptr; datalen: cardinal); cdecl;
{ Same, but piecemeal. }
  jpeg_write_m_header: procedure(cinfo: j_compress_ptr; marker: integer; datalen: cardinal); cdecl;
  jpeg_write_m_byte: procedure(cinfo: j_compress_ptr; val: integer); cdecl;

{ Alternate compression function: just write an abbreviated table file }
  jpeg_write_tables: procedure(cinfo: j_compress_ptr); cdecl;

{ Decompression startup: read start of JPEG datastream to see what's there }
  jpeg_read_header: function(cinfo: j_decompress_ptr; require_image: boolean): integer; cdecl;

{ Main entry points for decompression }
  jpeg_start_decompress: function(cinfo: j_decompress_ptr): boolean; cdecl;
  jpeg_read_scanlines: function(cinfo: j_decompress_ptr; scanlines: JSAMPARRAY; max_lines: JDIMENSION): JDIMENSION; cdecl;
  jpeg_finish_decompress: function(cinfo: j_decompress_ptr): boolean; cdecl;

{ Replaces jpeg_read_scanlines when reading raw downsampled data. }
  jpeg_read_raw_data: function(cinfo: j_decompress_ptr; data: JSAMPIMAGE; max_lines: JDIMENSION): JDIMENSION; cdecl;

{ Additional entry points for buffered-image mode. }
  jpeg_has_multiple_scans: function(cinfo: j_decompress_ptr): boolean; cdecl;
  jpeg_start_output: function(cinfo: j_decompress_ptr; scan_number: integer): boolean; cdecl;
  jpeg_finish_output: function(cinfo: j_decompress_ptr): boolean; cdecl;
  jpeg_input_complete: function(cinfo: j_decompress_ptr): boolean; cdecl;
  jpeg_new_colormap: procedure(cinfo: j_decompress_ptr); cdecl;
  jpeg_consume_input: function(cinfo: j_decompress_ptr): integer; cdecl;

{ Precalculate output dimensions for current decompression parameters. }
  jpeg_calc_output_dimensions: procedure(cinfo: j_decompress_ptr); cdecl;

{ Control saving of COM and APPn markers into marker_list. }
  jpeg_save_markers: procedure(cinfo: j_decompress_ptr; marker_code: integer; length_limit: cardinal); cdecl;

{ Install a special processing method for COM or APPn markers. }
  jpeg_set_marker_processor: procedure(cinfo: j_decompress_ptr; marker_code: integer; routine: jpeg_marker_parser_method); cdecl;

{ Read or write raw DCT coefficients --- useful for lossless transcoding. }
  jpeg_read_coefficients: function(cinfo: j_decompress_ptr): jvirt_barray_ptr_ptr; cdecl;
  jpeg_write_coefficients: procedure(cinfo: j_compress_ptr; coef_arrays: jvirt_barray_ptr_ptr); cdecl;
  jpeg_copy_critical_parameters: procedure(srcinfo: j_decompress_ptr; dstinfo: j_compress_ptr); cdecl;

{ If you choose to abort compression or decompression before completing
  jpeg_finish_(de)compress, then you need to clean up to release memory,
  temporary files, etc.  You can just call jpeg_destroy_(de)compress
  if you're done with the JPEG object, but if you want to clean it up and
  reuse it, call this:  }
  jpeg_abort_compress: procedure(cinfo: j_compress_ptr); cdecl;
  jpeg_abort_decompress: procedure(cinfo: j_decompress_ptr); cdecl;

{ Generic versions of jpeg_abort and jpeg_destroy that work on either
  flavor of JPEG object.  These may be more convenient in some places. }
  jpeg_abort: procedure(cinfo: j_common_ptr); cdecl;
  jpeg_destroy: procedure(cinfo: j_common_ptr); cdecl;

{ Default restart-marker-resync procedure for use by data source modules }
  jpeg_resync_to_restart: function(cinfo: j_decompress_ptr; desired: integer): boolean; cdecl;
{$ELSE}
  function jpeg_std_error(err: jpeg_error_mgr_ptr): jpeg_error_mgr_ptr; cdecl; external LIB_JPEG_NAME;
  procedure jpeg_CreateCompress(cinfo: j_compress_ptr; version: integer; structsize: cardinal); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_CreateDecompress(cinfo: j_decompress_ptr; version: integer; structsize: cardinal); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_destroy_compress(cinfo: j_compress_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_destroy_decompress(cinfo: j_decompress_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_set_defaults(cinfo: j_compress_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_set_colorspace(cinfo: j_common_ptr; colorspace: J_COLOR_SPACE); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_default_colorspace(cinfo: j_common_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_set_quality(cinfo: j_common_ptr; quality: integer; force_baseline: boolean); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_set_linear_quality(cinfo: j_common_ptr; scale_factor: integer; force_baseline: boolean); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_add_quant_table(cinfo: j_common_ptr; which_tbl: integer; const basic_table: pcardinal; scale_factor: integer; force_baseline: boolean); cdecl; external LIB_JPEG_NAME;
  function jpeg_quality_scaling(quality: integer): integer; cdecl; external LIB_JPEG_NAME;
  procedure jpeg_simple_progression(cinfo: j_common_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_suppress_tables(cinfo: j_common_ptr; suppress: boolean); cdecl; external LIB_JPEG_NAME;
  function jpeg_alloc_quant_table(cinfo: j_common_ptr): JQUANT_TBL_ptr; cdecl; external LIB_JPEG_NAME;
  function jpeg_alloc_huff_table(cinfo: j_common_ptr): JHUFF_TBL_ptr; cdecl; external LIB_JPEG_NAME;
  procedure jpeg_start_compress(cinfo: j_compress_ptr; write_all_tables: boolean); cdecl; external LIB_JPEG_NAME;
  function jpeg_write_scanlines(cinfo: j_compress_ptr; scanlines: JSAMPARRAY; num_lines: JDIMENSION): JDIMENSION; cdecl; external LIB_JPEG_NAME;
  procedure jpeg_finish_compress(cinfo: j_compress_ptr); cdecl; external LIB_JPEG_NAME;
  function jpeg_write_raw_data(cinfo: j_compress_ptr; data: JSAMPIMAGE; num_lines: JDIMENSION): JDIMENSION; cdecl; external LIB_JPEG_NAME;
  procedure jpeg_write_marker(cinfo: j_compress_ptr; marker: integer; const dataptr: JOCTET_ptr; datalen: cardinal); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_write_m_header(cinfo: j_compress_ptr; marker: integer; datalen: cardinal); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_write_m_byte(cinfo: j_compress_ptr; val: integer); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_write_tables(cinfo: j_compress_ptr); cdecl; external LIB_JPEG_NAME;
  function jpeg_read_header(cinfo: j_decompress_ptr; require_image: boolean): integer; cdecl; external LIB_JPEG_NAME;
  function jpeg_start_decompress(cinfo: j_decompress_ptr): boolean; cdecl; external LIB_JPEG_NAME;
  function jpeg_read_scanlines(cinfo: j_decompress_ptr; scanlines: JSAMPARRAY; max_lines: JDIMENSION): JDIMENSION; cdecl; external LIB_JPEG_NAME;
  function jpeg_finish_decompress(cinfo: j_decompress_ptr): boolean; cdecl; external LIB_JPEG_NAME;
  function jpeg_read_raw_data(cinfo: j_decompress_ptr; data: JSAMPIMAGE; max_lines: JDIMENSION): JDIMENSION; cdecl; external LIB_JPEG_NAME;
  function jpeg_has_multiple_scans(cinfo: j_decompress_ptr): boolean; cdecl; external LIB_JPEG_NAME;
  function jpeg_start_output(cinfo: j_decompress_ptr; scan_number: integer): boolean; cdecl; external LIB_JPEG_NAME;
  function jpeg_finish_output(cinfo: j_decompress_ptr): boolean; cdecl; external LIB_JPEG_NAME;
  function jpeg_input_complete(cinfo: j_decompress_ptr): boolean; cdecl; external LIB_JPEG_NAME;
  procedure jpeg_new_colormap(cinfo: j_decompress_ptr); cdecl; external LIB_JPEG_NAME;
  function jpeg_consume_input(cinfo: j_decompress_ptr): integer; cdecl; external LIB_JPEG_NAME;
  procedure jpeg_calc_output_dimensions(cinfo: j_decompress_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_save_markers(cinfo: j_decompress_ptr; marker_code: integer; length_limit: cardinal); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_set_marker_processor(cinfo: j_decompress_ptr; marker_code: integer; routine: jpeg_marker_parser_method); cdecl; external LIB_JPEG_NAME;
  function jpeg_read_coefficients(cinfo: j_decompress_ptr): jvirt_barray_ptr_ptr; cdecl; external LIB_JPEG_NAME;
  procedure jpeg_write_coefficients(cinfo: j_compress_ptr; coef_arrays: jvirt_barray_ptr_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_copy_critical_parameters(srcinfo: j_decompress_ptr; dstinfo: j_compress_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_abort_compress(cinfo: j_compress_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_abort_decompress(cinfo: j_decompress_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_abort(cinfo: j_common_ptr); cdecl; external LIB_JPEG_NAME;
  procedure jpeg_destroy(cinfo: j_common_ptr); cdecl; external LIB_JPEG_NAME;
  function jpeg_resync_to_restart(cinfo: j_decompress_ptr; desired: integer): boolean; cdecl; external LIB_JPEG_NAME;
{$ENDIF}

  procedure jpeg_create_compress(cinfo: j_compress_ptr);
  procedure jpeg_create_decompress(cinfo: j_decompress_ptr);

{$IFNDEF LIB_JPEG_8_STATIC_LINK}
  function InitLibJpeg8(const LibJpegName: AnsiString = LIB_JPEG_NAME): boolean;
  procedure QuitLibJpeg8;
{$ENDIF}

implementation

{$IFNDEF LIB_JPEG_8_STATIC_LINK}
uses
 SyncObjs;

var
  LibJpegRefCount: Integer;
  LibCS: TCriticalSection = nil;

  {$ifdef win32}
    LibJpegHandle: cardinal;
  {$else}
    LibJpegHandle: pointer;
  {$endif}

{$ifdef win32}
const
  Kernel32 = 'kernel32.dll';

  function LoadLibrary(lpFileName: pAnsiChar): LongWord; stdcall; external Kernel32 name 'LoadLibraryA';
  function FreeLibrary(hModule: LongWord): LongBool; stdcall; external Kernel32 name 'FreeLibrary';
  function GetProcAddress(hModule: LongWord; lpProcName: pAnsiChar): Pointer; stdcall; external Kernel32 name 'GetProcAddress';
{$else}
const
  libdl = {$IFDEF Linux} 'libdl.so.2'{$ELSE} 'c'{$ENDIF};

  RTLD_LAZY = $001;

  function dlopen(Name: pAnsiChar; Flags: LongInt): Pointer; cdecl; external libdl name 'dlopen';
  function dlclose(Lib: Pointer): LongInt; cdecl; external libdl name 'dlclose';
  function dlsym(Lib: Pointer; Name: pAnsiChar): Pointer; cdecl; external libdl name 'dlsym';
{$endif}

function GetProcAddr(Name: pAnsiChar): Pointer;
begin
  {$ifdef win32}
    GetProcAddr := GetProcAddress(LibJpegHandle, Name);
  {$else}
    GetProcAddr := dlsym(LibJpegHandle, Name);
  {$endif}
end;
{$ENDIF}

procedure jpeg_create_compress(cinfo: j_compress_ptr);
begin
  jpeg_CreateCompress(cinfo, JPEG_LIB_VERSION, sizeof(jpeg_compress_struct));
end;

procedure jpeg_create_decompress(cinfo: j_decompress_ptr);
begin
  jpeg_CreateDecompress(cinfo, JPEG_LIB_VERSION, sizeof(jpeg_decompress_struct));
end;

{$IFNDEF LIB_JPEG_8_STATIC_LINK}
function InitLibJpeg8(const LibJpegName: AnsiString): boolean;
var
  Temp: Boolean;
begin
  LibCS.Acquire;
  try
    if (LibJpegRefCount = 0) or (LibJpegHandle = {$ifdef win32} 0 {$else} nil {$endif}) then begin
      if LibJpegHandle = {$ifdef win32} 0 {$else} nil {$endif} then
        {$ifdef win32}
          LibJpegHandle := LoadLibrary(pAnsiChar(LibJpegName));
        {$else}
          LibJpegHandle := dlopen(pAnsiChar(LibJpegName), RTLD_LAZY);
        {$endif}

      if LibJpegHandle <> {$ifdef win32} 0 {$else} nil {$endif} then begin
        jpeg_std_error := GetProcAddr('jpeg_std_error');
        jpeg_CreateCompress := GetProcAddr('jpeg_CreateCompress');
        jpeg_CreateDecompress := GetProcAddr('jpeg_CreateDecompress');
        jpeg_destroy_compress := GetProcAddr('jpeg_destroy_compress');
        jpeg_destroy_decompress := GetProcAddr('jpeg_destroy_decompress');
        jpeg_set_defaults := GetProcAddr('jpeg_set_defaults');
        jpeg_set_colorspace := GetProcAddr('jpeg_set_colorspace');
        jpeg_default_colorspace := GetProcAddr('jpeg_default_colorspace');
        jpeg_set_quality := GetProcAddr('jpeg_set_quality');
        jpeg_set_linear_quality := GetProcAddr('jpeg_set_linear_quality');
        jpeg_add_quant_table := GetProcAddr('jpeg_add_quant_table');
        jpeg_quality_scaling := GetProcAddr('jpeg_quality_scaling');
        jpeg_simple_progression := GetProcAddr('jpeg_simple_progression');
        jpeg_suppress_tables := GetProcAddr('jpeg_suppress_tables');
        jpeg_alloc_quant_table := GetProcAddr('jpeg_alloc_quant_table');
        jpeg_alloc_huff_table := GetProcAddr('jpeg_alloc_huff_table');
        jpeg_start_compress := GetProcAddr('jpeg_start_compress');
        jpeg_write_scanlines := GetProcAddr('jpeg_write_scanlines');
        jpeg_finish_compress := GetProcAddr('jpeg_finish_compress');
        jpeg_write_raw_data := GetProcAddr('jpeg_write_raw_data');
        jpeg_write_marker := GetProcAddr('jpeg_write_marker');
        jpeg_write_m_header := GetProcAddr('jpeg_write_m_header');
        jpeg_write_m_byte := GetProcAddr('jpeg_write_m_byte');
        jpeg_write_tables := GetProcAddr('jpeg_write_tables');
        jpeg_read_header := GetProcAddr('jpeg_read_header');
        jpeg_start_decompress := GetProcAddr('jpeg_start_decompress');
        jpeg_read_scanlines := GetProcAddr('jpeg_read_scanlines');
        jpeg_finish_decompress := GetProcAddr('jpeg_finish_decompress');
        jpeg_read_raw_data := GetProcAddr('jpeg_read_raw_data');
        jpeg_has_multiple_scans := GetProcAddr('jpeg_has_multiple_scans');
        jpeg_start_output := GetProcAddr('jpeg_start_output');
        jpeg_finish_output := GetProcAddr('jpeg_finish_output');
        jpeg_input_complete := GetProcAddr('jpeg_input_complete');
        jpeg_new_colormap := GetProcAddr('jpeg_new_colormap');
        jpeg_consume_input := GetProcAddr('jpeg_consume_input');
        jpeg_calc_output_dimensions := GetProcAddr('jpeg_calc_output_dimensions');
        jpeg_save_markers := GetProcAddr('jpeg_save_markers');
        jpeg_set_marker_processor := GetProcAddr('jpeg_set_marker_processor');
        jpeg_read_coefficients := GetProcAddr('jpeg_read_coefficients');
        jpeg_write_coefficients := GetProcAddr('jpeg_write_coefficients');
        jpeg_copy_critical_parameters := GetProcAddr('jpeg_copy_critical_parameters');
        jpeg_abort_compress := GetProcAddr('jpeg_abort_compress');
        jpeg_abort_decompress := GetProcAddr('jpeg_abort_decompress');
        jpeg_abort := GetProcAddr('jpeg_abort');
        jpeg_destroy := GetProcAddr('jpeg_destroy');
        jpeg_resync_to_restart := GetProcAddr('jpeg_resync_to_restart');
      end;
    end;

    Temp :=
      (Addr(jpeg_std_error) <> nil) or
      (Addr(jpeg_CreateCompress) <> nil) or
      (Addr(jpeg_CreateDecompress) <> nil) or
      (Addr(jpeg_destroy_compress) <> nil) or
      (Addr(jpeg_destroy_decompress) <> nil) or
      (Addr(jpeg_set_defaults) <> nil) or
      (Addr(jpeg_set_colorspace) <> nil) or
      (Addr(jpeg_default_colorspace) <> nil) or
      (Addr(jpeg_set_quality) <> nil) or
      (Addr(jpeg_set_linear_quality) <> nil) or
      (Addr(jpeg_add_quant_table) <> nil) or
      (Addr(jpeg_quality_scaling) <> nil) or
      (Addr(jpeg_simple_progression) <> nil) or
      (Addr(jpeg_suppress_tables) <> nil) or
      (Addr(jpeg_alloc_quant_table) <> nil) or
      (Addr(jpeg_alloc_huff_table) <> nil) or
      (Addr(jpeg_start_compress) <> nil) or
      (Addr(jpeg_write_scanlines) <> nil) or
      (Addr(jpeg_finish_compress) <> nil) or
      (Addr(jpeg_write_raw_data) <> nil) or
      (Addr(jpeg_write_marker) <> nil) or
      (Addr(jpeg_write_m_header) <> nil) or
      (Addr(jpeg_write_m_byte) <> nil) or
      (Addr(jpeg_write_tables) <> nil) or
      (Addr(jpeg_read_header) <> nil) or
      (Addr(jpeg_start_decompress) <> nil) or
      (Addr(jpeg_read_scanlines) <> nil) or
      (Addr(jpeg_finish_decompress) <> nil) or
      (Addr(jpeg_read_raw_data) <> nil) or
      (Addr(jpeg_has_multiple_scans) <> nil) or
      (Addr(jpeg_start_output) <> nil) or
      (Addr(jpeg_finish_output) <> nil) or
      (Addr(jpeg_input_complete) <> nil) or
      (Addr(jpeg_new_colormap) <> nil) or
      (Addr(jpeg_consume_input) <> nil) or
      (Addr(jpeg_calc_output_dimensions) <> nil) or
      (Addr(jpeg_save_markers) <> nil) or
      (Addr(jpeg_set_marker_processor) <> nil) or
      (Addr(jpeg_read_coefficients) <> nil) or
      (Addr(jpeg_write_coefficients) <> nil) or
      (Addr(jpeg_copy_critical_parameters) <> nil) or
      (Addr(jpeg_abort_compress) <> nil) or
      (Addr(jpeg_abort_decompress) <> nil) or
      (Addr(jpeg_abort) <> nil) or
      (Addr(jpeg_destroy) <> nil) or
      (Addr(jpeg_resync_to_restart) <> nil);
    
    if Temp then
      Inc(LibJPEGRefCount);
    
    Result := Temp;
  finally
    LibCS.Release;
  end;
end;

procedure QuitLibJpeg8;
begin
  LibCS.Acquire;
  try
    Dec(LibJpegRefCount);
    
    if LibJpegRefCount <= 0 then begin
      if LibJpegHandle <> {$ifdef win32} 0 {$else} nil {$endif} then begin
        {$ifdef win32}
          FreeLibrary(LibJpegHandle);
          LibJpegHandle := 0;
        {$else}
          dlclose(LibJpegHandle);
          LibJpegHandle := nil;
        {$endif}
      end;

      jpeg_std_error := nil;
      jpeg_CreateCompress := nil;
      jpeg_CreateDecompress := nil;
      jpeg_destroy_compress := nil;
      jpeg_destroy_decompress := nil;
      jpeg_set_defaults := nil;
      jpeg_set_colorspace := nil;
      jpeg_default_colorspace := nil;
      jpeg_set_quality := nil;
      jpeg_set_linear_quality := nil;
      jpeg_add_quant_table := nil;
      jpeg_quality_scaling := nil;
      jpeg_simple_progression := nil;
      jpeg_suppress_tables := nil;
      jpeg_alloc_quant_table := nil;
      jpeg_alloc_huff_table := nil;
      jpeg_start_compress := nil;
      jpeg_write_scanlines := nil;
      jpeg_finish_compress := nil;
      jpeg_write_raw_data := nil;
      jpeg_write_marker := nil;
      jpeg_write_m_header := nil;
      jpeg_write_m_byte := nil;
      jpeg_write_tables := nil;
      jpeg_read_header := nil;
      jpeg_start_decompress := nil;
      jpeg_read_scanlines := nil;
      jpeg_finish_decompress := nil;
      jpeg_read_raw_data := nil;
      jpeg_has_multiple_scans := nil;
      jpeg_start_output := nil;
      jpeg_finish_output := nil;
      jpeg_input_complete := nil;
      jpeg_new_colormap := nil;
      jpeg_consume_input := nil;
      jpeg_calc_output_dimensions := nil;
      jpeg_save_markers := nil;
      jpeg_set_marker_processor := nil;
      jpeg_read_coefficients := nil;
      jpeg_write_coefficients := nil;
      jpeg_copy_critical_parameters := nil;
      jpeg_abort_compress := nil;
      jpeg_abort_decompress := nil;
      jpeg_abort := nil;
      jpeg_destroy := nil;
      jpeg_resync_to_restart := nil;
    end;
  finally
    LibCS.Release;
  end;
end;

initialization
  LibCS := TCriticalSection.Create;

finalization
  QuitLibJpeg8;
  LibCS.Free;

{$ENDIF}

end.
