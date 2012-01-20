unit LibECW;

{$MINENUMSIZE 4}

interface

  type NCSFileColorSpace =
  (
    NCSCS_NONE = 0,
    NCSCS_GREYSCALE	= 1,	// Greyscale
    NCSCS_YUV	= 2,	// YUV - JPEG Digital, JP2 ICT
    NCSCS_MULTIBAND	= 3,	// Multi-band imagery
    NCSCS_sRGB	= 4,	// sRGB
    NCSCS_YCbCr	= 5		// YCbCr - JP2 ONLY, Auto-converted to sRGB
  );

  type NCSError =
  (
      NCS_SUCCESS = 0,		// No error 
      NCS_QUEUE_NODE_CREATE_FAILED,			// Queue node creation failed 
      NCS_FILE_OPEN_FAILED,					// File open failed 
      NCS_FILE_LIMIT_REACHED,					// The Image Web Server's licensed file limit has been reached 
      NCS_FILE_SIZE_LIMIT_REACHED,			// The requested file is larger than is permitted by the license on this Image Web Server 
      NCS_FILE_NO_MEMORY,						// Not enough memory for new file
      NCS_CLIENT_LIMIT_REACHED,				// The Image Web Server's licensed client limit has been reached 
      NCS_DUPLICATE_OPEN,						// Detected duplicate open from net layer 
      NCS_PACKET_REQUEST_NYI,					// Packet request type not yet implemented 
      NCS_PACKET_TYPE_ILLEGAL,				// Packet type is illegal 
      NCS_DESTROY_CLIENT_DANGLING_REQUESTS,	// Client closed while requests outstanding 
      // NCS Network Errors
      NCS_UNKNOWN_CLIENT_UID,					// Client UID unknown 
      NCS_COULDNT_CREATE_CLIENT,				// Could not create new client
      NCS_NET_COULDNT_RESOLVE_HOST,			// Could not resolve address of Image Web Server 
      NCS_NET_COULDNT_CONNECT,				// Could not connect to host 
      NCS_NET_RECV_TIMEOUT,					// Receive timeout 
      NCS_NET_HEADER_SEND_FAILURE,			// Error sending header 
      NCS_NET_HEADER_RECV_FAILURE,			// Error receiving header 
      NCS_NET_PACKET_SEND_FAILURE,			// Error sending packet 
      NCS_NET_PACKET_RECV_FAILURE,			// Error receiving packet 
      NCS_NET_401_UNAUTHORISED,				// 401 Unauthorised: SDK doesn't do authentication so this suggests a misconfigured server
      NCS_NET_403_FORBIDDEN,					// 403 Forbidden: could be a 403.9 from IIS or PWS meaning that the maximum simultaneous request limit has been reached 
      NCS_NET_404_NOT_FOUND,					// 404 Not Found: this error suggests that the server hasn't got Image Web Server installed 
      NCS_NET_407_PROXYAUTH,					// 407 Proxy Authentication: the SDK doesn't do proxy authentication yet either, so this also suggests misconfiguration 
      NCS_NET_UNEXPECTED_RESPONSE,			// Unexpected HTTP response could not be handled 
      NCS_NET_BAD_RESPONSE,					// HTTP response received outside specification 
      NCS_NET_ALREADY_CONNECTED,				// Already connected 
      NCS_INVALID_CONNECTION,					// Connection is invalid 
      NCS_WINSOCK_FAILURE,					// A Windows sockets failure occurred
      // NCS Symbol Errors
      NCS_SYMBOL_ERROR,			// Symbology error 
      NCS_OPEN_DB_ERROR,			// Could not open database 
      NCS_DB_QUERY_FAILED,		// Could not execute the requested query on database 
      NCS_DB_SQL_ERROR,			// SQL statement could not be executed 
      NCS_GET_LAYER_FAILED,		// Open symbol layer failed 
      NCS_DB_NOT_OPEN,			// The database is not open
      NCS_QT_TYPE_UNSUPPORTED,	// This type of quadtree is not supported 
      // Preference errors
      NCS_PREF_INVALID_USER_KEY,		// Invalid local user key name specified 
      NCS_PREF_INVALID_MACHINE_KEY,	// Invalid local machine key name specified 
      NCS_REGKEY_OPENEX_FAILED,		// Failed to open registry key 
      NCS_REGQUERY_VALUE_FAILED,		// Registry query failed 
      NCS_INVALID_REG_TYPE,			// Type mismatch in registry variable
      // Misc Errors
      NCS_INVALID_ARGUMENTS,		// Invalid arguments passed to function 
      NCS_ECW_ERROR,				// ECW error 
      // unspecified, but coming out of ecw 
      NCS_SERVER_ERROR,			// Server error 
      // unspecified server error 
      NCS_UNKNOWN_ERROR,			// Unknown error
      NCS_EXTENT_ERROR,			// Extent conversion failed 
      NCS_COULDNT_ALLOC_MEMORY,	// Could not allocate enough memory 
      NCS_INVALID_PARAMETER,		// An invalid parameter was used 
      // Compression Errors
      NCS_FILEIO_ERROR,						// Error reading or writing file 
      NCS_COULDNT_OPEN_COMPRESSION,			// Compression task could not be initialised 
      NCS_COULDNT_PERFORM_COMPRESSION,		// Compression task could not be processed
      NCS_GENERATED_TOO_MANY_OUTPUT_LINES,	// Trying to generate too many output lines 
      NCS_USER_CANCELLED_COMPRESSION,			// Compression task was cancelled by client application 
      NCS_COULDNT_READ_INPUT_LINE,			// Could not read line from input data 
      NCS_INPUT_SIZE_EXCEEDED,				// Input image size was exceeded for this version of the SDK 
      // Decompression Errors
      NCS_REGION_OUTSIDE_FILE,	// Specified image region is outside image extents 
      NCS_NO_SUPERSAMPLE,			// Supersampling is not supported by the SDK functions
      NCS_ZERO_SIZE,				// Specified image region has a zero width or height 
      NCS_TOO_MANY_BANDS,			// More bands specified than exist in the input file 
      NCS_INVALID_BAND_NR,		// An invalid band number has been specified 
      // NEW Compression Error
      NCS_INPUT_SIZE_TOO_SMALL,	// Input image size is too small to compress - for ECW compression there is a minimum output file size 
      // NEW Network error
      NCS_INCOMPATIBLE_PROTOCOL_VERSION,	// The ECWP client version is incompatible with this server
      NCS_WININET_FAILURE,				// Windows Internet Client error 
      NCS_COULDNT_LOAD_WININET,			// wininet.dll could not be loaded - usually indicates Internet Explorer should be upgraded
      // NCSFile && NCSRenderer class errors
      NCS_FILE_INVALID_SETVIEW,			// The parameters specified for setting a file view were invalid, or the view was not set 
      NCS_FILE_NOT_OPEN,					// No file is open 
      // NEW JNI Java Errors
      NCS_JNI_REFRESH_NOT_IMPLEMENTED,	// Class does not implement ECWProgressiveDisplay interface
      // A class is trying to use RefreshUpdate() method, but has not implemented ECWProgressiveDisplay
      // NEW Coordinate Errors
      NCS_INCOMPATIBLE_COORDINATE_SYSTEMS,	// Incompatible coordinate systems 
      NCS_INCOMPATIBLE_COORDINATE_DATUM,		// Incompatible coordinate datum types 
      NCS_INCOMPATIBLE_COORDINATE_PROJECTION,	// Incompatible coordinate projection types 
      NCS_INCOMPATIBLE_COORDINATE_UNITS,		// Incompatible coordinate units types 
      NCS_COORDINATE_CANNOT_BE_TRANSFORMED,	// Non-linear coordinate systems not supported
      NCS_GDT_ERROR,							// Error involving the GDT database 
      // NEW NCScnet error
      NCS_NET_PACKET_RECV_ZERO_LENGTH,	// Zero length packet received 
      //*[01]*
      // Macintosh SDK specific errors
      NCS_UNSUPPORTEDLANGUAGE,			// Must use Japanese version of the ECW SDK 
      //*[02]*
      // Loss of connection
      NCS_CONNECTION_LOST,				// Connection to server was lost 
      //*[03]*
      NCS_COORD_CONVERT_ERROR,			// NCSGDT coordinate conversion failed
      // Metabase Stuff
      NCS_METABASE_OPEN_FAILED,			// Failed to open metabase 
      //*[04]*
      NCS_METABASE_GET_FAILED,			// Failed to get value from metabase 
      //*[04]*
      NCS_NET_HEADER_SEND_TIMEOUT,		// Timeout sending header 
      //*[05]*
      NCS_JNI_ERROR,						// Java JNI error
      //*[06]*
      NCS_DB_INVALID_NAME,				// No data source passed
      //*[07]*
      NCS_SYMBOL_COULDNT_RESOLVE_HOST,	// Could not resolve address of Image Web Server Symbol Server Extension 
      //*[07]*
      NCS_INVALID_ERROR_ENUM,				// The value of an NCSError error number was invalid!
      //*[08]*
      // NCSFileIO errors [10]
      NCS_FILE_EOF,						// End of file reached
      NCS_FILE_NOT_FOUND,					// File not found 
      NCS_FILE_INVALID,					// File was invalid or corrupt 
      NCS_FILE_SEEK_ERROR,				// Attempted to read, write or seek past file limits 
      NCS_FILE_NO_PERMISSIONS,			// Permissions not available to access file 
      NCS_FILE_OPEN_ERROR,				// Error opengin file 
      NCS_FILE_CLOSE_ERROR,				// Error closing file 
      NCS_FILE_IO_ERROR,					// Miscellaneous error involving file input or output
      NCS_SET_EXTENTS_ERROR,				// Illegal or invalid world coordinates supplied
      //*[09]*
      NCS_FILE_PROJECTION_MISMATCH,		// Image projection does not match that of the controlling layer
      //* 1.65 gdt errors [15]*
      NCS_GDT_UNKNOWN_PROJECTION,		// Unknown map projection 
      NCS_GDT_UNKNOWN_DATUM,			// Unknown geodetic datum 
      NCS_GDT_USER_SERVER_FAILED,		// User specified Geographic Projection Database data server failed
      NCS_GDT_REMOTE_PATH_DISABLED,	// Remote Geographic Projection Database file downloading has been disabled and no local GDT data is available 
      NCS_GDT_BAD_TRANSFORM_MODE,		// Invalid mode of transform 
      NCS_GDT_TRANSFORM_OUT_OF_BOUNDS,// Coordinate to be transformed is out of bounds 
      NCS_LAYER_DUPLICATE_LAYER_NAME,	// A layer already exists with the specified name 
      //*[17]*
      NCS_LAYER_INVALID_PARAMETER,	// The specified layer does not contain the specified parameter 
      //*[18]*
      NCS_PIPE_CREATE_FAILED,			// Failed to create pipe 
      //*[19]*
      // Directory creation errors
      NCS_FILE_MKDIR_EXISTS,			// Directory to be created already exists  //[20]
      NCS_FILE_MKDIR_PATH_NOT_FOUND,	// The path specified for directory creation does not exist  //[20]
      NCS_ECW_READ_CANCELLED,			// File read was cancelled 
      // JP2 georeferencing errors
      NCS_JP2_GEODATA_READ_ERROR,		// Error reading geodata from a JPEG 2000 file  //[21]
      NCS_JP2_GEODATA_WRITE_ERROR,    // Error writing geodata to a JPEG 2000 file 	//[21]
      NCS_JP2_GEODATA_NOT_GEOREFERENCED, // JPEG 2000 file not georeferenced 			//[21]
      // Progressive view too large error 
      NCS_PROGRESSIVE_VIEW_TOO_LARGE, // Progressive views are limited to 4000x4000 in size  //[23]
      // Insert new errors before here!
      NCS_MAX_ERROR_NUMBER			// The maximum error value in this enumerated type - should not itself be reported, must always be defined last  //[08]
  );

  type NCSEcwReadStatus =
  (
    // Successful read
    NCSECW_READ_OK			= 0,
    // Read failed due to an error
    NCSECW_READ_FAILED		= 1,
    { Read was cancelled, either because a new SetView arrived or a
        library shutdown is in progress }
    NCSECW_READ_CANCELLED	= 2
  );

  type CellSizeUnits =
  (
    // Invalid cell units
    ECW_CELL_UNITS_INVALID	=	0,
    // Cell units are standard meters
    ECW_CELL_UNITS_METERS	=	1,
    // Degrees
    ECW_CELL_UNITS_DEGREES	=	2,
    // US Survey feet
    ECW_CELL_UNITS_FEET		=	3,
    // Unknown cell units
    ECW_CELL_UNITS_UNKNOWN	=	4
  );

  type NCSFileViewFileInfo = record
    // Dataset cells in X direction */
     nSizeX:cardinal;
    // Dataset cells in Y direction */
     nSizeY:cardinal;
    // Number of bands in the file, e.g. 3 for a RGB file */
     nBands:Word;
    // Target compression rate, e,g, 20 == 20:1 compression.  May be zero */
     nCompressionRate:Word;
    // Units used for pixel size */
     eCellSizeUnits:CellSizeUnits;
    // Increment in eCellSizeUnits in X direction.  May be negative.  Never zero */
     fCellIncrementX:double;
    // Increment in eCellSizeUnits in Y direction.  May be negative.  Never zero */
     fCellIncrementY:double;
    // World X origin for top left corner of top left cell, in eCellSizeUnits */
     fOriginX:double;
    // World Y origin for top left corner of top left cell, in eCellSizeUnits */
     fOriginY:double;
    // ER Mapper style Datum name string, e.g. "RAW" or "NAD27".  Never NULL */
     szDatum:PChar;
    // ER Mapper style Projection name string, e.g. "RAW" or "GEODETIC".  Never NULL */
     szProjection:PChar;
  end;

  type CompressFormat =
  (
    // The compressed data is unformatted
    COMPRESS_NONE	= integer(NCSCS_NONE),
    // Greyscale format, single band
    COMPRESS_UINT8 = integer(NCSCS_GREYSCALE),
    // JPEG standard YUV digital format, three band
    COMPRESS_YUV		= integer(NCSCS_YUV),
    // Multiband format
    COMPRESS_MULTI		= integer(NCSCS_MULTIBAND),
    // RGB images (converted to COMPRESS_YUV internally)
    COMPRESS_RGB		= integer(NCSCS_sRGB)
  );

  type CompressHint =
  (
    // No compression hint
    COMPRESS_HINT_NONE	= 0,
    // Do the fastest compression possible
    COMPRESS_HINT_FAST	= 1,
    // Try to achieve the maximum possible compression ratio
    COMPRESS_HINT_BEST	= 2,
    // Optimise compression process for later Internet use of the compressed file
    COMPRESS_HINT_INTERNET = 3
  );

  PNCSEcwCompressClient = ^NCSEcwCompressClient;

  TReadCallback = function (pClient:PNCSEcwCompressClient;nNextLine:Cardinal;InputArray:Pointer): boolean; cdecl;
  StatusCallback = procedure (pClient:PNCSEcwCompressClient; nCurrentLine:Cardinal); cdecl;
  CancelCallback = function (pClient:PNCSEcwCompressClient):boolean; cdecl;

  NCSEcwCompressClient = record
    {** These fields are populated by the compression client
    */
    /** If this is specified but the output file is not, a default output filename will be created.
     *	Otherwise this field is unused.}
    szInputFilename:array [0..259] of Char;
    // An output filename must be specified if no input filename is specified */
    szOutputFilename:array [0..259] of Char;
    // The target compression ratio - must be specified */
    fTargetCompression:Single;
    //	The compression format to use.  See the related enumerated type definition */
    eCompressFormat:CompressFormat;
    { A guideline for an appropriate compression scheme to use.  This currently has
     *	no effect, though the default value is COMPRESS_HINT_INTERNET.  Reserved for
     *	future use, see the related enumerated type definition}
     eCompressHint:CompressHint;
    { X dimension of the block size to use.  Can be 64, 128, 256, 512, 1024, or 2048.
     *	The default for these is set to 64 which produces preferred performance over the internet.}
     nBlockSizeX:cardinal;
    { Y dimension of the block size to use.  Can be 64, 128, 256, 512, 1024, or 2048.
     *	The default for these is set to 64 which produces preferred performance over the internet.}
    nBlockSizeY:cardinal;				// Y Block size (64, 128, 256, 512)					*/
    // Number of cells of input data and compressed file in the X direction - must be specified */
    nInOutSizeX:cardinal;
    // Number of cells of input data and compressed file in the Y direction - must be specified */
    nInOutSizeY:cardinal;
    // Number of bands in the input data - must be specified */
    nInputBands:cardinal;
    // Number of bands in the output file - should not generally be specified */
    nOutputBands:cardinal;
    // Size of the input file in bytes - should not be specified, it will be determined automatically. */
    nInputSize:Int64;
    // Optional field specifying the cell size in the X direction in eCellSizeUnits */
    fCellIncrementX:Double;
    // Optional field specifying the cell size in the Y direction in eCellSizeUnits */
    fCellIncrementY:Double;
    // Optional field specifying the X world origin of the input data in eCellSizeUnits */
    fOriginX:Double;
    // Optional field specifying the Y world origin of the input data in eCellSizeUnits */
    fOriginY:Double;
    // Optional field specifying the units in which world cell sizes are specified, e.g. meters, feet */
    eCellSizeUnits:CellSizeUnits;
    // ER Mapper GDT style datum string */
    szDatum:array [0..15] of Char;
    // ER Mapper GDT style projection string */
    szProjection:array [0..15] of Char;
    // Callback function used to obtain lines of band data from the input data - must be specified */
    pReadCallback: TReadCallback;
    // Optional status callback function to track the progress of the compression process */
    pStatusCallback: StatusCallback;
    // Optional cancel callback function which can be used to cancel a compression process */
    pCancelCallback: CancelCallback;
    // (void *) Pointer to any private data you need to access in the three callback functions */
    pClientData:Pointer;
    // Created by NCSEcwCompressOpen() */
    pTask:Pointer;  // struct EcwCompressionTask
    // The remaining fields are populated by NCSEcwCompressClose()
    // Actual compression rate achieved - ratio of input data size to output file size */
    fActualCompression:Single;
    // Time taken to perform the complete compression, in seconds */
    fCompressionSeconds:Double;
    // MB/s throughput during the compression process */
    fCompressionMBSec:Double;
    // Total size of the output file in bytes */
    nOutputSize:Int64;
  end;

  NCSEcwEditInfo = record
    nVersion:byte;			// ECW file version == ERSWAVE_VERSION
    eCellSizeUnits:CellSizeUnits;		// Units used for pixel size
    fCellIncrementX:Double;	// Increment in CellSizeUnits in X direction. May be negative. Will never be zero
    fCellIncrementY:Double;	// Increment in CellSizeUnits in Y direction. May be negative. Will never be zero
    fOriginX:Double;			// World X origin for top-left corner of top-left cell, in CellSizeUnits
    fOriginY:Double;			// World Y origin for top-left corner of top-left cell, in CellSizeUnits
    szDatum:PChar;			// ER Mapper style Datum name string, e.g. "RAW" or "NAD27". Will never be NULL
    szProjection:PChar;		// ER Mapper style Projection name string, e.g. "RAW" or "WGS84". Will never be NULL
    bCompressedOffsetTable:boolean; // Is the block table compressed
  end;
    
  NCScbmOpenFileView = function (path:PChar; var NCSFileView:PInteger; Callback:Tobject): NCSError; stdcall;
  NCScbmCloseFileView = function (NCSFileView:Pinteger): NCSError; stdcall;
  NCScbmGetViewFileInfo = function (NCSFileView:Pointer; var FileViewFileInfoPtr:pointer): NCSError; stdcall;
  NCScbmSetFileView = function (NCSFileView:Pointer;nBands:cardinal; pBandList:array of cardinal;
                                nTLX,nTLY,nBRX,nBRY,nSizeX,nSizeY:cardinal):NCSError; stdcall;
  NCScbmReadViewLineRGB = function (NCSFileView:pointer; pRGB:pointer):NCSEcwReadStatus; stdcall;

var
  NCSEcwCompressAllocClient: function (): Pointer; cdecl;
  NCSEcwCompressOpen: function (pInfo: Pointer; bCalculateSizesOnly: Boolean): NCSError; cdecl;
  NCSEcwCompress: function (pInfo: Pointer): NCSError; cdecl;
  NCSEcwCompressClose: function (pInfo: Pointer): NCSError; cdecl;
  NCSEcwCompressFreeClient: function (pInfo: Pointer): NCSError; cdecl;

const
  LIB_ECW_NAME = 'NCSEcwC.dll';

function InitLibEcw(const ALibName: AnsiString = LIB_ECW_NAME): Boolean;

implementation

uses
  SyncObjs;

var
  LibHandle: Cardinal = 0;
  LibCS: TCriticalSection = nil;

const
  Kernel32 = 'kernel32.dll';

function LoadLibrary(lpFileName: pAnsiChar): LongWord; stdcall; external Kernel32 name 'LoadLibraryA';
function FreeLibrary(hModule: LongWord): LongBool; stdcall; external Kernel32 name 'FreeLibrary';
function GetProcAddress(hModule: LongWord; lpProcName: pAnsiChar): Pointer; stdcall; external Kernel32 name 'GetProcAddress';

function GetProcAddr(ProcName: PAnsiChar): Pointer;
begin
  GetProcAddr := GetProcAddress(LibHandle, ProcName);
end;

function InitLibEcw(const ALibName: AnsiString = LIB_ECW_NAME): Boolean;
begin
  LibCS.Acquire;
  try
    if LibHandle = 0 then begin
      LibHandle := LoadLibrary(PAnsiChar(ALibName));
      if LibHandle <> 0 then begin
        NCSEcwCompressAllocClient := GetProcAddr('NCSEcwCompressAllocClient');
        NCSEcwCompressOpen := GetProcAddr('NCSEcwCompressOpen');
        NCSEcwCompress := GetProcAddr('NCSEcwCompress');
        NCSEcwCompressClose := GetProcAddr('NCSEcwCompressClose');
        NCSEcwCompressFreeClient := GetProcAddr('NCSEcwCompressFreeClient');
      end;
    end;
    Result :=
      (LibHandle <> 0) and
      ( (Addr(NCSEcwCompressAllocClient) <> nil) or
        (Addr(NCSEcwCompressOpen) <> nil) or
        (Addr(NCSEcwCompress) <> nil) or
        (Addr(NCSEcwCompressClose) <> nil) or
        (Addr(NCSEcwCompressFreeClient) <> nil)
      );
  finally
    LibCS.Release;
  end;
end;

procedure QuitLibEcw;
begin
  LibCS.Acquire;
  try
    if LibHandle > 0 then begin
      FreeLibrary(LibHandle);
      LibHandle := 0;
      NCSEcwCompressAllocClient := nil;
      NCSEcwCompressOpen := nil;
      NCSEcwCompress := nil;
      NCSEcwCompressClose := nil;
      NCSEcwCompressFreeClient := nil;
    end;
  finally
    LibCS.Release;
  end;
end;

initialization
  LibCS := TCriticalSection.Create;
  //InitLibEcw

finalization
  QuitLibEcw;
  LibCS.Free;

end.

