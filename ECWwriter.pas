unit ECWWriter;
{$MINENUMSIZE 4}
interface
uses ECWReader;

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

    TReadCallback = function (pClient:pointer;nNextLine:Cardinal;InputArray:Pointer): boolean; cdecl;
    StatusCallback = procedure (pClient:pointer; nCurrentLine:Cardinal); cdecl;
    CancelCallback = function (pClient:pointer):boolean; cdecl;

    type NCSEcwCompressClient = record
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

    NCSEcwCompressAllocClient = function (): Pointer; stdcall;
    NCSEcwCompressOpen = function (pInfo:pointer; bCalculateSizesOnly:boolean): NCSError; stdcall;
    NCSEcwCompress = function (pInfo:Pointer): NCSError; stdcall;
    NCSEcwCompressClose = function (pInfo:Pointer):NCSError; stdcall;
    NCSEcwCompressFreeClient = function (pInfo:Pointer):NCSError; stdcall;

implementation


end.
 