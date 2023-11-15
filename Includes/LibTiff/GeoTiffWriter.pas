unit GeoTiffWriter;

interface

uses
  SysUtils,
  libgeotiff,
  libtiff;

type
  TTiffType = (
    ttOldTiff = 0,
    ttBigTiff = 1
  );

  TTiffStorageType = (
    tstStripped = 0,
    tstTiled = 1
  );

  TTiffCompression = (
    tcNone = 0,
    tcZip = 1,
    tcLZW = 2,
    tcJPG = 3
  );

  TGetLineCallBack = function(
    const ARowNumber: Integer;
    const AUserInfo: Pointer
  ): Pointer of object;

  TGetTileCallBack = function(
    const X, Y, Z: Integer;
    const AUserInfo: Pointer
  ): Pointer of object;

  TProjectionInfo = record
    EPSG: Integer;
    IsGeographic: Boolean;
    CellIncrementX: Double;
    CellIncrementY: Double;
    OriginX: Double;
    OriginY: Double;
  end;
  PProjectionInfo = ^TProjectionInfo;

  TGeoTiffWriter = class(TObject)
  private
    FWidth: Integer;
    FHeight: Integer;
    FTileWidth: Integer;
    FTileHeight: Integer;
    FOverVeiws: string;
    FBitsPerPixel: Integer;
    FSoftwareIDStr: AnsiString;
    FErrorMessage: string;
    procedure WriteTIFFDirectory(
      const ATiff: PTIFF;
      const AStorageType: TTiffStorageType;
      const ACompression: TTiffCompression;
      const AStoreAlphaChanel: Boolean;
      const AProjectionInfo: PProjectionInfo
    );
    procedure WriteGeoKeys(
      const AGTiff: PGTIFF;
      const AProjectionInfo: PProjectionInfo
    );
    function WriteImage(
      const ATiff: PTIFF;
      const AGetLineCallBack: TGetLineCallBack;
      const AGetTileCallBack: TGetTileCallBack;
      const AUserInfo: Pointer
    ): Boolean;
    function Write(
      const ATiffType: TTiffType;
      const AOutputFileName: string;
      const AWidth, AHeight: Integer;
      const AOverVeiws: string;
      const ACompression: TTiffCompression;
      const AGetLineCallBack: TGetLineCallBack;
      const AGetTileCallBack: TGetTileCallBack;
      const AStoreAlphaChanel: Boolean;
      const AProjectionInfo: PProjectionInfo;
      const AUserInfo: Pointer
    ): Boolean;
  public
    function WriteStripped(
      const ATiffType: TTiffType;
      const AOutputFileName: string;
      const AWidth, AHeight: Integer;
      const ACompression: TTiffCompression;
      const AGetLineCallBack: TGetLineCallBack;
      const AStoreAlphaChanel: Boolean;
      const AProjectionInfo: PProjectionInfo;
      const AUserInfo: Pointer;
      out AErrorMessage: string
    ): Boolean; inline;
    function WriteTiled(
      const ATiffType: TTiffType;
      const AOutputFileName: string;
      const AWidth, AHeight: Integer;
      const AOverVeiws: string;
      const ACompression: TTiffCompression;
      const AGetTileCallBack: TGetTileCallBack;
      const AStoreAlphaChanel: Boolean;
      const AProjectionInfo: PProjectionInfo;
      const AUserInfo: Pointer;
      out AErrorMessage: string
    ): Boolean; inline;
  public
    constructor Create(
      const ASoftwareIDStr: AnsiString = '';
      const ATileWidth: Integer = 256;
      const ATileHeight: Integer = 256
    );
    procedure AfterConstruction; override;
  end;

implementation

{ TGeoTiffWriter }

constructor TGeoTiffWriter.Create(
  const ASoftwareIDStr: AnsiString;
  const ATileWidth: Integer;
  const ATileHeight: Integer
);
begin
  inherited Create;
  FSoftwareIDStr := ASoftwareIDStr;
  FTileWidth := ATileWidth;
  FTileHeight := ATileHeight;
end;

procedure TGeoTiffWriter.AfterConstruction;
begin
  InitLibTiff;
  InitLibGeoTiff;

  if FSoftwareIDStr <> '' then begin
    FSoftwareIDStr := FSoftwareIDStr + ' with ' + AnsiString(TIFFGetVersion());
  end;
end;

function TGeoTiffWriter.WriteStripped(
  const ATiffType: TTiffType;
  const AOutputFileName: string;
  const AWidth, AHeight: Integer;
  const ACompression: TTiffCompression;
  const AGetLineCallBack: TGetLineCallBack;
  const AStoreAlphaChanel: Boolean;
  const AProjectionInfo: PProjectionInfo;
  const AUserInfo: Pointer;
  out AErrorMessage: string
): Boolean;
begin
  Result := Self.Write(
    ATiffType,
    AOutputFileName,
    AWidth,
    AHeight,
    '',
    ACompression,
    AGetLineCallBack,
    nil,
    AStoreAlphaChanel,
    AProjectionInfo,
    AUserInfo
  );
  if not Result then begin
    AErrorMessage := FErrorMessage;
  end;
end;

function TGeoTiffWriter.WriteTiled(
  const ATiffType: TTiffType;
  const AOutputFileName: string;
  const AWidth, AHeight: Integer;
  const AOverVeiws: string;
  const ACompression: TTiffCompression;
  const AGetTileCallBack: TGetTileCallBack;
  const AStoreAlphaChanel: Boolean;
  const AProjectionInfo: PProjectionInfo;
  const AUserInfo: Pointer;
  out AErrorMessage: string
): Boolean;
begin
  Result := Self.Write(
    ATiffType,
    AOutputFileName,
    AWidth,
    AHeight,
    AOverVeiws,
    ACompression,
    nil,
    AGetTileCallBack,
    AStoreAlphaChanel,
    AProjectionInfo,
    AUserInfo
  );
  if not Result then begin
    AErrorMessage := FErrorMessage;
  end;
end;

function TGeoTiffWriter.Write(
  const ATiffType: TTiffType;
  const AOutputFileName: string;
  const AWidth, AHeight: Integer;
  const AOverVeiws: string;
  const ACompression: TTiffCompression;
  const AGetLineCallBack: TGetLineCallBack;
  const AGetTileCallBack: TGetTileCallBack;
  const AStoreAlphaChanel: Boolean;
  const AProjectionInfo: PProjectionInfo;
  const AUserInfo: Pointer
): Boolean;
var
  VTiff: PTIFF;
  VGTiff: PGTIFF;
  VMode: AnsiString;
  VStorageType: TTiffStorageType;
begin
  Result := False;
  FErrorMessage := '';

  Assert(AOutputFileName <> '');
  Assert(AWidth > 0);
  Assert(AHeight > 0);

  FWidth := AWidth;
  FHeight := AHeight;
  FOverVeiws := AOverVeiws;

  if AProjectionInfo <> nil then begin
    XTIFFInitialize; // Registration of a GeoTIFF extension with libtiff
  end;

  VMode := 'w';
  if ATiffType = ttBigTiff then begin
    VMode := VMode + '8';
  end;

  {$IF DEFINED(WIN32) AND DEFINED(UNICODE)}
  VTiff := TIFFOpenW(PWideChar(WideString(AOutputFileName)), PAnsiChar(VMode));
  {$ELSE}
  VTiff := TIFFOpen(PAnsiChar(AnsiString(AOutputFileName)), PAnsiChar(VMode));
  {$IFEND}

  if VTiff = nil then begin
    FErrorMessage := 'TIFFOpen() failed!';
    Exit;
  end;

  try
    if Assigned(AGetLineCallBack) then begin
      VStorageType := tstStripped;
    end else begin
      VStorageType := tstTiled;
    end;

    WriteTIFFDirectory(
      VTiff,
      VStorageType,
      ACompression,
      AStoreAlphaChanel,
      AProjectionInfo
    );

    VGTiff := nil;
    try
      if AProjectionInfo <> nil then begin
        VGTiff := GTIFNew(VTiff);
        if VGTiff = nil then begin
          FErrorMessage := 'GTIFNew() failed!';
          Exit;
        end;
        WriteGeoKeys(VGTiff, AProjectionInfo);
        GTIFWriteKeys(VGTiff);
      end;

      Result := WriteImage(VTiff, AGetLineCallBack, AGetTileCallBack, AUserInfo);
    finally
      if VGTiff <> nil then begin
        GTIFFree(VGTiff);
      end;
    end;
  finally
    TIFFClose(VTiff);
  end;
end;

procedure TGeoTiffWriter.WriteTIFFDirectory(
  const ATiff: PTIFF;
  const AStorageType: TTiffStorageType;
  const ACompression: TTiffCompression;
  const AStoreAlphaChanel: Boolean;
  const AProjectionInfo: PProjectionInfo
);
var
  VExtras: array of Word;
  VCompression: Word;
  VRowsPerStrip: Word;
  VTiePoints: array [0..5] of Double;
  VPixScale: array [0..2] of Double;
begin
  case ACompression of
    tcZip: VCompression := COMPRESSION_DEFLATE;
    tcLZW: VCompression := COMPRESSION_LZW;
    tcJPG: VCompression := COMPRESSION_JPEG;
  else
    VCompression := COMPRESSION_NONE;
  end;

  if FSoftwareIDStr <> '' then begin
    TIFFSetField(ATiff, TIFFTAG_SOFTWARE, PAnsiChar(FSoftwareIDStr));
  end;

	TIFFSetField(ATiff, TIFFTAG_IMAGEWIDTH, FWidth);
	TIFFSetField(ATiff, TIFFTAG_IMAGELENGTH, FHeight);

  if AStorageType = tstTiled then begin
    TIFFSetField(ATiff, TIFFTAG_TILEWIDTH, FTileWidth);
    TIFFSetField(ATiff, TIFFTAG_TILELENGTH, FTileHeight);
    //TIFFSetField(ATiff, TIFFTAG_IMAGEDEPTH, 1); // todo: add overviews support
  end else begin
    if VCompression = COMPRESSION_JPEG then begin
      VRowsPerStrip := 8; // value must be multiply 8
    end else begin
      VRowsPerStrip := 1;
    end;
    TIFFSetField(ATiff, TIFFTAG_ROWSPERSTRIP, VRowsPerStrip);
  end;

  if AStoreAlphaChanel then begin
    FBitsPerPixel := 4*8;
    SetLength(VExtras, 1);
    VExtras[0] := EXTRASAMPLE_ASSOCALPHA;
    TIFFSetField(ATiff, TIFFTAG_SAMPLESPERPIXEL, 4);
    TIFFSetField(ATiff, TIFFTAG_EXTRASAMPLES, 1, VExtras);
  end else begin
    FBitsPerPixel := 3*8;
    TIFFSetField(ATiff, TIFFTAG_SAMPLESPERPIXEL, 3);
  end;

	TIFFSetField(ATiff, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
	TIFFSetField(ATiff, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	TIFFSetField(ATiff, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField(ATiff, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_UINT);
  TIFFSetField(ATiff, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);

  TIFFSetField(ATiff, TIFFTAG_COMPRESSION, VCompression);

  if AProjectionInfo <> nil then begin
    // GeoTiff info in Tiff Directory
    VTiePoints[0] := 0;
    VTiePoints[1] := 0;
    VTiePoints[2] := 0;
    VTiePoints[3] := AProjectionInfo.OriginX;
    VTiePoints[4] := AProjectionInfo.OriginY;
    VTiePoints[5] := 0;

    VPixScale[0] := AProjectionInfo.CellIncrementX;
    VPixScale[1] := AProjectionInfo.CellIncrementY;
    VPixScale[2] := 0;

    TIFFSetField(ATiff, TIFFTAG_GEOTIEPOINTS, 6, VTiePoints);
    TIFFSetField(ATiff, TIFFTAG_GEOPIXELSCALE, 3, VPixScale);
  end;
end;

procedure TGeoTiffWriter.WriteGeoKeys(
  const AGTiff: PGTIFF;
  const AProjectionInfo: PProjectionInfo
);
const
  cWGS84_a: Double = 6378137;
  cWGS84_b: Double = 6356752.314245;
  cWGS84_f: Double = 298.257223560493;
var
  VEpsg: Word;
  VProjCSCitation: AnsiString;
begin
  Assert(AProjectionInfo <> nil);
  Assert(AProjectionInfo.EPSG > 0);
  Assert(AProjectionInfo.EPSG < $FFFF);

  VEpsg := Word(AProjectionInfo.EPSG);
  VProjCSCitation := 'EPSG:' + AnsiString(IntToStr(VEpsg));

  if not AProjectionInfo.IsGeographic then begin
    GTIFKeySet(AGTiff, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
    GTIFKeySet(AGTiff, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
    GTIFKeySet(AGTiff, GTCitationGeoKey, TYPE_ASCII, 0, PAnsiChar(VProjCSCitation));

    GTIFKeySet(AGTiff, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, VEpsg);
    GTIFKeySet(AGTiff, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
  end else begin
    GTIFKeySet(AGTiff, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeGeographic);
    GTIFKeySet(AGTiff, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
    GTIFKeySet(AGTiff, GTCitationGeoKey, TYPE_ASCII, 0, PAnsiChar(VProjCSCitation));

    GTIFKeySet(AGTiff, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_WGS_84);
    GTIFKeySet(AGTiff, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);

    GTIFKeySet(AGTiff, GeogEllipsoidGeoKey, TYPE_SHORT, 1, Ellipse_WGS_84);
    GTIFKeySet(AGTiff, GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, cWGS84_a);
    GTIFKeySet(AGTiff, GeogSemiMinorAxisGeoKey, TYPE_DOUBLE, 1, cWGS84_b);
    GTIFKeySet(AGTiff, GeogInvFlatteningGeoKey, TYPE_DOUBLE, 1, cWGS84_f);
  end;
end;

function TGeoTiffWriter.WriteImage(
  const ATiff: PTIFF;
  const AGetLineCallBack: TGetLineCallBack;
  const AGetTileCallBack: TGetTileCallBack;
  const AUserInfo: Pointer
): Boolean;
var
  I: Integer;
  X, Y, Z: Integer;
  VData: Pointer;
begin
  Result := False;

  if Assigned(AGetLineCallBack) then begin
    for I := 0 to FHeight - 1 do begin
      VData := AGetLineCallBack(I, AUserInfo);
      if VData <> nil then begin
        if TIFFWriteScanline(ATiff, VData, I, 0) < 0 then begin
          FErrorMessage := 'TIFFWriteScanline() failed!';
          Exit;
        end;
      end else begin
        FErrorMessage := 'GetLineCallBack() failed!';
        Exit;
      end;
    end;
  end else
  if Assigned(AGetTileCallBack) then begin

    Z := 0; // todo: add overviews support

    for X := 0 to (FWidth div FTileWidth) - 1 do begin
      for Y := 0 to (FHeight div FTileHeight) - 1 do begin
        VData := AGetTileCallBack(X, Y, Z, AUserInfo);
        if VData <> nil then begin
          if TIFFWriteTile(ATiff, VData, X * FTileWidth, Y * FTileHeight, Z, 0) < 0 then begin
            FErrorMessage := 'TIFFWriteTile() failed!';
            Exit;
          end;
        end else begin
          FErrorMessage := 'GetTileCallBack() failed!';
          Exit;
        end;
      end;
    end;
  end else begin
    FErrorMessage := 'GetLine/GetTile CallBack not assigned!';
    Exit;
  end;

  Result := True;
end;

end.
