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

  TTiffCompression = (
    tcNone = 0,
    tcZip = 1,
    tcLZW = 2,
    tcJPG = 3
  );

  TGetLineCallBack = function(
    const ARowNumber: Integer;
    const ALineSize: Integer;
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
    FBitsPerPixel: Integer;
    FSoftwareIDStr: AnsiString;
    procedure WriteTIFFDirectory(
      const ATiff: PTIFF;
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
      const AUserInfo: Pointer
    ): Boolean;
  public
    procedure Write(
      const ATiffType: TTiffType;
      const AOutputFileName: WideString;
      const AWidth, AHeight: Integer;
      const ACompression: TTiffCompression;
      const AGetLineCallBack: TGetLineCallBack;
      const AStoreAlphaChanel: Boolean = False;
      const AProjectionInfo: PProjectionInfo = nil;
      const AUserInfo: Pointer = nil
    );
    constructor Create(const ASoftwareIDStr: AnsiString = '');
    procedure AfterConstruction; override;
  end;

implementation

{ TGeoTiffWriter }

constructor TGeoTiffWriter.Create(
  const ASoftwareIDStr: AnsiString
);
begin
  inherited Create;
  FSoftwareIDStr := ASoftwareIDStr;
end;

procedure TGeoTiffWriter.AfterConstruction;
begin
  InitLibTiff; // (!)
  InitLibGeoTiff; // (!)

  if FSoftwareIDStr <> '' then begin
    FSoftwareIDStr := FSoftwareIDStr + ' with ';
  end;
  FSoftwareIDStr := FSoftwareIDStr + AnsiString(TIFFGetVersion());
end;

procedure TGeoTiffWriter.Write(
  const ATiffType: TTiffType;
  const AOutputFileName: WideString;
  const AWidth, AHeight: Integer;
  const ACompression: TTiffCompression;
  const AGetLineCallBack: TGetLineCallBack;
  const AStoreAlphaChanel: Boolean = False;
  const AProjectionInfo: PProjectionInfo = nil;
  const AUserInfo: Pointer = nil
);
var
  VTiff: PTIFF;
  VGTiff: PGTIFF;
  VMode: AnsiString;
  VIsWriteOK: Boolean;
begin
  Assert(AOutputFileName <> '');
  Assert(AWidth > 0);
  Assert(AHeight > 0);

  FWidth := AWidth;
  FHeight := AHeight;

  if AProjectionInfo <> nil then begin
    XTIFFInitialize; // (!) Registers an GeoTIFF extension with libtiff
  end;

  VMode := 'w';
  if ATiffType = ttBigTiff then begin
    VMode := VMode + '8';
  end;

  VTiff := TIFFOpenW(PWideChar(AOutputFileName), PAnsiChar(VMode));
  if VTiff <> nil then begin
    try
      WriteTIFFDirectory(
        VTiff,
        ACompression,
        AStoreAlphaChanel,
        AProjectionInfo
      );
      if AProjectionInfo <> nil then begin
        VIsWriteOK := False;
        VGTiff := GTIFNew(VTiff);
        if VGTiff <> nil then begin
          try
            WriteGeoKeys(
              VGTiff,
              AProjectionInfo
            );
            GTIFWriteKeys(VGTiff);
            VIsWriteOK := WriteImage(VTiff, AGetLineCallBack, AUserInfo);
          finally
            GTIFFree(VGTiff);
          end;
        end else begin
          // error create GeoTiff struct
        end;
      end else begin
        VIsWriteOK := WriteImage(VTiff, AGetLineCallBack, AUserInfo);
      end;
      if not VIsWriteOK then begin
        // error write TIFF
      end;
    finally
      TIFFClose(VTiff);
    end;
  end else begin
    // error open TIFF
  end;
end;

procedure TGeoTiffWriter.WriteTIFFDirectory(
  const ATiff: PTIFF;
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
  TIFFSetField(ATiff, TIFFTAG_SOFTWARE, PAnsiChar(FSoftwareIDStr));

	TIFFSetField(ATiff, TIFFTAG_IMAGEWIDTH, FWidth);
	TIFFSetField(ATiff, TIFFTAG_IMAGELENGTH, FHeight);

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

  case ACompression of
    tcZip: VCompression := COMPRESSION_DEFLATE;
    tcLZW: VCompression := COMPRESSION_LZW;
    tcJPG: VCompression := COMPRESSION_JPEG;
  else
    VCompression := COMPRESSION_NONE;
  end;
  TIFFSetField(ATiff, TIFFTAG_COMPRESSION, VCompression);

	TIFFSetField(ATiff, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
	TIFFSetField(ATiff, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	TIFFSetField(ATiff, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField(ATiff, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);

  if VCompression = COMPRESSION_JPEG then begin
    VRowsPerStrip := 8; // value must be multiply 8
  end else begin
    VRowsPerStrip := 1;
  end;
	TIFFSetField(ATiff, TIFFTAG_ROWSPERSTRIP, VRowsPerStrip);

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
  const AUserInfo: Pointer
): Boolean;
var
  I: Integer;
  VLine: Pointer;
  VLineSize: Integer;
  VWriteResult: Integer;
begin
  Assert(Assigned(AGetLineCallBack));

  Result := True;

  VLineSize := FWidth * (FBitsPerPixel div 8);
  Assert(VLineSize > 0);

  for I := 0 to FHeight - 1 do begin
    VLine := AGetLineCallBack(I, VLineSize, AUserInfo);
    if VLine <> nil then begin
      VWriteResult := TIFFWriteScanline(ATiff, VLine, I, 0);
      if VWriteResult < 0 then begin
        Result := False; // Failure in WriteScanline
        Break;
      end;
    end else begin
      Result := False; // Failure in GetLine
      Break;
    end;
  end;
end;

end.
