{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_ImportMarksFromSHP;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_ProjConverter,
  i_GeometryLonLatFactory,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  i_VectorItemSubsetBuilder,
  i_VectorItemTree;

type
  TImportMarksFromSHP = class
  private
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FProjConverterFactory: IProjConverterFactory;
  public
    constructor Create(
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AProjConverterFactory: IProjConverterFactory
    );
    function ImportFileToTree(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string
    ): IVectorItemTree;
  end;

  EShpImportMarksError = class(Exception);

implementation

uses
  Classes,
  Math,
  Generics.Collections,
  Generics.Defaults,
  libshp,
  c_ShpMarksCommon,
  t_GeoTypes,
  i_GeometryLonLat,
  i_DoublePoints,
  i_DoublePointsAggregator,
  i_InterfaceListSimple,
  i_VectorItemSubset,
  u_DoublePointsAggregator,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_VectorItemTree,
  u_GlobalDllName;

const
  CFieldNameAliasTitle = 'TITLE';
  CFieldNameAliasLabel = 'LABEL';
  CFieldNameDescAlias = 'DESCRIPTION';

  CCodePageUtf8 = 65001;
  CCodePageLatin1 = 28591; // ISO-8859-1
  CCp437 = 437;
  CCp866 = 866;
  CCp1251 = 1251;

  // Sanity limits for files without a usable .prj: the values must be degrees
  // (360 instead of 180 to tolerate longitudes in the 0..360 convention)
  CMaxAbsLon = 360;
  CMaxAbsLat = 90;

type
  TShpCategoryEntry = record
    Name: string;
    Builder: IVectorItemSubsetBuilder;
  end;

  TShpFileReader = class
  private
    FMainInfoFactory: IVectorDataItemMainInfoFactory;
    FDataFactory: IVectorDataFactory;
    FGeometryFactory: IGeometryLonLatFactory;
    FSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FProjConverterFactory: IProjConverterFactory;

    FBaseName: string; // full path without extension
    FSHP: SHPHandle;
    FDBF: DBFHandle;
    FShapeCount: Integer;
    FShapeType: LongInt;
    FCodePage: Integer;
    FFieldIdxName: Integer;
    FNameFallbackIdx: Integer;
    FNameFallbackWidth: Integer;
    FFieldIdxDesc: Integer;
    FDescContIdx: array of Integer;
    FFieldIdxCategory: Integer;
    FCatContIdx: array of Integer;
    FExtraFieldIdx: array of Integer;
    FExtraFieldName: array of string;
    FCoordConverter: IProjConverter;
    FLineBuilder: IGeometryLonLatLineBuilder;
    FPolygonBuilder: IGeometryLonLatPolygonBuilder;
    FRootBuilder: IVectorItemSubsetBuilder;
    FCategories: array of TShpCategoryEntry;
    FCategoryIdx: TDictionary<string, Integer>;
  private
    function GetShpFileName: string;
    function GetDbfFileName: string;
    function GetCpgFileName: string;
    function GetPrjFileName: string;
    procedure Open;
    procedure Close;
    procedure DetectEncoding;
    procedure ReadDbfSchema;
    procedure SetupCoordConverter;
    function ReadDbfField(
      const AShapeIndex, AFieldIdx: Integer
    ): string;
    function JoinContinuedField(
      const AShapeIndex: Integer;
      const AFieldIdx: Integer;
      const AContIdxs: array of Integer
    ): string;
    function ReadShapeAttributes(
      const AShapeIndex: Integer;
      out AName, ADesc, ACategory: string
    ): Boolean;
    function ToLonLat(const X, Y: Double): TDoublePoint; inline;
    function RingSignedArea(
      const AObj: PShpObject;
      const APartIndex: Integer
    ): Double;
    procedure AddItem(
      const AGeometry: IGeometryLonLat;
      const AName, ADesc, ACategory: string
    );
    procedure AddPointItem(
      const APoint: TDoublePoint;
      const AName, ADesc, ACategory: string
    ); inline;
    function BuildLineGeometry(const AObj: PShpObject): IGeometryLonLat;
    function BuildPolygonGeometry(const AObj: PShpObject): IGeometryLonLat;
    function GetCategoryBuilder(const ACategory: string): IVectorItemSubsetBuilder;
    function MakeTree: IVectorItemTree;
  public
    constructor Create(
      const AMainInfoFactory: IVectorDataItemMainInfoFactory;
      const ADataFactory: IVectorDataFactory;
      const AGeometryFactory: IGeometryLonLatFactory;
      const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AProjConverterFactory: IProjConverterFactory
    );
    destructor Destroy; override;
    function ReadAllToTree(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string
    ): IVectorItemTree;
  end;

function ReadTextFileIfAny(const ABaseName: string): string;
var
  VStream: TFileStream;
  VSize: Integer;
  VAnsi: AnsiString;
begin
  Result := '';
  if not FileExists(ABaseName) then begin
    Exit;
  end;
  VStream := TFileStream.Create(ABaseName, fmOpenRead or fmShareDenyWrite);
  try
    VSize := VStream.Size;
    if VSize > 0 then begin
      SetString(VAnsi, nil, VSize);
      VStream.ReadBuffer(PAnsiChar(VAnsi)^, VSize);
      // WKT and .cpg content are plain ASCII text
      Result := Trim(string(VAnsi));
    end;
  finally
    VStream.Free;
  end;
end;

function ExtractCodePageNumber(const ATag: string): Integer;
var
  I, VStart: Integer;
begin
  Result := 0;
  if Copy(ATag, 1, 2) = 'CP' then begin
    VStart := 3;
  end else if Copy(ATag, 1, 8) = 'WINDOWS-' then begin
    VStart := 9;
  end else begin
    VStart := 1;
  end;
  if VStart <= Length(ATag) then begin
    for I := VStart to Length(ATag) do begin
      if not CharInSet(ATag[I], ['0'..'9']) then begin
        Exit; // not a pure number
      end;
    end;
    Result := StrToIntDef(Copy(ATag, VStart, MaxInt), 0);
    if (Result < 1) or (Result > 65535) then begin
      Result := 0;
    end;
  end;
end;

// Parses ["EPSG","nnnn"] (WKT1 AUTHORITY) or ["EPSG",nnnn] (WKT2 ID) whose
// opening bracket follows APos (the position right after the keyword).
// Returns the code, 0 - not an EPSG reference.
function ParseEpsgNode(const AUpper: string; const APos: Integer): Integer;
var
  I, VLen, VStart: Integer;
begin
  Result := 0;
  VLen := Length(AUpper);
  I := APos;
  while (I <= VLen) and (AUpper[I] <= ' ') do begin
    Inc(I);
  end;
  if (I > VLen) or not CharInSet(AUpper[I], ['[', '(']) then begin
    Exit;
  end;
  Inc(I);
  while (I <= VLen) and (AUpper[I] <= ' ') do begin
    Inc(I);
  end;
  if Copy(AUpper, I, 6) <> '"EPSG"' then begin
    Exit;
  end;
  Inc(I, 6);
  while (I <= VLen) and (AUpper[I] <= ' ') do begin
    Inc(I);
  end;
  if (I > VLen) or (AUpper[I] <> ',') then begin
    Exit;
  end;
  Inc(I);
  while (I <= VLen) and (AUpper[I] <= ' ') do begin
    Inc(I);
  end;
  if (I <= VLen) and (AUpper[I] = '"') then begin
    Inc(I); // WKT1 keeps the code in quotes, WKT2 does not
  end;
  VStart := I;
  while (I <= VLen) and CharInSet(AUpper[I], ['0'..'9']) do begin
    Inc(I);
  end;
  if I > VStart then begin
    Result := StrToIntDef(Copy(AUpper, VStart, I - VStart), 0);
  end;
end;

// Returns the EPSG code of the CRS described by the WKT, i.e. of the AUTHORITY
// (WKT1) / ID (WKT2) node that is a DIRECT child of the root node. Nested
// authorities (datum, unit, base GEOGCS of a PROJCS, ...) are ignored - the
// "last AUTHORITY in the text" rule returned e.g. 9001 (metre) for a PROJCS
// that has no authority of its own. 0 - none found.
function ExtractEpsgFromWkt(const AWkt: string): Integer;
var
  VUpper: string;
  I, VDepth, VCode: Integer;
  VInQuote: Boolean;
  VPrevIsIdent: Boolean;
begin
  Result := 0;
  VUpper := AnsiUpperCase(AWkt);
  VDepth := 0;
  VInQuote := False;
  for I := 1 to Length(VUpper) do begin
    if VUpper[I] = '"' then begin
      VInQuote := not VInQuote;
    end else if not VInQuote then begin
      if CharInSet(VUpper[I], ['[', '(']) then begin
        Inc(VDepth);
      end else if CharInSet(VUpper[I], [']', ')']) then begin
        Dec(VDepth);
      end else if VDepth = 1 then begin
        VPrevIsIdent := (I > 1) and CharInSet(VUpper[I - 1], ['A'..'Z', '0'..'9', '_']);
        VCode := 0;
        if not VPrevIsIdent then begin
          if Copy(VUpper, I, 9) = 'AUTHORITY' then begin
            VCode := ParseEpsgNode(VUpper, I + 9);
          end else if Copy(VUpper, I, 2) = 'ID' then begin
            VCode := ParseEpsgNode(VUpper, I + 2);
          end;
        end;
        if VCode > 0 then begin
          Result := VCode;
        end;
      end;
    end;
  end;
end;

// A projected (not geographic) CRS in either WKT dialect. PROJCS/PROJECTION
// are rejected explicitly: an ESRI projected WKT (UTM, Web Mercator, ...)
// contains a nested GEOGCS["GCS_WGS_1984"...] and would pass the check, so
// metres would be read as degrees.
function IsProjectedWkt(const AWktUpper: string): Boolean;
begin
  Result :=
    (Pos('PROJCS', AWktUpper) > 0) or
    (Pos('PROJECTION', AWktUpper) > 0) or
    (Pos('PROJECTEDCRS', AWktUpper) > 0);
end;

// A geographic (not projected) WGS84 CRS.
function IsGeographicWgs84Wkt(const AWktUpper: string): Boolean;
begin
  Result :=
    not IsProjectedWkt(AWktUpper) and
    (Pos('GEOGCS', AWktUpper) > 0) and
    (
      (Pos('WGS_1984', AWktUpper) > 0) or
      (Pos('WGS 1984', AWktUpper) > 0) or
      (Pos('WGS84', AWktUpper) > 0)
    );
end;

type
  TEsriGcsNameEpsg = record
    GcsName: string;
    Epsg: Integer;
  end;

// ESRI geographic CRS names without an EPSG AUTHORITY node (the WKT of, e.g.,
// old ArcView files names the CRS but has no reference at all). Only codes
// that Proj4ArgsByEpsg resolves to a self-contained proj string are listed
// here - the datum shift must not depend on the proj data files. The lookup
// runs on a WKT already known to be geographic, so a name can only belong to
// the root node.
const
  CEsriGcsNameEpsg: array [0..1] of TEsriGcsNameEpsg = (
    (GcsName: 'GCS_PULKOVO_1942';        Epsg: 4284),
    (GcsName: 'GCS_NORTH_AMERICAN_1983'; Epsg: 4269)
  );

// Returns the EPSG code of a known ESRI geographic CRS name, 0 - unknown name
function EpsgFromEsriGcsName(const AWktUpper: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(CEsriGcsNameEpsg) to High(CEsriGcsNameEpsg) do begin
    if Pos(CEsriGcsNameEpsg[I].GcsName, AWktUpper) > 0 then begin
      Result := CEsriGcsNameEpsg[I].Epsg;
      Break;
    end;
  end;
end;

function EscapeHtml(const AValue: string): string;
begin
  Result := StringReplace(AValue, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

function IsContinuationField(const AName, ABase: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Length(AName) > Length(ABase)) and SameText(Copy(AName, 1, Length(ABase)), ABase) then begin
    for I := Length(ABase) + 1 to Length(AName) do begin
      if not CharInSet(AName[I], ['0'..'9']) then begin
        Exit;
      end;
    end;
    Result := True;
  end;
end;

{ TShpFileReader }

constructor TShpFileReader.Create(
  const AMainInfoFactory: IVectorDataItemMainInfoFactory;
  const ADataFactory: IVectorDataFactory;
  const AGeometryFactory: IGeometryLonLatFactory;
  const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjConverterFactory: IProjConverterFactory
);
begin
  inherited Create;
  FMainInfoFactory := AMainInfoFactory;
  FDataFactory := ADataFactory;
  FGeometryFactory := AGeometryFactory;
  FSubsetBuilderFactory := ASubsetBuilderFactory;
  FProjConverterFactory := AProjConverterFactory;

  FCategoryIdx := TDictionary<string, Integer>.Create(
    TEqualityComparer<string>.Construct(
      function(const ALeft, ARight: string): Boolean
      begin
        Result := AnsiSameText(ALeft, ARight);
      end,
      function(const AValue: string): Integer
      begin
        Result := AnsiUpperCase(AValue).GetHashCode;
      end
    )
  );
end;

destructor TShpFileReader.Destroy;
begin
  Close;
  FCategoryIdx.Free;
  inherited Destroy;
end;

function TShpFileReader.GetShpFileName: string;
begin
  Result := FBaseName + '.shp';
end;

function TShpFileReader.GetDbfFileName: string;
begin
  Result := FBaseName + '.dbf';
end;

function TShpFileReader.GetCpgFileName: string;
begin
  Result := FBaseName + '.cpg';
end;

function TShpFileReader.GetPrjFileName: string;
begin
  Result := FBaseName + '.prj';
end;

procedure TShpFileReader.Open;
var
  VHooks: SAHooks;
  VMinBound: array [0..3] of Double; // X, Y, Z, M
  VMaxBound: array [0..3] of Double;
begin
  if not FileExists(GetShpFileName) then begin
    raise EShpImportMarksError.CreateFmt('Shapefile not found: %s', [GetShpFileName]);
  end;

  if not FileExists(GetDbfFileName) then begin
    raise EShpImportMarksError.CreateFmt(
      'DBF file not found: %s - a shapefile needs both .shp and .dbf files',
      [GetDbfFileName]
    );
  end;

  SASetupUtf8Hooks(@VHooks);

  FSHP := SHPOpenLL(PAnsiChar(UTF8String(GetShpFileName)), 'rb', @VHooks);
  if FSHP = nil then begin
    raise EShpImportMarksError.CreateFmt('Failed to open shapefile: %s', [GetShpFileName]);
  end;

  try
    FDBF := DBFOpenLL(PAnsiChar(UTF8String(GetDbfFileName)), 'rb', @VHooks);
    if FDBF = nil then begin
      raise EShpImportMarksError.CreateFmt('Failed to open DBF file: %s', [GetDbfFileName]);
    end;

    FillChar(VMinBound, SizeOf(VMinBound), 0);
    FillChar(VMaxBound, SizeOf(VMaxBound), 0);
    SHPGetInfo(FSHP, @FShapeCount, @FShapeType, @VMinBound[0], @VMaxBound[0]);

    case FShapeType of
      SHPT_POINT, SHPT_POINTZ, SHPT_POINTM,
      SHPT_MULTIPOINT, SHPT_MULTIPOINTZ, SHPT_MULTIPOINTM,
      SHPT_ARC, SHPT_ARCZ, SHPT_ARCM,
      SHPT_POLYGON, SHPT_POLYGONZ, SHPT_POLYGONM:
        ; // supported
    else
      raise EShpImportMarksError.CreateFmt('Unsupported shape type %d in: %s', [FShapeType, GetShpFileName]);
    end;

    DetectEncoding;
    ReadDbfSchema;
    SetupCoordConverter;

    if (FCoordConverter = nil) and (FShapeCount > 0) and
      (
        (Abs(VMinBound[0]) > CMaxAbsLon) or (Abs(VMaxBound[0]) > CMaxAbsLon) or
        (Abs(VMinBound[1]) > CMaxAbsLat) or (Abs(VMaxBound[1]) > CMaxAbsLat)
      )
    then begin
      raise EShpImportMarksError.CreateFmt(
        'Coordinates in %s are outside the lon/lat range and there is no usable .prj: ' +
        'the file looks projected',
        [GetShpFileName]
      );
    end;

    FLineBuilder := FGeometryFactory.MakeLineBuilder;
    FPolygonBuilder := FGeometryFactory.MakePolygonBuilder;
    FRootBuilder := FSubsetBuilderFactory.Build;
  except
    Close;
    raise;
  end;
end;

procedure TShpFileReader.Close;
begin
  if FDBF <> nil then begin
    DBFClose(FDBF);
    FDBF := nil;
  end;
  if FSHP <> nil then begin
    SHPClose(FSHP);
    FSHP := nil;
  end;
  FLineBuilder := nil;
  FPolygonBuilder := nil;
  FRootBuilder := nil;
  SetLength(FCategories, 0);
  FCategoryIdx.Clear;
end;

procedure TShpFileReader.DetectEncoding;
var
  VTag: string;
  P: PAnsiChar;
begin
  FCodePage := 0; // system default ANSI

  VTag := ReadTextFileIfAny(GetCpgFileName);
  if VTag = '' then begin
    P := DBFGetCodePage(FDBF);
    if P <> nil then begin
      VTag := string(AnsiString(P));
    end;
  end;
  VTag := AnsiUpperCase(Trim(VTag));
  if VTag = '' then begin
    Exit;
  end;

  if
    (Pos('UTF-8', VTag) > 0) or
    (Pos('UTF8', VTag) > 0) or
    (Pos('65001', VTag) > 0)
  then begin
    FCodePage := CCodePageUtf8;
    Exit;
  end;

  if (Pos('ISO-8859-1', VTag) > 0) or (Pos('ISO8859-1', VTag) > 0) then begin
    FCodePage := CCodePageLatin1;
    Exit;
  end;

  // LDID/nn - the DBF header language driver id, mapped like GDAL does
  // (GetEncodingFromLDIDNumber). LDID/87 means ANSI/Latin-1, not UTF-8: there
  // is no UTF-8 LDID, UTF-8 files are marked via .cpg (see the export).
  if Copy(VTag, 1, 5) = 'LDID/' then begin
    case StrToIntDef(Copy(VTag, 6, MaxInt), 0) of
      87: FCodePage := CCodePageLatin1; // ANSI, American; Windows
      25: FCodePage := CCp437;          // per GDAL
      38, 101: FCodePage := CCp866;     // Russian MS-DOS
      201: FCodePage := CCp1251;        // Windows, Cyrillic
    end;
    Exit; // an unknown LDID keeps the system default ANSI
  end;

  // a Windows code page number: "1251", "CP1251", "WINDOWS-1251"
  FCodePage := ExtractCodePageNumber(VTag);
end;

procedure TShpFileReader.ReadDbfSchema;
var
  I, VCount: Integer;
  VNameBuf: array [0..XBASE_FLDNAME_LEN_READ] of AnsiChar;
  VWidth, VDecimals: LongInt;
  VFieldType: DBFFieldType;
  VName: string;
  VAliasNameIdx: Integer;
begin
  FFieldIdxName := -1;
  FNameFallbackIdx := -1;
  FNameFallbackWidth := 0;
  FFieldIdxDesc := -1;
  FFieldIdxCategory := -1;
  VAliasNameIdx := -1;
  SetLength(FDescContIdx, 0);
  SetLength(FCatContIdx, 0);
  SetLength(FExtraFieldIdx, 0);
  SetLength(FExtraFieldName, 0);

  VCount := DBFGetFieldCount(FDBF);
  for I := 0 to VCount - 1 do begin
    FillChar(VNameBuf, SizeOf(VNameBuf), 0);
    VFieldType := DBFGetFieldInfo(FDBF, I, @VNameBuf[0], @VWidth, @VDecimals);
    if VFieldType = ftInvalid then begin
      Continue;
    end;
    VName := Trim(string(AnsiString(PAnsiChar(@VNameBuf[0]))));
    if (FFieldIdxName < 0) and SameText(VName, CFieldNameName) then begin
      FFieldIdxName := I;
    end else if (VAliasNameIdx < 0) and
      (SameText(VName, CFieldNameAliasTitle) or SameText(VName, CFieldNameAliasLabel))
    then begin
      // a foreign file rarely has a NAME field; a real NAME found later wins
      VAliasNameIdx := I;
    end else if (FFieldIdxDesc < 0) and
      (SameText(VName, CFieldNameDesc) or SameText(VName, CFieldNameDescAlias))
    then begin
      FFieldIdxDesc := I;
    end else if IsContinuationField(VName, CFieldNameDesc) then begin
      SetLength(FDescContIdx, Length(FDescContIdx) + 1);
      FDescContIdx[High(FDescContIdx)] := I;
    end else if (FFieldIdxCategory < 0) and SameText(VName, CFieldNameCategory) then begin
      FFieldIdxCategory := I;
    end else if IsContinuationField(VName, CFieldNameCategory) then begin
      SetLength(FCatContIdx, Length(FCatContIdx) + 1);
      FCatContIdx[High(FCatContIdx)] := I;
    end else begin
      SetLength(FExtraFieldIdx, Length(FExtraFieldIdx) + 1);
      FExtraFieldIdx[High(FExtraFieldIdx)] := I;
      SetLength(FExtraFieldName, Length(FExtraFieldName) + 1);
      FExtraFieldName[High(FExtraFieldName)] := VName;
      // the name fallback candidate; a numeric or a logical field makes a
      // poor name even when it is the widest one
      if (VFieldType = ftString) and (VWidth > FNameFallbackWidth) then begin
        FNameFallbackIdx := I;
        FNameFallbackWidth := VWidth;
      end;
    end;
  end;
  if FFieldIdxName < 0 then begin
    FFieldIdxName := VAliasNameIdx;
  end;
end;

procedure TShpFileReader.SetupCoordConverter;
var
  VPrj: string;
  VPrjUpper: string;
  VEpsg: Integer;
begin
  FCoordConverter := nil; // WGS84 lon/lat by default (same as without a .prj)

  VPrj := ReadTextFileIfAny(GetPrjFileName);
  if VPrj = '' then begin
    Exit;
  end;
  VPrjUpper := AnsiUpperCase(VPrj);

  VEpsg := ExtractEpsgFromWkt(VPrj);
  if (VEpsg = 0) and not IsProjectedWkt(VPrjUpper) then begin
    // no AUTHORITY node: match a geographic CRS by its ESRI name instead
    VEpsg := EpsgFromEsriGcsName(VPrjUpper);
  end;
  if VEpsg > 0 then begin
    if VEpsg = 4326 then begin
      Exit; // geographic WGS84
    end;
    // a geographic CRS other than WGS84 (e.g. EPSG:4284 Pulkovo 1942) is fine
    // too: GetByEPSG builds a +proj=longlat converter and XY2LonLat applies
    // the datum shift; an unsupported code fails the proj4 init and raises
    FCoordConverter := FProjConverterFactory.GetByEPSG(VEpsg);
    if FCoordConverter = nil then begin
      raise EShpImportMarksError.CreateFmt(
        'Unsupported projection EPSG:%d in: %s', [VEpsg, GetPrjFileName]
      );
    end;
  end else begin
    // no EPSG reference: accept only geographic WGS84 written the ESRI way
    if not IsGeographicWgs84Wkt(VPrjUpper) then begin
      raise EShpImportMarksError.CreateFmt(
        'Unsupported projection in: %s - only geographic WGS84 or a CRS with an EPSG reference is supported',
        [GetPrjFileName]
      );
    end;
  end;
end;

function TShpFileReader.ReadDbfField(
  const AShapeIndex, AFieldIdx: Integer
): string;
var
  P: PAnsiChar;
  VRaw: RawByteString;
begin
  Result := '';
  if AFieldIdx < 0 then begin
    Exit;
  end;
  if DBFIsAttributeNULL(FDBF, AShapeIndex, AFieldIdx) <> 0 then begin
    Exit;
  end;
  P := DBFReadStringAttribute(FDBF, AShapeIndex, AFieldIdx);
  if P = nil then begin
    Exit;
  end;
  // copy the value now: the buffer is reused by the next DBF call
  VRaw := AnsiString(P);
  if FCodePage <> 0 then begin
    // re-tag the bytes with the file code page (no conversion here); the
    // conversion to UTF-16 happens in the string() cast below
    SetCodePage(VRaw, FCodePage, False);
  end;
  // FCodePage = 0: the AnsiString keeps the system ANSI code page
  // the value is returned as is (padded with spaces to the field width) -
  // the callers trim it, see ReadShapeAttributes
  Result := string(VRaw);
end;

function TShpFileReader.JoinContinuedField(
  const AShapeIndex: Integer;
  const AFieldIdx: Integer;
  const AContIdxs: array of Integer
): string;
var
  I, VLast: Integer;
  VParts: array of string;
begin
  SetLength(VParts, Length(AContIdxs) + 1);
  VParts[0] := ReadDbfField(AShapeIndex, AFieldIdx);
  for I := 0 to High(AContIdxs) do begin
    VParts[I + 1] := ReadDbfField(AShapeIndex, AContIdxs[I]);
  end;
  VLast := -1;
  for I := 0 to High(VParts) do begin
    if Trim(VParts[I]) <> '' then begin
      VLast := I;
    end;
  end;
  Result := '';
  for I := 0 to VLast do begin
    if I = VLast then begin
      Result := Result + TrimRight(VParts[I]);
    end else begin
      Result := Result + VParts[I];
    end;
  end;
end;

function TShpFileReader.ReadShapeAttributes(
  const AShapeIndex: Integer;
  out AName, ADesc, ACategory: string
): Boolean;
var
  I: Integer;
  VField: string;
begin
  if DBFIsRecordDeleted(FDBF, AShapeIndex) <> 0 then begin
    Exit(False);
  end;
  AName := Trim(ReadDbfField(AShapeIndex, FFieldIdxName));
  if AName = '' then begin
    AName := Trim(ReadDbfField(AShapeIndex, FNameFallbackIdx));
  end;
  ADesc := JoinContinuedField(AShapeIndex, FFieldIdxDesc, FDescContIdx);
  ACategory := Trim(JoinContinuedField(AShapeIndex, FFieldIdxCategory, FCatContIdx));
  // other attributes are appended to the description as "name: value" lines
  for I := 0 to High(FExtraFieldIdx) do begin
    VField := Trim(ReadDbfField(AShapeIndex, FExtraFieldIdx[I]));
    if VField <> '' then begin
      if ADesc <> '' then begin
        ADesc := ADesc + '<br>';
      end;
      ADesc := ADesc + EscapeHtml(FExtraFieldName[I]) + ': ' + EscapeHtml(VField);
    end;
  end;
  Result := True;
end;

function TShpFileReader.ToLonLat(const X, Y: Double): TDoublePoint;
begin
  Result.X := X;
  Result.Y := Y;
  if FCoordConverter <> nil then begin
    Result := FCoordConverter.XY2LonLat(Result);
  end;
end;

function TShpFileReader.RingSignedArea(
  const AObj: PShpObject;
  const APartIndex: Integer
): Double;
var
  I, J, VStart, VFinish: Integer;
begin
  // shoelace formula: > 0 - counter-clockwise, < 0 - clockwise
  Result := 0;
  VStart := AObj.panPartStart[APartIndex];
  if APartIndex + 1 < AObj.nParts then begin
    VFinish := AObj.panPartStart[APartIndex + 1];
  end else begin
    VFinish := AObj.nVertices;
  end;
  for I := VStart to VFinish - 1 do begin
    J := I + 1;
    if J = VFinish then begin
      J := VStart;
    end;
    Result := Result + AObj.padfX[I] * AObj.padfY[J] - AObj.padfX[J] * AObj.padfY[I];
  end;
  Result := Result / 2;
end;

procedure TShpFileReader.AddItem(
  const AGeometry: IGeometryLonLat;
  const AName, ADesc, ACategory: string
);
var
  VBuilder: IVectorItemSubsetBuilder;
begin
  if ACategory <> '' then begin
    VBuilder := GetCategoryBuilder(ACategory);
  end else begin
    VBuilder := FRootBuilder;
  end;
  VBuilder.Add(
    FDataFactory.BuildItem(
      FMainInfoFactory.BuildMainInfo(nil, AName, ADesc),
      nil,
      AGeometry
    )
  );
end;

procedure TShpFileReader.AddPointItem(
  const APoint: TDoublePoint;
  const AName, ADesc, ACategory: string
);
begin
  AddItem(FGeometryFactory.CreateLonLatPoint(APoint), AName, ADesc, ACategory);
end;

function TShpFileReader.BuildLineGeometry(const AObj: PShpObject): IGeometryLonLat;
var
  I, J, VStart, VFinish: Integer;
  VAgg: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  VAdded: Boolean;
begin
  Result := nil;
  VAdded := False;
  for I := 0 to AObj.nParts - 1 do begin
    VStart := AObj.panPartStart[I];
    if I + 1 < AObj.nParts then begin
      VFinish := AObj.panPartStart[I + 1];
    end else begin
      VFinish := AObj.nVertices;
    end;
    if VFinish - VStart < 2 then begin
      Continue; // discard the invalid part
    end;
    VAgg := TDoublePointsAggregator.Create(VFinish - VStart);
    for J := VStart to VFinish - 1 do begin
      VPoint := ToLonLat(AObj.padfX[J], AObj.padfY[J]);
      VAgg.Add(VPoint);
    end;
    FLineBuilder.AddLine(VAgg.MakeStaticAndClear);
    VAdded := True;
  end;
  if VAdded then begin
    Result := FLineBuilder.MakeStaticAndClear;
  end;
end;

function TShpFileReader.BuildPolygonGeometry(const AObj: PShpObject): IGeometryLonLat;
var
  I, J: Integer;
  VStart, VFinish, VOuterCount: Integer;
  VAgg: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  VFirstPoint: TDoublePoint;
  VIsOuter: Boolean;
begin
  Result := nil;

  // reorder the rings per the ESRI convention: outer rings clockwise, holes
  // counter-clockwise - so the ring orientation can be used for classifying
  SHPRewindObject(FSHP, AObj);

  VOuterCount := 0;
  for I := 0 to AObj.nParts - 1 do begin
    VStart := AObj.panPartStart[I];
    if I + 1 < AObj.nParts then begin
      VFinish := AObj.panPartStart[I + 1];
    end else begin
      VFinish := AObj.nVertices;
    end;
    if VFinish - VStart < 3 then begin
      Continue; // discard the invalid ring
    end;
    VAgg := TDoublePointsAggregator.Create(VFinish - VStart);
    VFirstPoint := ToLonLat(AObj.padfX[VStart], AObj.padfY[VStart]);
    for J := VStart to VFinish - 1 do begin
      VPoint := ToLonLat(AObj.padfX[J], AObj.padfY[J]);
      if (J = VFinish - 1) and DoublePointsEqual(VPoint, VFirstPoint) then begin
        Continue; // drop the closing vertex
      end;
      VAgg.Add(VPoint);
    end;
    // a valid ring holds at least 3 distinct vertices
    if VAgg.Count < 3 then begin
      Continue; // discard the invalid ring
    end;

    VIsOuter := RingSignedArea(AObj, I) < 0; // clockwise
    // AddHole attaches the hole to the LAST outer ring added (the holes are
    // kept in a list that is flushed by the next AddOuter - see
    // TGeometryLonLatPolygonBuilder); the export writes
    // "outer, its holes, next outer, ..."
    if VIsOuter or (VOuterCount = 0) then begin
      // a hole before any outer ring is treated as an outer ring (defensive)
      FPolygonBuilder.AddOuter(VAgg.MakeStaticAndClear);
      Inc(VOuterCount);
    end else begin
      FPolygonBuilder.AddHole(VAgg.MakeStaticAndClear);
    end;
  end;

  if VOuterCount > 0 then begin
    Result := FPolygonBuilder.MakeStaticAndClear;
  end;
end;

function TShpFileReader.GetCategoryBuilder(
  const ACategory: string
): IVectorItemSubsetBuilder;
var
  VIndex: Integer;
begin
  if FCategoryIdx.TryGetValue(ACategory, VIndex) then begin
    Result := FCategories[VIndex].Builder;
    Exit;
  end;
  VIndex := Length(FCategories);
  SetLength(FCategories, VIndex + 1);
  FCategories[VIndex].Name := ACategory;
  FCategories[VIndex].Builder := FSubsetBuilderFactory.Build;
  FCategoryIdx.AddOrSetValue(ACategory, VIndex);
  Result := FCategories[VIndex].Builder;
end;

function TShpFileReader.MakeTree: IVectorItemTree;
var
  I: Integer;
  VSubTreeList: IInterfaceListSimple;
  VSubTree: IVectorItemTree;
begin
  VSubTreeList := TInterfaceListSimple.Create;
  for I := 0 to High(FCategories) do begin
    VSubTree :=
      TVectorItemTree.Create(
        FCategories[I].Name,
        FCategories[I].Builder.MakeStaticAndClear,
        nil
      );
    VSubTreeList.Add(VSubTree);
  end;
  Result :=
    TVectorItemTree.Create(
      '',
      FRootBuilder.MakeStaticAndClear,
      VSubTreeList.MakeStaticAndClear
    );
end;

function TShpFileReader.ReadAllToTree(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string
): IVectorItemTree;

  function ShpBaseNameFromFileName: string;
  begin
    if SameText(ExtractFileExt(AFileName), '.shp') then begin
      Result := ChangeFileExt(AFileName, '');
    end else begin
      Result := AFileName;
    end;
  end;

var
  I, V: Integer;
  VObj: PShpObject;
  VName, VDesc, VCategory: string;
  VGeometry: IGeometryLonLat;
begin
  FBaseName := ShpBaseNameFromFileName;

  Open;
  try
    for I := 0 to FShapeCount - 1 do begin
      if (ACancelNotifier <> nil) and ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        raise EShpImportMarksError.Create('Operation canceled by user');
      end;

      // the DBF record may be missing for a shape (NULL shapes, deleted rows)
      if not ReadShapeAttributes(I, VName, VDesc, VCategory) then begin
        Continue;
      end;

      VObj := SHPReadObject(FSHP, I);
      if VObj = nil then begin
        Continue;
      end;
      try
        case FShapeType of
          SHPT_POINT, SHPT_POINTZ, SHPT_POINTM: begin
            if VObj.nVertices >= 1 then begin
              AddPointItem(ToLonLat(VObj.padfX[0], VObj.padfY[0]), VName, VDesc, VCategory);
            end;
          end;

          SHPT_MULTIPOINT, SHPT_MULTIPOINTZ, SHPT_MULTIPOINTM: begin
            // each vertex becomes a separate point mark with the same attributes
            for V := 0 to VObj.nVertices - 1 do begin
              AddPointItem(ToLonLat(VObj.padfX[V], VObj.padfY[V]), VName, VDesc, VCategory);
            end;
          end;

          SHPT_ARC, SHPT_ARCZ, SHPT_ARCM: begin
            VGeometry := BuildLineGeometry(VObj);
            if VGeometry <> nil then begin
              AddItem(VGeometry, VName, VDesc, VCategory);
            end;
          end;

          SHPT_POLYGON, SHPT_POLYGONZ, SHPT_POLYGONM: begin
            VGeometry := BuildPolygonGeometry(VObj);
            if VGeometry <> nil then begin
              AddItem(VGeometry, VName, VDesc, VCategory);
            end;
          end;
        end;
        // SHPT_NULL shapes carry no geometry and are skipped
      finally
        SHPDestroyObject(VObj);
      end;
    end;

    Result := MakeTree;
  finally
    Close;
  end;
end;

{ TImportMarksFromSHP }

constructor TImportMarksFromSHP.Create(
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjConverterFactory: IProjConverterFactory
);
begin
  inherited Create;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FVectorDataFactory := AVectorDataFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FProjConverterFactory := AProjConverterFactory;
end;

function TImportMarksFromSHP.ImportFileToTree(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string
): IVectorItemTree;
var
  VReader: TShpFileReader;
begin
  LoadLibShp(GDllName.Shp, True);

  VReader :=
    TShpFileReader.Create(
      FVectorDataItemMainInfoFactory,
      FVectorDataFactory,
      FVectorGeometryLonLatFactory,
      FVectorItemSubsetBuilderFactory,
      FProjConverterFactory
    );
  try
    Result := VReader.ReadAllToTree(AOperationID, ACancelNotifier, AFileName);
  finally
    VReader.Free;
  end;
end;

end.
