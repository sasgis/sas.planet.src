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

unit u_ExportMarks2SHP;

interface

uses
  SysUtils,
  libshp,
  c_ShpMarksCommon,
  t_GeoTypes,
  i_EnumDoublePoint,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemTree,
  i_NotifierOperation;

type
  TShpKind = (skPoint, skLine, skPoly);

const
  CShpKindSuffix: array [TShpKind] of string = ('', '_line', '_poly');
  CShpKindShapeType: array [TShpKind] of LongInt = (SHPT_POINT, SHPT_ARC, SHPT_POLYGON);

  // The DBF header language driver id (byte 29): LDID/87 (0x57) nominally means
  // "ANSI, American" (ISO-8859-1/CP1252) - there is no LDID for UTF-8. GDAL
  // writes the same pair for UTF-8 shapefiles: LDID/87 in the header and the
  // encoding name in the .cpg sidecar, which is what readers use.
  CDbfCodePage = 'LDID/87';
  CCpgContent = 'UTF-8';

  // ESRI WKT of GCS WGS 1984 - marks are always stored in WGS84 lon/lat
  CPrjContent =
    'GEOGCS["GCS_WGS_1984",' +
    'DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],' +
    'PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]';

type
  TShpDoubleArray = array of Double;
  TShpLongIntArray = array of LongInt;

  TShpFileWriter = class
  private
    FKind: TShpKind;
    FBaseName: string; // full path without extension
    FSHP: SHPHandle;
    FDBF: DBFHandle;
    FCount: Integer;
    FFieldIdxName: LongInt;
    FFieldIdxDesc: LongInt;
    FFieldIdxCategory: LongInt;
    FDescFieldIdx: TShpLongIntArray;
    FCatFieldIdx: TShpLongIntArray;
    FVertCount: Integer;
    FXs: TShpDoubleArray;
    FYs: TShpDoubleArray;
    FPartCount: Integer;
    FPartStart: TShpLongIntArray;
  private
    function GetShpFileName: string;
    function GetDbfFileName: string;
    function GetCpgFileName: string;
    function GetPrjFileName: string;
    procedure EnsureOpen;
    procedure EnsureCapacity(const ANeeded: Integer);
    procedure ResetShape;
    procedure WriteSupportFile(
      const AFileName: string;
      const AContent: AnsiString
    );
    function GetContinuationFieldIdx(
      const ABaseName: string;
      const AFieldWidth: Integer;
      var AContIdxs: TShpLongIntArray;
      const APartIdx: Integer
    ): LongInt;
    procedure WriteContinuedField(
      const AShapeIndex: Integer;
      const AFieldIdx: LongInt;
      const ABaseName: string;
      const AFieldWidth: Integer;
      var AContIdxs: TShpLongIntArray;
      const AValue: string
    );
    procedure WriteStringAttributeChecked(
      const AShapeIndex: Integer;
      const AFieldIdx: LongInt;
      const AValue: UTF8String
    );
    procedure WriteDbfRow(
      const AShapeIndex: Integer;
      const AName, ADesc, ACategory: string
    );
    class function PrepareDbfString(
      const AValue: string;
      const AMaxWidth: Integer
    ): UTF8String; static;
  public
    constructor Create(
      const ABaseName: string;
      const AKind: TShpKind
    );
    destructor Destroy; override;

    class procedure DeleteFileSet(
      const ABaseName: string;
      const AKind: TShpKind
    ); static;

    procedure AddPoint(
      const APoint: TDoublePoint;
      const AName, ADesc, ACategory: string
    );

    function AddPart(
      const AEnum: IEnumLonLatPoint;
      const AIsRing: Boolean
    ): Boolean;
    procedure EndShape(
      const AName, ADesc, ACategory: string
    );
    procedure Finish;
  end;

  TWriters = array [TShpKind] of TShpFileWriter;

  TExportMarks2SHP = class
  private
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    function GetCategoryName(const AMark: IVectorDataItem): string;
    procedure CheckCanceled;
    procedure AddPolygonToWriter(
      const APolygon: IGeometryLonLatSinglePolygon;
      const AWriter: TShpFileWriter
    );
    procedure AddMark(
      const AMark: IVectorDataItem;
      const AWriters: TWriters
    );
    procedure AddMarks(
      const AMarksSubset: IVectorItemSubset;
      const AWriters: TWriters
    );
    procedure AddTree(
      const ATree: IVectorItemTree;
      const AWriters: TWriters
    );
  public
    procedure ExportTreeToSHP(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATree: IVectorItemTree;
      const AFileName: string
    );
  end;

  EShpExportMarksError = class(Exception);

implementation

uses
  Classes,
  u_GlobalDllName;

{ TShpFileWriter }

constructor TShpFileWriter.Create(
  const ABaseName: string;
  const AKind: TShpKind
);
begin
  inherited Create;
  FBaseName := ABaseName;
  FKind := AKind;
  FFieldIdxName := -1;
  FFieldIdxDesc := -1;
  FFieldIdxCategory := -1;
end;

destructor TShpFileWriter.Destroy;
begin
  Finish;
  inherited Destroy;
end;

function TShpFileWriter.GetShpFileName: string;
begin
  Result := FBaseName + CShpKindSuffix[FKind] + '.shp';
end;

function TShpFileWriter.GetDbfFileName: string;
begin
  Result := FBaseName + CShpKindSuffix[FKind] + '.dbf';
end;

function TShpFileWriter.GetCpgFileName: string;
begin
  Result := FBaseName + CShpKindSuffix[FKind] + '.cpg';
end;

function TShpFileWriter.GetPrjFileName: string;
begin
  Result := FBaseName + CShpKindSuffix[FKind] + '.prj';
end;

class procedure TShpFileWriter.DeleteFileSet(
  const ABaseName: string;
  const AKind: TShpKind
);
const
  CExts: array [0..4] of string = ('.shp', '.shx', '.dbf', '.cpg', '.prj');
var
  I: Integer;
begin
  for I := Low(CExts) to High(CExts) do begin
    SysUtils.DeleteFile(ABaseName + CShpKindSuffix[AKind] + CExts[I]);
  end;
end;

procedure TShpFileWriter.EnsureOpen;
var
  VHooks: SAHooks;
begin
  if FSHP <> nil then begin
    Exit;
  end;

  SASetupUtf8Hooks(@VHooks);

  FSHP := SHPCreateLL(
    PAnsiChar(UTF8String(GetShpFileName)),
    CShpKindShapeType[FKind],
    @VHooks
  );
  if FSHP = nil then begin
    raise EShpExportMarksError.CreateFmt('Failed to create shapefile: %s', [GetShpFileName]);
  end;

  FDBF := DBFCreateLL(
    PAnsiChar(UTF8String(GetDbfFileName)),
    CDbfCodePage,
    @VHooks
  );
  if FDBF = nil then begin
    raise EShpExportMarksError.CreateFmt('Failed to create DBF file: %s', [GetDbfFileName]);
  end;

  FFieldIdxName := DBFAddField(FDBF, CFieldNameName, ftString, CFieldWidthName, 0);
  FFieldIdxDesc := DBFAddField(FDBF, CFieldNameDesc, ftString, CFieldWidthDesc, 0);
  FFieldIdxCategory := DBFAddField(FDBF, CFieldNameCategory, ftString, CFieldWidthCategory, 0);
  if (FFieldIdxName < 0) or (FFieldIdxDesc < 0) or (FFieldIdxCategory < 0) then begin
    raise EShpExportMarksError.CreateFmt('Failed to add DBF fields to: %s', [GetDbfFileName]);
  end;
end;

procedure TShpFileWriter.EnsureCapacity(const ANeeded: Integer);
var
  VNewCapacity: Integer;
begin
  if ANeeded > Length(FXs) then begin
    if Length(FXs) = 0 then begin
      VNewCapacity := 1024;
    end else begin
      VNewCapacity := Length(FXs) * 2;
    end;
    while VNewCapacity < ANeeded do begin
      VNewCapacity := VNewCapacity * 2;
    end;
    SetLength(FXs, VNewCapacity);
    SetLength(FYs, VNewCapacity);
  end;
end;

procedure TShpFileWriter.ResetShape;
begin
  FVertCount := 0;
  FPartCount := 0;
end;

procedure TShpFileWriter.WriteSupportFile(
  const AFileName: string;
  const AContent: AnsiString
);
var
  VStream: TFileStream;
begin
  VStream := TFileStream.Create(AFileName, fmCreate);
  try
    if AContent <> '' then begin
      VStream.WriteBuffer(PAnsiChar(AContent)^, Length(AContent));
    end;
  finally
    VStream.Free;
  end;
end;

class function TShpFileWriter.PrepareDbfString(
  const AValue: string;
  const AMaxWidth: Integer
): UTF8String;
var
  N: Integer;
begin
  Result := UTF8String(AValue);
  if Length(Result) > AMaxWidth then begin
    // Cut by bytes (once, O(n)) and step back to a character boundary:
    // Result[N + 1] is the first dropped byte, a UTF-8 continuation byte
    // ($80..$BF) means the cut would split a character.
    N := AMaxWidth;
    while (N > 0) and ((Byte(Result[N + 1]) and $C0) = $80) do begin
      Dec(N);
    end;
    SetLength(Result, N);
  end;
end;

function TShpFileWriter.GetContinuationFieldIdx(
  const ABaseName: string;
  const AFieldWidth: Integer;
  var AContIdxs: TShpLongIntArray;
  const APartIdx: Integer
): LongInt;
var
  VFieldIdx: LongInt;
begin
  while Length(AContIdxs) < APartIdx do begin
    VFieldIdx :=
      DBFAddField(
        FDBF,
        PAnsiChar(AnsiString(ABaseName + IntToStr(Length(AContIdxs) + 1))),
        ftString,
        AFieldWidth,
        0
      );
    if VFieldIdx < 0 then begin
      raise EShpExportMarksError.CreateFmt('Failed to add DBF fields to: %s', [GetDbfFileName]);
    end;
    SetLength(AContIdxs, Length(AContIdxs) + 1);
    AContIdxs[High(AContIdxs)] := VFieldIdx;
  end;
  Result := AContIdxs[APartIdx - 1];
end;

procedure TShpFileWriter.WriteStringAttributeChecked(
  const AShapeIndex: Integer;
  const AFieldIdx: LongInt;
  const AValue: UTF8String
);
begin
  if DBFWriteStringAttribute(FDBF, AShapeIndex, AFieldIdx, PAnsiChar(AValue)) = 0 then begin
    raise EShpExportMarksError.CreateFmt('Failed to write DBF record %d to: %s', [AShapeIndex, GetDbfFileName]);
  end;
end;

procedure TShpFileWriter.WriteContinuedField(
  const AShapeIndex: Integer;
  const AFieldIdx: LongInt;
  const ABaseName: string;
  const AFieldWidth: Integer;
  var AContIdxs: TShpLongIntArray;
  const AValue: string
);

  function _PartLength(const AUtf8: UTF8String; const AStart: Integer): Integer;
  var
    N: Integer;
  begin
    N := Length(AUtf8) - AStart + 1;
    if N > AFieldWidth then begin
      N := AFieldWidth;
      // AUtf8[AStart + N] is the first dropped byte; a continuation byte
      // ($80..$BF) means the cut would split a character
      while (N > 0) and ((Byte(AUtf8[AStart + N]) and $C0) = $80) do begin
        Dec(N);
      end;
    end;
    Result := N;
  end;

var
  VUtf8: UTF8String;
  VStart: Integer;
  VPartLen: Integer;
  VPartIdx: Integer;
begin
  VUtf8 := UTF8String(AValue);
  VStart := 1;
  VPartIdx := 0;
  while VStart <= Length(VUtf8) do begin
    VPartLen := _PartLength(VUtf8, VStart);
    if VPartIdx = 0 then begin
      WriteStringAttributeChecked(AShapeIndex, AFieldIdx, Copy(VUtf8, VStart, VPartLen));
    end else begin
      WriteStringAttributeChecked(
        AShapeIndex,
        GetContinuationFieldIdx(ABaseName, AFieldWidth, AContIdxs, VPartIdx),
        Copy(VUtf8, VStart, VPartLen)
      );
    end;
    Inc(VStart, VPartLen);
    Inc(VPartIdx);
    if VPartIdx > CContFieldMaxCount then begin
      Break; // pathological value: the rest of the text is lost
    end;
  end;
end;

procedure TShpFileWriter.WriteDbfRow(
  const AShapeIndex: Integer;
  const AName, ADesc, ACategory: string
);
begin
  WriteStringAttributeChecked(AShapeIndex, FFieldIdxName, PrepareDbfString(AName, CFieldWidthName));
  WriteContinuedField(AShapeIndex, FFieldIdxDesc, CFieldNameDesc, CFieldWidthDesc, FDescFieldIdx, ADesc);
  WriteContinuedField(AShapeIndex, FFieldIdxCategory, CFieldNameCategory, CFieldWidthCategory, FCatFieldIdx, ACategory);
end;

procedure TShpFileWriter.AddPoint(
  const APoint: TDoublePoint;
  const AName, ADesc, ACategory: string
);
var
  X: Double;
  Y: Double;
  VObj: PShpObject;
  VShapeIndex: Integer;
begin
  EnsureOpen;

  X := APoint.X;
  Y := APoint.Y;

  VObj := SHPCreateSimpleObject(SHPT_POINT, 1, @X, @Y, nil);
  try
    SHPComputeExtents(VObj);
    VShapeIndex := SHPWriteObject(FSHP, -1, VObj);
  finally
    SHPDestroyObject(VObj);
  end;
  if VShapeIndex < 0 then begin
    raise EShpExportMarksError.CreateFmt('Failed to write shape to: %s', [GetShpFileName]);
  end;

  WriteDbfRow(VShapeIndex, AName, ADesc, ACategory);

  if FCount = 0 then begin
    WriteSupportFile(GetCpgFileName, CCpgContent);
    WriteSupportFile(GetPrjFileName, CPrjContent);
  end;
  Inc(FCount);
end;

function TShpFileWriter.AddPart(
  const AEnum: IEnumLonLatPoint;
  const AIsRing: Boolean
): Boolean;
var
  VPoint: TDoublePoint;
  VStartIndex: Integer;
  VPointCount: Integer;
  VMinPoints: Integer;
  VCapacity: Integer;
begin
  VStartIndex := FVertCount;
  while AEnum.Next(VPoint) do begin
    EnsureCapacity(FVertCount + 1);
    FXs[FVertCount] := VPoint.X;
    FYs[FVertCount] := VPoint.Y;
    Inc(FVertCount);
  end;
  VPointCount := FVertCount - VStartIndex;

  if AIsRing then begin
    // The shapefile spec requires a closed ring (first vertex = last vertex).
    // Close it here if the enumerator returned an open one.
    if (VPointCount > 0) and
      ((FXs[VStartIndex] <> FXs[FVertCount - 1]) or
       (FYs[VStartIndex] <> FYs[FVertCount - 1]))
    then begin
      EnsureCapacity(FVertCount + 1);
      FXs[FVertCount] := FXs[VStartIndex];
      FYs[FVertCount] := FYs[VStartIndex];
      Inc(FVertCount);
      Inc(VPointCount);
    end;
    VMinPoints := 4; // 3 distinct vertices + the closing one
  end else begin
    VMinPoints := 2;
  end;

  if VPointCount < VMinPoints then begin
    FVertCount := VStartIndex; // discard the invalid part
    Result := False;
    Exit;
  end;

  VCapacity := Length(FPartStart);
  if FPartCount >= VCapacity then begin
    if VCapacity = 0 then begin
      SetLength(FPartStart, 4);
    end else begin
      SetLength(FPartStart, VCapacity * 2);
    end;
  end;
  FPartStart[FPartCount] := VStartIndex;
  Inc(FPartCount);
  Result := True;
end;

procedure TShpFileWriter.EndShape(
  const AName, ADesc, ACategory: string
);
var
  VObj: PShpObject;
  VShapeIndex: Integer;
begin
  if (FPartCount = 0) or (FVertCount = 0) then begin
    ResetShape;
    Exit;
  end;
  EnsureOpen;

  VObj :=
    SHPCreateObject(
      CShpKindShapeType[FKind], -1,
      FPartCount, PLongInt(FPartStart), nil, // part types are for MULTIPATCH only
      FVertCount, PDouble(FXs), PDouble(FYs), nil, nil
    );
  try
    SHPComputeExtents(VObj);
    if FKind = skPoly then begin
      // reorder the rings: outer rings and holes per the ESRI convention
      SHPRewindObject(FSHP, VObj);
    end;
    VShapeIndex := SHPWriteObject(FSHP, -1, VObj);
  finally
    SHPDestroyObject(VObj);
  end;
  if VShapeIndex < 0 then begin
    raise EShpExportMarksError.CreateFmt('Failed to write shape to: %s', [GetShpFileName]);
  end;

  WriteDbfRow(VShapeIndex, AName, ADesc, ACategory);
  ResetShape;

  if FCount = 0 then begin
    WriteSupportFile(GetCpgFileName, CCpgContent);
    WriteSupportFile(GetPrjFileName, CPrjContent);
  end;
  Inc(FCount);
end;

procedure TShpFileWriter.Finish;
begin
  if (FSHP = nil) and (FDBF = nil) then begin
    Exit; // no shapes were written - no files were created
  end;

  if FDBF <> nil then begin
    DBFClose(FDBF);
    FDBF := nil;
  end;
  if FSHP <> nil then begin
    SHPClose(FSHP);
    FSHP := nil;
  end;

  if FCount = 0 then begin
    DeleteFileSet(FBaseName, FKind); // no shapes were written - no files
  end;
end;

{ TExportMarks2SHP }

procedure TExportMarks2SHP.CheckCanceled;
begin
  if (FCancelNotifier <> nil) and FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    raise EShpExportMarksError.Create('Operation canceled by user');
  end;
end;

procedure TExportMarks2SHP.ExportTreeToSHP(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATree: IVectorItemTree;
  const AFileName: string
);
var
  VKind: TShpKind;
  VWriters: TWriters;
  VBaseName: string;
  VFailed: Boolean;
begin
  LoadLibShp(GDllName.Shp, True);

  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  if not ForceDirectories(AFileName) then begin
    RaiseLastOsError;
  end;

  VBaseName :=
    IncludeTrailingPathDelimiter(AFileName) +
    ExtractFileName(ExcludeTrailingPathDelimiter(AFileName));

  for VKind := Low(TShpKind) to High(TShpKind) do begin
    TShpFileWriter.DeleteFileSet(VBaseName, VKind);
  end;

  VFailed := False;
  for VKind := Low(TShpKind) to High(TShpKind) do begin
    VWriters[VKind] := TShpFileWriter.Create(VBaseName, VKind);
  end;
  try
    try
      AddTree(ATree, VWriters);
    except
      VFailed := True;
      raise;
    end;
  finally
    for VKind := Low(TShpKind) to High(TShpKind) do begin
      VWriters[VKind].Free;
    end;
    if VFailed then begin
      for VKind := Low(TShpKind) to High(TShpKind) do begin
        TShpFileWriter.DeleteFileSet(VBaseName, VKind);
      end;
    end;
  end;
end;

procedure TExportMarks2SHP.AddTree(
  const ATree: IVectorItemTree;
  const AWriters: TWriters
);
var
  I: Integer;
begin
  if ATree = nil then begin
    Exit;
  end;

  AddMarks(ATree.Items, AWriters);

  for I := 0 to ATree.SubTreeItemCount - 1 do begin
    CheckCanceled;
    AddTree(ATree.GetSubTreeItem(I), AWriters); // recursion
  end;
end;

procedure TExportMarks2SHP.AddMarks(
  const AMarksSubset: IVectorItemSubset;
  const AWriters: TWriters
);
var
  I: Integer;
  VMark: IVectorDataItem;
begin
  if AMarksSubset = nil then begin
    Exit;
  end;

  for I := 0 to AMarksSubset.Count - 1 do begin
    CheckCanceled;
    VMark := AMarksSubset.Items[I];
    if VMark <> nil then begin
      AddMark(VMark, AWriters);
    end;
  end;
end;

function TExportMarks2SHP.GetCategoryName(const AMark: IVectorDataItem): string;
var
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  Result := '';
  if Supports(AMark.MainInfo, IVectorDataItemWithCategory, VMarkWithCategory) and
    (VMarkWithCategory.Category <> nil)
  then begin
    Result := VMarkWithCategory.Category.Name;
  end;
end;

procedure TExportMarks2SHP.AddPolygonToWriter(
  const APolygon: IGeometryLonLatSinglePolygon;
  const AWriter: TShpFileWriter
);
var
  I: Integer;
begin
  // holes without an outer ring would corrupt the shape, so a polygon whose
  // outer border is degenerate is skipped as a whole
  if not AWriter.AddPart(APolygon.OuterBorder.GetEnum, True) then begin
    Exit;
  end;
  for I := 0 to APolygon.HoleCount - 1 do begin
    AWriter.AddPart(APolygon.HoleBorder[I].GetEnum, True); // a bad hole is just dropped
  end;
end;

procedure TExportMarks2SHP.AddMark(
  const AMark: IVectorDataItem;
  const AWriters: TWriters
);
var
  I: Integer;
  VName: string;
  VDesc: string;
  VCategory: string;
  VPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
  VPointGeom: IGeometryLonLatPoint;
  VMultiPointGeom: IGeometryLonLatMultiPoint;
  VLineGeom: IGeometryLonLatLine;
  VSingleLineGeom: IGeometryLonLatSingleLine;
  VMultiLineGeom: IGeometryLonLatMultiLine;
  VPolygonGeom: IGeometryLonLatPolygon;
  VSinglePolygonGeom: IGeometryLonLatSinglePolygon;
  VMultiPolygonGeom: IGeometryLonLatMultiPolygon;
begin
  VName := AMark.Name;
  VDesc := AMark.Desc;
  VCategory := GetCategoryName(AMark);

  if Supports(AMark.Geometry, IGeometryLonLatPoint, VPointGeom) then begin
    AWriters[skPoint].AddPoint(VPointGeom.Point, VName, VDesc, VCategory);
  end else if Supports(AMark.Geometry, IGeometryLonLatMultiPoint, VMultiPointGeom) then begin
    // a shapefile point layer holds single points only,
    // so each vertex becomes a separate shape with the same attributes
    VEnum := VMultiPointGeom.GetEnum;
    while VEnum.Next(VPoint) do begin
      AWriters[skPoint].AddPoint(VPoint, VName, VDesc, VCategory);
    end;
  end else if Supports(AMark.Geometry, IGeometryLonLatLine, VLineGeom) then begin
    if Supports(VLineGeom, IGeometryLonLatSingleLine, VSingleLineGeom) then begin
      AWriters[skLine].AddPart(VSingleLineGeom.GetEnum, False);
      AWriters[skLine].EndShape(VName, VDesc, VCategory);
    end else if Supports(VLineGeom, IGeometryLonLatMultiLine, VMultiLineGeom) then begin
      for I := 0 to VMultiLineGeom.Count - 1 do begin
        AWriters[skLine].AddPart(VMultiLineGeom.Item[I].GetEnum, False);
      end;
      AWriters[skLine].EndShape(VName, VDesc, VCategory);
    end;
  end else if Supports(AMark.Geometry, IGeometryLonLatPolygon, VPolygonGeom) then begin
    if Supports(VPolygonGeom, IGeometryLonLatSinglePolygon, VSinglePolygonGeom) then begin
      AddPolygonToWriter(VSinglePolygonGeom, AWriters[skPoly]);
      AWriters[skPoly].EndShape(VName, VDesc, VCategory);
    end else if Supports(VPolygonGeom, IGeometryLonLatMultiPolygon, VMultiPolygonGeom) then begin
      for I := 0 to VMultiPolygonGeom.Count - 1 do begin
        AddPolygonToWriter(VMultiPolygonGeom.Item[I], AWriters[skPoly]);
      end;
      AWriters[skPoly].EndShape(VName, VDesc, VCategory);
    end;
  end;
end;

end.
