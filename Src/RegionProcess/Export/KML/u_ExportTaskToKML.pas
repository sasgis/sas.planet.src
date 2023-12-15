{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_ExportTaskToKML;

interface

uses
  Types,
  SysUtils,
  Classes,
  t_GeoTypes,
  i_NotifierOperation,
  i_ContentTypeInfo,
  i_RegionProcessProgressInfo,
  i_TileIteratorFactory,
  i_GeometryLonLat,
  i_MapVersionInfo,
  i_BitmapTileProvider,
  i_BitmapTileSaveLoad,
  i_TileStorage,
  i_TileFileNameGenerator,
  u_ExportTaskAbstract;

type
  TExportTaskToKML = class(TExportTaskAbstract)
  private
    FTileStorage: ITileStorage;
    FVersion: IMapVersionInfo;
    FSaveExistsOnly: Boolean;
    FKmlFileName: string;
    FTilesPath: string;
    FTryUseRelativePath: Boolean;
    FExtractTilesFromStorage: Boolean;
    FTileFileNameGenerator: ITileFileNameGenerator;
    FBitmapTileSaver: IBitmapTileSaver;
    FBitmapTileProviderArr: TBitmapTileProviderDynArray;
    FContentTypeInfo: IContentTypeInfoBasic;
    FProviderIndexByZoom: array[0..23] of Integer;
    FTilesToProcess: Int64;
    FTilesProcessed: Int64;
    FKmlStream: TFileStream;
    FUseTileProviderAndSaver: Boolean;
    procedure KmlFileWrite(
      const ATile: TPoint;
      const AZoom: Byte;
      const ALevel: Byte
    );
    procedure WriteTextToKmlStream(
      const AText: string
    );
    function CopyTileToFileSystem(
      const ATile: TPoint;
      const AZoom: Byte
    ): string;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AOutputFileName: string;
      const ATileIteratorFactory: ITileIteratorFactory;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const AVersion: IMapVersionInfo;
      const ANotSaveNotExists: Boolean;
      const ARelativePath: Boolean;
      const AExtractTilesFromStorage: Boolean;
      const ATileFileNameGenerator: ITileFileNameGenerator;
      const ABitmapTileSaver: IBitmapTileSaver;
      const AContentTypeInfo: IContentTypeInfoBasic;
      const ABitmapTileProviderArr: TBitmapTileProviderDynArray
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  StrUtils,
  i_BinaryData,
  i_Bitmap32Static,
  i_Projection,
  i_TileInfoBasic,
  i_TileIterator,
  u_TileIteratorByRect,
  u_GeoToStrFunc,
  u_GeoFunc,
  u_ResStrings;

constructor TExportTaskToKML.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AOutputFileName: string;
  const ATileIteratorFactory: ITileIteratorFactory;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const AVersion: IMapVersionInfo;
  const ANotSaveNotExists: Boolean;
  const ARelativePath: Boolean;
  const AExtractTilesFromStorage: Boolean;
  const ATileFileNameGenerator: ITileFileNameGenerator;
  const ABitmapTileSaver: IBitmapTileSaver;
  const AContentTypeInfo: IContentTypeInfoBasic;
  const ABitmapTileProviderArr: TBitmapTileProviderDynArray
);
var
  I, J: Integer;
begin
  if AExtractTilesFromStorage then begin
    Assert(ATileFileNameGenerator <> nil);
  end;

  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    ATileIteratorFactory
  );

  FKmlFileName := AOutputFileName;
  FTilesPath := ExtractFilePath(FKmlFileName) + 'files' + PathDelim;
  FTryUseRelativePath := ARelativePath;
  FTileFileNameGenerator := ATileFileNameGenerator;
  FTileStorage := ATileStorage;
  FExtractTilesFromStorage := AExtractTilesFromStorage;
  FSaveExistsOnly := ANotSaveNotExists;
  FVersion := AVersion;
  FBitmapTileSaver := ABitmapTileSaver;
  FContentTypeInfo := AContentTypeInfo;
  FBitmapTileProviderArr := ABitmapTileProviderArr;

  FUseTileProviderAndSaver :=
    FExtractTilesFromStorage and
    (FBitmapTileSaver <> nil) and
    (FContentTypeInfo <> nil) and
    (Length(FBitmapTileProviderArr) = Length(AZoomArr));

  if FUseTileProviderAndSaver then begin
    for I := 0 to Length(FProviderIndexByZoom) - 1 do begin
      FProviderIndexByZoom[I] := -1;
      for J := 0 to Length(AZoomArr) - 1 do begin
        if I = AZoomArr[J] then begin
          FProviderIndexByZoom[I] := J;
          Break;
        end;
      end;
    end;
  end;
end;

destructor TExportTaskToKML.Destroy;
begin
  FreeAndNil(FKmlStream);
  inherited Destroy;
end;

procedure TExportTaskToKML.KmlFileWrite(
  const ATile: TPoint;
  const AZoom, ALevel: Byte
);
var
  VZoom: Byte;
  VIterator: TTileIteratorByRectRecord;
  VSavePath, VNorth, VSouth, VEast, VWest: string;
  VTileRect: TRect;
  VTile: TPoint;
  VLonLatRect: TDoubleRect;
  VTileInfo: ITileInfoBasic;
begin
  Inc(FTilesProcessed);
  if FTilesProcessed mod 100 = 0 then begin
    ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);
  end;

  if FSaveExistsOnly then begin
    VTileInfo := FTileStorage.GetTileInfo(ATile, AZoom, FVersion, gtimAsIs);
    if not Assigned(VTileInfo) or not VTileInfo.GetIsExists then begin
      Exit;
    end;
  end;

  if FExtractTilesFromStorage then begin
    VSavePath := CopyTileToFileSystem(ATile, AZoom);
    if VSavePath = '' then begin
      Exit;
    end;
  end else begin
    VSavePath := FTileStorage.GetTileFileName(ATile, AZoom, FVersion);
  end;

  if FTryUseRelativePath then begin
    VSavePath := ExtractRelativePath(ExtractFilePath(FKmlFileName), VSavePath);
  end;

  VLonLatRect := FTileStorage.ProjectionSet.Zooms[AZoom].TilePos2LonLatRect(ATile);
  VNorth := R2StrPoint(VLonLatRect.Top);
  VSouth := R2StrPoint(VLonLatRect.Bottom);
  VEast := R2StrPoint(VLonLatRect.Right);
  VWest := R2StrPoint(VLonLatRect.Left);

  WriteTextToKmlStream(
    #13#10 +
    '<Folder>' + #13#10 +
    '  <Region>' + #13#10 +
    '    <LatLonAltBox>' + #13#10 +
    '      <north>' + VNorth + '</north>' + #13#10 +
    '      <south>' + VSouth + '</south>' + #13#10 +
    '      <east>' + VEast + '</east>' + #13#10 +
    '      <west>' + VWest + '</west>' + #13#10 +
    '    </LatLonAltBox>' + #13#10 +
    '    <Lod>' + #13#10 +
    '      <minLodPixels>' + IfThen(ALevel > 1, '128', '16') +'</minLodPixels>' + #13#10 +
    '      <maxLodPixels>-1</maxLodPixels>' + #13#10 +
    '    </Lod>' + #13#10 +
    '  </Region>' + #13#10 +
    '  <GroundOverlay>' + #13#10 +
    '    <drawOrder>' + IntToStr(ALevel) + '</drawOrder>' + #13#10 +
    '    <Icon>' + #13#10 +
    '      <href>' + VSavePath + '</href>' + #13#10 +
    '    </Icon>' + #13#10 +
    '    <LatLonBox>' + #13#10 +
    '      <north>' + VNorth + '</north>' + #13#10 +
    '      <south>' + VSouth + '</south>' + #13#10 +
    '      <east>' + VEast + '</east>' + #13#10 +
    '      <west>' + VWest + '</west>' + #13#10 +
    '    </LatLonBox>' + #13#10 +
    '  </GroundOverlay>'
  );

  if ALevel < Length(FZooms) then begin
    VZoom := FZooms[ALevel];
    VTileRect :=
      RectFromDoubleRect(
        FTileStorage.ProjectionSet.Zooms[VZoom].RelativeRect2TileRectFloat(
          FTileStorage.ProjectionSet.Zooms[AZoom].TilePos2RelativeRect(ATile)
        ),
        rrClosest
      );
    VIterator.Init(VTileRect);
    while VIterator.Next(VTile) do begin
      KmlFileWrite(VTile, VZoom, ALevel + 1);
    end;
  end;

  WriteTextToKmlStream(#13#10 + '</Folder>');
end;

procedure TExportTaskToKML.ProcessRegion;
var
  I: Integer;
  VZoom: Byte;
  VProjection: IProjection;
  VTempIterator: ITileIterator;
  VIterator: ITileIterator;
  VTile: TPoint;
begin
  inherited;

  FTilesProcessed := 0;
  FTilesToProcess := 0;

  if Length(FZooms) > 0 then begin
    VZoom := FZooms[0];
    VProjection := FTileStorage.ProjectionSet.Zooms[VZoom];
    VIterator := Self.MakeTileIterator(VProjection);
    FTilesToProcess := FTilesToProcess + VIterator.TilesTotal;
    for I := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[I];
      VProjection := FTileStorage.ProjectionSet.Zooms[VZoom];
      VTempIterator := Self.MakeTileIterator(VProjection);
      FTilesToProcess := FTilesToProcess + VTempIterator.TilesTotal;
    end;
  end;

  ProgressInfo.SetCaption(SAS_STR_ExportTiles);
  ProgressInfo.SetFirstLine(
    SAS_STR_AllSaves + ' ' + IntToStr(FTilesToProcess) + ' ' + SAS_STR_Files
  );
  ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);

  try
    WriteTextToKmlStream(
      '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 +
      '<kml xmlns="http://earth.google.com/kml/2.1">' + #13#10 +
      '<Document>' + #13#10 +
      '<name>' + ExtractFileName(FKmlFileName) + '</name>'
    );

    VZoom := FZooms[0];
    while VIterator.Next(VTile) do begin
      if not CancelNotifier.IsOperationCanceled(OperationID) then begin
        KmlFileWrite(VTile, VZoom, 1);
      end;
    end;

    WriteTextToKmlStream(#13#10 + '</Document>' + #13#10 + '</kml>' + #13#10);
  finally
    ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);
  end;
end;

procedure TExportTaskToKML.WriteTextToKmlStream(const AText: string);
var
  VUtf8Text: UTF8String;
begin
  if AText = '' then begin
    Exit;
  end;
  if FKmlStream = nil then begin
    FKmlStream := TFileStream.Create(FKmlFileName, fmCreate);
  end;
  VUtf8Text := UTF8Encode(AText);
  FKmlStream.WriteBuffer(VUtf8Text[1], Length(VUtf8Text));
end;

function TExportTaskToKML.CopyTileToFileSystem(
  const ATile: TPoint;
  const AZoom: Byte
): string;

  function _GetTileFileName(const AContentTypeInfo: IContentTypeInfoBasic): string;
  begin
    Result := FTilesPath +
      ChangeFileExt(
        FTileFileNameGenerator.GetTileFileName(ATile, AZoom),
        AContentTypeInfo.GetDefaultExt
      );
  end;

  function _GetTile(out AFileName: string; out AData: IBinaryData): Boolean;
  var
    I: Integer;
    VBitmap: IBitmap32Static;
    VTileInfo: ITileInfoBasic;
    VTileInfoWithData: ITileInfoWithData;
  begin
    if FUseTileProviderAndSaver then begin
      I := FProviderIndexByZoom[AZoom];
      Assert((I >= 0) and (I < Length(FBitmapTileProviderArr)));
      VBitmap := FBitmapTileProviderArr[I].GetTile(Self.OperationID, Self.CancelNotifier, ATile);
      AData := FBitmapTileSaver.Save(VBitmap);
      AFileName := _GetTileFileName(FContentTypeInfo);
    end else begin
      VTileInfo := FTileStorage.GetTileInfo(ATile, AZoom, FVersion, gtimWithData);
      if Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
        AData := VTileInfoWithData.TileData;
        AFileName := _GetTileFileName(VTileInfo.ContentType);
      end;
    end;
    Result := AData <> nil;
  end;

var
  VData: IBinaryData;
begin
  if not _GetTile(Result, VData) then begin
    Result := '';
    Exit;
  end;

  if not ForceDirectories(ExtractFileDir(Result)) then begin
    RaiseLastOSError;
  end;

  with TFileStream.Create(Result, fmCreate) do
  try
    WriteBuffer(VData.Buffer^, VData.Size);
  finally
    Free;
  end;
end;

end.
