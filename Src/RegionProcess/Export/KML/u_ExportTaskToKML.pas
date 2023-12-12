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
  i_RegionProcessProgressInfo,
  i_TileIteratorFactory,
  i_GeometryLonLat,
  i_MapVersionInfo,
  i_TileStorage,
  u_ExportTaskAbstract;

type
  TExportTaskToKML = class(TExportTaskAbstract)
  private
    FTileStorage: ITileStorage;
    FVersion: IMapVersionInfo;
    FNotSaveNotExists: Boolean;
    FPathExport: string;
    FRelativePath: Boolean;
    FTilesToProcess: Int64;
    FTilesProcessed: Int64;
    procedure KmlFileWrite(
      const AStream: TStream;
      const ATile: TPoint;
      const AZoom: Byte;
      const ALevel: Byte
    );
    procedure WriteTextToStream(
      const AText: string;
      const AStream: TStream
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APath: string;
      const ATileIteratorFactory: ITileIteratorFactory;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const AVersion: IMapVersionInfo;
      const ANotSaveNotExists: Boolean;
      const ARelativePath: Boolean
    );
  end;

implementation

uses
  Math,
  StrUtils,
  i_Projection,
  i_TileInfoBasic,
  i_TileIterator,
  u_TileIteratorByRect,
  u_GeoToStrFunc,
  u_GeoFunc,
  u_ResStrings;

constructor TExportTaskToKML.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APath: string;
  const ATileIteratorFactory: ITileIteratorFactory;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const AVersion: IMapVersionInfo;
  const ANotSaveNotExists: Boolean;
  const ARelativePath: Boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    ATileIteratorFactory
  );
  FPathExport := APath;
  FNotSaveNotExists := ANotSaveNotExists;
  FRelativePath := ARelativePath;
  FTileStorage := ATileStorage;
  FVersion := AVersion;
end;

procedure TExportTaskToKML.KmlFileWrite(
  const AStream: TStream;
  const ATile: TPoint;
  const AZoom, ALevel: Byte
);
var
  VZoom: Byte;
  VIterator: TTileIteratorByRectRecord;
  VSavePath, VNorth, VSouth, VEast, VWest: string;
  VText: string;
  VTileRect: TRect;
  VTile: TPoint;
  VLonLatRect: TDoubleRect;
  VTileInfo: ITileInfoBasic;
begin
  //TODO: Нужно думать на случай когда тайлы будут в базе данных
  if FNotSaveNotExists then begin
    VTileInfo := FTileStorage.GetTileInfo(ATile, AZoom, FVersion, gtimAsIs);
    if not Assigned(VTileInfo) or not VTileInfo.GetIsExists then begin
      Exit;
    end;
  end;

  VSavePath := FTileStorage.GetTileFileName(ATile, AZoom, FVersion);
  if FRelativePath then begin
    VSavePath := ExtractRelativePath(ExtractFilePath(FPathExport), VSavePath);
  end;

  VLonLatRect := FTileStorage.ProjectionSet.Zooms[AZoom].TilePos2LonLatRect(ATile);
  VNorth := R2StrPoint(VLonLatRect.Top);
  VSouth := R2StrPoint(VLonLatRect.Bottom);
  VEast := R2StrPoint(VLonLatRect.Right);
  VWest := R2StrPoint(VLonLatRect.Left);

  VText :=
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
    '  </GroundOverlay>';

  WriteTextToStream(VText, AStream);

  Inc(FTilesProcessed);
  if FTilesProcessed mod 100 = 0 then begin
    ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);
  end;

  if ALevel < Length(FZooms) then begin
    VZoom := FZooms[ALevel];
    VTileRect :=
      RectFromDoubleRect(
        FTileStorage.ProjectionSet.Zooms[VZoom].RelativeRect2TileRectFloat(FTileStorage.ProjectionSet.Zooms[AZoom].TilePos2RelativeRect(ATile)),
        rrClosest
      );
    VIterator.Init(VTileRect);
    while VIterator.Next(VTile) do begin
      KmlFileWrite(AStream, VTile, VZoom, ALevel + 1);
    end;
  end;

  WriteTextToStream(#13#10 + '</Folder>', AStream);
end;

procedure TExportTaskToKML.ProcessRegion;
var
  I: Integer;
  VZoom: Byte;
  VText: string;
  VProjection: IProjection;
  VTempIterator: ITileIterator;
  VIterator: ITileIterator;
  VTile: TPoint;
  VKmlStream: TFileStream;
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
    VKmlStream := TFileStream.Create(FPathExport, fmCreate);
    try
      VText :=
        '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 +
        '<kml xmlns="http://earth.google.com/kml/2.1">' + #13#10 +
        '<Document>' + #13#10 +
        '<name>' + ExtractFileName(FPathExport) + '</name>' + #13#10;

      WriteTextToStream(VText, VKmlStream);

      VZoom := FZooms[0];
      while VIterator.Next(VTile) do begin
        if not CancelNotifier.IsOperationCanceled(OperationID) then begin
          KmlFileWrite(VKmlStream, VTile, VZoom, 1);
        end;
      end;

      VText := #13#10 + '</Document>' + #13#10 + '</kml>' + #13#10;
      WriteTextToStream(VText, VKmlStream);
    finally
      VKmlStream.Free;
    end;
  finally
    ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);
  end;
end;

procedure TExportTaskToKML.WriteTextToStream(
  const AText: string;
  const AStream: TStream
);
var
  VUtf8Text: UTF8String;
begin
  if AText <> '' then begin
    VUtf8Text := UTF8Encode(AText);
    AStream.WriteBuffer(VUtf8Text[1], Length(VUtf8Text));
  end;
end;

end.
