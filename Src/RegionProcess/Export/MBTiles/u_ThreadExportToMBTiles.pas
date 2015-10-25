{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_ThreadExportToMBTiles;

interface

uses
  Types,
  Windows,
  SysUtils,
  Classes,
  SQLite3Handler,
  t_GeoTypes,
  i_BinaryData,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_ProjectionSetFactory,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_TileInfoBasic,
  i_TileStorage,
  i_TileIterator,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  u_ThreadExportAbstract;

type
  TThreadExportToMBTiles = class(TThreadExportAbstract)
  private
    FFormatSettings: TFormatSettings;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FProjectionSetFactory: IProjectionSetFactory;
    FExportPath: string;
    FExportFileName: string;
    FTileStorage: ITileStorage;
    FMapVersion: IMapVersionRequest;
    FBitmapTileSaver: IBitmapTileSaver;
    FBitmapProvider: IBitmapTileUniProvider;
    FDirectTilesCopy: Boolean;
    FSQLite3DB: TSQLite3DbHandler;
    FSQLiteAvailable: Boolean;
    FUseXYZScheme: Boolean;
    FName: string;
    FDescription: string;
    FAttribution: string;
    FImgType: string;
    FImgFormat: string;
    FBasePoint: TPoint;
  private
    procedure OpenSQLiteStorage(const ATileIterator: ITileIterator);
    procedure CloseSQLiteStorage;
    procedure SaveTileToSQLiteStorage(
      const ATile: TPoint;
      const AZoom: Byte;
      const AData: IBinaryData
    );
    function GetBounds(const ATileIterator: ITileIterator): string;
    function GetLonLatRect(const ATileIterator: ITileIterator): TDoubleRect;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AExportPath: string;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectionSetFactory: IProjectionSetFactory;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const AMapVersion: IMapVersionRequest;
      const ABitmapTileSaver: IBitmapTileSaver;
      const ABitmapProvider: IBitmapTileUniProvider;
      const ADirectTilesCopy: Boolean;
      const AUseXYZScheme: Boolean;
      const AName: string;
      const ADescription: string;
      const AAttribution: string;
      const AIsLayer: Boolean;
      const AImgFormat: string
    );
  end;

implementation

uses
  ALString,
  ALSqlite3Wrapper,
  c_CoordConverter,
  i_GeometryProjected,
  i_ProjectionSet,
  i_Projection,
  i_Bitmap32Static,
  i_TileRect,
  u_TileIteratorByPolygon,
  u_GeoFunc,
  u_ResStrings;

const
  // metadata
  TABLE_METADATA_DDL = 'CREATE TABLE IF NOT EXISTS metadata (name text, value text)';
  INDEX_METADATA_DDL = 'CREATE UNIQUE INDEX IF NOT EXISTS metadata_idx  ON metadata (name)';
  INSERT_METADATA_SQL = 'INSERT INTO metadata (name, value) VALUES (%s,%s)';

  // tiles
  TABLE_TILES_DDL = 'CREATE TABLE IF NOT EXISTS tiles (zoom_level integer, tile_column integer, tile_row integer, tile_data blob)';
  INDEX_TILES_DDL = 'CREATE INDEX IF NOT EXISTS tiles_idx on tiles (zoom_level, tile_column, tile_row)';
  INSERT_TILES_SQL = 'INSERT OR REPLACE INTO tiles (zoom_level, tile_column, tile_row, tile_data) VALUES (%d,%d,%d,?)';

{ TThreadExportToMBTiles }

constructor TThreadExportToMBTiles.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AExportPath: string;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectionSetFactory: IProjectionSetFactory;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const AMapVersion: IMapVersionRequest;
  const ABitmapTileSaver: IBitmapTileSaver;
  const ABitmapProvider: IBitmapTileUniProvider;
  const ADirectTilesCopy: Boolean;
  const AUseXYZScheme: Boolean;
  const AName: string;
  const ADescription: string;
  const AAttribution: string;
  const AIsLayer: Boolean;
  const AImgFormat: string
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    Self.ClassName
  );
  FFormatSettings.DecimalSeparator := '.';
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FProjectionSetFactory := AProjectionSetFactory;
  FExportPath := ExtractFilePath(AExportPath);
  FExportFileName := ExtractFileName(AExportPath);
  FTileStorage := ATileStorage;
  FMapVersion := AMapVersion;
  FBitmapTileSaver := ABitmapTileSaver;
  FBitmapProvider := ABitmapProvider;
  FDirectTilesCopy := ADirectTilesCopy;
  FUseXYZScheme := AUseXYZScheme;
  FName := AName;
  FDescription := ADescription;
  FAttribution := AAttribution;
  if AIsLayer then begin
    FImgType := 'overlay';
  end else begin
    FImgType := 'baselayer';
  end;
  FImgFormat := AImgFormat;
  FSQLiteAvailable := FSQLite3DB.Init;
end;

procedure TThreadExportToMBTiles.ProcessRegion;
var
  I: Integer;
  VZoom: Byte;
  VTile: TPoint;
  VDoDirectCopy: Boolean;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VProjectionSet: IProjectionSet;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VProjectedPolygons: array of IGeometryProjectedPolygon;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VTileInfo: ITileInfoWithData;
  VBitmapTile: IBitmap32Static;
  VTileData: IBinaryData;
  VProjection: IProjection;
begin
  inherited;

  VDoDirectCopy := FDirectTilesCopy and Assigned(FTileStorage);

  if not VDoDirectCopy then begin
    Assert(FBitmapProvider <> nil);
    Assert(FBitmapTileSaver <> nil);
  end;

  if not DirectoryExists(FExportPath) then begin
    if not ForceDirectories(FExportPath) then begin
      RaiseLastOSError;
    end;
  end;

  SetLength(VTileIterators, Length(FZooms));
  SetLength(VProjectedPolygons, Length(FZooms));

  VTilesToProcess := 0;

  if VDoDirectCopy then begin
    VProjectionSet := FTileStorage.ProjectionSet;
  end else begin
    VProjectionSet := FProjectionSetFactory.GetProjectionSetByCode(
      CGoogleProjectionEPSG,
      CTileSplitQuadrate256x256
    );
  end;

  for I := 0 to Length(FZooms) - 1 do begin
    VProjection := VProjectionSet.Zooms[FZooms[I]];
    VProjectedPolygons[I] :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        PolygLL
      );
    VTileIterators[I] :=
      TTileIteratorByPolygon.Create(
        VProjection,
        VProjectedPolygons[I]
      );
    VTilesToProcess := VTilesToProcess + VTileIterators[I].TilesTotal;
  end;

  OpenSQLiteStorage(VTileIterators[0]);
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

    for I := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[I];
      VTileIterator := VTileIterators[I];
      VProjectedPolygon := VProjectedPolygons[I];
      if Assigned(VTileIterator) then begin
        VProjection := VTileIterator.TilesRect.Projection;
        FBasePoint := VTileIterator.TilesRect.TopLeft;
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            Exit;
          end;

          if VDoDirectCopy then begin
            if Supports(FTileStorage.GetTileInfoEx(VTile, VZoom, FMapVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
              SaveTileToSQLiteStorage(VTile, VZoom, VTileInfo.TileData);
            end;
          end else begin
            VBitmapTile :=
              FBitmapProvider.GetTile(
                Self.OperationID,
                Self.CancelNotifier,
                VProjection,
                VTile
              );
            if Assigned(VBitmapTile) then begin
              VTileData := FBitmapTileSaver.Save(VBitmapTile);
              SaveTileToSQLiteStorage(VTile, VZoom, VTileData);
            end;
          end;

          Inc(VTilesProcessed);
          if VTilesProcessed mod 100 = 0 then begin
            ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
          end;
        end;
      end;
    end;
  finally
    CloseSQLiteStorage;
    for I := 0 to Length(FZooms) - 1 do begin
      VTileIterators[I] := nil;
      VProjectedPolygons[I] := nil;
    end;
    VTileIterators := nil;
    VProjectedPolygons := nil;
  end;
end;

procedure TThreadExportToMBTiles.OpenSQLiteStorage(const ATileIterator: ITileIterator);

  procedure _DoInsert(const AName, AValue: string);
  begin
    FSQLite3DB.ExecSQL(
      ALFormat(
        INSERT_METADATA_SQL,
        [ '''' + UTF8Encode(AName) + '''', '''' + UTF8Encode(AValue) + '''']
      )
    );
  end;

  procedure _WriteMetadata;
  var
    VRectCenter: TDoublePoint;
    VScheme, VCenter, VMinZoom, VMaxZoom: string;
  begin
    FSQLite3DB.BeginTran;
    try
      if FName <> '' then begin
        _DoInsert('name', FName);
      end else begin
        _DoInsert('name', 'Unnamed map');
      end;

      _DoInsert('type', FImgType);
      _DoInsert('version', '1.2');

      if FDescription <> '' then begin
        _DoInsert('description', FDescription);
      end else begin
        _DoInsert('description', 'Created by SAS.Planet');
      end;

      _DoInsert('format', FImgFormat);
      _DoInsert('bounds', GetBounds(ATileIterator));

      if FAttribution <> '' then begin
        _DoInsert('attribution', FAttribution);
      end;

      // insert additional fiels from TileJSON standart
      // https://github.com/mapbox/tilejson-spec/tree/master/2.1.0

      if FUseXYZScheme then begin
        VScheme := 'xyz';
      end else begin
        VScheme := 'tms';
      end;
      _DoInsert('scheme', VScheme);

      VMinZoom := IntToStr(FZooms[Low(FZooms)]);
      VMaxZoom := IntToStr(FZooms[High(FZooms)]);
      _DoInsert('minzoom', VMinZoom);
      _DoInsert('maxzoom', VMaxZoom);

      VRectCenter := RectCenter(GetLonLatRect(ATileIterator));
      VCenter := Format('%.8f, %.8f, %s', [VRectCenter.X, VRectCenter.Y, VMinZoom], FFormatSettings);
      _DoInsert('center', VCenter);

      FSQLite3DB.Commit;
    except
      FSQLite3DB.Rollback;
      raise;
    end;
  end;

var
  VFileName: string;
begin
  if not FSQLiteAvailable then begin
    raise ESQLite3SimpleError.Create('SQLite not available');
  end;

  CloseSQLiteStorage;

  VFileName := FExportPath + FExportFileName;

  if FileExists(VFileName) then begin
    if not DeleteFile(VFileName) then begin
      raise ESQLite3SimpleError.CreateFmt('Can''t delete database: %s', [VFileName]);
    end;
  end;

  FSQLite3Db.OpenW(VFileName);

  FSQLite3DB.SetExclusiveLockingMode;
  FSQLite3DB.ExecSQL('PRAGMA synchronous=OFF');

  FSQLite3DB.ExecSQL(TABLE_TILES_DDL);
  FSQLite3DB.ExecSQL(INDEX_TILES_DDL);

  FSQLite3DB.ExecSQL(TABLE_METADATA_DDL);
  FSQLite3DB.ExecSQL(INDEX_METADATA_DDL);

  _WriteMetadata;

  FSQLite3DB.BeginTran;
end;

procedure TThreadExportToMBTiles.CloseSQLiteStorage;
begin
  if FSQLite3DB.Opened then begin
    FSQLite3DB.Commit;
    FSQLite3DB.Close;
  end;
end;

procedure TThreadExportToMBTiles.SaveTileToSQLiteStorage(
  const ATile: TPoint;
  const AZoom: Byte;
  const AData: IBinaryData
);
var
  X, Y: Integer;
begin
  Assert(AData <> nil);

  X := ATile.X;

  if FUseXYZScheme then begin
    Y := ATile.Y;
  end else begin
    Y := (1 shl AZoom) - ATile.Y - 1;
  end;

  FSQLite3DB.ExecSQLWithBLOB(
    ALFormat(INSERT_TILES_SQL, [AZoom, X, Y]),
    AData.Buffer,
    AData.Size
  );
end;

function TThreadExportToMBTiles.GetBounds(const ATileIterator: ITileIterator): string;
var
  VLonLatRect: TDoubleRect;
begin
  VLonLatRect := GetLonLatRect(ATileIterator);
  Result :=
    Format(
      '%.8f,%.8f,%.8f,%.8f',
      [VLonLatRect.Left, VLonLatRect.Bottom, VLonLatRect.Right, VLonLatRect.Top],
      FFormatSettings
    );
end;

function TThreadExportToMBTiles.GetLonLatRect(const ATileIterator: ITileIterator): TDoubleRect;
var
  VRect: TRect;
  VTileRect: ITileRect;
  VProjection: IProjection;
begin
  VTileRect := ATileIterator.TilesRect;
  VProjection := VTileRect.Projection;
  VRect := VTileRect.Rect;
  Result := VProjection.TileRect2LonLatRect(VRect);
end;

end.
