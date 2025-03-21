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

unit u_ExportTaskToOruxMapsSQLite;

interface

uses
  Types,
  Windows,
  SysUtils,
  Classes,
  libsqlite3,
  i_BinaryData,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_ProjectionSetFactory,
  i_GeometryProjectedFactory,
  i_TileIteratorFactory,
  i_GeometryLonLat,
  i_TileInfoBasic,
  i_TileStorage,
  i_TileIterator,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  u_SQLite3Handler,
  u_ExportTaskAbstract;

type
  TExportTaskToOruxMapsSQLite = class(TExportTaskAbstract)
  private
    FFormatSettings: TFormatSettings;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FProjectionSetFactory: IProjectionSetFactory;
    FExportPath: string;
    FTileStorage: ITileStorage;
    FMapVersion: IMapVersionRequest;
    FBitmapTileSaver: IBitmapTileSaver;
    FBitmapProvider: IBitmapTileUniProvider;
    FDirectTilesCopy: Boolean;
    FSQLite3DB: TSQLite3DbHandler;
    FInsertStmt: TSQLite3StmtData;
    FIsInsertStmtPrepared: Boolean;
    FSQLiteAvailable: Boolean;
    FBlankTile: IBinaryData;
    FBasePoint: TPoint;
  private
    procedure WriteMainOtrk2File(const ATileIterators: array of ITileIterator);
    procedure OpenSQLiteStorage;
    procedure CloseSQLiteStorage;
    procedure SaveTileToSQLiteStorage(
      const ATile: TPoint;
      const AZoom: Byte;
      const AData: IBinaryData
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AExportPath: string;
      const ATileIteratorFactory: ITileIteratorFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectionSetFactory: IProjectionSetFactory;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const AMapVersion: IMapVersionRequest;
      const ABitmapTileSaver: IBitmapTileSaver;
      const ABitmapProvider: IBitmapTileUniProvider;
      const ABlankTile: IBinaryData;
      const ADirectTilesCopy: Boolean
    );
  end;

implementation

uses
  Math,
  c_CoordConverter,
  t_GeoTypes,
  i_GeometryProjected,
  i_ProjectionSet,
  i_Projection,
  i_Bitmap32Static,
  i_TileRect,
  u_AnsiStr,
  u_Dialogs,
  u_TileRect,
  u_TileIteratorByRect,
  u_TileIteratorByPolygon,
  u_GeoFunc,
  u_ResStrings;

const
  CDataBaseFileName = 'OruxMapsImages.db';

const
  TABLE_TILES_DDL = 'CREATE TABLE IF NOT EXISTS tiles (x int, y int, z int, image blob, PRIMARY KEY (x,y,z))';
  INDEX_DDL = 'CREATE INDEX IF NOT EXISTS IND on tiles (x,y,z)';
  TABLE_ANDROID_METADATA_DDL = 'CREATE TABLE IF NOT EXISTS android_metadata (locale TEXT)';

const
  INSERT_SQL = 'INSERT or IGNORE INTO tiles (x,y,z,image) VALUES (?,?,?,?)';

{ TExportTaskToOruxMapsSQLite }

constructor TExportTaskToOruxMapsSQLite.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AExportPath: string;
  const ATileIteratorFactory: ITileIteratorFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectionSetFactory: IProjectionSetFactory;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const AMapVersion: IMapVersionRequest;
  const ABitmapTileSaver: IBitmapTileSaver;
  const ABitmapProvider: IBitmapTileUniProvider;
  const ABlankTile: IBinaryData;
  const ADirectTilesCopy: Boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    ATileIteratorFactory
  );
  FFormatSettings.DecimalSeparator := '.';
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FProjectionSetFactory := AProjectionSetFactory;
  FExportPath := AExportPath;
  FTileStorage := ATileStorage;
  FMapVersion := AMapVersion;
  FBitmapTileSaver := ABitmapTileSaver;
  FBitmapProvider := ABitmapProvider;
  FBlankTile := ABlankTile;
  FDirectTilesCopy := ADirectTilesCopy;
  FSQLiteAvailable := FSQLite3DB.Init;
end;

procedure TExportTaskToOruxMapsSQLite.ProcessRegion;
var
  I: Integer;
  VZoom: Byte;
  VTile: TPoint;
  VDoubleRect: TDoubleRect;
  VDoDirectCopy: Boolean;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VProjectionSet: IProjectionSet;
  VRect: TRect;
  VTileRect: ITileRect;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VProjectedPolygons: array of IGeometryProjectedPolygon;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VTileInfo: ITileInfoWithData;
  VBitmapTile: IBitmap32Static;
  VTileData: IBinaryData;
  VProjection: IProjection;
  VIterByRect: Boolean;
  VDoSaveBlank: Boolean;
begin
  inherited;

  if not FSQLiteAvailable then begin
    ShowErrorMessageSync('The SQLite3 library is not available!');
    Exit;
  end;

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

  VIterByRect := Assigned(FBlankTile);

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
    if VIterByRect then begin
      VRect :=
        RectFromDoubleRect(
          VProjection.PixelRectFloat2TileRectFloat(VProjectedPolygons[I].Bounds),
          rrOutside
        );
      VTileRect := TTileRect.Create(VProjection, VRect);
      VTileIterators[I] := TTileIteratorByRect.Create(VTileRect);
    end else begin
      VTileIterators[I] := Self.MakeTileIterator(VProjection);
    end;
    VTilesToProcess := VTilesToProcess + VTileIterators[I].TilesTotal;
  end;

  WriteMainOtrk2File(VTileIterators);

  OpenSQLiteStorage;
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

          VDoSaveBlank := False;

          if VIterByRect then begin
            VDoubleRect := VProjection.TilePos2PixelRectFloat(VTile);
            VDoSaveBlank := not VProjectedPolygon.IsRectIntersectPolygon(VDoubleRect);
          end;

          if not VDoSaveBlank then begin
            if VDoDirectCopy then begin
              if Supports(FTileStorage.GetTileInfoEx(VTile, VZoom, FMapVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
                // save tile as is
                SaveTileToSQLiteStorage(VTile, VZoom, VTileInfo.TileData);
              end else begin
                VDoSaveBlank := True;
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
                // save tile with overlay
                SaveTileToSQLiteStorage(VTile, VZoom, VTileData);
              end else begin
                VDoSaveBlank := True;
              end;
            end;
          end;

          if VIterByRect and VDoSaveBlank then begin
            SaveTileToSQLiteStorage(VTile, VZoom, FBlankTile);
          end;

          Inc(VTilesProcessed);
          if VTilesProcessed mod 100 = 0 then begin
            ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
          end;
          if VTilesProcessed mod 10000 = 0 then begin
            FSQLite3Db.CommitTransaction;
            FSQLite3Db.BeginTransaction;
          end;
        end;
      end;
    end;
  finally
    CloseSQLiteStorage;
  end;
end;

procedure TExportTaskToOruxMapsSQLite.WriteMainOtrk2File(
  const ATileIterators: array of ITileIterator
);
const
  CR = #13#10;
var
  I: Integer;
  VRect: TRect;
  VTileRect: ITileRect;
  VLonLatRect: TDoubleRect;
  VProjection: IProjection;
  VText: string;
  VMapName: string;
  VLayerFormat: string;
  VProjectionStr: string;
  xMax, yMax: Integer;
  VUtf8Text: UTF8String;
  VMemStream: TMemoryStream;
begin
  VMapName := ExtractFileName(ExcludeTrailingPathDelimiter(FExportPath));

  VText :=
    '<?xml version="1.0" encoding="UTF-8"?>' + CR +
    '<OruxTracker xmlns="http://oruxtracker.com/app/res/calibration"' + CR +
    'versionCode="3.0">' + CR +
    '<MapCalibration layers="true" layerLevel="0">' + CR +
    '<MapName><![CDATA[' + VMapName + ']]></MapName>' + CR;

  VLayerFormat :=
    '<OruxTracker  versionCode="2.1">' + CR +
    '<MapCalibration layers="false" layerLevel="%d">' + CR +
    '<MapName><![CDATA[%s]]></MapName>' + CR +
    '<MapChunks xMax="%d" yMax="%d" datum="WGS84" projection="%s" img_height="256" img_width="256" file_name="%s" />' + CR +
    '<MapDimensions height="%d" width="%d" />' + CR +
    '<MapBounds minLat="%.8f" maxLat="%.8f" minLon="%.8f" maxLon="%.8f" />' + CR +
    '<CalibrationPoints>' + CR +
    '<CalibrationPoint corner="TL" lon="%.6f" lat="%.6f" />' + CR +
    '<CalibrationPoint corner="BR" lon="%.6f" lat="%.6f" />' + CR +
    '<CalibrationPoint corner="TR" lon="%.6f" lat="%.6f" />' + CR +
    '<CalibrationPoint corner="BL" lon="%.6f" lat="%.6f" />' + CR +
    '</CalibrationPoints>' + CR +
    '</MapCalibration>' + CR +
    '</OruxTracker>' + CR;

  for I := 0 to Length(ATileIterators) - 1 do begin
    VTileRect := ATileIterators[I].TilesRect;
    VProjection := VTileRect.Projection;
    VRect := VTileRect.Rect;

    VLonLatRect := VProjection.TileRect2LonLatRect(VRect);

    xMax := VRect.Right - VRect.Left;
    yMax := VRect.Bottom - VRect.Top;

    if VProjectionStr = '' then begin
      case VProjection.ProjectionType.ProjectionEPSG of
        CGoogleProjectionEPSG: VProjectionStr := 'Mercator';
        CYandexProjectionEPSG: VProjectionStr := 'Mercator Ellipsoidal';
        CGELonLatProjectionEPSG: VProjectionStr := 'Latitude/Longitude';
      else
        raise Exception.CreateFmt('Unknown projection EPSG: %d', [VProjection.ProjectionType.ProjectionEPSG]);
      end;
    end;

    VText := VText +
      Format(VLayerFormat, [
        VProjection.Zoom,
        VMapName,
        xMax, yMax, VProjectionStr, VMapName,
        yMax*256, xMax*256,
        VLonLatRect.Bottom, VLonLatRect.Top, VLonLatRect.Left, VLonLatRect.Right,
        VLonLatRect.TopLeft.X, VLonLatRect.TopLeft.Y,
        VLonLatRect.BottomRight.X, VLonLatRect.BottomRight.Y,
        VLonLatRect.BottomRight.X, VLonLatRect.TopLeft.Y,
        VLonLatRect.TopLeft.X, VLonLatRect.BottomRight.Y
      ], FFormatSettings);
  end;

  VText := VText + '</MapCalibration>' + CR + '</OruxTracker>';

  VUtf8Text := AnsiToUtf8(VText);

  VMemStream := TMemoryStream.Create;
  try
    VMemStream.WriteBuffer(VUtf8Text[1], Length(VUtf8Text));
    VMemStream.SaveToFile(FExportPath + VMapName + '.otrk2.xml');
  finally
    VMemStream.Free;
  end;
end;

procedure TExportTaskToOruxMapsSQLite.OpenSQLiteStorage;
const
  cLocale = '''en_US''';
var
  VFileName: string;
begin
  CloseSQLiteStorage;

  VFileName := FExportPath + CDataBaseFileName;

  if FileExists(VFileName) then begin
    if not DeleteFile(VFileName) then begin
      raise ESQLite3SimpleError.CreateFmt('Can''t delete database: %s', [VFileName]);
    end;
  end;

  FSQLite3Db.Open(VFileName, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE or SQLITE_OPEN_NOMUTEX);

  FSQLite3DB.ExecSQL('PRAGMA locking_mode = EXCLUSIVE');
  FSQLite3DB.ExecSQL('PRAGMA synchronous = OFF');
  FSQLite3DB.ExecSQL('PRAGMA journal_mode = OFF');

  FSQLite3DB.ExecSQL(TABLE_TILES_DDL);
  FSQLite3DB.ExecSQL(INDEX_DDL);

  FSQLite3DB.ExecSQL(TABLE_ANDROID_METADATA_DDL);
  FSQLite3DB.ExecSQL('INSERT INTO android_metadata VALUES (' + cLocale + ')');

  FIsInsertStmtPrepared := FSQLite3DB.PrepareStatement(@FInsertStmt, INSERT_SQL);
  if not FIsInsertStmtPrepared then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  FSQLite3Db.BeginTransaction;
end;

procedure TExportTaskToOruxMapsSQLite.CloseSQLiteStorage;
begin
  if not FSQLite3DB.IsOpened then begin
    Exit;
  end;

  if FIsInsertStmtPrepared then begin
    FInsertStmt.ClearBindings;
    FSQLite3DB.ClosePrepared(@FInsertStmt);
  end;

  FSQLite3DB.CommitTransaction;
  FSQLite3DB.Close;
end;

procedure TExportTaskToOruxMapsSQLite.SaveTileToSQLiteStorage(
  const ATile: TPoint;
  const AZoom: Byte;
  const AData: IBinaryData
);
var
  VBindResult: Boolean;
begin
  Assert(AData <> nil);
  Assert(FIsInsertStmtPrepared);

  VBindResult :=
    FInsertStmt.BindInt(1, ATile.X - FBasePoint.X) and
    FInsertStmt.BindInt(2, ATile.Y - FBasePoint.Y) and
    FInsertStmt.BindInt(3, AZoom) and
    FInsertStmt.BindBlob(4, AData.Buffer, AData.Size);

  if not VBindResult then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  if not FSQLite3DB.ExecPrepared(@FInsertStmt) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;
end;

end.
