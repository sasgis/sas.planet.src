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

unit u_ExportTaskToRMapsSQLite;

interface

uses
  Types,
  Windows,
  SysUtils,
  libsqlite3,
  t_RMapsSQLite,
  i_BinaryData,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_ProjectionSetFactory,
  i_TileIteratorFactory,
  i_GeometryLonLat,
  i_TileInfoBasic,
  i_TileStorage,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  u_SQLite3Handler,
  u_ExportTaskAbstract;

type
  TExportTaskToRMapsSQLite = class(TExportTaskAbstract)
  private
    FProjectionSetFactory: IProjectionSetFactory;
    FExportPath: string;
    FTileStorage: ITileStorage;
    FMapVersion: IMapVersionRequest;
    FBitmapTileSaver: IBitmapTileSaver;
    FBitmapProvider: IBitmapTileUniProvider;
    FForceDropTarget: Boolean;
    FIsReplace: Boolean;
    FDirectTilesCopy: Boolean;
    FSQLite3DB: TSQLite3DbHandler;
    FInsertStmt: TSQLite3StmtData;
    FIsInsertStmtPrepared: Boolean;
    FSQLiteAvailable: Boolean;
    FModType: TRMapsSQLiteModType;
    FIsEllipsoid: Boolean;
    FFormatSettings: TFormatSettings;
  private
    procedure OpenSQLiteStorage;
    procedure CloseSQLiteStorage;
    procedure SaveTileToSQLiteStorage(
      const ATile: TPoint;
      const AZoom: Byte;
      const AData: IBinaryData
    );
    procedure PrepareInsertStmt;
    function CoordToStr(const AValue: Double): AnsiString; inline;
    procedure FillZoomsCallback(const AHandler: PSQLite3DbHandler;
      const ACallbackPtr: Pointer; const AStmtData: PSQLite3StmtData);
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AExportPath: string;
      const ATileIteratorFactory: ITileIteratorFactory;
      const AProjectionSetFactory: IProjectionSetFactory;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const AMapVersion: IMapVersionRequest;
      const ABitmapTileSaver: IBitmapTileSaver;
      const ABitmapProvider: IBitmapTileUniProvider;
      const AForceDropTarget: Boolean;
      const AReplace: Boolean;
      const ADirectTilesCopy: Boolean;
      const AModType: TRMapsSQLiteModType
    );
  end;

implementation

uses
  t_GeoTypes,
  c_CoordConverter,
  i_ProjectionSet,
  i_Projection,
  i_TileIterator,
  i_Bitmap32Static,
  u_AnsiStr,
  u_Dialogs,
  u_ResStrings;

{ TExportTaskToRMapsSQLite }

constructor TExportTaskToRMapsSQLite.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AExportPath: string;
  const ATileIteratorFactory: ITileIteratorFactory;
  const AProjectionSetFactory: IProjectionSetFactory;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const AMapVersion: IMapVersionRequest;
  const ABitmapTileSaver: IBitmapTileSaver;
  const ABitmapProvider: IBitmapTileUniProvider;
  const AForceDropTarget: Boolean;
  const AReplace: Boolean;
  const ADirectTilesCopy: Boolean;
  const AModType: TRMapsSQLiteModType
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    ATileIteratorFactory
  );
  FProjectionSetFactory := AProjectionSetFactory;
  FExportPath := AExportPath;
  FTileStorage := ATileStorage;
  FMapVersion := AMapVersion;
  FBitmapTileSaver := ABitmapTileSaver;
  FBitmapProvider := ABitmapProvider;
  FForceDropTarget := AForceDropTarget;
  FIsReplace := AReplace;
  FDirectTilesCopy := ADirectTilesCopy;
  FSQLiteAvailable := FSQLite3DB.Init;
  FModType := AModType;
  FFormatSettings.DecimalSeparator := '.';
end;

procedure TExportTaskToRMapsSQLite.ProcessRegion;

  function GetProjectionSet(const ADirectCopy: Boolean): IProjectionSet;
  var
    VEpsg: Integer;
  begin
    Result := nil;

    if ADirectCopy then begin
      Result := FTileStorage.ProjectionSet;
    end else begin

      if Assigned(FTileStorage) and (FModType in [mtOsmAnd]) then begin
        VEpsg := FTileStorage.ProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG;
        if (VEpsg = CGoogleProjectionEPSG) or (VEpsg = CYandexProjectionEPSG) then begin
          Result := FTileStorage.ProjectionSet;
        end;
      end;

      if Result = nil then begin
        Result :=
          FProjectionSetFactory.GetProjectionSetByCode(
            CGoogleProjectionEPSG,
            CTileSplitQuadrate256x256
          );
      end;
    end;
  end;

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
  VTileInfo: ITileInfoWithData;
  VBitmapTile: IBitmap32Static;
  VTileData: IBinaryData;
  VProjection: IProjection;
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

  SetLength(VTileIterators, Length(FZooms));
  VTilesToProcess := 0;

  VProjectionSet := GetProjectionSet(VDoDirectCopy);
  FIsEllipsoid := VProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG = CYandexProjectionEPSG;

  for I := 0 to Length(FZooms) - 1 do begin
    VProjection := VProjectionSet.Zooms[FZooms[I]];
    VTileIterators[I] := Self.MakeTileIterator(VProjection);
    VTilesToProcess := VTilesToProcess + VTileIterators[I].TilesTotal;
  end;

  OpenSQLiteStorage;
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + IntToStr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

    for I := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[I];
      VTileIterator := VTileIterators[I];
      if Assigned(VTileIterator) then begin
        VProjection := VTileIterator.TilesRect.Projection;
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            Exit;
          end;

          if VDoDirectCopy then begin
            if Supports(FTileStorage.GetTileInfoEx(VTile, VZoom, FMapVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
              // save tile as is
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
              // save reprojected tile with overlay
              SaveTileToSQLiteStorage(VTile, VZoom, VTileData);
            end;
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

function TExportTaskToRMapsSQLite.CoordToStr(const AValue: Double): AnsiString;
begin
  Result := FormatA('%.8f', [AValue], FFormatSettings);
end;

procedure TExportTaskToRMapsSQLite.OpenSQLiteStorage;
var
  VCenter: TDoublePoint;
  VCreateNewDB: Boolean;
begin
  CloseSQLiteStorage;

  if FileExists(FExportPath) then begin
    if FForceDropTarget then begin
      if not DeleteFile(FExportPath) then begin
        raise ESQLite3SimpleError.CreateFmt('Can''t delete database: %s', [FExportPath]);
      end;
      VCreateNewDB := True;
    end else begin
      VCreateNewDB := False;
    end;
  end else begin
    VCreateNewDB := True;
  end;

  FSQLite3Db.Open(FExportPath, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE or SQLITE_OPEN_NOMUTEX);

  FSQLite3DB.ExecSQL('PRAGMA locking_mode = EXCLUSIVE');
  FSQLite3DB.ExecSQL('PRAGMA synchronous = OFF');
  FSQLite3DB.ExecSQL('PRAGMA journal_mode = OFF');

  if VCreateNewDB then begin
    FSQLite3DB.ExecSQL(
      'CREATE TABLE IF NOT EXISTS tiles (x int, y int, z int, s int, image blob, PRIMARY KEY (x,y,z,s))'
    );

    case FModType of
      mtRMaps: begin
        FSQLite3DB.ExecSQL('CREATE TABLE IF NOT EXISTS info (maxzoom Int, minzoom Int)');
        FSQLite3DB.ExecSQL('INSERT OR REPLACE INTO info (minzoom, maxzoom) VALUES (0,0)');
      end;

      mtOsmAnd: begin
        FSQLite3DB.ExecSQL(
          'CREATE TABLE IF NOT EXISTS info (minzoom INTEGER, maxzoom INTEGER,' +
          ' tilenumbering TEXT, timecolumn TEXT, expireminutes TEXT)'
        );
        FSQLite3DB.ExecSQL(
          'INSERT OR REPLACE INTO info '+
          '(minzoom, maxzoom, tilenumbering, timecolumn, expireminutes) VALUES ' +
          '(0,0,"BigPlanet","no","0")'
        );
        if FIsEllipsoid then begin
          FSQLite3DB.ExecSQL('ALTER TABLE info ADD COLUMN ellipsoid TEXT');
          FSQLite3DB.ExecSQL('UPDATE info SET ellipsoid="1"');
        end;
      end;

      mtLocus: begin
        FSQLite3DB.ExecSQL(
          'CREATE TABLE IF NOT EXISTS info (minzoom INTEGER, maxzoom INTEGER,' +
          ' center_x DOUBLE, center_y DOUBLE, zooms TEXT, provider INTEGER)'
        );

        VCenter := PolygLL.GetGoToPoint;

        FSQLite3DB.ExecSQL(
          'INSERT OR REPLACE INTO info (minzoom, maxzoom, center_x, center_y) VALUES (' +
          '0,0,' + CoordToStr(VCenter.Y) + ',' + CoordToStr(VCenter.X) + ')'
        );
      end;
    else
      Assert(False);
    end;
  end;

  // prepare statement
  PrepareInsertStmt;

  // открываем транзакцию для пущей скорости
  FSQLite3DB.BeginTransaction;
end;

procedure TExportTaskToRMapsSQLite.PrepareInsertStmt;
var
  VText: AnsiString;
begin
  if FIsReplace then begin
    VText := 'REPLACE';
  end else begin
    VText := 'IGNORE';
  end;

  VText := 'INSERT OR ' + VText + ' INTO tiles (x,y,z,s,image) VALUES (?,?,?,0,?)';

  FIsInsertStmtPrepared := FSQLite3DB.PrepareStatement(@FInsertStmt, VText);
  if not FIsInsertStmtPrepared then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;
end;

procedure TExportTaskToRMapsSQLite.FillZoomsCallback(
  const AHandler: PSQLite3DbHandler;
  const ACallbackPtr: Pointer;
  const AStmtData: PSQLite3StmtData
);
var
  VLen: Integer;
  VZoom: AnsiString;
  VZoomsPtr: ^AnsiString absolute ACallbackPtr;
begin
  VZoom := AStmtData.ColumnAsAnsiString(0);
  if VZoom <> '' then begin
    VLen := Length(VZoomsPtr^);
    if VLen > 0 then begin
      VZoom := ';' + VZoom;
    end;
    SetLength(VZoomsPtr^, VLen + Length(VZoom));
    Move(VZoom[1], PPAnsiChar(VZoomsPtr)^[VLen], Length(VZoom));
  end;
end;

procedure TExportTaskToRMapsSQLite.CloseSQLiteStorage;
var
  VZooms: AnsiString;
begin
  if not FSQLite3DB.IsOpened then begin
    Exit;
  end;

  if FModType = mtLocus then begin
    VZooms := '';
    FSQLite3DB.OpenSQL(
      'SELECT DISTINCT z FROM tiles ORDER BY z DESC',
      Self.FillZoomsCallback,
      @VZooms,
      True
    );
    FSQLite3DB.ExecSQL('UPDATE info SET zooms = ("' + VZooms + '")');

    // Locus min/maxzoom is opposite to RMaps & OsmAnd
    FSQLite3DB.ExecSQL('UPDATE info SET maxzoom = (SELECT DISTINCT z FROM tiles ORDER BY z ASC LIMIT 1)');
    FSQLite3DB.ExecSQL('UPDATE info SET minzoom = (SELECT DISTINCT z FROM tiles ORDER BY z DESC LIMIT 1)');
  end else begin
    FSQLite3DB.ExecSQL('UPDATE info SET minzoom = (SELECT DISTINCT z FROM tiles ORDER BY z ASC LIMIT 1)');
    FSQLite3DB.ExecSQL('UPDATE info SET maxzoom = (SELECT DISTINCT z FROM tiles ORDER BY z DESC LIMIT 1)');
  end;

  if FIsInsertStmtPrepared then begin
    FInsertStmt.ClearBindings;
    FSQLite3DB.ClosePrepared(@FInsertStmt);
  end;

  FSQLite3DB.CommitTransaction;
  FSQLite3DB.Close;
end;

procedure TExportTaskToRMapsSQLite.SaveTileToSQLiteStorage(
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
    FInsertStmt.BindInt(1, ATile.X) and
    FInsertStmt.BindInt(2, ATile.Y) and
    FInsertStmt.BindInt(3, 17 - AZoom) and
    FInsertStmt.BindBlob(4, AData.Buffer, AData.Size);

  if not VBindResult then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  if not FSQLite3DB.ExecPrepared(@FInsertStmt) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;
end;

end.
