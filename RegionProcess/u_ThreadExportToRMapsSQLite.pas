{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ThreadExportToRMapsSQLite;

interface

uses
  Types,
  Windows,
  SysUtils,
  Classes,
  SQLite3Handler,
  i_BinaryData,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_VectorItemsFactory,
  i_VectorItemLonLat,
  i_TileInfoBasic,
  i_TileStorage,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  u_MapType,
  u_ThreadExportAbstract;

type
  TThreadExportToRMapsSQLite = class(TThreadExportAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FExportPath: string;
    FMapType: TMapType;
    FBitmapTileSaver: IBitmapTileSaver;
    FBitmapProvider: IBitmapLayerProvider;
    FForceDropTarget: Boolean;
    FIsReplace: Boolean;
    FDirectTilesCopy: Boolean;
    FInsertSQLText: AnsiString;
    FSQLite3DB: TSQLite3DbHandler;
    FSQLiteAvailable: Boolean;
  private
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
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const APolygon: ILonLatPolygon;
      const AZoomArr: TByteDynArray;
      const AMapType: TMapType;
      const ABitmapTileSaver: IBitmapTileSaver;
      const ABitmapProvider: IBitmapLayerProvider;
      const AForceDropTarget: Boolean;
      const AReplace: Boolean;
      const ADirectTilesCopy: Boolean
    );
  end;

implementation

uses
  ALFcnString,
  ALSqlite3Wrapper,
  c_CoordConverter,
  i_VectorItemProjected,
  i_CoordConverter,
  i_MapVersionInfo,
  i_TileIterator,
  i_Bitmap32Static,
  u_TileIteratorByPolygon,
  u_ResStrings;

{ TThreadExportToRMapsSQLite }

constructor TThreadExportToRMapsSQLite.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AExportPath: string;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const APolygon: ILonLatPolygon;
  const AZoomArr: TByteDynArray;
  const AMapType: TMapType;
  const ABitmapTileSaver: IBitmapTileSaver;
  const ABitmapProvider: IBitmapLayerProvider;
  const AForceDropTarget: Boolean;
  const AReplace: Boolean;
  const ADirectTilesCopy: Boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    Self.ClassName
  );
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FExportPath := AExportPath;
  FMapType := AMapType;
  FBitmapTileSaver := ABitmapTileSaver;
  FBitmapProvider := ABitmapProvider;
  FForceDropTarget := AForceDropTarget;
  FIsReplace := AReplace;
  FDirectTilesCopy := ADirectTilesCopy;
  FSQLiteAvailable := FSQLite3DB.Init;
end;

procedure TThreadExportToRMapsSQLite.ProcessRegion;
var
  I: Integer;
  VZoom: Byte;
  VTile: TPoint;
  VDoDirectCopy: Boolean;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VGeoConvert: ICoordConverter;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VProjectedPolygon: IProjectedPolygon;
  VTileInfo: ITileInfoWithData;
  VSourceVersion: IMapVersionInfo;
  VBitmapTile: IBitmap32Static;
  VTileData: IBinaryData;
begin
  inherited;

  VDoDirectCopy := FDirectTilesCopy and Assigned(FMapType) and FMapType.IsBitmapTiles;

  if not VDoDirectCopy then begin
    Assert(FBitmapProvider <> nil);
    Assert(FBitmapTileSaver <> nil);
  end;

  SetLength(VTileIterators, Length(FZooms));
  VTilesToProcess := 0;

  for I := 0 to Length(FZooms) - 1 do begin
    if VDoDirectCopy then begin
      VGeoConvert := FMapType.GeoConvert;
    end else begin
      VGeoConvert := FCoordConverterFactory.GetCoordConverterByCode(
        CGoogleProjectionEPSG,
        CTileSplitQuadrate256x256
      );
    end;
    VProjectedPolygon := FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      FProjectionFactory.GetByConverterAndZoom(VGeoConvert, I),
      PolygLL
    );
    VTileIterators[I] := TTileIteratorByPolygon.Create(VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[I].TilesTotal;
  end;

  OpenSQLiteStorage;
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

    if VDoDirectCopy then begin
      VSourceVersion := FMapType.VersionConfig.Version;
    end;

    for I := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[I];
      VTileIterator := VTileIterators[I];
      if Assigned(VTileIterator) then begin
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            Exit;
          end;

          if VDoDirectCopy then begin
            if Supports(FMapType.TileStorage.GetTileInfo(VTile, VZoom, VSourceVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
              // save tile as is
              SaveTileToSQLiteStorage(VTile, VZoom, VTileInfo.TileData);
            end;
          end else begin
            VBitmapTile :=
              FBitmapProvider.GetBitmapRect(
                Self.OperationID,
                Self.CancelNotifier,
                FLocalConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert)
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
        end;
      end;
    end;
  finally
    CloseSQLiteStorage;
    for I := 0 to Length(FZooms) - 1 do begin
      VTileIterators[I] := nil;
    end;
    VTileIterators := nil;
  end;
end;

procedure TThreadExportToRMapsSQLite.OpenSQLiteStorage;
var
  VCreateNewDB: Boolean;
begin
  // check library
  if not FSQLiteAvailable then begin
    raise ESQLite3SimpleError.Create('SQLite not available');
  end;

  // закрываем предыдущее (если есть)
  CloseSQLiteStorage;

  // make sqlite database
  if FileExists(FExportPath) then begin
    // база уже есть - будем дописывать или грохнем
    if FForceDropTarget then begin
      DeleteFile(FExportPath);
      VCreateNewDB := True;
    end else begin
      VCreateNewDB := False;
    end;
  end else begin
    // базы ещё нет
    VCreateNewDB := True;
  end;

  // создаём новую или открываем существующую
  FSQLite3Db.OpenW(FExportPath);

  // настраиваем текст SQL
  if FIsReplace then begin
    FInsertSQLText := 'REPLACE'
  end else begin
    FInsertSQLText := 'IGNORE';
  end;

  FInsertSQLText := 'INSERT OR ' + FInsertSQLText + ' INTO tiles (x,y,z,s,image) VALUES (';

  // если новая - забацаем структуру
  if VCreateNewDB then begin
    FSQLite3DB.ExecSQL('CREATE TABLE IF NOT EXISTS tiles (x int, y int, z int, s int, image blob, PRIMARY KEY (x,y,z,s))');
    FSQLite3DB.ExecSQL('CREATE TABLE IF NOT EXISTS info (maxzoom Int, minzoom Int)');
    FSQLite3DB.ExecSQL('INSERT OR REPLACE INTO info (maxzoom, minzoom) VALUES (0,0)');

    (*
      ещё есть
      CREATE TABLE android_metadata (locale  text);
      с одной строкой
      'en_US'
    *)
  end;

  FSQLite3DB.SetExclusiveLockingMode;
  FSQLite3DB.ExecSQL('PRAGMA synchronous=OFF');

  // открываем транзакцию для пущей скорости
  FSQLite3DB.BeginTran;
end;

procedure TThreadExportToRMapsSQLite.CloseSQLiteStorage;
begin
  if FSQLite3DB.Opened then begin
    // перед закрытием надо обновить зумы
    FSQLite3DB.ExecSQL('UPDATE info SET minzoom = (SELECT DISTINCT z FROM tiles ORDER BY z ASC LIMIT 1)');
    FSQLite3DB.ExecSQL('UPDATE info SET maxzoom = (SELECT DISTINCT z FROM tiles ORDER BY z DESC LIMIT 1)');
    // закрытие
    FSQLite3DB.Commit;
    FSQLite3DB.Close;
  end;
end;

procedure TThreadExportToRMapsSQLite.SaveTileToSQLiteStorage(
  const ATile: TPoint;
  const AZoom: Byte;
  const AData: IBinaryData
);
var
  VSQLText: AnsiString;
begin
  Assert(AData <> nil);

  VSQLText := FInsertSQLText +
              ALIntToStr(ATile.X) + ',' +
              ALIntToStr(ATile.Y) + ',' +
              ALIntToStr(17 - AZoom) +
              ',0,?)';

  FSQLite3DB.ExecSQLWithBLOB(
    VSQLText,
    AData.Buffer,
    AData.Size
  );
end;

end.


