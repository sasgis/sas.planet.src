{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_ThreadExportToBDB;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_TileFileNameGenerator,
  u_MapType,
  u_ResStrings,
  u_TileStorageBerkeleyDBHelper,
  u_ThreadExportAbstract;

type
  TThreadExportToBDB = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FTileNameGen: ITileFileNameGenerator;
    FStream: TMemoryStream;
    FIsMove: boolean;
    FIsReplace: boolean;
    FPathExport: string;
    function TileExportToRemoteBDB(
      AHelper: TTileStorageBerkeleyDBHelper;
      AMapType: TMapType;
      AXY: TPoint;
      AZoom: Byte;
      ARemotePath: string
    ): Boolean;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      APath: string;
      AProjectionFactory: IProjectionInfoFactory;
      AVectorItmesFactory: IVectorItmesFactory;
      APolygon: ILonLatPolygon;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Amove: boolean;
      Areplace: boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  Variants,
  i_CoordConverter,
  i_VectorItemProjected,
  i_TileIterator,
  i_TileInfoBasic,
  i_MapVersionInfo,
  u_TileFileNameBDB,
  u_TileIteratorByPolygon;

constructor TThreadExportToBDB.Create(
  APath: string;
  AProjectionFactory: IProjectionInfoFactory;
  AVectorItmesFactory: IVectorItmesFactory;
  APolygon: ILonLatPolygon;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Amove, Areplace: boolean
);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FPathExport := APath;
  FIsMove := AMove;
  FTileNameGen := TTileFileNameBDB.Create;
  FIsReplace := AReplace;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 0 to length(Atypemaparr) - 1 do begin
    FMapTypeArr[i] := Atypemaparr[i];
  end;
  FStream := TMemoryStream.Create;
end;

destructor TThreadExportToBDB.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TThreadExportToBDB.TileExportToRemoteBDB(
  AHelper: TTileStorageBerkeleyDBHelper;
  AMapType: TMapType;
  AXY: TPoint;
  AZoom: Byte;
  ARemotePath: string
): Boolean;
var
  VExportSDBFile: string;
  VTileInfo: ITileInfoBasic;
  VVersionInfo: IMapVersionInfo;
  VTileExists: Boolean;
  VSDBFileExists: Boolean;
  VLoadDate: TDateTime;
  VVersionStr: PWideChar;
  VContenetTypeStr: PWideChar;
begin
  Result := False;
  VExportSDBFile :=
    IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) +
    AMapType.GetShortFolderName) +
    FTileNameGen.GetTileFileName(AXY, AZoom) +
    '.sdb';
  VSDBFileExists := FileExists(VExportSDBFile);
  if VSDBFileExists then begin
    VTileExists := AHelper.TileExists(VExportSDBFile, AXY, AZoom, '');
  end else begin
    VTileExists := False;
  end;
  if not VTileExists or (VTileExists and FIsReplace) then begin
    FStream.Clear;
    VTileInfo := nil;
    VVersionInfo := nil; // TODO: ??
    if AMapType.TileStorage.LoadTile(AXY, AZoom, VVersionInfo, FStream, VTileInfo) then begin
      if VSDBFileExists or AHelper.CreateDirIfNotExists(VExportSDBFile) then begin
        if VTileInfo <> nil then begin
          VLoadDate := VTileInfo.LoadDate;
        end else begin
          VLoadDate := Now;
        end;
        if (VTileInfo <> nil) and (VTileInfo.VersionInfo <> nil) then begin
          VVersionStr := PWideChar(VarToWideStrDef(VTileInfo.VersionInfo.Version, ''));
        end else begin
          VVersionStr := '';
        end;
        if (VTileInfo <> nil) and (VTileInfo.ContentType <> nil) then begin
          VContenetTypeStr := PWideChar(VTileInfo.ContentType.GetContentType);
        end else begin
          VContenetTypeStr := PWideChar(AMapType.TileStorage.GetMainContentType.GetContentType);
        end; 
        FStream.Position := 0;
        Result := AHelper.SaveTile(
          VExportSDBFile,
          AXY,
          AZoom,
          VLoadDate,
          VVersionStr,
          VContenetTypeStr,
          FStream
        );
      end;
    end;
  end;
end;

procedure TThreadExportToBDB.ProcessRegion;
var
  i, j: integer;
  VZoom: Byte;
  VPath: string;
  VTile: TPoint;
  VMapType: TMapType;
  VGeoConvert: ICoordConverter;
  VTileIterators: array of array of ITileIterator;
  VTileIterator: ITileIterator;
  VHelper: TTileStorageBerkeleyDBHelper;
  VProjectedPolygon: IProjectedPolygon;
begin
  inherited;
  SetLength(VTileIterators, length(FMapTypeArr), Length(FZooms));
  FTilesToProcess := 0;
  for i := 0 to length(FMapTypeArr) - 1 do begin
    for j := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[j];
      VGeoConvert := FMapTypeArr[i].GeoConvert;
      VProjectedPolygon :=
        FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(
            VGeoConvert,
            VZoom
          ),
          PolygLL
        );
      VTileIterators[i, j] := TTileIteratorByPolygon.Create(VProjectedPolygon);
      FTilesToProcess := FTilesToProcess + VTileIterators[i, j].TilesTotal;
    end;
  end;
  try
    ProgressFormUpdateCaption(
      SAS_STR_ExportTiles,
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
      );
    FTilesProcessed := 0;
    ProgressFormUpdateOnProgress;
    for i := 0 to length(FMapTypeArr) - 1 do begin
      VMapType := FMapTypeArr[i];
      VGeoConvert := VMapType.GeoConvert;
      VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + VMapType.GetShortFolderName);
      VHelper := TTileStorageBerkeleyDBHelper.Create(VPath, VMapType.GeoConvert.Datum.EPSG);
      try
        for j := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[j];
          VTileIterator := VTileIterators[i, j];
          while VTileIterator.Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;
            if TileExportToRemoteBDB(VHelper, VMapType, VTile, VZoom, VPath) then begin
              if FIsMove then begin
                VMapType.DeleteTile(VTile, VZoom);
              end;
            end;
            inc(FTilesProcessed);
            if FTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress;
            end;
          end;
        end;
      finally
        FreeAndNil(VHelper);
      end;
    end;
  finally
    for i := 0 to length(FMapTypeArr) - 1 do begin
      for j := 0 to Length(FZooms) - 1 do begin
        VTileIterators[i, j] := nil;
      end;
    end;
    VTileIterators := nil;
  end;
  ProgressFormUpdateOnProgress;
  FTileNameGen := nil;
end;

end.
