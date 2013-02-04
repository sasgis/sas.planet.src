{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_ThreadExportToBerkeleyDB;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  i_MapVersionInfo,
  i_VectorItemLonLat,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_TileFileNameGenerator,
  i_GlobalBerkeleyDBHelper,
  i_MapTypes,
  u_MapType,
  u_ResStrings,
  u_TileStorageBerkeleyDBHelper,
  u_ThreadExportAbstract;

type
  TThreadExportToBerkeleyDB = class(TThreadExportAbstract)
  private
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FMapTypeArr: IMapTypeListStatic;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FTileNameGen: ITileFileNameGenerator;
    FIsMove: boolean;
    FIsReplace: boolean;
    FPathExport: string;
    function GetFullPathName(const ARelativePathName: string): string;
    function TileExportToRemoteBDB(
      AHelper: TTileStorageBerkeleyDBHelper;
      AMapType: TMapType;
      const AXY: TPoint;
      AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ARemotePath: string
    ): Boolean;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APath: string;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const APolygon: ILonLatPolygon;
      const AZoomArr: TByteDynArray;
      const AMapTypeArr: IMapTypeListStatic;
      AMove: boolean;
      AReplace: boolean
    );
  end;

implementation

uses
  ShLwApi,
  i_CoordConverter,
  i_ContentTypeInfo,
  i_VectorItemProjected,
  i_TileIterator,
  i_TileInfoBasic,
  i_TileStorage,
  u_TileFileNameBerkeleyDB,
  u_TileIteratorByPolygon;

{ TThreadExportToBerkeleyDB }

constructor TThreadExportToBerkeleyDB.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APath: string;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const APolygon: ILonLatPolygon;
  const AZoomArr: TByteDynArray;
  const AMapTypeArr: IMapTypeListStatic;
  AMove, AReplace: boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    AnsiString(Self.ClassName)
  );
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
  FProjectionFactory := AProjectionFactory;
  FVectorItemsFactory := AVectorItemsFactory;
  FPathExport := GetFullPathName(APath);
  if FPathExport = '' then begin
    raise Exception.Create('Can''t ExpandFileName: ' + APath);
  end;
  FIsMove := AMove;
  FTileNameGen := TTileFileNameBerkeleyDB.Create as ITileFileNameGenerator;
  FIsReplace := AReplace;
  FMapTypeArr := AMapTypeArr;
end;

function TThreadExportToBerkeleyDB.GetFullPathName(const ARelativePathName: string): string;
begin
  SetLength(Result, MAX_PATH);
  PathCombine(@Result[1], PChar(ExtractFilePath(ParamStr(0))), PChar(ARelativePathName));
  SetLength(Result, StrLen(PChar(Result)));
end;

function TThreadExportToBerkeleyDB.TileExportToRemoteBDB(
  AHelper: TTileStorageBerkeleyDBHelper;
  AMapType: TMapType;
  const AXY: TPoint;
  AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ARemotePath: string
): Boolean;
var
  VExportSDBFile: string;
  VTileInfo: ITileInfoWithData;
  VTileExists: Boolean;
  VSDBFileExists: Boolean;
  VLoadDate: TDateTime;
  VContenetType: IContentTypeInfoBasic;
begin
  Result := False;
  VExportSDBFile :=
    IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) +
      AMapType.GetShortFolderName) +
    FTileNameGen.GetTileFileName(AXY, AZoom) +
    '.sdb';
  VSDBFileExists := FileExists(VExportSDBFile);
  if VSDBFileExists then begin
    VTileExists := AHelper.TileExists(VExportSDBFile, AXY, AZoom, AVersionInfo);
  end else begin
    VTileExists := False;
  end;
  if not VTileExists or (VTileExists and FIsReplace) then begin
    if Supports(AMapType.TileStorage.GetTileInfo(AXY, AZoom, AVersionInfo, gtimWithData), ITileInfoWithData, VTileInfo) then begin
      if VSDBFileExists or AHelper.CreateDirIfNotExists(VExportSDBFile) then begin
        if VTileInfo <> nil then begin
          VLoadDate := VTileInfo.LoadDate;
        end else begin
          VLoadDate := Now;
        end;
        if (VTileInfo <> nil) then begin
          VContenetType := VTileInfo.ContentType;
        end else begin
          VContenetType := AMapType.ContentType;
        end;
        Result := AHelper.SaveTile(
          VExportSDBFile,
          AXY,
          AZoom,
          VLoadDate,
          VTileInfo.VersionInfo,
          VContenetType,
          VTileInfo.TileData
        );
      end;
    end;
  end;
end;

procedure TThreadExportToBerkeleyDB.ProcessRegion;
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
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VVersionInfo: IMapVersionInfo;
begin
  inherited;
  SetLength(VTileIterators, FMapTypeArr.Count, Length(FZooms));
  VTilesToProcess := 0;
  for i := 0 to FMapTypeArr.Count - 1 do begin
    for j := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[j];
      VGeoConvert := FMapTypeArr.Items[i].MapType.GeoConvert;
      VProjectedPolygon :=
        FVectorItemsFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(VGeoConvert, VZoom),
          PolygLL
        );
      VTileIterators[i, j] := TTileIteratorByPolygon.Create(VProjectedPolygon);
      VTilesToProcess := VTilesToProcess + VTileIterators[i, j].TilesTotal;
    end;
  end;
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    for i := 0 to FMapTypeArr.Count - 1 do begin
      VMapType := FMapTypeArr.Items[i].MapType;
      VVersionInfo := VMapType.VersionConfig.Version;
      VGeoConvert := VMapType.GeoConvert;
      VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + VMapType.GetShortFolderName);
      VHelper :=
        TTileStorageBerkeleyDBHelper.Create(
          FGlobalBerkeleyDBHelper,
          VPath,
          VMapType.GeoConvert.ProjectionEPSG
        );
      try
        for j := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[j];
          VTileIterator := VTileIterators[i, j];
          while VTileIterator.Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;
            if TileExportToRemoteBDB(VHelper, VMapType, VTile, VZoom, VVersionInfo, VPath) then begin
              if FIsMove then begin
                VMapType.TileStorage.DeleteTile(VTile, VZoom, VVersionInfo);
              end;
            end;
            inc(VTilesProcessed);
            if VTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
            end;
          end;
        end;
      finally
        FreeAndNil(VHelper);
      end;
    end;
  finally
    for i := 0 to FMapTypeArr.Count - 1 do begin
      for j := 0 to Length(FZooms) - 1 do begin
        VTileIterators[i, j] := nil;
      end;
    end;
    VTileIterators := nil;
  end;
  FTileNameGen := nil;
end;

end.
