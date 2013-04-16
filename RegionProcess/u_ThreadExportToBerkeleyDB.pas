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

unit u_ThreadExportToBerkeleyDB;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  i_TileStorage,
  i_TileInfoBasic,
  i_MapVersionInfo,
  i_VectorItemLonLat,
  i_NotifierTime,
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
    FTimerNoifier: INotifierTime;
    FSourceTileStorage: ITileStorage;
    FDestTileStorage: ITileStorage;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FMapTypeArr: IMapTypeListStatic;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FTileNameGen: ITileFileNameGenerator;
    FIsMove: Boolean;
    FDestOverwriteTiles: Boolean;
    FPathExport: string;
    FSetTargetVersionEnabled: Boolean;
    FSetTargetVersionValue: string;
    function GetFullPathName(const ARelativePathName: string): string;  
    function ProcessTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const ASrcVersionInfo: IMapVersionInfo;
      const ATargetVersionInfo: IMapVersionInfo
    ): Integer; inline;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const ATimerNoifier: INotifierTime;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APath: string;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const APolygon: ILonLatPolygon;
      const AZoomArr: TByteDynArray;
      const AMapTypeArr: IMapTypeListStatic;
      const ASetTargetVersionEnabled: Boolean;
      const ASetTargetVersionValue: string;
      const AMove: Boolean;
      const AReplace: Boolean
    );
  end;

implementation

uses
  ShLwApi,
  i_CoordConverter,
  i_ContentTypeInfo,
  i_VectorItemProjected,
  i_TileIterator,
  u_TileStorageBerkeleyDB,
  u_TileFileNameBerkeleyDB,
  u_TileIteratorByPolygon;

{ TThreadExportToBerkeleyDB }

constructor TThreadExportToBerkeleyDB.Create(
  const ATimerNoifier: INotifierTime;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APath: string;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const APolygon: ILonLatPolygon;
  const AZoomArr: TByteDynArray;
  const AMapTypeArr: IMapTypeListStatic;
  const ASetTargetVersionEnabled: Boolean;
  const ASetTargetVersionValue: string;
  const AMove: Boolean;
  const AReplace: Boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    AnsiString(Self.ClassName)
  );
  FTimerNoifier := ATimerNoifier;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
  FProjectionFactory := AProjectionFactory;
  FVectorItemsFactory := AVectorItemsFactory;
  FPathExport := GetFullPathName(APath);
  if FPathExport = '' then begin
    raise Exception.Create('Can''t ExpandFileName: ' + APath);
  end;
  FSetTargetVersionEnabled := ASetTargetVersionEnabled;
  FSetTargetVersionValue := ASetTargetVersionValue;
  FIsMove := AMove;
  FTileNameGen := TTileFileNameBerkeleyDB.Create as ITileFileNameGenerator;
  FDestOverwriteTiles := AReplace;
  FMapTypeArr := AMapTypeArr;
end;

function TThreadExportToBerkeleyDB.GetFullPathName(const ARelativePathName: string): string;
begin
  SetLength(Result, MAX_PATH);
  PathCombine(@Result[1], PChar(ExtractFilePath(ParamStr(0))), PChar(ARelativePathName));
  SetLength(Result, StrLen(PChar(Result)));
end;

function TThreadExportToBerkeleyDB.ProcessTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const ASrcVersionInfo: IMapVersionInfo;
  const ATargetVersionInfo: IMapVersionInfo
): Integer;
var
  VSrcTileInfo, VDestTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VSrcVersionInfo: IMapVersionInfo;
  VTargetVersionInfo: IMapVersionInfo;
  VMapVersionList: IMapVersionListStatic;
  VVersionCount: Integer;
begin
  Result := 0;
  VVersionCount := 0;

  if not FSetTargetVersionEnabled then begin
    // will try copy all versions from source and ingnore ATargetVersionInfo
    VMapVersionList := FSourceTileStorage.GetListOfTileVersions(AXY, AZoom, ASrcVersionInfo);
  end else begin
    // will copy only one (current) version from source and save it with a ATargetVersionInfo
    VMapVersionList := nil;
  end;

  repeat
    Inc(Result);
    if Assigned(VMapVersionList) and (VVersionCount < VMapVersionList.Count) then begin
      VSrcVersionInfo := VMapVersionList.Item[VVersionCount];
      VTargetVersionInfo := VSrcVersionInfo;
      Inc(VVersionCount);
    end else begin
      VSrcVersionInfo := ASrcVersionInfo;
      VTargetVersionInfo := ATargetVersionInfo;
      VVersionCount := -1;
    end;
    VSrcTileInfo := FSourceTileStorage.GetTileInfo(AXY, AZoom, VSrcVersionInfo, gtimWithData);
    if Assigned(VSrcTileInfo) then begin
      if not FDestOverwriteTiles then begin
        VDestTileInfo := FDestTileStorage.GetTileInfo(AXY, AZoom, VTargetVersionInfo, gtimWithoutData);
        if Assigned(VDestTileInfo) then begin
          if (VDestTileInfo.IsExists or (VDestTileInfo.IsExistsTNE and VSrcTileInfo.IsExistsTNE)) then begin
            Continue;
          end;
        end;
      end;
      if VSrcTileInfo.IsExists then begin
        if Supports(VSrcTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
          FDestTileStorage.SaveTile(
            AXY,
            AZoom,
            VTargetVersionInfo,
            VTileInfoWithData.LoadDate,
            VTileInfoWithData.ContentType,
            VTileInfoWithData.TileData
          );
        end;
      end else if VSrcTileInfo.IsExistsTNE then begin
        FDestTileStorage.SaveTNE(
          AXY,
          AZoom,
          VTargetVersionInfo,
          VSrcTileInfo.LoadDate
        );
      end;
      if FIsMove then begin
        FSourceTileStorage.DeleteTile(AXY, AZoom, VSrcVersionInfo);
      end;
    end;
  until VVersionCount < 0;
end;

procedure TThreadExportToBerkeleyDB.ProcessRegion;
var
  I, J: Integer;
  VZoom: Byte;
  VTile: TPoint;
  VMapType: TMapType;
  VGeoConvert: ICoordConverter;
  VTileIterators: array of array of ITileIterator;
  VTileIterator: ITileIterator;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VProcessedCount: Integer;
  VSrcVersionInfo: IMapVersionInfo;
  VTargetVersionInfo: IMapVersionInfo;
begin
  inherited;
  SetLength(VTileIterators, FMapTypeArr.Count, Length(FZooms));
  VTilesToProcess := 0;
  for I := 0 to FMapTypeArr.Count - 1 do begin
    for J := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[J];
      VGeoConvert := FMapTypeArr.Items[I].MapType.GeoConvert;
      VProjectedPolygon :=
        FVectorItemsFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(VGeoConvert, VZoom),
          PolygLL
        );
      VTileIterators[I, J] := TTileIteratorByPolygon.Create(VProjectedPolygon);
      VTilesToProcess := VTilesToProcess + VTileIterators[I, J].TilesTotal;
    end;
  end;
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    for I := 0 to FMapTypeArr.Count - 1 do begin
      VMapType := FMapTypeArr.Items[I].MapType;
      FSourceTileStorage := VMapType.TileStorage;
      VSrcVersionInfo := VMapType.VersionConfig.Version;

      if FSetTargetVersionEnabled then begin
        VTargetVersionInfo :=
          VMapType.VersionConfig.VersionFactory.CreateByStoreString(
            FSetTargetVersionValue,
            VSrcVersionInfo.ShowPrevVersion
          );
      end else begin
        VTargetVersionInfo := VSrcVersionInfo;
      end;

      FDestTileStorage :=
        TTileStorageBerkeleyDB.Create(
          FGlobalBerkeleyDBHelper,
          VMapType.GeoConvert,
          IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + VMapType.GetShortFolderName),
          FTimerNoifier,
          nil, // MemCache - not needed here
          VMapType.ContentTypeManager,
          VMapType.VersionConfig.VersionFactory,
          VMapType.ContentType
        );

      for J := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[J];
        VTileIterator := VTileIterators[I, J];
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            Exit;
          end;

          VProcessedCount := ProcessTile(VTile, VZoom, VSrcVersionInfo, VTargetVersionInfo);

          Inc(VTilesProcessed, VProcessedCount);
          if VTilesProcessed mod 100 = 0 then begin
            ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
          end;
        end;
      end;  
    end;
  finally
    for I := 0 to FMapTypeArr.Count - 1 do begin
      for J := 0 to Length(FZooms) - 1 do begin
        VTileIterators[I, J] := nil;
      end;
    end;
    VTileIterators := nil;
  end;
  FTileNameGen := nil;
end;

end.
