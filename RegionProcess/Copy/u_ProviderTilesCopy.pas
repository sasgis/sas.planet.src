{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_ProviderTilesCopy;

interface

uses
  Forms,
  i_NotifierTime,
  i_LanguageManager,
  i_GeometryLonLat,
  i_MapTypeSet,
  i_MapTypeListBuilder,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ContentTypeManager,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  i_TileStorageTypeList,
  i_GlobalBerkeleyDBHelper,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_TilesCopy;

type
  TProviderTilesCopy = class(TExportProviderAbstract)
  private
    FMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
    FTimerNoifier: INotifierTime;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FContentTypeManager: IContentTypeManager;
    FTileStorageTypeList: ITileStorageTypeListStatic;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ATimerNoifier: INotifierTime;
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  end;


implementation

uses
  Types,
  Classes,
  SysUtils,
  i_MapTypes,
  i_MapTypeListStatic,
  i_TileStorageTypeListItem,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadCopyFromStorageToStorage,
  u_ResStrings;

{ TProviderTilesCopy }

constructor TProviderTilesCopy.Create(
  const ATimerNoifier: INotifierTime;
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FMapTypeListBuilderFactory := AMapTypeListBuilderFactory;
  FTimerNoifier := ATimerNoifier;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
  FContentTypeManager := AContentTypeManager;
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FTileStorageTypeList := ATileStorageTypeList;
end;

function TProviderTilesCopy.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesCopy.Create(
      Self.LanguageManager,
      FMapTypeListBuilderFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameTilesCopy));
end;

function TProviderTilesCopy.GetCaption: string;
begin
  Result := SAS_STR_OperationTilesCopyCaption;
end;

procedure TProviderTilesCopy.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VReplace: Boolean;
  VDeleteSource: Boolean;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VCacheType: Byte;
  VPlaceInSubFolder: Boolean;
  VSetTargetVersionEnabled: Boolean;
  VSetTargetVersionValue: String;
  VMaps: IMapTypeListStatic;
  VThread: TThread;
  VTasks: TCopyTaskArray;
  i: Integer;
  VTargetStoragePath: string;
  VMapType: IMapType;
  VStorageType: ITileStorageTypeListItem;
begin
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMaps := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).MapTypeList;
  VReplace := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).ReplaseTarget;
  VPlaceInSubFolder := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).PlaceInNameSubFolder;
  VDeleteSource := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).DeleteSource;
  VCacheType := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).TargetCacheType;
  if VMaps.Count > 1 then begin
    VPlaceInSubFolder := True;
  end;

  // set version options
  VSetTargetVersionEnabled := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionEnabled;
  if VSetTargetVersionEnabled then begin
    VSetTargetVersionValue := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionValue
  end else begin
    VSetTargetVersionValue := '';
  end;

  SetLength(VTasks, VMaps.Count);
  for i := 0 to VMaps.Count - 1 do begin
    VMapType := VMaps.Items[i];
    VTasks[i].FSource := VMapType.TileStorage;
    VTasks[i].FSourceVersion := VMapType.VersionRequestConfig.GetStatic;
    VTargetStoragePath := IncludeTrailingPathDelimiter(VPath);
    if VPlaceInSubFolder then begin
      VTargetStoragePath := IncludeTrailingPathDelimiter(VPath + VMapType.GetShortFolderName);
    end;
    VStorageType := FTileStorageTypeList.GetItemByCode(VCacheType);
    if Assigned(VStorageType) then begin
      VTasks[i].FTarget :=
        VStorageType.StorageType.BuildStorage(
          nil,
          VTasks[i].FSource.CoordConverter,
          VMapType.ContentType,
          VTargetStoragePath,
          nil
        );
    end;
    if VSetTargetVersionEnabled then begin
      VTasks[i].FTargetVersionForce := VMapType.VersionRequestConfig.VersionFactory.GetStatic.CreateByStoreString(VSetTargetVersionValue);
    end else begin
      VTasks[i].FTargetVersionForce := nil;
    end;
  end;

  VProgressInfo := ProgressFactory.Build(APolygon);
  VThread :=
    TThreadCopyFromStorageToStorage.Create(
      VProgressInfo,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      APolygon,
      VTasks,
      VZoomArr,
      True,
      VDeleteSource,
      VReplace
    );
  if Assigned(VThread) then begin
    VThread.Resume;
  end;
end;

end.


