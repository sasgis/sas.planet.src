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

unit u_ProviderTilesCopy;

interface

uses
  Types,
  Forms,
  i_NotifierTime,
  i_LanguageManager,
  i_GeometryLonLat,
  i_MapType,
  i_MapTypeSet,
  i_MapTypeListBuilder,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ContentTypeManager,
  i_TileIteratorFactory,
  i_TileStorage,
  i_TileStorageTypeList,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_MapTypeListStatic,
  i_MapTypeListChangeable,
  i_GlobalViewMainConfig,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  i_BitmapTileProvider,
  i_BitmapTileProviderBuilder,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  u_ExportProviderAbstract,
  fr_MapSelect;

type
  TProviderTilesCopy = class(TExportProviderBase)
  private
    FActiveMapsList: IMapTypeListChangeable;
    FMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
    FContentTypeManager: IContentTypeManager;
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FMainMapConfig: IActiveMapConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FViewConfig: IGlobalViewMainConfig;
    FBitmapTileProviderBuilder: IBitmapTileProviderBuilder;
  private
    function PrepareDirectCopy(
      const APolygon: IGeometryLonLatPolygon;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AMaps: IMapTypeListStatic;
      const ACacheType: Byte;
      const AZoomArr: TByteDynArray;
      const ADeleteSource, AReplace: Boolean
    ): IRegionProcessTask;
    function PrepareBitmapTileProviders(
      const AMapType: IMapType;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray
    ): TBitmapTileProviderDynArray;
    function PrepareModification(
      const APolygon: IGeometryLonLatPolygon;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ACacheType: Byte;
      const AZoomArr: TByteDynArray;
      const ADeleteSource, AReplace: Boolean
    ): IRegionProcessTask;
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    function PrepareTask(
      const APolygon: IGeometryLonLatPolygon;
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IRegionProcessTask; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const ATileIteratorFactory: ITileIteratorFactory;
      const AActiveMapsList: IMapTypeListChangeable;
      const AMainMapConfig: IActiveMapConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AContentTypeManager: IContentTypeManager;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AViewConfig: IGlobalViewMainConfig;
      const ABitmapTileProviderBuilder: IBitmapTileProviderBuilder
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  gnugettext,
  i_BitmapLayerProvider,
  i_TileStorageAbilities,
  i_TileStorageTypeListItem,
  i_RegionProcessParamsFrame,
  u_ThreadCopyFromStorageToStorage,
  u_ThreadCopyWithModification,
  fr_TilesCopy;

{ TProviderTilesCopy }

constructor TProviderTilesCopy.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const ATileIteratorFactory: ITileIteratorFactory;
  const AActiveMapsList: IMapTypeListChangeable;
  const AMainMapConfig: IActiveMapConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AContentTypeManager: IContentTypeManager;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AViewConfig: IGlobalViewMainConfig;
  const ABitmapTileProviderBuilder: IBitmapTileProviderBuilder
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder,
    ATileIteratorFactory
  );
  FActiveMapsList := AActiveMapsList;
  FMainMapConfig := AMainMapConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FMapTypeListBuilderFactory := AMapTypeListBuilderFactory;
  FContentTypeManager := AContentTypeManager;
  FTileStorageTypeList := ATileStorageTypeList;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FViewConfig := AViewConfig;
  FBitmapTileProviderBuilder := ABitmapTileProviderBuilder;
end;

function TProviderTilesCopy.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesCopy.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      FActiveMapsList,
      FMapTypeListBuilderFactory,
      FMainMapConfig,
      FFullMapsSet,
      FGUIConfigList,
      FTileStorageTypeList,
      FBitmap32StaticFactory,
      FBitmapTileSaveLoadFactory,
      FContentTypeManager
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameTilesCopy));
end;

function TProviderTilesCopy.GetCaption: string;
begin
  Result := _('Copy');
end;

function TProviderTilesCopy.PrepareDirectCopy(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AMaps: IMapTypeListStatic;
  const ACacheType: Byte;
  const AZoomArr: TByteDynArray;
  const ADeleteSource, AReplace: Boolean
): IRegionProcessTask;
var
  I: Integer;
  VTasks: TCopyTaskArray;
  VPlaceInSubFolder: Boolean;
  VSetTargetVersionEnabled: Boolean;
  VSetTargetVersionValue: string;
  VTargetStoragePath: string;
  VMapType: IMapType;
  VStorageType: ITileStorageTypeListItem;
  VPath: string;
begin
  VPlaceInSubFolder := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).PlaceInNameSubFolder;
  if AMaps.Count > 1 then begin
    VPlaceInSubFolder := True;
  end;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  if VPlaceInSubFolder then begin
    VPath := IncludeTrailingPathDelimiter(VPath);
  end;

  // set version options
  VSetTargetVersionEnabled := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionEnabled;
  if VSetTargetVersionEnabled then begin
    VSetTargetVersionValue := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionValue;
  end else begin
    VSetTargetVersionValue := '';
  end;
  VStorageType := FTileStorageTypeList.GetItemByCode(ACacheType);

  SetLength(VTasks, AMaps.Count);
  for I := 0 to AMaps.Count - 1 do begin
    VMapType := AMaps.Items[I];
    VTasks[I].FSource := VMapType.TileStorage;
    VTasks[I].FSourceVersion := VMapType.VersionRequest.GetStatic;
    if VPlaceInSubFolder then begin
      VTargetStoragePath := VPath + VMapType.GetShortFolderName;
    end else begin
      VTargetStoragePath := VPath;
    end;
    if VStorageType.StorageType.Abilities.StorageClass in [tstcFolder, tstcInSeparateFiles] then begin
      VTargetStoragePath := IncludeTrailingPathDelimiter(VTargetStoragePath);
    end;
    if Assigned(VStorageType) then begin
      VTasks[I].FTarget :=
        VStorageType.StorageType.BuildStorage(
          nil,
          VTasks[I].FSource.ProjectionSet,
          VMapType.ContentType,
          nil,
          VTargetStoragePath,
          nil
        );
    end;
    if VSetTargetVersionEnabled then begin
      VTasks[I].FTargetVersionForce := VMapType.VersionFactory.GetStatic.CreateByStoreString(VSetTargetVersionValue);
    end else begin
      VTasks[I].FTargetVersionForce := nil;
    end;
  end;

  Result :=
    TThreadCopyFromStorageToStorage.Create(
      AProgressInfo,
      Self.TileIteratorFactory,
      APolygon,
      VTasks,
      AZoomArr,
      True,
      ADeleteSource,
      AReplace
    );
end;

function TProviderTilesCopy.PrepareBitmapTileProviders(
  const AMapType: IMapType;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray
): TBitmapTileProviderDynArray;
var
  I: Integer;
  VUniProvider: IBitmapTileUniProvider;
begin
  VUniProvider := (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider;
  if VUniProvider = nil then begin
    Result := nil;
    Exit;
  end;
  SetLength(Result, Length(AZoomArr));
  for I := 0 to Length(AZoomArr) - 1 do begin
    Result[I] :=
      FBitmapTileProviderBuilder.Build(
        (ParamsFrame as IRegionProcessParamsFrameTilesCopy).UseMarks,
        (ParamsFrame as IRegionProcessParamsFrameTilesCopy).UseRecolor,
        (ParamsFrame as IRegionProcessParamsFrameTilesCopy).UseFillingMap,
        (ParamsFrame as IRegionProcessParamsFrameTilesCopy).UseGrids,
        (ParamsFrame as IRegionProcessParamsFrameTilesCopy).UsePreciseCropping,
        FViewConfig.BackGroundColor,
        FViewConfig.BackGroundColor,
        VUniProvider,
        APolygon,
        AMapType.TileStorage.ProjectionSet.Zooms[AZoomArr[I]]
      );
  end;
end;

function TProviderTilesCopy.PrepareModification(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ACacheType: Byte;
  const AZoomArr: TByteDynArray;
  const ADeleteSource, AReplace: Boolean
): IRegionProcessTask;
var
  VSetTargetVersionEnabled: Boolean;
  VSetTargetVersionValue: string;
  VStorageType: ITileStorageTypeListItem;
  VTarget: ITileStorage;
  VTargetVersionForce: IMapVersionInfo;
  VMapType: IMapType;
  VContentType: IContentTypeInfoBasic;
  VPath: string;
  VBitmapTileProviderArr: TBitmapTileProviderDynArray;
begin
  VMapType := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).MapSource;
  VContentType := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).ContentType;
  VPath := IncludeTrailingPathDelimiter((ParamsFrame as IRegionProcessParamsFrameTargetPath).Path);

  VStorageType := FTileStorageTypeList.GetItemByCode(ACacheType);
  if Assigned(VStorageType) then begin
    VTarget :=
      VStorageType.StorageType.BuildStorage(
        nil,
        VMapType.TileStorage.ProjectionSet,
        VContentType,
        nil,
        VPath,
        nil
      );
  end;

  // set version options
  VSetTargetVersionEnabled := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionEnabled;
  if VSetTargetVersionEnabled then begin
    VSetTargetVersionValue := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionValue;
  end else begin
    VSetTargetVersionValue := '';
  end;

  if VSetTargetVersionEnabled then begin
    VTargetVersionForce := VMapType.VersionFactory.GetStatic.CreateByStoreString(VSetTargetVersionValue);
  end else begin
    VTargetVersionForce := nil;
  end;

  VBitmapTileProviderArr := PrepareBitmapTileProviders(VMapType, APolygon, AZoomArr);

  Result :=
    TThreadCopyWithModification.Create(
      AProgressInfo,
      Self.TileIteratorFactory,
      APolygon,
      VTarget,
      VTargetVersionForce,
      VMapType,
      (ParamsFrame as IRegionProcessParamsFrameTilesCopy).Overlay,
      VBitmapTileProviderArr,
      (ParamsFrame as IRegionProcessParamsFrameTilesCopy).BitmapTileSaver,
      AZoomArr,
      VContentType,
      True,
      AReplace
    );
end;

function TProviderTilesCopy.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VCacheType: Byte;
  VZoomArr: TByteDynArray;
  VDeleteSource: Boolean;
  VReplace: Boolean;
  VMaps: IMapTypeListStatic;
begin
  VMaps := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).MapTypeList;
  VCacheType := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).TargetCacheType;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VDeleteSource := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).DeleteSource;
  VReplace := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).ReplaseTarget;

  if VMaps <> nil then begin
    Result := PrepareDirectCopy(APolygon, AProgressInfo, VMaps, VCacheType, VZoomArr, VDeleteSource, VReplace);
  end else begin
    Result := PrepareModification(APolygon, AProgressInfo, VCacheType, VZoomArr, VDeleteSource, VReplace);
  end;
end;

end.
