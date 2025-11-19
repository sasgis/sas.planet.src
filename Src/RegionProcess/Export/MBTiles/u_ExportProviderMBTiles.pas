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

unit u_ExportProviderMBTiles;

interface

uses
  Types,
  Forms,
  i_LanguageManager,
  i_BitmapTileProvider,
  i_BitmapTileProviderBuilder,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_TileIteratorFactory,
  i_RegionProcessProgressInfoInternalFactory,
  i_GeometryLonLat,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_MapType,
  i_MapTypeListChangeable,
  i_GlobalViewMainConfig,
  i_GeometryProjectedFactory,
  i_ProjectionSetFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportMBTiles;

type
  TExportProviderMBTiles = class(TExportProviderBase)
  private
    FActiveMapsList: IMapTypeListChangeable;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FProjectionSetFactory: IProjectionSetFactory;
    FBitmapTileProviderBuilder: IBitmapTileProviderBuilder;
    FViewConfig: IGlobalViewMainConfig;
    FGeometryProjectedFactory: IGeometryProjectedFactory;
    function PrepareBitmapTileProviders(
      const AMapType: IMapType;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray
    ): TBitmapTileProviderDynArray;
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
      const AActiveMapsList: IMapTypeListChangeable;
      const ATileIteratorFactory: ITileIteratorFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AViewConfig: IGlobalViewMainConfig;
      const AGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapTileProviderBuilder: IBitmapTileProviderBuilder
    );
  end;

implementation

uses
  SysUtils,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_RegionProcessParamsFrame,
  i_TileStorage,
  i_MapVersionRequest,
  u_ExportTaskToMBTiles,
  u_ResStrings;

{ TExportProviderMBTiles }

constructor TExportProviderMBTiles.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsList: IMapTypeListChangeable;
  const ATileIteratorFactory: ITileIteratorFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AViewConfig: IGlobalViewMainConfig;
  const AGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapTileProviderBuilder: IBitmapTileProviderBuilder
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder,
    ATileIteratorFactory
  );
  FActiveMapsList := AActiveMapsList;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FProjectionSetFactory := AProjectionSetFactory;
  FViewConfig := AViewConfig;
  FGeometryProjectedFactory := AGeometryProjectedFactory;
  FBitmapTileProviderBuilder := ABitmapTileProviderBuilder;
end;

function TExportProviderMBTiles.CreateFrame: TFrame;
begin
  Result :=
    TfrExportMBTiles.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      FActiveMapsList,
      FBitmap32StaticFactory,
      FBitmapTileSaveLoadFactory
    );

  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameImageProvider));
  Assert(Supports(Result, IRegionProcessParamsFrameMBTilesExport));
end;

function TExportProviderMBTiles.GetCaption: string;
begin
  Result := SAS_STR_ExportMBTilesExportCaption;
end;

function TExportProviderMBTiles.PrepareBitmapTileProviders(
  const AMapType: IMapType;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray
): TBitmapTileProviderDynArray;
var
  I: Integer;
  VParams: IRegionProcessParamsFrameMBTilesExport;
  VUniProvider: IBitmapTileUniProvider;
begin
  VUniProvider := (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider;
  if VUniProvider = nil then begin
    Result := nil;
    Exit;
  end;
  VParams := ParamsFrame as IRegionProcessParamsFrameMBTilesExport;
  SetLength(Result, Length(AZoomArr));
  for I := 0 to Length(AZoomArr) - 1 do begin
    Result[I] :=
      FBitmapTileProviderBuilder.Build(
        VParams.UseMarks,
        VParams.UseRecolor,
        VParams.UseFillingMap,
        VParams.UseGrids,
        VParams.UsePreciseCropping,
        FViewConfig.BackGroundColor,
        FViewConfig.BackGroundColor,
        VUniProvider,
        APolygon,
        AMapType.TileStorage.ProjectionSet.Zooms[AZoomArr[I]]
      );
  end;
end;

function TExportProviderMBTiles.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VDirectTilesCopy: Boolean;
  VBitmapTileSaver: IBitmapTileSaver;
  VMapType: IMapType;
  VMapVersion: IMapVersionRequest;
  VTileStorage: ITileStorage;
  VName, VDesc, VAttr, VImgFormat: string;
  VIsLayer, VUseXYZScheme: Boolean;
  VForceDropTarget, VReplaceExistingTiles: Boolean;
  VMakeTileMillCompatibility: Boolean;
  VBitmapTileProviderArr: TBitmapTileProviderDynArray;
begin
  inherited;

  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VBitmapTileProviderArr := PrepareBitmapTileProviders(VMapType, APolygon, VZoomArr);

  VTileStorage := nil;
  VMapVersion := nil;
  if Assigned(VMapType) then begin
    VMapVersion := VMapType.VersionRequest.GetStatic;
    VTileStorage := VMapType.TileStorage;
  end;

  with (ParamsFrame as IRegionProcessParamsFrameMBTilesExport) do begin
    VForceDropTarget := ForceDropTarget;
    VReplaceExistingTiles := not VForceDropTarget and ReplaceExistingTiles;
    VDirectTilesCopy := DirectTilesCopy;
    VName := Name;
    VDesc := Description;
    VAttr := Attribution;
    VIsLayer := IsLayer;
    VUseXYZScheme := UseXYZScheme;
    VMakeTileMillCompatibility := MakeTileMillCompatibility;
    GetBitmapTileSaver(VBitmapTileSaver, VImgFormat);
  end;

  Result :=
    TExportTaskToMBTiles.Create(
      AProgressInfo,
      VPath,
      Self.TileIteratorFactory,
      FProjectionSetFactory,
      APolygon,
      VZoomArr,
      VTileStorage,
      VMapVersion,
      VBitmapTileSaver,
      VBitmapTileProviderArr,
      VDirectTilesCopy,
      VUseXYZScheme,
      VMakeTileMillCompatibility,
      VName,
      VDesc,
      VAttr,
      VIsLayer,
      VImgFormat,
      VForceDropTarget,
      VReplaceExistingTiles
    );
end;

end.
