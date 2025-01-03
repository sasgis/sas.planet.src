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

unit u_BitmapTileProviderBuilder;

interface

uses
  t_Bitmap32,
  t_GeoTypes,
  i_ProjectionSetChangeable,
  i_BitmapLayerProvider,
  i_BitmapTileProvider,
  i_HashFunction,
  i_Projection,
  i_GeometryProjected,
  i_GeometryLonLat,
  i_GeometryProjectedProvider,
  i_VectorItemSubsetBuilder,
  i_Bitmap32BufferFactory,
  i_BitmapPostProcessing,
  i_MapLayerGridsConfig,
  i_CoordToStringConverter,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapType,
  i_FillingMapLayerConfig,
  i_FillingMapPolygon,
  i_GeometryProjectedFactory,
  i_BitmapTileProviderBuilder,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderBuilder = class(TBaseInterfacedObject, IBitmapTileProviderBuilder)
  private
    FHashFunction: IHashFunction;
    FBitmapFactory: IBitmap32StaticFactory;
    FProjectionSet: IProjectionSetChangeable;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FProjectedGeometryProvider: IGeometryProjectedProvider;
    FVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FMarksDB: IMarkSystem;
    FMarksShowConfig: IUsedMarksConfig;
    FMarksDrawConfig: IMarksDrawConfig;
    FBitmapPostProcessing: IBitmapPostProcessingChangeable;
    FFillingMapConfig: IFillingMapLayerConfig;
    FFillingMapType: IMapTypeChangeable;
    FFillingMapPolygon: IFillingMapPolygon;
    FGridsConfig: IMapLayerGridsConfig;
    FCoordToStringConverter: ICoordToStringConverterChangeable;
    function BuildGridsProvider(const AProjection: IProjection): IBitmapTileProvider;
    function BuildFillingMapProvider(const AProjection: IProjection): IBitmapTileProvider;
  private
    { IBitmapTileProviderBuilder }
    function Build(
      const AUseMarks: Boolean;
      const AUseRecolor: Boolean;
      const AUseFillingMap: Boolean;
      const AUseGrids: Boolean;
      const AUsePreciseCropping: Boolean;
      const ABackGroundColor: TColor32;
      const AEmptyColor: TColor32;
      const ASourceProvider: IBitmapTileUniProvider;
      const APolygon: IGeometryLonLatPolygon;
      const AProjection: IProjection
    ): IBitmapTileProvider;
  public
    constructor Create(
      const AProjectionSet: IProjectionSetChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const AHashFunction: IHashFunction;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AFillingMapConfig: IFillingMapLayerConfig;
      const AFillingMapType: IMapTypeChangeable;
      const AFillingMapPolygon: IFillingMapPolygon;
      const AGridsConfig: IMapLayerGridsConfig;
      const ACoordToStringConverter: ICoordToStringConverterChangeable
    );
  end;

implementation

uses
  i_LonLatRect,
  i_InternalPerformanceCounter,
  i_MarkCategoryList,
  i_TextDrawerBasic,
  i_MarkerProviderByAppearancePointIcon,
  i_MarkerProviderForVectorItem,
  u_InternalPerformanceCounterFake,
  i_VectorItemSubset,
  i_VectorTileProvider,
  i_VectorTileRenderer,
  i_FillingMapColorer,
  i_MapVersionRequest,
  u_GeoFunc,
  u_TextDrawerBasic,
  u_MarkerProviderByAppearancePointIcon,
  u_MarkerProviderForVectorItemForMarkPoints,
  u_VectorTileProviderByFixedSubset,
  u_VectorTileRendererForMarks,
  u_FillingMapColorerSimple,
  u_BitmapLayerProviderFillingMap,
  u_BitmapLayerProviderComplex,
  u_BitmapLayerProviderGridGenshtab,
  u_BitmapLayerProviderGridDegree,
  u_BitmapLayerProviderGridTiles,
  u_BitmapTileProviderByBitmapTileUniProvider,
  u_BitmapTileProviderWithRecolor,
  u_BitmapTileProviderByVectorTileProvider,
  u_BitmapTileProviderComplex,
  u_BitmapTileProviderInPolygon,
  u_BitmapTileProviderWithBGColor;

{ TBitmapTileProviderBuilder }

constructor TBitmapTileProviderBuilder.Create(
  const AProjectionSet: IProjectionSetChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const AHashFunction: IHashFunction;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AFillingMapConfig: IFillingMapLayerConfig;
  const AFillingMapType: IMapTypeChangeable;
  const AFillingMapPolygon: IFillingMapPolygon;
  const AGridsConfig: IMapLayerGridsConfig;
  const ACoordToStringConverter: ICoordToStringConverterChangeable
);
begin
  inherited Create;

  FMarksShowConfig := AMarksShowConfig;
  FMarksDrawConfig := AMarksDrawConfig;
  FMarksDB := AMarksDB;
  FBitmapPostProcessing := ABitmapPostProcessing;
  FHashFunction := AHashFunction;
  FBitmapFactory := ABitmapFactory;
  FProjectionSet := AProjectionSet;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FProjectedGeometryProvider := AProjectedGeometryProvider;
  FVectorSubsetBuilderFactory := AVectorSubsetBuilderFactory;
  FFillingMapConfig := AFillingMapConfig;
  FFillingMapType := AFillingMapType;
  FFillingMapPolygon := AFillingMapPolygon;
  FGridsConfig := AGridsConfig;
  FCoordToStringConverter := ACoordToStringConverter;
end;

function TBitmapTileProviderBuilder.BuildFillingMapProvider(
  const AProjection: IProjection
): IBitmapTileProvider;
var
  VConfig: IFillingMapLayerConfigStatic;
  VResult: IBitmapTileUniProvider;
  VMap: IMapType;
  VColorer: IFillingMapColorer;
  VVersionRequest: IMapVersionRequest;
begin
  VResult := nil;
  VConfig := FFillingMapConfig.GetStatic;
  if VConfig.Visible then begin
    VMap := FFillingMapType.GetStatic;
    VVersionRequest := VMap.VersionRequest.GetStatic;
    VColorer :=
      TFillingMapColorerSimple.Create(
        VConfig.NoTileColor,
        VConfig.ShowTNE,
        VConfig.TNEColor,
        VConfig.FillMode,
        VConfig.FilterMode,
        VConfig.FillFirstDay,
        VConfig.FillLastDay
      );
    VResult :=
      TBitmapLayerProviderFillingMap.Create(
        FBitmapFactory,
        FVectorGeometryProjectedFactory,
        VMap.TileStorage,
        VVersionRequest,
        VConfig.UseRelativeZoom,
        VConfig.Zoom,
        FFillingMapPolygon.Polygon,
        VColorer
      );
    Result :=
      TBitmapTileProviderByBitmapTileUniProvider.Create(
        AProjection,
        VResult
      );
  end;
end;

function TBitmapTileProviderBuilder.BuildGridsProvider(
  const AProjection: IProjection
): IBitmapTileProvider;
var
  VVisible: Boolean;
  VColor: TColor32;
  VUseRelativeZoom: Boolean;
  VZoom: Integer;
  VShowText: Boolean;
  VShowLines: Boolean;
  VScale: Integer;
  VScaleDegree: Double;
  VProvider: IBitmapTileUniProvider;
  VResult: IBitmapTileUniProvider;
begin
  Result := nil;
  VResult := nil;
  FGridsConfig.TileGrid.LockRead;
  try
    VVisible := FGridsConfig.TileGrid.Visible;
    VColor := FGridsConfig.TileGrid.GridColor;
    VUseRelativeZoom := FGridsConfig.TileGrid.UseRelativeZoom;
    VZoom := FGridsConfig.TileGrid.Zoom;
    VShowText := FGridsConfig.TileGrid.ShowText;
    VShowLines := True;
  finally
    FGridsConfig.TileGrid.UnlockRead;
  end;
  if VVisible then begin
    VResult :=
      TBitmapLayerProviderGridTiles.Create(
        FBitmapFactory,
        FProjectionSet.GetStatic,
        VColor,
        VUseRelativeZoom,
        VZoom,
        VShowText,
        VShowLines
      );
  end;
  FGridsConfig.GenShtabGrid.LockRead;
  try
    VVisible := FGridsConfig.GenShtabGrid.Visible;
    VColor := FGridsConfig.GenShtabGrid.GridColor;
    VScale := FGridsConfig.GenShtabGrid.Scale;
    VShowText := FGridsConfig.GenShtabGrid.ShowText;
    VShowLines := True;
  finally
    FGridsConfig.GenShtabGrid.UnlockRead;
  end;
  if VVisible then begin
    VProvider :=
      TBitmapLayerProviderGridGenshtab.Create(
        FBitmapFactory,
        VColor,
        VScale,
        VShowText,
        VShowLines
      );

    if VResult <> nil then begin
      VResult :=
        TBitmapLayerProviderComplex.Create(
          FBitmapFactory,
          VResult,
          VProvider
        );
    end else begin
      VResult := VProvider;
    end;
  end;
  FGridsConfig.DegreeGrid.LockRead;
  try
    VVisible := FGridsConfig.DegreeGrid.Visible;
    VColor := FGridsConfig.DegreeGrid.GridColor;
    VScaleDegree := FGridsConfig.DegreeGrid.Scale;
    VShowText := FGridsConfig.DegreeGrid.ShowText;
    VShowLines := True;
  finally
    FGridsConfig.DegreeGrid.UnlockRead;
  end;
  if VVisible then begin
    VProvider :=
      TBitmapLayerProviderGridDegree.Create(
        FBitmapFactory,
        VColor,
        VScaleDegree,
        VShowText,
        VShowLines,
        FCoordToStringConverter.GetStatic
      );
    if VResult <> nil then begin
      VResult :=
        TBitmapLayerProviderComplex.Create(
          FBitmapFactory,
          VResult,
          VProvider
        );
    end else begin
      VResult := VProvider;
    end;
  end;
  if Assigned(VResult) then begin
    Result :=
      TBitmapTileProviderByBitmapTileUniProvider.Create(
        AProjection,
        VResult
      );
  end;
end;

function TBitmapTileProviderBuilder.Build(
  const AUseMarks: Boolean;
  const AUseRecolor: Boolean;
  const AUseFillingMap: Boolean;
  const AUseGrids: Boolean;
  const AUsePreciseCropping: Boolean;
  const ABackGroundColor: TColor32;
  const AEmptyColor: TColor32;
  const ASourceProvider: IBitmapTileUniProvider;
  const APolygon: IGeometryLonLatPolygon;
  const AProjection: IProjection
): IBitmapTileProvider;
var
  VRect: ILonLatRect;
  VLonLatRect: TDoubleRect;
  VMarksSubset: IVectorItemSubset;
  VPerf: IInternalPerformanceCounterList;
  VMarksConfigStatic: IUsedMarksConfigStatic;
  VTextDrawerBasic: ITextDrawerBasic;
  VIconProvider :IMarkerProviderByAppearancePointIcon;
  VList: IMarkCategoryList;
  VMarksImageProvider: IBitmapTileProvider;
  VRecolorConfig: IBitmapPostProcessing;
  VVectorTileProvider: IVectorTileUniProvider;
  VVectorTileRenderer: IVectorTileRenderer;
  VMarkerProvider: IMarkerProviderForVectorItem;
  VGridsProvider: IBitmapTileProvider;
  VFillingMapProvider: IBitmapTileProvider;
  VProjectedPolygon: IGeometryProjectedPolygon;
begin
  Result :=
    TBitmapTileProviderByBitmapTileUniProvider.Create(
      AProjection,
      ASourceProvider
    );

  if AUseRecolor then begin
    VRecolorConfig := FBitmapPostProcessing.GetStatic;
    Result :=
      TBitmapTileProviderWithRecolor.Create(
        VRecolorConfig,
        Result
      );
  end;

  VRect := APolygon.Bounds;
  VLonLatRect := VRect.Rect;
  AProjection.ProjectionType.ValidateLonLatRect(VLonLatRect);

  if AUseMarks then begin
    VMarksSubset := nil;
    VMarksConfigStatic := FMarksShowConfig.GetStatic;
    if VMarksConfigStatic.IsUseMarks then begin
      VList := nil;
      if not VMarksConfigStatic.IgnoreCategoriesVisible then begin
        VList := FMarksDB.CategoryDB.GetVisibleCategories(AProjection.Zoom);
      end;
      try
        if (VList <> nil) and (VList.Count = 0) then begin
          VMarksSubset := nil;
        end else begin
          VMarksSubset :=
            FMarksDB.MarkDb.GetMarkSubsetByCategoryListInRect(
              VLonLatRect,
              VList,
              VMarksConfigStatic.IgnoreMarksVisible,
              DoublePoint(0, 0) // ToDo
            );
        end;
      finally
        VList := nil;
      end;
    end;
    if VMarksSubset <> nil then begin
      VPerf := TInternalPerformanceCounterFake.Create;
      VTextDrawerBasic :=
        TTextDrawerBasic.Create(
          VPerf,
          FHashFunction,
          FBitmapFactory,
          512,
          True,
          FMarksDrawConfig.CaptionDrawConfig.FontName
        );
      VIconProvider :=
        TMarkerProviderByAppearancePointIcon.Create(
          VPerf,
          FHashFunction,
          FBitmapFactory,
          nil
        );

      VMarkerProvider :=
        TMarkerProviderForVectorItemForMarkPoints.Create(
          VTextDrawerBasic,
          VIconProvider
        );
      VVectorTileRenderer :=
        TVectorTileRendererForMarks.Create(
          FMarksDrawConfig.CaptionDrawConfig.GetStatic,
          FBitmapFactory,
          FProjectedGeometryProvider,
          VMarkerProvider
        );
      VVectorTileProvider :=
        TVectorTileProviderByFixedSubset.Create(
          FVectorSubsetBuilderFactory,
          FMarksDrawConfig.DrawOrderConfig.GetStatic.OverSizeRect,
          VMarksSubset
        );
      VMarksImageProvider :=
        TBitmapTileProviderByVectorTileProvider.Create(
          AProjection,
          VVectorTileProvider,
          VVectorTileRenderer
        );
      Result :=
        TBitmapTileProviderComplex.Create(
          FBitmapFactory,
          Result,
          VMarksImageProvider
        );
    end;
  end;

  if AUseFillingMap then begin
    VFillingMapProvider := BuildFillingMapProvider(AProjection);
    if Assigned(VFillingMapProvider) then begin
      Result :=
        TBitmapTileProviderComplex.Create(
          FBitmapFactory,
          Result,
          VFillingMapProvider
        );
    end;
  end;

  if AUseGrids then begin
    VGridsProvider := BuildGridsProvider(AProjection);
    if Assigned(VGridsProvider) then begin
      Result :=
        TBitmapTileProviderComplex.Create(
          FBitmapFactory,
          Result,
          VGridsProvider
        );
    end;
  end;

  VProjectedPolygon :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      AProjection,
      APolygon
    );

  Result :=
    TBitmapTileProviderInPolygon.Create(
      VProjectedPolygon,
      Result
    );

  Result :=
    TBitmapTileProviderWithBGColor.Create(
      AUsePreciseCropping,
      ABackGroundColor,
      AEmptyColor,
      VProjectedPolygon,
      FBitmapFactory,
      Result
    );
end;

end.
