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

unit u_ProviderMapCombine;

interface

uses
  Windows,
  Forms,
  t_GeoTypes,
  i_LanguageManager,
  i_ProjectionSetList,
  i_ProjectionSetChangeable,
  i_BitmapLayerProvider,
  i_BitmapTileProvider,
  i_Projection,
  i_GeometryProjected,
  i_GeometryLonLat,
  i_GeometryProjectedProvider,
  i_VectorItemSubsetBuilder,
  i_UseTilePrevZoomConfig,
  i_Bitmap32BufferFactory,
  i_BitmapPostProcessing,
  i_MapLayerGridsConfig,
  i_ValueToStringConverter,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapCalibration,
  i_GeometryProjectedFactory,
  i_GlobalViewMainConfig,
  i_MapTypeListChangeable,
  i_BitmapMapCombiner,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_MapCombine;

type
  TProviderMapCombineBase = class(TExportProviderAbstract)
  private
    FDefaultExt: string;
    FFormatName: string;
    FUseQuality: Boolean;
    FUseExif: Boolean;
    FUseAlfa: Boolean;
    FViewConfig: IGlobalViewMainConfig;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FBitmapFactory: IBitmap32StaticFactory;
    FProjectionSet: IProjectionSetChangeable;
    FProjectionSetList: IProjectionSetList;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FProjectedGeometryProvider: IGeometryProjectedProvider;
    FVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FMarksDB: IMarkSystem;
    FMarksShowConfig: IUsedMarksConfig;
    FMarksDrawConfig: IMarksDrawConfig;
    FActiveMapsSet: IMapTypeListChangeable;
    FBitmapPostProcessing: IBitmapPostProcessingChangeable;
    FMapCalibrationList: IMapCalibrationList;
    FGridsConfig: IMapLayerGridsConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FMinPartSize: TPoint;
    FMaxPartSize: TPoint;
    function PrepareGridsProvider: IBitmapTileUniProvider;
  protected
    function PrepareTargetFileName: string;
    function PrepareTargetRect(
      const AProjection: IProjection;
      const APolygon: IGeometryProjectedPolygon
    ): TRect;
    function PrepareImageProvider(
      const APolygon: IGeometryLonLatPolygon;
      const AProjection: IProjection;
      const AProjectedPolygon: IGeometryProjectedPolygon
    ): IBitmapTileProvider;
    function PrepareProjection: IProjection;
    function PreparePolygon(
      const AProjection: IProjection;
      const APolygon: IGeometryLonLatPolygon
    ): IGeometryProjectedPolygon;
  protected
    function CreateFrame: TFrame; override;
    function PrepareMapCombiner(
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IBitmapMapCombiner; virtual; abstract;
  protected
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsSet: IMapTypeListChangeable;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AProjectionSet: IProjectionSetChangeable;
      const AProjectionSetList: IProjectionSetList;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AGridsConfig: IMapLayerGridsConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AMapCalibrationList: IMapCalibrationList;
      const AMinPartSize: TPoint;
      const AMaxPartSize: TPoint;
      const AUseQuality: Boolean;
      const AUseExif: Boolean;
      const AUseAlfa: Boolean;
      const ADefaultExt: string;
      const AFormatName: string
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  Types,
  Math,
  gnugettext,
  t_Bitmap32,
  i_LonLatRect,
  i_MarkCategoryList,
  i_MarkerProviderForVectorItem,
  i_VectorItemSubset,
  i_VectorTileProvider,
  i_VectorTileRenderer,
  i_RegionProcessParamsFrame,
  u_GeoFunc,
  u_ThreadMapCombineBase,
  u_MarkerProviderForVectorItemForMarkPoints,
  u_VectorTileProviderByFixedSubset,
  u_VectorTileRendererForMarks,
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

{ TProviderMapCombineBase }

constructor TProviderMapCombineBase.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsSet: IMapTypeListChangeable;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AProjectionSet: IProjectionSetChangeable;
  const AProjectionSetList: IProjectionSetList;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AGridsConfig: IMapLayerGridsConfig;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AMapCalibrationList: IMapCalibrationList;
  const AMinPartSize: TPoint;
  const AMaxPartSize: TPoint;
  const AUseQuality: Boolean;
  const AUseExif: Boolean;
  const AUseAlfa: Boolean;
  const ADefaultExt: string;
  const AFormatName: string
);
begin
  Assert(AMinPartSize.X <= AMaxPartSize.X);
  Assert(AMinPartSize.Y <= AMaxPartSize.Y);
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FMapCalibrationList := AMapCalibrationList;
  FViewConfig := AViewConfig;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;
  FMarksShowConfig := AMarksShowConfig;
  FMarksDrawConfig := AMarksDrawConfig;
  FMarksDB := AMarksDB;
  FActiveMapsSet := AActiveMapsSet;
  FBitmapPostProcessing := ABitmapPostProcessing;
  FBitmapFactory := ABitmapFactory;
  FProjectionSet := AProjectionSet;
  FProjectionSetList := AProjectionSetList;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FProjectedGeometryProvider := AProjectedGeometryProvider;
  FVectorSubsetBuilderFactory := AVectorSubsetBuilderFactory;
  FGridsConfig := AGridsConfig;
  FValueToStringConverter := AValueToStringConverter;
  FMinPartSize := AMinPartSize;
  FMaxPartSize := AMaxPartSize;
  FUseQuality := AUseQuality;
  FUseExif := AUseExif;
  FUseAlfa := AUseAlfa;
  FDefaultExt := ADefaultExt;
  FFormatName := AFormatName;
end;

function TProviderMapCombineBase.CreateFrame: TFrame;
begin
  Result :=
    TfrMapCombine.Create(
      Self.LanguageManager,
      FProjectionSetList,
      FVectorGeometryProjectedFactory,
      FBitmapFactory,
      Self.MapSelectFrameBuilder,
      FActiveMapsSet,
      FViewConfig,
      FUseTilePrevZoomConfig,
      FMapCalibrationList,
      FMinPartSize,
      FMaxPartSize,
      FUseQuality,
      FUseExif,
      FUseAlfa,
      FDefaultExt,
      FFormatName
    );
  Assert(Supports(Result, IRegionProcessParamsFrameImageProvider));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCalibrationList));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetProjection));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCombine));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCombineJpg));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCombineWithAlfa));
end;

function TProviderMapCombineBase.GetCaption: string;
begin
  Result := _(FFormatName);
end;

function TProviderMapCombineBase.PrepareGridsProvider: IBitmapTileUniProvider;
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
        FValueToStringConverter.GetStatic
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
  Result := VResult;
end;

function TProviderMapCombineBase.PrepareImageProvider(
  const APolygon: IGeometryLonLatPolygon;
  const AProjection: IProjection;
  const AProjectedPolygon: IGeometryProjectedPolygon
): IBitmapTileProvider;
var
  VRect: ILonLatRect;
  VLonLatRect: TDoubleRect;
  VMarksSubset: IVectorItemSubset;
  VMarksConfigStatic: IUsedMarksConfigStatic;
  VList: IMarkCategoryList;
  VMarksImageProvider: IBitmapTileProvider;
  VRecolorConfig: IBitmapPostProcessing;
  VSourceProvider: IBitmapTileUniProvider;
  VUseMarks: Boolean;
  VUseGrids: Boolean;
  VUseRecolor: Boolean;
  VVectorTileProvider: IVectorTileUniProvider;
  VVectorTileRenderer: IVectorTileRenderer;
  VMarkerProvider: IMarkerProviderForVectorItem;
  VGridsProvider: IBitmapTileProvider;
begin
  VSourceProvider := (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider;
  Result :=
    TBitmapTileProviderByBitmapTileUniProvider.Create(
      AProjection,
      VSourceProvider
    );
  VUseRecolor := (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseRecolor;
  if VUseRecolor then begin
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

  VUseMarks := (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseMarks;
  if VUseMarks then begin
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
      VMarkerProvider :=
        TMarkerProviderForVectorItemForMarkPoints.Create(
          FBitmapFactory,
          nil
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
  VUseGrids := (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseGrids;
  VGridsProvider := nil;
  if VUseGrids then begin
    VGridsProvider :=
      TBitmapTileProviderByBitmapTileUniProvider.Create(
        AProjection,
        PrepareGridsProvider
      );
    Result :=
      TBitmapTileProviderComplex.Create(
        FBitmapFactory,
        Result,
        VGridsProvider
      );
  end;

  Result :=
    TBitmapTileProviderInPolygon.Create(
      AProjectedPolygon,
      Result
    );
  Result :=
    TBitmapTileProviderWithBGColor.Create(
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).BGColor,
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).BGColor,
      FBitmapFactory,
      Result
    );
end;

function TProviderMapCombineBase.PreparePolygon(
  const AProjection: IProjection;
  const APolygon: IGeometryLonLatPolygon
): IGeometryProjectedPolygon;
begin
  Result :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      AProjection,
      APolygon
    );
end;

function TProviderMapCombineBase.PrepareProjection: IProjection;
begin
  Result := (ParamsFrame as IRegionProcessParamsFrameTargetProjection).Projection;
end;

function TProviderMapCombineBase.PrepareTargetRect(
  const AProjection: IProjection;
  const APolygon: IGeometryProjectedPolygon
): TRect;
begin
  Result := RectFromDoubleRect(APolygon.Bounds, rrOutside);
end;

procedure TProviderMapCombineBase.StartProcess(
  const APolygon: IGeometryLonLatPolygon
);
var
  VMapCalibrations: IMapCalibrationList;
  VFileName: string;
  VSplitCount: TPoint;
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VImageProvider: IBitmapTileProvider;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
  VCombiner: IBitmapMapCombiner;
begin
  VProjection := PrepareProjection;
  VProjectedPolygon := PreparePolygon(VProjection, APolygon);
  VImageProvider := PrepareImageProvider(APolygon, VProjection, VProjectedPolygon);
  VMapCalibrations := (ParamsFrame as IRegionProcessParamsFrameMapCalibrationList).MapCalibrationList;
  VFileName := PrepareTargetFileName;
  VSplitCount := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SplitCount;

  VProgressInfo := ProgressFactory.Build(APolygon);
  VCombiner := PrepareMapCombiner(VProgressInfo);
  VThread :=
    TThreadMapCombineBase.Create(
      VProgressInfo,
      APolygon,
      PrepareTargetRect(VProjection, VProjectedPolygon),
      VCombiner,
      VImageProvider,
      VMapCalibrations,
      VFileName,
      VSplitCount,
      Self.ClassName + 'Thread'
    );
  VThread.Resume;
end;

function TProviderMapCombineBase.PrepareTargetFileName: string;
begin
  Result := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  if Result = '' then begin
    raise Exception.Create(_('Please, select output file first!'));
  end;
end;

end.
