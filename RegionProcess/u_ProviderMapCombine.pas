unit u_ProviderMapCombine;

interface

uses
  Windows,
  Forms,
  GR32,
  t_GeoTypes,
  i_LanguageManager,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_BitmapLayerProvider,
  i_VectorItemProjected,
  i_VectorItemLonLat,
  i_ProjectedGeometryProvider,
  i_MapTypes,
  i_UseTilePrevZoomConfig,
  i_Bitmap32StaticFactory,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessing,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapCalibration,
  i_VectorItemsFactory,
  i_GlobalViewMainConfig,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapCombine;

type
  TProviderMapCombineBase = class(TExportProviderAbstract)
  private
    FDefaultExt: string;
    FFormatName: string;
    FUseQuality: Boolean;
    FUseAlfa: Boolean;
    FViewConfig: IGlobalViewMainConfig;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FBitmapFactory: IBitmap32StaticFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FCoordConverterList: ICoordConverterList;
    FVectorItemsFactory: IVectorItemsFactory;
    FProjectedGeometryProvider: IProjectedGeometryProvider;
    FMarksDB: IMarkSystem;
    FMarksShowConfig: IUsedMarksConfig;
    FMarksDrawConfig: IMarksDrawConfig;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FBitmapPostProcessing: IBitmapPostProcessingChangeable;
    FMapCalibrationList: IMapCalibrationList;
  protected
    function PrepareTargetFileName: string;
    function PrepareTargetConverter(
      const AProjectedPolygon: IProjectedPolygon
    ): ILocalCoordConverter;
    function PrepareImageProvider(
      const APolygon: ILonLatPolygon;
      const AProjectedPolygon: IProjectedPolygon
    ): IBitmapLayerProvider;
    function PreparePolygon(const APolygon: ILonLatPolygon): IProjectedPolygon;
    property LocalConverterFactory: ILocalCoordConverterFactorySimpe read FLocalConverterFactory;
  protected
    function CreateFrame: TFrame; override;
  public
    function GetCaption: string; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AProjectionFactory: IProjectionInfoFactory;
      const ACoordConverterList: ICoordConverterList;
      const AVectorItemsFactory: IVectorItemsFactory;
      const AProjectedGeometryProvider: IProjectedGeometryProvider;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AMapCalibrationList: IMapCalibrationList;
      const AUseQuality: Boolean;
      const AUseAlfa: Boolean;
      const ADefaultExt: string;
      const AFormatName: string
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  gnugettext,
  i_InterfaceListStatic,
  i_LonLatRect,
  i_MarkerProviderForVectorItem,
  i_VectorItemSubset,
  i_RegionProcessParamsFrame,
  i_ProjectionInfo,
  u_GeoFun,
  u_MarkerProviderForVectorItemForMarkPoints,
  u_BitmapLayerProviderByMarksSubset,
  u_BitmapLayerProviderSimpleForCombine,
  u_BitmapLayerProviderInPolygon,
  u_BitmapLayerProviderWithBGColor,
  u_ResStrings;

{ TProviderMapCombineBase }

constructor TProviderMapCombineBase.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const ACoordConverterList: ICoordConverterList;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AProjectedGeometryProvider: IProjectedGeometryProvider;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AMapCalibrationList: IMapCalibrationList;
  const AUseQuality: Boolean;
  const AUseAlfa: Boolean;
  const ADefaultExt: string;
  const AFormatName: string
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FMapCalibrationList := AMapCalibrationList;
  FViewConfig := AViewConfig;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;
  FMarksShowConfig := AMarksShowConfig;
  FMarksDrawConfig := AMarksDrawConfig;
  FMarksDB := AMarksDB;
  FLocalConverterFactory := ALocalConverterFactory;
  FBitmapPostProcessing := ABitmapPostProcessing;
  FBitmapFactory := ABitmapFactory;
  FProjectionFactory := AProjectionFactory;
  FCoordConverterList := ACoordConverterList;
  FVectorItemsFactory := AVectorItemsFactory;
  FProjectedGeometryProvider := AProjectedGeometryProvider;
  FUseQuality := AUseQuality;
  FUseAlfa := AUseAlfa;
  FDefaultExt := ADefaultExt;
  FFormatName := AFormatName;
end;

function TProviderMapCombineBase.CreateFrame: TFrame;
begin
  Result :=
    TfrMapCombine.Create(
      Self.LanguageManager,
      FProjectionFactory,
      FCoordConverterList,
      FVectorItemsFactory,
      FBitmapFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      FViewConfig,
      FUseTilePrevZoomConfig,
      FMapCalibrationList,
      FUseQuality,
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

function TProviderMapCombineBase.PrepareImageProvider(
  const APolygon: ILonLatPolygon;
  const AProjectedPolygon: IProjectedPolygon
): IBitmapLayerProvider;
var
  VRect: ILonLatRect;
  VLonLatRect: TDoubleRect;
  VZoom: Byte;
  VGeoConverter: ICoordConverter;
  VMarksSubset: IVectorItemSubset;
  VMarksConfigStatic: IUsedMarksConfigStatic;
  VList: IInterfaceListStatic;
  VMarksImageProvider: IBitmapLayerProvider;
  VRecolorConfig: IBitmapPostProcessing;
  VSourceProvider: IBitmapLayerProvider;
  VUseMarks: Boolean;
  VUseRecolor: Boolean;
  VMarkerProvider: IMarkerProviderForVectorItem;
  VMarksDrawConfig: IMarksDrawConfigStatic;
begin
  VSourceProvider := (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider;
  VRect := APolygon.Item[0].Bounds;
  VLonLatRect := VRect.Rect;
  VGeoConverter := AProjectedPolygon.Projection.GeoConverter;
  VZoom := AProjectedPolygon.Projection.Zoom;
  VGeoConverter.CheckLonLatRect(VLonLatRect);

  VMarksSubset := nil;
  VUseMarks := (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseMarks;
  if VUseMarks then begin
    VMarksConfigStatic := FMarksShowConfig.GetStatic;
    if VMarksConfigStatic.IsUseMarks then begin
      VList := nil;
      if not VMarksConfigStatic.IgnoreCategoriesVisible then begin
        VList := FMarksDB.GetVisibleCategories(VZoom);
      end;
      try
        if (VList <> nil) and (VList.Count = 0) then begin
          VMarksSubset := nil;
        end else begin
          VMarksSubset :=
            FMarksDB.MarkDb.GetMarkSubsetByCategoryListInRect(
              VLonLatRect,
              VList,
              VMarksConfigStatic.IgnoreMarksVisible
            );
        end;
      finally
        VList := nil;
      end;
    end;
  end else begin
    VMarksSubset := nil;
  end;
  VMarksImageProvider := nil;
  if VMarksSubset <> nil then begin
    VMarksDrawConfig := FMarksDrawConfig.GetStatic;
    VMarkerProvider :=
      TMarkerProviderForVectorItemForMarkPoints.Create(
        FBitmapFactory,
        nil,
        VMarksDrawConfig
      );
    VMarksImageProvider :=
      TBitmapLayerProviderByMarksSubset.Create(
        VMarksDrawConfig,
        FVectorItemsFactory,
        FBitmapFactory,
        AProjectedPolygon.Projection,
        FProjectedGeometryProvider,
        VMarkerProvider,
        VMarksSubset
      );
  end;
  VRecolorConfig := nil;
  VUseRecolor := (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseRecolor;
  if VUseRecolor then begin
    VRecolorConfig := FBitmapPostProcessing.GetStatic;
  end;
  Result :=
    TBitmapLayerProviderSimpleForCombine.Create(
      FBitmapFactory,
      VRecolorConfig,
      VSourceProvider,
      VMarksImageProvider
    );
  Result :=
    TBitmapLayerProviderInPolygon.Create(
      AProjectedPolygon,
      Result
    );
  Result :=
    TBitmapLayerProviderWithBGColor.Create(
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).BGColor,
      FBitmapFactory,
      Result
    );
end;

function TProviderMapCombineBase.PreparePolygon(
  const APolygon: ILonLatPolygon
): IProjectedPolygon;
var
  VProjection: IProjectionInfo;
begin
  VProjection := (ParamsFrame as IRegionProcessParamsFrameTargetProjection).Projection;

  Result :=
    FVectorItemsFactory.CreateProjectedPolygonByLonLatPolygon(
      VProjection,
      APolygon
    );
end;

function TProviderMapCombineBase.PrepareTargetConverter(
  const AProjectedPolygon: IProjectedPolygon
): ILocalCoordConverter;
var
  VMapRect: TRect;
  VMapSize: TPoint;
begin
  VMapRect := RectFromDoubleRect(AProjectedPolygon.Bounds, rrOutside);
  VMapSize.X := VMapRect.Right - VMapRect.Left;
  VMapSize.Y := VMapRect.Bottom - VMapRect.Top;

  Result :=
    FLocalConverterFactory.CreateConverterNoScale(
      Rect(0, 0, VMapSize.X, VMapSize.Y),
      AProjectedPolygon.Projection.Zoom,
      AProjectedPolygon.Projection.GeoConverter,
      VMapRect.TopLeft
    );
end;

function TProviderMapCombineBase.PrepareTargetFileName: string;
var
  VMsg: string;
begin
  Result := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  if Result = '' then begin
    raise Exception.Create(_('Please, select output file first!'));
  end;
  if FileExists(Result) then begin
    VMsg := Format(SAS_MSG_FileExists, [Result]);
    if (Application.MessageBox(pchar(VMsg), pchar(SAS_MSG_coution), 36) <> IDYES) then begin
      raise EAbort.CreateFmt('File %0:s is available on disk.', [Result]);
    end;
  end;
end;

end.


