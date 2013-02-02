unit u_ProviderMapCombine;

interface

uses
  Windows,
  Forms,
  GR32,
  t_GeoTypes,
  i_NotifierTime,
  i_NotifierOperation,
  i_LanguageManager,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_BitmapLayerProvider,
  i_VectorItemProjected,
  i_VectorItemLonLat,
  i_RegionProcessProgressInfo,
  i_MapTypes,
  i_UseTilePrevZoomConfig,
  i_Bitmap32StaticFactory,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessing,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarksSystem,
  i_MapCalibration,
  i_VectorItemsFactory,
  i_GlobalViewMainConfig,
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
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
    FProjectionFactory: IProjectionInfoFactory;
    FCoordConverterList: ICoordConverterList;
    FVectorItemsFactory: IVectorItemsFactory;
    FMarksDB: IMarksSystem;
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
    procedure PrepareProcessInfo(
      out ACancelNotifier: INotifierOperation;
      out AOperationID: Integer;
      out AProgressInfo: IRegionProcessProgressInfoInternal
    );
    property LocalConverterFactory: ILocalCoordConverterFactorySimpe read FLocalConverterFactory;
  protected
    function CreateFrame: TFrame; override;
  public
    function GetCaption: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AProjectionFactory: IProjectionInfoFactory;
      const ACoordConverterList: ICoordConverterList;
      const AVectorItemsFactory: IVectorItemsFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarksSystem;
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
  i_LonLatRect,
  i_MarksSimple,
  i_MarkerProviderForVectorItem,
  i_RegionProcessParamsFrame,
  i_ProjectionInfo,
  u_GeoFun,
  u_MarkerProviderForVectorItemForMarkPoints,
  u_IdCacheSimpleThreadSafe,
  u_BitmapLayerProviderByMarksSubset,
  u_BitmapLayerProviderSimpleForCombine,
  u_BitmapLayerProviderInPolygon,
  u_BitmapLayerProviderWithBGColor,
  u_Notifier,
  u_NotifierOperation,
  u_RegionProcessProgressInfo,
  u_ResStrings,
  frm_ProgressSimple;

{ TProviderMapCombineBase }

constructor TProviderMapCombineBase.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AProjectionFactory: IProjectionInfoFactory;
  const ACoordConverterList: ICoordConverterList;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarksSystem;
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
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FMapCalibrationList := AMapCalibrationList;
  FViewConfig := AViewConfig;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FMarksShowConfig := AMarksShowConfig;
  FMarksDrawConfig := AMarksDrawConfig;
  FMarksDB := AMarksDB;
  FLocalConverterFactory := ALocalConverterFactory;
  FBitmapPostProcessing := ABitmapPostProcessing;
  FBitmapFactory := ABitmapFactory;
  FProjectionFactory := AProjectionFactory;
  FCoordConverterList := ACoordConverterList;
  FVectorItemsFactory := AVectorItemsFactory;
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
  VMarksSubset: IMarksSubset;
  VMarksConfigStatic: IUsedMarksConfigStatic;
  VList: IInterfaceList;
  VMarksImageProvider: IBitmapLayerProvider;
  VMapRect: TRect;
  VLineClipRect: TDoubleRect;
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
          VMarksSubset := FMarksDB.MarksDb.GetMarksSubset(VLonLatRect, VList, VMarksConfigStatic.IgnoreMarksVisible);
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
    VMapRect := RectFromDoubleRect(AProjectedPolygon.Bounds, rrOutside);
    VLineClipRect.Left := VMapRect.Left - 10;
    VLineClipRect.Top := VMapRect.Top - 10;
    VLineClipRect.Right := VMapRect.Right + 10;
    VLineClipRect.Bottom := VMapRect.Bottom + 10;
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
        TIdCacheSimpleThreadSafe.Create,
        VMarkerProvider,
        VLineClipRect,
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

procedure TProviderMapCombineBase.PrepareProcessInfo(
  out ACancelNotifier: INotifierOperation;
  out AOperationID: Integer;
  out AProgressInfo: IRegionProcessProgressInfoInternal
);
var
  VCancelNotifierInternal: INotifierOperationInternal;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
  ACancelNotifier := VCancelNotifierInternal;
  AOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create;
  AProgressInfo := VProgressInfo;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo
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


