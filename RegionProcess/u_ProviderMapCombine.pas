unit u_ProviderMapCombine;

interface

uses
  Windows,
  Forms,
  t_GeoTypes,
  i_Notifier, 
  i_NotifierOperation,
  i_LanguageManager,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_CoordConverterFactory,
  i_BitmapLayerProvider,
  i_VectorItemProjected,
  i_VectorItemLonLat,
  i_RegionProcessProgressInfo,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessingConfig,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MapCalibration,
  i_VectorItmesFactory,
  i_GlobalViewMainConfig,
  u_ExportProviderAbstract,
  u_MarksSystem,
  fr_MapCombine;

type
  TProviderMapCombine = class(TExportProviderAbstract)
  private
    FViewConfig: IGlobalViewMainConfig;
    FAppClosingNotifier: INotifier;
    FTimerNoifier: INotifier;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FMarksDB: TMarksSystem;
    FMarksShowConfig: IUsedMarksConfig;
    FMarksDrawConfig: IMarksDrawConfig;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
    FMapCalibrationList: IMapCalibrationList;
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
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AAppClosingNotifier: INotifier;
      const ATimerNoifier: INotifier;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      AMarksDB: TMarksSystem;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
      const AMapCalibrationList: IMapCalibrationList
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  Classes,
  Dialogs,
  SysUtils,
  gnugettext,
  i_LonLatRect,
  i_MarksSimple,
  i_RegionProcessParamsFrame,
  i_ProjectionInfo,
  u_GeoFun,
  u_IdCacheSimpleThreadSafe,
  u_BitmapLayerProviderByMarksSubset,
  u_BitmapLayerProviderSimpleForCombine,
  u_BitmapLayerProviderInPolygon,
  u_BitmapLayerProviderWithBGColor,
  u_ThreadMapCombineBMP,
  u_ThreadMapCombineECW,
  u_ThreadMapCombineJPG,
  u_ThreadMapCombineKMZ,
  u_ThreadMapCombinePNG,
  u_NotifierOperation,
  u_RegionProcessProgressInfo,
  u_ResStrings,
  frm_ProgressSimple;

{ TProviderTilesDelete }

constructor TProviderMapCombine.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AAppClosingNotifier: INotifier;
  const ATimerNoifier: INotifier;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  AMarksDB: TMarksSystem;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
  const AMapCalibrationList: IMapCalibrationList
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
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FMarksShowConfig := AMarksShowConfig;
  FMarksDrawConfig := AMarksDrawConfig;
  FMarksDB := AMarksDB;
  FLocalConverterFactory := ALocalConverterFactory;
  FBitmapPostProcessingConfig := ABitmapPostProcessingConfig;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
end;

function TProviderMapCombine.CreateFrame: TFrame;
begin
  Result :=
    TfrMapCombine.Create(
      Self.LanguageManager,
      FProjectionFactory,
      FVectorItmesFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      FMapCalibrationList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameImageProvider));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCalibrationList));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetProjection));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCombine));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCombineJpg));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCombineWithAlfa));
end;

function TProviderMapCombine.GetCaption: string;
begin
  Result := SAS_STR_OperationMapCombineCaption;
end;

function TProviderMapCombine.PrepareImageProvider(
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
  VRecolorConfig: IBitmapPostProcessingConfigStatic;
  VSourceProvider: IBitmapLayerProvider;
  VUseMarks: Boolean;
  VUseRecolor: Boolean;
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
    VMarksImageProvider :=
      TBitmapLayerProviderByMarksSubset.Create(
        FMarksDrawConfig.GetStatic,
        FVectorItmesFactory,
        AProjectedPolygon.Projection,
        TIdCacheSimpleThreadSafe.Create,
        VLineClipRect,
        VMarksSubset
      );
  end;
  VRecolorConfig := nil;
  VUseRecolor := (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseRecolor;
  if VUseRecolor then begin
    VRecolorConfig := FBitmapPostProcessingConfig.GetStatic;
  end;
  Result :=
    TBitmapLayerProviderSimpleForCombine.Create(
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
      FViewConfig.BackGroundColor,
      Result
    );
end;

function TProviderMapCombine.PreparePolygon(
  const APolygon: ILonLatPolygon
): IProjectedPolygon;
var
  VProjection: IProjectionInfo;
begin
  VProjection := (ParamsFrame as IRegionProcessParamsFrameTargetProjection).Projection;

  Result :=
    FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
      VProjection,
      APolygon
    );
end;

procedure TProviderMapCombine.PrepareProcessInfo(
  out ACancelNotifier: INotifierOperation;
  out AOperationID: Integer;
  out AProgressInfo: IRegionProcessProgressInfoInternal
);
var
  VCancelNotifierInternal: INotifierOperationInternal;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  VCancelNotifierInternal := TNotifierOperation.Create;
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

function TProviderMapCombine.PrepareTargetConverter(
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

function TProviderMapCombine.PrepareTargetFileName: string;
begin
  Result := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  if Result = '' then begin
    raise Exception.Create(_('Please, select output file first!'));
  end;
end;

procedure TProviderMapCombine.StartProcess(const APolygon: ILonLatPolygon);
var
  VMapCalibrations: IMapCalibrationList;
  VFileName: string;
  VSplitCount: TPoint;
  VFileExt: string;
  VProjectedPolygon: IProjectedPolygon;
  VTargetConverter: ILocalCoordConverter;
  VImageProvider: IBitmapLayerProvider;
  VMapSize: TPoint;
  VMapPieceSize: TPoint;
  VKmzImgesCount: TPoint;
  VCancelNotifier: INotifierOperation;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfoInternal;
begin
  PrepareProcessInfo(VCancelNotifier, VOperationID, VProgressInfo);
  VProjectedPolygon := PreparePolygon(APolygon);
  VTargetConverter := PrepareTargetConverter(VProjectedPolygon);
  VImageProvider := PrepareImageProvider(APolygon, VProjectedPolygon);
  VMapCalibrations := (ParamsFrame as IRegionProcessParamsFrameMapCalibrationList).MapCalibrationList;
  VFileName := PrepareTargetFileName;
  VSplitCount := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SplitCount;

  VFileExt := UpperCase(ExtractFileExt(VFileName));
  if (VFileExt = '.ECW') or (VFileExt = '.JP2') then begin
    TThreadMapCombineECW.Create(
      VCancelNotifier,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VMapCalibrations,
      VFileName,
      VSplitCount,
      (ParamsFrame as IRegionProcessParamsFrameMapCombineJpg).Quality
    );
  end else if (VFileExt = '.BMP') then begin
    TThreadMapCombineBMP.Create(
      VCancelNotifier,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VMapCalibrations,
      VFileName,
      VSplitCount
    );
  end else if (VFileExt = '.KMZ') then begin
    VMapSize := VTargetConverter.GetLocalRectSize;
    VMapPieceSize.X := VMapSize.X div VSplitCount.X;
    VMapPieceSize.Y := VMapSize.Y div VSplitCount.Y;
    VKmzImgesCount.X := ((VMapPieceSize.X - 1) div 1024) + 1;
    VKmzImgesCount.Y := ((VMapPieceSize.Y - 1) div 1024) + 1;
    if ((VKmzImgesCount.X * VKmzImgesCount.Y) > 100) then begin
      ShowMessage(SAS_MSG_GarminMax1Mp);
    end;

    TThreadMapCombineKMZ.Create(
      VCancelNotifier,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VMapCalibrations,
      VFileName,
      VSplitCount,
      (ParamsFrame as IRegionProcessParamsFrameMapCombineJpg).Quality
    );
  end else if (VFileExt = '.JPG') then begin
    TThreadMapCombineJPG.Create(
      VCancelNotifier,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VMapCalibrations,
      VFileName,
      VSplitCount,
      (ParamsFrame as IRegionProcessParamsFrameMapCombineJpg).Quality
    );
  end else if (VFileExt = '.PNG') then begin
    TThreadMapCombinePNG.Create(
      VCancelNotifier,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VMapCalibrations,
      VFileName,
      VSplitCount,
      (ParamsFrame as IRegionProcessParamsFrameMapCombineWithAlfa).IsSaveAlfa
    );
  end;
end;

end.


