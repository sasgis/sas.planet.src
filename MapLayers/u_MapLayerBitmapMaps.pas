unit u_MapLayerBitmapMaps;

interface

uses
  GR32_Image,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_TileError,
  i_BitmapPostProcessing,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_UseTilePrevZoomConfig,
  i_Bitmap32StaticFactory,
  i_ThreadConfig,
  i_MapTypes,
  i_MapTypeListChangeable,
  i_InternalPerformanceCounter,
  i_ImageResamplerConfig,
  u_TiledLayerWithThreadBase;

type
  TMapLayerBitmapMaps = class(TTiledLayerWithThreadBase)
  private
    FErrorLogger: ITileErrorLogger;
    FBitmapFactory: IBitmap32StaticFactory;
    FPostProcessing: IBitmapPostProcessingChangeable;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FMainMap: IMapTypeChangeable;
    FLayesList: IMapTypeListChangeable;

    procedure OnMainMapChange;
    procedure OnLayerListChange;
    procedure OnConfigChange;
  protected
    function CreateLayerProvider(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AView: ILocalCoordConverterChangeable;
      const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMainMap: IMapTypeChangeable;
      const ALayesList: IMapTypeListChangeable;
      const APostProcessing: IBitmapPostProcessingChangeable;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AThreadConfig: IThreadConfig;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: INotifierTime
    );
  end;

implementation

uses
  i_TileMatrix,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_BitmapLayerProviderForViewMaps;

{ TMapMainLayerNew }

constructor TMapLayerBitmapMaps.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMainMap: IMapTypeChangeable;
  const ALayesList: IMapTypeListChangeable;
  const APostProcessing: IBitmapPostProcessingChangeable;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AThreadConfig: IThreadConfig;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: INotifierTime
);
var
  VTileMatrixFactory: ITileMatrixFactory;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      ATileMatrixDraftResamplerConfig,
      ABitmapFactory,
      AConverterFactory
    );
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    APosition,
    AView,
    VTileMatrixFactory,
    ATimerNoifier,
    False,
    AThreadConfig
  );
  FMainMap := AMainMap;
  FLayesList := ALayesList;
  FErrorLogger := AErrorLogger;
  FBitmapFactory := ABitmapFactory;
  FPostProcessing := APostProcessing;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
    FMainMap.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerListChange),
    FLayesList.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FUseTilePrevZoomConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FPostProcessing.GetChangeNotifier
  );
  Visible := True;
end;

function TMapLayerBitmapMaps.CreateLayerProvider(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALayerConverter: ILocalCoordConverter
): IBitmapLayerProvider;
var
  VMainMap: IMapType;
  VPostProcessingConfig: IBitmapPostProcessing;
  VLayersList: IMapTypeListStatic;
  VUsePrevConfig: IUseTilePrevZoomTileConfigStatic;
begin
  VMainMap := FMainMap.GetStatic;
  VLayersList := FLayesList.List;
  VUsePrevConfig := FUseTilePrevZoomConfig.GetStatic;
  VPostProcessingConfig := FPostProcessing.GetStatic;

  Result :=
    TBitmapLayerProviderForViewMaps.Create(
      FBitmapFactory,
      VMainMap,
      VLayersList,
      VUsePrevConfig.UsePrevZoomAtMap,
      VUsePrevConfig.UsePrevZoomAtLayer,
      True,
      VPostProcessingConfig,
      FErrorLogger
    );
end;

procedure TMapLayerBitmapMaps.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerBitmapMaps.OnLayerListChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerBitmapMaps.OnMainMapChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerBitmapMaps.StartThreads;
begin
  OnConfigChange;
  inherited;
end;

end.
