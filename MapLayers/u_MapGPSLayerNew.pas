unit u_MapGPSLayerNew;

interface

uses
  Windows,
  Types,
  SysUtils,
  GR32_Image,
  i_JclNotify,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_MapLayerGPSTrackConfig,
  i_GPSRecorder,
  i_ImageResamplerConfig,
  u_TiledLayerWithThreadBase;

type
  TMapGPSLayerNew = class(TTiledLayerWithThreadBase)
  private
    FConfig: IMapLayerGPSTrackConfig;
    FGPSRecorder: IGPSRecorder;

    FGetTrackCounter: IInternalPerformanceCounter;
    FGpsPosChangeCounter: Integer;
    procedure OnConfigChange;
    procedure OnGPSRecorderChange;
    procedure OnTimer;
  protected
    function CreateLayerProvider(
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const ATimerNoifier: IJclNotifier;
      const AConfig: IMapLayerGPSTrackConfig;
      const AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  u_NotifyEventListener,
  u_BitmapLayerProviderByTrackPath;

{ TMapGPSLayerNew }

constructor TMapGPSLayerNew.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: IJclNotifier; AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: IJclNotifier; const AConfig: IMapLayerGPSTrackConfig;
  const AGPSRecorder: IGPSRecorder);
begin
  inherited Create(
    APerfList,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory,
    ATimerNoifier,
    False,
    AConfig.ThreadConfig
  );
  FGPSRecorder := AGPSRecorder;
  FConfig := AConfig;

  FGetTrackCounter := PerfList.CreateAndAddNewCounter('GetTrack');

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnGPSRecorderChange),
    FGPSRecorder.GetChangeNotifier
  );

  FGpsPosChangeCounter := 0;
end;

function TMapGPSLayerNew.CreateLayerProvider(
  const ALayerConverter: ILocalCoordConverter): IBitmapLayerProvider;
var
  VTrackColorer: ITrackColorerStatic;
  VPointsCount: Integer;
  VLineWidth: Double;
  VCounterContext: TInternalPerformanceCounterContext;
  VEnum: IEnumGPSTrackPoint;
begin
  Result := nil;
  FConfig.LockRead;
  try
    VPointsCount := FConfig.LastPointCount;
    VLineWidth := FConfig.LineWidth;
    VTrackColorer := FConfig.TrackColorerConfig.GetStatic;
  finally
    FConfig.UnlockRead
  end;

  if (VPointsCount > 1) then begin
    VCounterContext := FGetTrackCounter.StartOperation;
    try
      VEnum := FGPSRecorder.LastPoints(VPointsCount);
    finally
      FGetTrackCounter.FinishOperation(VCounterContext);
    end;
    Result :=
      TBitmapLayerProviderByTrackPath.Create(
        VPointsCount,
        VLineWidth,
        VTrackColorer,
        ALayerConverter.ProjectionInfo,
        VEnum
      );
  end;
end;

procedure TMapGPSLayerNew.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapGPSLayerNew.OnGPSRecorderChange;
begin
  InterlockedIncrement(FGpsPosChangeCounter);
end;

procedure TMapGPSLayerNew.OnTimer;
begin
  if InterlockedExchange(FGpsPosChangeCounter, 0) > 0 then begin
    ViewUpdateLock;
    try
      SetNeedUpdateLayerProvider;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

end.
