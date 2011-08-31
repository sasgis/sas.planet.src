unit u_MapLayerGPSMarker;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_MapLayerGPSMarkerConfig,
  i_GPSRecorder,
  i_BitmapMarker,
  i_ViewPortState,
  u_MapLayerBasic;


type
  TMapLayerGPSMarker = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IMapLayerGPSMarkerConfig;
    FGPSRecorder: IGPSRecorder;
    FMovedMarkerProvider: IBitmapMarkerProviderChangeable;
    FMovedMarkerProviderStatic: IBitmapMarkerProvider;
    FStopedMarkerProvider: IBitmapMarkerProviderChangeable;
    FStopedMarkerProviderStatic: IBitmapMarkerProvider;

    FGpsPosChangeCounter: Integer;
    FStopedMarker: IBitmapMarker;
    FMarker: IBitmapMarker;

    FFixedLonLat: TDoublePoint;

    procedure GPSReceiverReceive(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure PrepareMarker(ASpeed, AAngle: Double);
  protected
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ATimerNoifier: IJclNotifier;
      AConfig: IMapLayerGPSMarkerConfig;
      AMovedMarkerProvider: IBitmapMarkerProviderChangeable;
      AStopedMarkerProvider: IBitmapMarkerProviderChangeable;
      AGPSRecorder: IGPSRecorder
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_GeoFun,
  i_GPS,
  u_NotifyEventListener;

{ TMapLayerGPSMarker }

constructor TMapLayerGPSMarker.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ATimerNoifier: IJclNotifier;
  AConfig: IMapLayerGPSMarkerConfig;
  AMovedMarkerProvider: IBitmapMarkerProviderChangeable;
  AStopedMarkerProvider: IBitmapMarkerProviderChangeable;
  AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  FMovedMarkerProvider := AMovedMarkerProvider;
  FStopedMarkerProvider := AStopedMarkerProvider;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FMovedMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FStopedMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.GPSReceiverReceive),
    FGPSRecorder.GetChangeNotifier
  );

  FGpsPosChangeCounter := 0;
end;

destructor TMapLayerGPSMarker.Destroy;
begin
  inherited;
end;

procedure TMapLayerGPSMarker.GPSReceiverReceive(Sender: TObject);
begin
  InterlockedIncrement(FGpsPosChangeCounter);
end;

procedure TMapLayerGPSMarker.OnConfigChange(Sender: TObject);
begin
  FMovedMarkerProviderStatic := FMovedMarkerProvider.GetStatic;
  FStopedMarkerProviderStatic := FStopedMarkerProvider.GetStatic;
  FStopedMarker := FStopedMarkerProviderStatic.GetMarker;
  GPSReceiverReceive(nil);
end;

procedure TMapLayerGPSMarker.OnTimer(Sender: TObject);
var
  VGPSPosition: IGPSPosition;
begin
  if InterlockedExchange(FGpsPosChangeCounter, 0) > 0 then begin
    ViewUpdateLock;
    try
      VGPSPosition := FGPSRecorder.CurrentPosition;
      if VGPSPosition.IsFix = 0 then begin
        Hide;
      end else begin
        FFixedLonLat := VGPSPosition.Position;
        PrepareMarker(VGPSPosition.Speed_KMH, VGPSPosition.Heading);
        Show;
        SetNeedRedraw;
      end;
    finally
      ViewUpdateUnlock;
    end;
    ViewUpdate;
  end;
end;

procedure TMapLayerGPSMarker.PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter);
var
  VTargetPoint: TDoublePoint;
  VMarker: IBitmapMarker;
begin
  VMarker := FMarker;
  if VMarker <> nil then begin
    VTargetPoint := ALocalConverter.LonLat2LocalPixelFloat(FFixedLonLat);
    VTargetPoint.X := VTargetPoint.X - VMarker.AnchorPoint.X;
    VTargetPoint.Y := VTargetPoint.Y - VMarker.AnchorPoint.Y;
    if PixelPointInRect(VTargetPoint, DoubleRect(ALocalConverter.GetLocalRect)) then begin
      ABuffer.Draw(Trunc(VTargetPoint.X), Trunc(VTargetPoint.Y), VMarker.Bitmap);
    end;
  end;
end;

procedure TMapLayerGPSMarker.PrepareMarker(ASpeed, AAngle: Double);
var
  VMarker: IBitmapMarker;
begin
  if ASpeed > FConfig.MinMoveSpeed then begin
    VMarker := FMovedMarkerProviderStatic.GetMarkerWithRotation(AAngle);
  end else begin
    VMarker := FStopedMarker;
  end;
  FMarker := VMarker;
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
