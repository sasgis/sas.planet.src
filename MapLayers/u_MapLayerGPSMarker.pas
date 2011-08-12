unit u_MapLayerGPSMarker;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Transforms,
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
    FMovedMarkerProvider: IBitmapMarkerProvider;
    FStopedMarkerProvider: IBitmapMarkerProvider;

    FGpsPosChangeCounter: Integer;
    FStopedMarker: IBitmapMarker;
    FTransform: TAffineTransformation;
    FMarker: TCustomBitmap32;

    FFixedLonLat: TDoublePoint;
    FFixedOnBitmap: TDoublePoint;

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
      AMovedMarkerProvider: IBitmapMarkerProvider;
      AStopedMarkerProvider: IBitmapMarkerProvider;
      AGPSRecorder: IGPSRecorder
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Math,
  u_GeoFun,
  i_GPS,
  u_NotifyEventListener;

{ TMapLayerGPSMarker }

constructor TMapLayerGPSMarker.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ATimerNoifier: IJclNotifier;
  AConfig: IMapLayerGPSMarkerConfig;
  AMovedMarkerProvider: IBitmapMarkerProvider;
  AStopedMarkerProvider: IBitmapMarkerProvider;
  AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  FMovedMarkerProvider := AMovedMarkerProvider;
  FStopedMarkerProvider := AStopedMarkerProvider;
  FMarker := TCustomBitmap32.Create;
  FMarker.DrawMode := dmBlend;
  FMarker.CombineMode := cmBlend;
  FTransform := TAffineTransformation.Create;

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
  FreeAndNil(FTransform);
  FreeAndNil(FMarker);
  inherited;
end;

procedure TMapLayerGPSMarker.GPSReceiverReceive(Sender: TObject);
begin
  InterlockedIncrement(FGpsPosChangeCounter);
end;

procedure TMapLayerGPSMarker.OnConfigChange(Sender: TObject);
begin
  FStopedMarker := FStopedMarkerProvider.GetMarker;
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
begin
  FMarker.Lock;
  try
    VTargetPoint := ALocalConverter.LonLat2LocalPixelFloat(FFixedLonLat);
    VTargetPoint.X := VTargetPoint.X - FFixedOnBitmap.X;
    VTargetPoint.Y := VTargetPoint.Y - FFixedOnBitmap.Y;
    if PixelPointInRect(VTargetPoint, DoubleRect(ALocalConverter.GetLocalRect)) then begin
      ABuffer.Draw(Trunc(VTargetPoint.X), Trunc(VTargetPoint.Y), FMarker);
    end;
  finally
    FMarker.Unlock;
  end;
end;

procedure TMapLayerGPSMarker.PrepareMarker(ASpeed, AAngle: Double);
var
  VSizeTarget: TPoint;
  VMarker: IBitmapMarker;
begin
  if ASpeed > FConfig.MinMoveSpeed then begin
    VMarker := FMovedMarkerProvider.GetMarkerWithRotation(AAngle);
  end else begin
    VMarker := FStopedMarker;
  end;
  FMarker.Lock;
  try
    VSizeTarget := VMarker.BitmapSize;
    FMarker.SetSize(VSizeTarget.X, VSizeTarget.Y);
    FFixedOnBitmap := VMarker.AnchorPoint;
    FMarker.Assign(VMarker.Bitmap);
  finally
    FMarker.Unlock;
  end;
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
