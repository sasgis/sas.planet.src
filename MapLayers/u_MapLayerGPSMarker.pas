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
    FMovedMarker: IBitmapMarker;
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
  FMovedMarker := FMovedMarkerProvider.GetMarker;
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
  VSizeSource: TPoint;
  VSizeSourceFloat: TFloatPoint;
  VSizeTarget: TPoint;
  VDiag: Integer;
  VFixedOnBitmap: TFloatPoint;
  VMarker: IBitmapMarker;
begin
  if ASpeed > FConfig.MinMoveSpeed then begin
    VMarker := FMovedMarker;
  end else begin
    VMarker := FStopedMarker;
  end;
  if VMarker.UseDirection then begin
    VSizeSource := VMarker.BitmapSize;
    VSizeSourceFloat := FloatPoint(VSizeSource.X, VSizeSource.Y);
    VDiag := Trunc(Hypot(VSizeSourceFloat.X, VSizeSourceFloat.Y));
    VSizeTarget.X := VDiag;
    VSizeTarget.Y := VDiag;
    FMarker.Lock;
    try
      FTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
      FTransform.Clear;
      FTransform.Translate(-VSizeSource.X / 2, -VSizeSource.Y / 2);
      FTransform.Rotate(0, 0, VMarker.DefaultDirection - AAngle);
      FTransform.Translate(VSizeTarget.X / 2, VSizeTarget.Y / 2);

      FMarker.SetSize(VSizeTarget.X, VSizeTarget.Y);
      FMarker.Clear(0);
      Transform(FMarker, VMarker.Bitmap, FTransform);
      VFixedOnBitmap := FTransform.Transform(FloatPoint(VMarker.AnchorPoint.X, VMarker.AnchorPoint.Y));
      FFixedOnBitmap := DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y);
    finally
      FMarker.Unlock;
    end;
  end else begin
    VSizeTarget := VMarker.BitmapSize;
    FMarker.Lock;
    try
      FMarker.SetSize(VSizeTarget.X, VSizeTarget.Y);
      FFixedOnBitmap := VMarker.AnchorPoint;
      VMarker.Bitmap.DrawTo(FMarker);
    finally
      FMarker.Unlock;
    end;
  end;
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
