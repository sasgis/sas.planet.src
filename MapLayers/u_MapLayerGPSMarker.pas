unit u_MapLayerGPSMarker;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
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

    procedure GPSReceiverReceive;
    procedure OnConfigChange;
    procedure OnTimer;
    procedure PrepareMarker(const ASpeed, AAngle: Double; AForceStopped: Boolean);
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const ATimerNoifier: IJclNotifier;
      const AConfig: IMapLayerGPSMarkerConfig;
      const AMovedMarkerProvider: IBitmapMarkerProviderChangeable;
      const AStopedMarkerProvider: IBitmapMarkerProviderChangeable;
      const AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  Types,
  SysUtils,
  GR32_Resamplers,
  u_GeoFun,
  i_GPS,
  vsagps_public_base,
  vsagps_public_position,
  u_NotifyEventListener;

{ TMapLayerGPSMarker }

constructor TMapLayerGPSMarker.Create(
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const ATimerNoifier: IJclNotifier;
  const AConfig: IMapLayerGPSMarkerConfig;
  const AMovedMarkerProvider: IBitmapMarkerProviderChangeable;
  const AStopedMarkerProvider: IBitmapMarkerProviderChangeable;
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  FMovedMarkerProvider := AMovedMarkerProvider;
  FStopedMarkerProvider := AStopedMarkerProvider;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FMovedMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FStopedMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.GPSReceiverReceive),
    FGPSRecorder.GetChangeNotifier
  );

  FGpsPosChangeCounter := 0;
end;

procedure TMapLayerGPSMarker.GPSReceiverReceive;
begin
  InterlockedIncrement(FGpsPosChangeCounter);
end;

procedure TMapLayerGPSMarker.OnConfigChange;
begin
  FMovedMarkerProviderStatic := FMovedMarkerProvider.GetStatic;
  FStopedMarkerProviderStatic := FStopedMarkerProvider.GetStatic;
  FStopedMarker := FStopedMarkerProviderStatic.GetMarker;
  GPSReceiverReceive;
end;

procedure TMapLayerGPSMarker.OnTimer;
var
  VGPSPosition: IGPSPosition;
  VpPos: PSingleGPSData;
  VForceStoppedMarker: Boolean;  
begin
  if InterlockedExchange(FGpsPosChangeCounter, 0) > 0 then begin
    ViewUpdateLock;
    try
      VGPSPosition := FGPSRecorder.CurrentPosition;
      VpPos := VGPSPosition.GetPosParams;
      if (not VpPos^.PositionOK) then begin
        // no position
        Hide;
      end else begin
        // ok
        FFixedLonLat.X := VpPos^.PositionLon;
        FFixedLonLat.Y := VpPos^.PositionLat;
        VForceStoppedMarker:=((not VpPos^.AllowCalcStats) or NoData_Float64(VpPos^.Speed_KMH) or NoData_Float64(VpPos^.Heading));
        PrepareMarker(VpPos^.Speed_KMH, VpPos^.Heading, VForceStoppedMarker);
        Show;
        SetNeedRedraw;
      end;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerGPSMarker.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VMarker: IBitmapMarker;
  VFixedOnView: TDoublePoint;
  VTargetPoint: TPoint;
begin
  VMarker := FMarker;
  if VMarker <> nil then begin
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(FFixedLonLat);
    VTargetPoint :=
      PointFromDoublePoint(
        DoublePoint(
          VFixedOnView.X - VMarker.AnchorPoint.X,
          VFixedOnView.Y - VMarker.AnchorPoint.Y
        ),
        prToTopLeft
      );
    if PtInRect(ALocalConverter.GetLocalRect, VTargetPoint) then begin
      BlockTransfer(
        ABuffer,
        VTargetPoint.X, VTargetPoint.Y,
        ABuffer.ClipRect,
        VMarker.Bitmap,
        VMarker.Bitmap.BoundsRect,
        dmBlend,
        cmBlend
      );
    end;
  end;
end;

procedure TMapLayerGPSMarker.PrepareMarker(
  const ASpeed, AAngle: Double;
  AForceStopped: Boolean
);
var
  VMarker: IBitmapMarker;
  VMarkerProvider: IBitmapMarkerProvider;
  VMarkerWithDirectionProvider: IBitmapMarkerWithDirectionProvider;
begin
  if (not AForceStopped) and (ASpeed > FConfig.MinMoveSpeed) then begin
    VMarkerProvider := FMovedMarkerProviderStatic;
    if Supports(VMarkerProvider, IBitmapMarkerWithDirectionProvider, VMarkerWithDirectionProvider) then begin
      VMarker := VMarkerWithDirectionProvider.GetMarkerWithRotation(AAngle);
    end else begin
      VMarker := VMarkerProvider.GetMarker;
    end;
  end else begin
    VMarker := FStopedMarker;
  end;
  FMarker := VMarker;
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
