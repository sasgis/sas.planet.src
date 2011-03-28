unit u_MapLayerGPSMarker;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Transforms,
  GR32_Image,
  t_GeoTypes,
  i_MapLayerGPSMarkerConfig,
  i_GPSRecorder,
  i_ViewPortState,
  u_MapLayerBasic;


type
  TMapLayerGPSMarker = class(TMapLayerBasicFullView)
  private
    FConfig: IMapLayerGPSMarkerConfig;
    FGPSRecorder: IGPSRecorder;
    FTransform: TAffineTransformation;

    FMarker: TCustomBitmap32;

    FFixedLonLat: TDoublePoint;
    FFixedOnBitmap: TDoublePoint;

    procedure GPSReceiverReceive(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
    procedure PrepareMarker(ASpeed, AAngle: Double);
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32);
  protected
    procedure DoRedraw; override;
    procedure DoShow; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IMapLayerGPSMarkerConfig;
      AGPSRecorder: IGPSRecorder
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Layers,
  GR32_Math,
  Ugeofun,
  i_GPS,
  i_LocalCoordConverter,
  u_NotifyEventListener;

{ TMapLayerGPSMarker }

constructor TMapLayerGPSMarker.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IMapLayerGPSMarkerConfig;
  AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(TPositionedLayer.Create(AParentMap.Layers), AViewPortState);
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  FMarker := TCustomBitmap32.Create;
  FMarker.DrawMode := dmBlend;
  FMarker.CombineMode := cmMerge;
  FTransform := TAffineTransformation.Create;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.GPSReceiverReceive),
    FGPSRecorder.GetChangeNotifier
  );
end;

destructor TMapLayerGPSMarker.Destroy;
begin
  FreeAndNil(FTransform);
  FreeAndNil(FMarker);
  inherited;
end;

procedure TMapLayerGPSMarker.DoRedraw;
begin
  inherited;
  LayerPositioned.Changed;
end;

procedure TMapLayerGPSMarker.DoShow;
begin
  inherited;
  Redraw;
end;

procedure TMapLayerGPSMarker.GPSReceiverReceive(Sender: TObject);
var
  VGPSPosition: IGPSPosition;
begin
  VGPSPosition := FGPSRecorder.CurrentPosition;
  if VGPSPosition.IsFix = 0 then begin
    Hide;
  end else begin
    FFixedLonLat := VGPSPosition.Position;
    PrepareMarker(VGPSPosition.Speed_KMH, VGPSPosition.Heading);
    Redraw;
    Show;
  end;
end;

procedure TMapLayerGPSMarker.OnConfigChange(Sender: TObject);
begin
  GPSReceiverReceive(nil);
end;

procedure TMapLayerGPSMarker.PaintLayer(Sender: TObject; Buffer: TBitmap32);
var
  VConverter: ILocalCoordConverter;
  VTargetPoint: TDoublePoint;
begin
  VConverter := VisualCoordConverter;
  if VConverter <> nil then begin
    FMarker.Lock;
    try
      VTargetPoint := VConverter.LonLat2LocalPixelFloat(FFixedLonLat);
      VTargetPoint.X := VTargetPoint.X - FFixedOnBitmap.X;
      VTargetPoint.Y := VTargetPoint.Y - FFixedOnBitmap.Y;
      if PixelPointInRect(VTargetPoint, DoubleRect(VConverter.GetLocalRect)) then begin
        Buffer.Draw(Trunc(VTargetPoint.X), Trunc(VTargetPoint.Y), FMarker);
      end;
    finally
      FMarker.Unlock;
    end;
  end;
end;

procedure TMapLayerGPSMarker.PrepareMarker(ASpeed, AAngle: Double);
var
  VSizeSource: TPoint;
  VSizeSourceFloat: TFloatPoint;
  VSizeTarget: TPoint;
  VMarker: TCustomBitmap32;
  VDiag: Integer;
  VFixedOnBitmap: TFloatPoint;
begin
  FConfig.LockRead;
  try
    if ASpeed > FConfig.MinMoveSpeed then begin
      VMarker := FConfig.GetMarkerMoved;
      VSizeSource := Point(VMarker.Width, VMarker.Height);
      VSizeSourceFloat := FloatPoint(VSizeSource.X, VSizeSource.Y);
      VDiag := Trunc(Hypot(VSizeSourceFloat.X, VSizeSourceFloat.Y));
      VSizeTarget.X := VDiag;
      VSizeTarget.Y := VDiag;
      FMarker.Lock;
      try
        FMarker.SetSize(VSizeTarget.X, VSizeTarget.Y);
        FTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
        FTransform.Clear;
        FTransform.Translate(-VSizeSource.X / 2, -VSizeSource.Y / 2);
        FTransform.Rotate(0, 0, -AAngle);
        FTransform.Translate(VSizeTarget.X / 2, VSizeTarget.Y / 2);
        FMarker.Clear(0);
        Transform(FMarker, VMarker, FTransform);
        SinCos(-AAngle*Pi/180, VSizeSource.Y / 2, VFixedOnBitmap.X, VFixedOnBitmap.Y);
        FFixedOnBitmap.X := VSizeTarget.X / 2 + VFixedOnBitmap.X;
        FFixedOnBitmap.Y := VSizeTarget.Y / 2 + VFixedOnBitmap.Y;
      finally
        FMarker.Unlock;
      end;
    end else begin
      VMarker := FConfig.GetMarkerStoped;
      VSizeSource := Point(VMarker.Width, VMarker.Height);
      VSizeTarget := VSizeSource;
      FMarker.Lock;
      try
        FMarker.SetSize(VSizeTarget.X, VSizeTarget.Y);
        FFixedOnBitmap.X := VSizeTarget.X / 2;
        FFixedOnBitmap.Y := VSizeTarget.Y / 2;
        VMarker.DrawTo(FMarker);
      finally
        FMarker.Unlock;
      end;
    end;
  finally
    FConfig.UnlockRead;
  end;
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
  LayerPositioned.OnPaint := PaintLayer;
end;

end.
