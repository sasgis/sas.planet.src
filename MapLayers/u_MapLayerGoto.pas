unit u_MapLayerGoto;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_BitmapMarker,
  i_ViewPortState,
  i_GotoLayerConfig,
  u_MapLayerBasic;

type
  TGotoLayer = class(TMapLayerFixedWithBitmap)
  private
    FConfig: IGotoLayerConfig;

    FMarkerProvider: IBitmapMarkerProvider;
    FMarker: IBitmapMarker;

    FShowTickCount: Cardinal;
    FShowTick: Cardinal;

    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AMarkerProvider: IBitmapMarkerProvider;
      AConfig: IGotoLayerConfig
    );
    procedure ShowGotoIcon(APoint: TDoublePoint);
  end;



implementation

uses
  SysUtils,
  u_NotifyEventListener,
  u_GeoFun;

{ TGotoLayer }

constructor TGotoLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AMarkerProvider: IBitmapMarkerProvider;
  AConfig: IGotoLayerConfig
);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  FMarkerProvider := AMarkerProvider;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FMarkerProvider.GetChangeNotifier
  );
end;

procedure TGotoLayer.DoUpdateLayerLocation(ANewLocation: TFloatRect);
var
  VCurrTime: Cardinal;
begin
  if FShowTick <> 0 then begin
    VCurrTime := GetTickCount;
    if (FShowTick <= VCurrTime) then begin
      if (VCurrTime < FShowTick + FShowTickCount) then begin
        inherited;
      end else begin
        Visible := False;
        FShowTick := 0;
      end;
    end else begin
      Visible := False;
      FShowTick := 0;
    end;
  end else begin
    Visible := False;
  end;
end;

procedure TGotoLayer.OnConfigChange(Sender: TObject);
var
  VBitmapSize: TPoint;
  VMarker: IBitmapMarker;
begin
  VMarker := FMarkerProvider.GetMarker;
  FMarker := VMarker;
  ViewUpdateLock;
  try
    FLayer.Bitmap.Assign(VMarker.Bitmap);
    FLayer.Bitmap.DrawMode := dmBlend;
    VBitmapSize := VMarker.BitmapSize;
    FFixedOnBitmap := VMarker.AnchorPoint;
    FShowTickCount := FConfig.ShowTickCount;
    DoUpdateLayerSize(VBitmapSize);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TGotoLayer.ShowGotoIcon(APoint: TDoublePoint);
begin
  ViewUpdateLock;
  try
    FFixedLonLat := APoint;
    FShowTick := GetTickCount;
    Show;
    SetNeedUpdateLocation;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TGotoLayer.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
