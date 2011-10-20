unit u_MapLayerGoto;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_InternalPerformanceCounter,
  i_BitmapMarker,
  i_ViewPortState,
  i_LocalCoordConverter,
  i_GotoLayerConfig,
  u_MapLayerBasic;

type
  TGotoLayer = class(TMapLayerFixedWithBitmap)
  private
    FConfig: IGotoLayerConfig;

    FMarkerProvider: IBitmapMarkerProviderChangeable;
    FMarkerProviderStatic: IBitmapMarkerProvider;
    FMarker: IBitmapMarker;

    FShowTickCount: Cardinal;
    FShowTick: Cardinal;

    procedure OnConfigChange(Sender: TObject);
  protected
    function GetVisibleForNewPos(ANewVisualCoordConverter: ILocalCoordConverter): Boolean; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AMarkerProvider: IBitmapMarkerProviderChangeable;
      AConfig: IGotoLayerConfig
    );
    procedure ShowGotoIcon(APoint: TDoublePoint);
  end;



implementation

uses
  SysUtils,
  u_NotifyEventListener;

{ TGotoLayer }

constructor TGotoLayer.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AMarkerProvider: IBitmapMarkerProviderChangeable;
  AConfig: IGotoLayerConfig
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
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

function TGotoLayer.GetVisibleForNewPos(
  ANewVisualCoordConverter: ILocalCoordConverter): Boolean;
var
  VCurrTime: Cardinal;
begin
  if FShowTick <> 0 then begin
    VCurrTime := GetTickCount;
    if (FShowTick <= VCurrTime) then begin
      if (VCurrTime < FShowTick + FShowTickCount) then begin
         Result := inherited GetVisibleForNewPos(ANewVisualCoordConverter);
      end else begin
        Result := False;
        FShowTick := 0;
      end;
    end else begin
      Result := False;
      FShowTick := 0;
    end;
  end else begin
    Result := False;
  end;
end;

procedure TGotoLayer.OnConfigChange(Sender: TObject);
var
  VBitmapSize: TPoint;
  VMarker: IBitmapMarker;
begin
  FMarkerProviderStatic := FMarkerProvider.GetStatic;
  VMarker := FMarkerProviderStatic.GetMarker;
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
