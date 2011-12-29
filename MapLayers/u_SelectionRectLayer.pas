unit u_SelectionRectLayer;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_SelectionRect,
  i_SelectionRectLayerConfig,
  u_MapLayerBasic;

type
  TSelectionRectLayer = class(TMapLayerBasicNoBitmap)
  private
    FConfig: ISelectionRectLayerConfig;
    FSelection: ISelectionRect;
    FSelectedLonLat: TDoubleRect;

    FFillColor: TColor32;
    FBorderColor: TColor32;
    FFontSize: Integer;
    FZoomDeltaColors: TArrayOfColor32;

    procedure OnSelectionChange;
    procedure OnConfigChange;
  protected
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ASelection: ISelectionRect;
      AConfig: ISelectionRectLayerConfig
    );
  end;


implementation

uses
  SysUtils,
  i_CoordConverter,
  u_NotifyEventListener;

{ TSelectionRectLayer }

constructor TSelectionRectLayer.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ASelection: ISelectionRect;
  AConfig: ISelectionRectLayerConfig
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  FSelection := ASelection;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSelectionChange),
    FSelection.GetChangeNotifier
  );
end;

procedure TSelectionRectLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      FFillColor := FConfig.FillColor;
      FBorderColor := FConfig.BorderColor;
      FFontSize := FConfig.FontSize;
      FZoomDeltaColors := FConfig.ZoomDeltaColors;
    finally
      FConfig.UnlockRead;
    end;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TSelectionRectLayer.OnSelectionChange;
begin
  ViewUpdateLock;
  try
    if FSelection.IsEmpty then begin
      Hide
    end else begin
      FSelectedLonLat := FSelection.GetRect;
      SetNeedRedraw;
      Show;
    end;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TSelectionRectLayer.PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter);
var
  jj: integer;
  xy1, xy2: TPoint;
  VSelectedPixels: TRect;
  VZoomDelta: Byte;
  VColor: TColor32;
  VSelectedRelative: TDoubleRect;
  VSelectedTiles: TRect;
  VMaxZoomDelta: Integer;
  VGeoConvert: ICoordConverter;
  VZoom: Byte;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VSelectedPixels := VGeoConvert.LonLatRect2PixelRect(FSelectedLonLat, VZoom);

  xy1 := ALocalConverter.LonLat2LocalPixel(FSelectedLonLat.TopLeft);
  xy2 := ALocalConverter.LonLat2LocalPixel(FSelectedLonLat.BottomRight);

  ABuffer.FillRectTS(xy1.x, xy1.y, xy2.x, xy2.y, FFillColor);
  ABuffer.FrameRectTS(xy1.x, xy1.y, xy2.x, xy2.y, FBorderColor);
  ABuffer.FrameRectTS(xy1.x - 1, xy1.y - 1, xy2.x + 1, xy2.y + 1, FBorderColor);

  VSelectedRelative := VGeoConvert.PixelRect2RelativeRect(VSelectedPixels, VZoom);

  jj := VZoom;
  VZoomDelta := 0;
  VMaxZoomDelta := Length(FZoomDeltaColors) - 1;
  while (VZoomDelta <= VMaxZoomDelta) and (jj < 24) do begin
    VSelectedTiles := VGeoConvert.RelativeRect2TileRect(VSelectedRelative, jj);
    VSelectedPixels := VGeoConvert.RelativeRect2PixelRect(
      VGeoConvert.TileRect2RelativeRect(VSelectedTiles, jj), VZoom
    );

    xy1 := ALocalConverter.MapPixel2LocalPixel(VSelectedPixels.TopLeft);
    xy2 := ALocalConverter.MapPixel2LocalPixel(VSelectedPixels.BottomRight);

    VColor := FZoomDeltaColors[VZoomDelta];

    ABuffer.FrameRectTS(
      xy1.X - (VZoomDelta + 1), xy1.Y - (VZoomDelta + 1),
      xy2.X + (VZoomDelta + 1), xy2.Y + (VZoomDelta + 1),
      VColor
    );

    ABuffer.Font.Size := FFontSize;
    ABuffer.RenderText(
      xy2.x - ((xy2.x - xy1.x) div 2) - 42 + VZoomDelta * 26,
      xy2.y - ((xy2.y - xy1.y) div 2) - 6,
      'z' + inttostr(jj + 1), 3, VColor
    );
    Inc(jj);
    Inc(VZoomDelta);
  end;
end;

procedure TSelectionRectLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
