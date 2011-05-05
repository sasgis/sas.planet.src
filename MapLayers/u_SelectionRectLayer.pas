unit u_SelectionRectLayer;

interface

uses
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_ViewPortState,
  i_SelectionRectLayerConfig,
  u_MapLayerBasic;

type
  TSelectionRectLayer = class(TMapLayerBasicNoBitmap)
  private
    FConfig: ISelectionRectLayerConfig;
    FSelectedLonLat: TDoubleRect;

    FFillColor: TColor32;
    FBorderColor: TColor32;
    FFontSize: Integer;
    FZoomDeltaColors: TArrayOfColor32;

    procedure OnConfigChange(Sender: TObject);
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState; AConfig: ISelectionRectLayerConfig);
    procedure DrawNothing;
    procedure DrawSelectionRect(ASelectedLonLat: TDoubleRect);
  end;


implementation

uses
  SysUtils,
  GR32_Layers,
  i_LocalCoordConverter,
  i_CoordConverter,
  u_NotifyEventListener;

{ TSelectionRectLayer }

constructor TSelectionRectLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: ISelectionRectLayerConfig
);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

procedure TSelectionRectLayer.DrawNothing;
begin
  Hide;
end;

procedure TSelectionRectLayer.DrawSelectionRect(ASelectedLonLat: TDoubleRect);
begin
  ViewUpdateLock;
  try
    FSelectedLonLat := ASelectedLonLat;
    SetNeedRedraw;
    Show;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TSelectionRectLayer.OnConfigChange(Sender: TObject);
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

procedure TSelectionRectLayer.PaintLayer(ABuffer: TBitmap32);
var
  jj: integer;
  xy1, xy2: TPoint;
  VSelectedPixels: TRect;
  VZoomDelta: Byte;
  VColor: TColor32;
  VSelectedRelative: TDoubleRect;
  VSelectedTiles: TRect;
  VMaxZoomDelta: Integer;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
  VZoom: Byte;
begin
  VLocalConverter := ViewCoordConverter;
  if VLocalConverter <> nil then begin
    VGeoConvert := VLocalConverter.GetGeoConverter;
    VZoom := VLocalConverter.GetZoom;
    VSelectedPixels := VGeoConvert.LonLatRect2PixelRect(FSelectedLonLat, VZoom);

    xy1 := VLocalConverter.LonLat2LocalPixel(FSelectedLonLat.TopLeft);
    xy2 := VLocalConverter.LonLat2LocalPixel(FSelectedLonLat.BottomRight);

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

      xy1 := VLocalConverter.MapPixel2LocalPixel(VSelectedPixels.TopLeft);
      xy2 := VLocalConverter.MapPixel2LocalPixel(VSelectedPixels.BottomRight);

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
end;

procedure TSelectionRectLayer.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
