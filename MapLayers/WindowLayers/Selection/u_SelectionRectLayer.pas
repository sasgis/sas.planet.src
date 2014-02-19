unit u_SelectionRectLayer;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
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
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const ASelection: ISelectionRect;
      const AConfig: ISelectionRectLayerConfig
    );
  end;


implementation

uses
  SysUtils,
  i_CoordConverter,
  u_ListenerByEvent,
  u_GeoFunc;

{ TSelectionRectLayer }

constructor TSelectionRectLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ASelection: ISelectionRect;
  const AConfig: ISelectionRectLayerConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
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
end;

procedure TSelectionRectLayer.OnSelectionChange;
begin
  ViewUpdateLock;
  try
    if FSelection.IsEmpty then begin
      Hide;
    end else begin
      FSelectedLonLat := FSelection.GetRect;
      SetNeedRedraw;
      Show;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TSelectionRectLayer.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  jj: integer;
  VDrawRect: TRect;
  VSelectedLonLat: TDoubleRect;
  VSelectedPixels: TDoubleRect;
  VZoomDelta: Byte;
  VColor: TColor32;
  VSelectedRelative: TDoubleRect;
  VSelectedTilesFloat: TDoubleRect;
  VSelectedTiles: TRect;
  VMaxZoomDelta: Integer;
  VGeoConvert: ICoordConverter;
  VZoom: Byte;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VSelectedLonLat := FSelectedLonLat;
  VGeoConvert.CheckLonLatRect(VSelectedLonLat);
  VSelectedRelative := VGeoConvert.LonLatRect2RelativeRect(VSelectedLonLat);
  VSelectedPixels := VGeoConvert.RelativeRect2PixelRectFloat(VSelectedRelative, VZoom);
  VDrawRect :=
    RectFromDoubleRect(
      ALocalConverter.MapRectFloat2LocalRectFloat(VSelectedPixels),
      rrToTopLeft
    );
  ABuffer.FillRectTS(
    VDrawRect.Left,
    VDrawRect.Top,
    VDrawRect.Right,
    VDrawRect.Bottom,
    FFillColor
  );
  ABuffer.FrameRectTS(
    VDrawRect.Left,
    VDrawRect.Top,
    VDrawRect.Right,
    VDrawRect.Bottom,
    FBorderColor
  );
  ABuffer.FrameRectTS(
    VDrawRect.Left - 1,
    VDrawRect.Top - 1,
    VDrawRect.Right + 1,
    VDrawRect.Bottom + 1,
    FBorderColor
  );

  jj := VZoom;
  VZoomDelta := 0;
  VMaxZoomDelta := Length(FZoomDeltaColors) - 1;
  while (VZoomDelta <= VMaxZoomDelta) and (jj < 24) do begin
    VSelectedTilesFloat := VGeoConvert.RelativeRect2TileRectFloat(VSelectedRelative, jj);
    VSelectedTiles := RectFromDoubleRect(VSelectedTilesFloat, rrOutside);
    VSelectedPixels :=
      VGeoConvert.RelativeRect2PixelRectFloat(
        VGeoConvert.TileRect2RelativeRect(VSelectedTiles, jj),
        VZoom
      );
    VDrawRect :=
      RectFromDoubleRect(
        ALocalConverter.MapRectFloat2LocalRectFloat(VSelectedPixels),
        rrToTopLeft
      );
    VColor := FZoomDeltaColors[VZoomDelta];

    ABuffer.FrameRectTS(
      VDrawRect.Left - (VZoomDelta + 1),
      VDrawRect.Top - (VZoomDelta + 1),
      VDrawRect.Right + (VZoomDelta + 1),
      VDrawRect.Bottom + (VZoomDelta + 1),
      VColor
    );

    ABuffer.Font.Size := FFontSize;
    ABuffer.RenderText(
      VDrawRect.Right - ((VDrawRect.Right - VDrawRect.Left) div 2) - 42 + VZoomDelta * 26,
      VDrawRect.Bottom - ((VDrawRect.Bottom - VDrawRect.Top) div 2) - 6,
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
