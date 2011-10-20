unit u_MapLayerGrids;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_InternalPerformanceCounter,
  i_ImageResamplerConfig,
  i_MapLayerGridsConfig,
  i_ViewPortState,
  u_MapLayerBasic;

type
  TMapLayerGrids = class(TMapLayerBasic)
  private
    FConfig: IMapLayerGridsConfig;
    procedure generate_granica;
    procedure DrawGenShBorders;
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DoRedraw; override;
    function GetVisibleForNewPos(ANewVisualCoordConverter: ILocalCoordConverter): Boolean; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      AConfig: IMapLayerGridsConfig
    );
  end;

implementation

uses
  SysUtils,
  i_CoordConverter,
  u_GeoFun,
  u_NotifyEventListener,
  u_GeoToStr;

const
  GSHprec = 100000000;

{ TMapLayerGrids }

constructor TMapLayerGrids.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AResamplerConfig: IImageResamplerConfig;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  AConfig: IMapLayerGridsConfig
);
begin
  inherited Create(
    APerfList,
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory
  );
  FConfig := AConfig;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

procedure TMapLayerGrids.DoRedraw;
begin
  inherited;
  if FConfig.TileGrid.Visible then begin
    generate_granica;
  end;
  if FConfig.GenShtabGrid.Visible then begin
    DrawGenShBorders;
  end;
end;

function FloatPoint2RectWihtClip(ASource: TDoublePoint): TPoint;
const
  CMaxClip = 1 shl 14;
begin
  if ASource.X < - CMaxClip then begin
    Result.X := - CMaxClip;
  end else if ASource.X > CMaxClip then begin
    Result.X := CMaxClip;
  end else begin
    Result.X := trunc(ASource.X);
  end;

  if ASource.Y < - CMaxClip then begin
    Result.Y := - CMaxClip;
  end else if ASource.Y > CMaxClip then begin
    Result.Y := CMaxClip;
  end else begin
    Result.Y := trunc(ASource.Y);
  end;
end;

procedure TMapLayerGrids.DrawGenShBorders;
var
  z: TDoublePoint;
  twidth, theight: integer;
  ListName: WideString;
  VZoom: Byte;
  VLoadedRect: TDoubleRect;
  VLoadedLonLatRect: TDoubleRect;
  VGridLonLatRect: TDoubleRect;
  VGridRect: TRect;
  VDrawLonLatRect: TDoubleRect;
  VDrawRectFloat: TDoubleRect;
  VDrawScreenRect: TRect;
  VCanShowText: Boolean;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
  VScale: Integer;
  VShowText: Boolean;
  VColor: TColor32;
begin
  FConfig.GenShtabGrid.LockRead;
  try
    VScale := FConfig.GenShtabGrid.Scale;
    VShowText := FConfig.GenShtabGrid.ShowText;
    VColor := FConfig.GenShtabGrid.GridColor;
  finally
    FConfig.GenShtabGrid.UnlockRead;
  end;
  if VScale = 0 then begin
    exit;
  end;
  z := GetGhBordersStepByScale(VScale);
  VLocalConverter := LayerCoordConverter;
  VGeoConvert := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VLoadedRect := VLocalConverter.GetRectInMapPixelFloat;

  VGeoConvert.CheckPixelRectFloat(VLoadedRect, VZoom);

  VLoadedLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VLoadedRect, VZoom);
  if VLoadedLonLatRect.Top>90 then VLoadedLonLatRect.Top:=90;
  if VLoadedLonLatRect.Bottom<-90 then VLoadedLonLatRect.Bottom:=-90;

  VGridLonLatRect.Left := VLoadedLonLatRect.Left - z.X;
  VGridLonLatRect.Top := VLoadedLonLatRect.Top + z.Y;
  VGridLonLatRect.Right := VLoadedLonLatRect.Right + z.X;
  VGridLonLatRect.Bottom := VLoadedLonLatRect.Bottom - z.Y;
  VGeoConvert.CheckLonLatRect(VGridLonLatRect);

  VGridLonLatRect.Left := VGridLonLatRect.Left - (round(VGridLonLatRect.Left * GSHprec) mod round(z.X * GSHprec)) / GSHprec;
  VGridLonLatRect.Top := VGridLonLatRect.Top - (round(VGridLonLatRect.Top * GSHprec) mod round(z.Y * GSHprec)) / GSHprec;
  VGridLonLatRect.Bottom := VGridLonLatRect.Bottom - (round(VGridLonLatRect.Bottom * GSHprec) mod round(z.Y * GSHprec)) / GSHprec;

  VGridRect := VGeoConvert.LonLatRect2PixelRect(VGridLonLatRect, VZoom);

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.BottomRight := DoublePoint(VGridLonLatRect.Left + z.X, VGridLonLatRect.Bottom);
  VDrawRectFloat := VGeoConvert.LonLatRect2PixelRectFloat(VDrawLonLatRect, VZoom);

  if abs(VDrawRectFloat.Right - VDrawRectFloat.Left) < 4 then begin
    exit;
  end;

  if (abs(VDrawRectFloat.Right - VDrawRectFloat.Left) > 30) and (VShowText) then begin
    VCanShowText := true;
  end else begin
    VCanShowText := false;
  end;

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.Right := VGridLonLatRect.Left;
  VDrawLonLatRect.Bottom := VGridLonLatRect.Bottom;

  while VDrawLonLatRect.Left <= VGridLonLatRect.Right do begin
    VDrawRectFloat := VGeoConvert.LonLatRect2PixelRectFloat(VDrawLonLatRect, VZoom);

    VDrawScreenRect.TopLeft := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.TopLeft));
    VDrawScreenRect.BottomRight := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.BottomRight));

    Layer.bitmap.LineAS(
      VDrawScreenRect.Left, VDrawScreenRect.Top,
      VDrawScreenRect.Right, VDrawScreenRect.Bottom, VColor
    );

    VDrawLonLatRect.Left := VDrawLonLatRect.Left + z.X;
    VDrawLonLatRect.Right := VDrawLonLatRect.Left;
  end;

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.Right := VGridLonLatRect.Right;
  VDrawLonLatRect.Bottom := VGridLonLatRect.Top;

  while VDrawLonLatRect.Top - VGridLonLatRect.Bottom > -0.000001 do begin
    VDrawRectFloat := VGeoConvert.LonLatRect2PixelRectFloat(VDrawLonLatRect, VZoom);

    VDrawScreenRect.TopLeft := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.TopLeft));
    VDrawScreenRect.BottomRight := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.BottomRight));
    Layer.bitmap.LineAS(
      VDrawScreenRect.Left, VDrawScreenRect.Top,
      VDrawScreenRect.Right, VDrawScreenRect.Bottom, VColor
    );


    VDrawLonLatRect.Top := VDrawLonLatRect.Top - z.Y;
    VDrawLonLatRect.Bottom := VDrawLonLatRect.Top;
  end;

  if not VCanShowText then begin
    exit;
  end;

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.Right := VDrawLonLatRect.Left + z.X;
  VDrawLonLatRect.Bottom := VDrawLonLatRect.Top - z.Y;
  while VDrawLonLatRect.Top - VGridLonLatRect.Bottom > -0.000001 do begin
    while VDrawLonLatRect.Left + z.X / 2 <= VGridLonLatRect.Right do begin
      VDrawRectFloat := VGeoConvert.LonLatRect2PixelRectFloat(VDrawLonLatRect, VZoom);
      ListName := LonLat2GShListName(
        DoublePoint(VDrawLonLatRect.Left + z.X / 2, VDrawLonLatRect.Top - z.Y / 2),
        VScale, GSHprec
      );
      twidth := Layer.bitmap.TextWidth(ListName);
      theight := Layer.bitmap.TextHeight(ListName);

      VDrawScreenRect.TopLeft := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.TopLeft));
      VDrawScreenRect.BottomRight := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.BottomRight));

      Layer.bitmap.RenderTextW(
        VDrawScreenRect.Left + (VDrawScreenRect.Right - VDrawScreenRect.Left) div 2 - (twidth div 2),
        VDrawScreenRect.Top + (VDrawScreenRect.Bottom - VDrawScreenRect.Top) div 2 - (theight div 2),
        ListName, 0, VColor
      );

      VDrawLonLatRect.Left := VDrawLonLatRect.Right;
      VDrawLonLatRect.Right := VDrawLonLatRect.Right + z.X;
    end;
    VDrawLonLatRect.Left := VGridLonLatRect.Left;
    VDrawLonLatRect.Right := VDrawLonLatRect.Left + z.X;
    VDrawLonLatRect.Top := VDrawLonLatRect.Bottom;
    VDrawLonLatRect.Bottom := VDrawLonLatRect.Bottom - z.Y;
  end;
end;

procedure TMapLayerGrids.generate_granica;
var
  i, j: integer;
  VColor: TColor32;
  textoutx, textouty: string;
  Sz1, Sz2: TSize;
  VLoadedRect: TDoubleRect;
  VLoadedRelativeRect: TDoubleRect;
  VCurrentZoom: Byte;
  VTilesRect: TRect;
  VTileRelativeRect: TDoubleRect;
  VTileRect: TRect;
  VTileIndex: TPoint;
  VTileScreenRect: TRect;
  VTileCenter: TPoint;
  VTileSize: TPoint;
  VGridZoom: Byte;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
  VShowText: Boolean;
begin
  VLocalConverter := LayerCoordConverter;
  VGeoConvert := VLocalConverter.GetGeoConverter;
  VCurrentZoom := VLocalConverter.GetZoom;

  FConfig.TileGrid.LockRead;
  try
    VColor := FConfig.TileGrid.GridColor;
    VShowText := FConfig.TileGrid.ShowText;
    if FConfig.TileGrid.UseRelativeZoom then begin
      VGridZoom := VCurrentZoom + FConfig.TileGrid.Zoom;
    end else begin
      VGridZoom := FConfig.TileGrid.Zoom;
    end;
  finally
    FConfig.TileGrid.UnlockRead;
  end;

  if VShowText then begin
    if (VGridZoom >= VCurrentZoom - 2) and (VGridZoom <= VCurrentZoom + 3) then begin
      VLoadedRect := VLocalConverter.GetRectInMapPixelFloat;
      VGeoConvert.CheckPixelRectFloat(VLoadedRect, VCurrentZoom);

      VLoadedRelativeRect := VGeoConvert.PixelRectFloat2RelativeRect(VLoadedRect, VCurrentZoom);
      VTilesRect := VGeoConvert.RelativeRect2TileRect(VLoadedRelativeRect, VGridZoom);
      for i := VTilesRect.Top to VTilesRect.Bottom - 1 do begin
        VTileIndex.Y := i;
        for j := VTilesRect.Left to VTilesRect.Right - 1 do begin
          VTileIndex.X := j;
          VTileRelativeRect := VGeoConvert.TilePos2RelativeRect(VTileIndex, VGridZoom);
          VTileRect := VGeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
          VTileScreenRect.TopLeft := VLocalConverter.MapPixel2LocalPixel(VTileRect.TopLeft);
          VTileScreenRect.BottomRight := VLocalConverter.MapPixel2LocalPixel(VTileRect.BottomRight);

          VTileSize.X := VTileRect.Right - VTileRect.Left;
          VTileSize.Y := VTileRect.Bottom - VTileRect.Top;
          VTileCenter.X := VTileScreenRect.Left + VTileSize.X div 2;
          VTileCenter.Y := VTileScreenRect.Top + VTileSize.Y div 2;
          textoutx := 'x=' + inttostr(VTileIndex.X);
          textouty := 'y=' + inttostr(VTileIndex.Y);
          Sz1 := Layer.bitmap.TextExtent(textoutx);
          Sz2 := Layer.bitmap.TextExtent(textouty);
          if (Sz1.cx < VTileSize.X) and (Sz2.cx < VTileSize.X) then begin
            Layer.bitmap.RenderText(VTileCenter.X - (Sz1.cx div 2) + 1, VTileCenter.Y - Sz2.cy, textoutx, 0, VColor);
            Layer.bitmap.RenderText(VTileCenter.X - (Sz2.cx div 2) + 1, VTileCenter.Y, textouty, 0, VColor);
          end;
        end;
      end;
    end;
  end;
end;

function TMapLayerGrids.GetVisibleForNewPos(ANewVisualCoordConverter: ILocalCoordConverter): Boolean;
var
  VConverter: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
  VZoom: Byte;
  VTileGridZoom: Byte;
begin
  Result := false;
  VConverter := ANewVisualCoordConverter;
  if VConverter <> nil then begin
    VGeoConverter := VConverter.GetGeoConverter;
    VZoom := VConverter.GetZoom;
    FConfig.LockRead;
    try
      if FConfig.TileGrid.Visible then begin
        if FConfig.TileGrid.ShowText then begin
          if FConfig.TileGrid.UseRelativeZoom then begin
            VTileGridZoom := VZoom + FConfig.TileGrid.Zoom;
          end else begin
            VTileGridZoom := FConfig.TileGrid.Zoom;
          end;
          if VGeoConverter.CheckZoom(VTileGridZoom) then begin
            Result := (VTileGridZoom >= VZoom - 2) and (VTileGridZoom <= VZoom + 3);
          end;
        end;
      end;
      if FConfig.GenShtabGrid.Visible then begin
        if FConfig.GenShtabGrid.Scale > 0 then begin
          Result := True;
        end;
      end;
    finally
      FConfig.UnlockRead;
    end;
  end;
end;

procedure TMapLayerGrids.OnConfigChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
    SetVisible(GetVisibleForNewPos(ViewPortState.GetVisualCoordConverter));
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TMapLayerGrids.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
