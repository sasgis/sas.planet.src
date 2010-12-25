unit u_MapLayerGrids;

interface

uses
  Windows,
  Types,
  GR32,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TMapLayerGrids = class(TMapLayerBasic)
  private
    procedure generate_granica;
    procedure DrawGenShBorders;
  protected
    procedure DoRedraw; override;
  end;

implementation

uses
  SysUtils,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  Ugeofun,
  u_GeoToStr,
  u_GlobalState;

const
  GSHprec = 100000000;

{ TMapLayerGrids }

procedure TMapLayerGrids.DoRedraw;
begin
  inherited;
  generate_granica;
  DrawGenShBorders;
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
  VColor: TColor32;
  VDrawScreenRect: TRect;
  VShowText: Boolean;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
begin
  if GState.GShScale = 0 then begin
    exit;
  end;
  z := GetGhBordersStepByScale(GState.GShScale);
  VLocalConverter := FBitmapCoordConverter;
  VGeoConvert := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VLoadedRect := VLocalConverter.GetRectInMapPixelFloat;

  VGeoConvert.CheckPixelRectFloat(VLoadedRect, VZoom);

  VLoadedLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VLoadedRect, VZoom);

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

  if (abs(VDrawRectFloat.Right - VDrawRectFloat.Left) > 30) and (GState.ShowBorderText) then begin
    VShowText := true;
  end else begin
    VShowText := false;
  end;

  VColor := SetAlpha(Color32(GState.BorderColor), GState.BorderAlpha);

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.Right := VGridLonLatRect.Left;
  VDrawLonLatRect.Bottom := VGridLonLatRect.Bottom;

  while VDrawLonLatRect.Left <= VGridLonLatRect.Right do begin
    VDrawRectFloat := VGeoConvert.LonLatRect2PixelRectFloat(VDrawLonLatRect, VZoom);

    VDrawScreenRect.TopLeft := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.TopLeft));
    VDrawScreenRect.BottomRight := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.BottomRight));

    FLayer.bitmap.LineAS(
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
    FLayer.bitmap.LineAS(
      VDrawScreenRect.Left, VDrawScreenRect.Top,
      VDrawScreenRect.Right, VDrawScreenRect.Bottom, VColor
    );


    VDrawLonLatRect.Top := VDrawLonLatRect.Top - z.Y;
    VDrawLonLatRect.Bottom := VDrawLonLatRect.Top;
  end;

  if not VShowText then begin
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
        GState.GShScale, GSHprec
      );
      twidth := FLayer.bitmap.TextWidth(ListName);
      theight := FLayer.bitmap.TextHeight(ListName);

      VDrawScreenRect.TopLeft := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.TopLeft));
      VDrawScreenRect.BottomRight := FloatPoint2RectWihtClip(VLocalConverter.MapPixelFloat2LocalPixelFloat(VDrawRectFloat.BottomRight));

      FLayer.bitmap.RenderTextW(
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
  drawcolor: TColor32;
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
  VTilesLineRect: TRect;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
  VBitmapRect: TDoubleRect;
begin
  VLocalConverter := FBitmapCoordConverter;
  VGeoConvert := VLocalConverter.GetGeoConverter;
  VCurrentZoom := VLocalConverter.GetZoom;
  if GState.TileGridZoom = 99 then begin
    VGridZoom := VCurrentZoom;
  end else begin
    VGridZoom := GState.TileGridZoom - 1;
  end;
  if (VGridZoom < VCurrentZoom) or (VGridZoom - VCurrentZoom > 5) then begin
    exit;
  end;

  VLoadedRect := VLocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VLoadedRect, VCurrentZoom);

  VLoadedRelativeRect := VGeoConvert.PixelRectFloat2RelativeRect(VLoadedRect, VCurrentZoom);
  VTilesRect := VGeoConvert.RelativeRect2TileRect(VLoadedRelativeRect, VGridZoom);

  drawcolor := SetAlpha(Color32(GState.BorderColor), GState.BorderAlpha);

  VTilesLineRect.Left := VTilesRect.Left;
  VTilesLineRect.Right := VTilesRect.Right;
  for i := VTilesRect.Top to VTilesRect.Bottom do begin
    VTilesLineRect.Top := i;
    VTilesLineRect.Bottom := i + 1;

    VTileRelativeRect := VGeoConvert.TileRect2RelativeRect(VTilesLineRect, VGridZoom);
    VTileRect := VGeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
    VTileScreenRect.TopLeft := VLocalConverter.MapPixel2LocalPixel(VTileRect.TopLeft);
    VTileScreenRect.BottomRight := VLocalConverter.MapPixel2LocalPixel(VTileRect.BottomRight);
    FLayer.bitmap.LineAS(VTileScreenRect.Left, VTileScreenRect.Top,
      VTileScreenRect.Right, VTileScreenRect.Top, drawcolor);
  end;

  VTilesLineRect.Top := VTilesRect.Top;
  VTilesLineRect.Bottom := VTilesRect.Bottom;
  for j := VTilesRect.Left to VTilesRect.Right do begin
    VTilesLineRect.Left := j;
    VTilesLineRect.Right := j + 1;

    VTileRelativeRect := VGeoConvert.TileRect2RelativeRect(VTilesLineRect, VGridZoom);
    VTileRect := VGeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
    VTileScreenRect.TopLeft := VLocalConverter.MapPixel2LocalPixel(VTileRect.TopLeft);
    VTileScreenRect.BottomRight := VLocalConverter.MapPixel2LocalPixel(VTileRect.BottomRight);
    FLayer.bitmap.LineAS(VTileScreenRect.Left, VTileScreenRect.Top,
      VTileScreenRect.Left, VTileScreenRect.Bottom, drawcolor);
  end;

  if not (GState.ShowBorderText) then begin
    exit;
  end;
  if VGridZoom - VCurrentZoom > 2 then begin
    exit;
  end;

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
      Sz1 := FLayer.bitmap.TextExtent(textoutx);
      Sz2 := FLayer.bitmap.TextExtent(textouty);
      if (Sz1.cx < VTileSize.X) and (Sz2.cx < VTileSize.X) then begin
        FLayer.bitmap.RenderText(VTileCenter.X - (Sz1.cx div 2) + 1, VTileCenter.Y - Sz2.cy, textoutx, 0, drawcolor);
        FLayer.bitmap.RenderText(VTileCenter.X - (Sz2.cx div 2) + 1, VTileCenter.Y, textouty, 0, drawcolor);
      end;
    end;
  end;
end;

end.
