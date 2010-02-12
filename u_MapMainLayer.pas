unit u_MapMainLayer;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TMapMainLayer = class(TMapLayerBasic)
  protected
    procedure DrawGenShBorders;
    procedure generate_granica;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    destructor Destroy; override;
  end;

const
  GSHprec=100000000;


implementation

uses
  SysUtils,
  Ugeofun,
  u_GeoToStr,
  u_GlobalState;
{ TMapMainLayer }

constructor TMapMainLayer.Create(AParentMap: TImage32; ACenter: TPoint);
begin

end;

destructor TMapMainLayer.Destroy;
begin

  inherited;
end;

procedure TMapMainLayer.DoRedraw;
begin
  inherited;

end;

procedure TMapMainLayer.DrawGenShBorders;
var
  zLonR,zLatR:extended;
  twidth,theight:integer;
  ListName:WideString;
  VZoomCurr: Byte;
  VLoadedRect: TRect;
  VLoadedLonLatRect: TExtendedRect;
  VGridLonLatRect: TExtendedRect;
  VGridRect: TRect;
  VDrawLonLatRect: TExtendedRect;
  VDrawRect: TRect;
  VColor: TColor32;
  VDrawScreenRect: TRect;
  VShowText: Boolean;
begin
  if GState.GShScale=0 then exit;
  case GState.GShScale of
    1000000: begin zLonR:=6; zLatR:=4; end;
     500000: begin zLonR:=3; zLatR:=2; end;
     200000: begin zLonR:=1; zLatR:=0.66666666666666666666666666666667; end;
     100000: begin zLonR:=0.5; zLatR:=0.33333333333333333333333333333333; end;
      50000: begin zLonR:=0.25; zLatR:=0.1666666666666666666666666666665; end;
      25000: begin zLonR:=0.125; zLatR:=0.08333333333333333333333333333325; end;
      10000: begin zLonR:=0.0625; zLatR:=0.041666666666666666666666666666625; end;
    else begin zLonR:=6; zLatR:=4; end;
  end;
  VZoomCurr := FZoom;
  VLoadedRect.TopLeft := BitmapPixel2MapPixel(Point(0, 0));
  VLoadedRect.BottomRight := BitmapPixel2MapPixel(GetBitmapSizeInPixel);

  FGeoConvert.CheckPixelRect(VLoadedRect, VZoomCurr, False);
  VLoadedLonLatRect := FGeoConvert.PixelRect2LonLatRect(VLoadedRect, VZoomCurr);

  VGridLonLatRect.Left := VLoadedLonLatRect.Left-zLonR;
  VGridLonLatRect.Top := VLoadedLonLatRect.Top+zLatR;
  VGridLonLatRect.Right := VLoadedLonLatRect.Right+zLonR;
  VGridLonLatRect.Bottom := VLoadedLonLatRect.Bottom-zLatR;
  FGeoConvert.CheckLonLatRect(VGridLonLatRect);

  VGridLonLatRect.Left := VGridLonLatRect.Left-(round(VGridLonLatRect.Left*GSHprec) mod round(zLonR*GSHprec))/GSHprec;
  VGridLonLatRect.Top := VGridLonLatRect.Top-(round(VGridLonLatRect.Top*GSHprec) mod round(zLatR*GSHprec))/GSHprec;
  VGridLonLatRect.Bottom := VGridLonLatRect.Bottom-(round(VGridLonLatRect.Bottom*GSHprec) mod round(zLatR*GSHprec))/GSHprec;

  VGridRect := FGeoConvert.LonLatRect2PixelRect(VGridLonLatRect, VZoomCurr);

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.BottomRight := ExtPoint(VGridLonLatRect.Left+zLonR, VGridLonLatRect.Bottom);
  VDrawRect := FGeoConvert.LonLatRect2PixelRect(VDrawLonLatRect, VZoomCurr);

  if abs(VDrawRect.Right - VDrawRect.Left) < 4 then exit;

  if (abs(VDrawRect.Right - VDrawRect.Left) > 30)and(GState.ShowBorderText) then begin
    VShowText := true;
  end else begin
    VShowText := false;
  end;

  VColor := SetAlpha(Color32(GState.BorderColor), GState.BorderAlpha);

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.Right := VGridLonLatRect.Left;
  VDrawLonLatRect.Bottom := VGridLonLatRect.Bottom;

  while VDrawLonLatRect.Left <=VGridLonLatRect.Right do begin
    VDrawRect := FGeoConvert.LonLatRect2PixelRect(VDrawLonLatRect, VZoomCurr);

    VDrawScreenRect.TopLeft := MapPixel2BitmapPixel(VDrawRect.TopLeft);
    VDrawScreenRect.BottomRight := MapPixel2BitmapPixel(VDrawRect.BottomRight);
    FLayer.bitmap.LineAS(
      VDrawScreenRect.Left, VDrawScreenRect.Top,
      VDrawScreenRect.Right, VDrawScreenRect.Bottom, VColor
    );

    VDrawLonLatRect.Left := VDrawLonLatRect.Left+zLonR;
    VDrawLonLatRect.Right := VDrawLonLatRect.Left;
  end;

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.Right := VGridLonLatRect.Right;
  VDrawLonLatRect.Bottom := VGridLonLatRect.Top;

  while VDrawLonLatRect.Top - VGridLonLatRect.Bottom > -0.000001 do begin
    VDrawRect := FGeoConvert.LonLatRect2PixelRect(VDrawLonLatRect, VZoomCurr);

    VDrawScreenRect.TopLeft := MapPixel2BitmapPixel(VDrawRect.TopLeft);
    VDrawScreenRect.BottomRight := MapPixel2BitmapPixel(VDrawRect.BottomRight);
    FLayer.bitmap.LineAS(
      VDrawScreenRect.Left, VDrawScreenRect.Top,
      VDrawScreenRect.Right, VDrawScreenRect.Bottom, VColor
    );


    VDrawLonLatRect.Top := VDrawLonLatRect.Top-zLatR;
    VDrawLonLatRect.Bottom := VDrawLonLatRect.Top;
  end;

  if not VShowText then exit;

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.Right := VDrawLonLatRect.Left + zLonR;
  VDrawLonLatRect.Bottom := VDrawLonLatRect.Top - zLatR;
  while VDrawLonLatRect.Top - VGridLonLatRect.Bottom > -0.000001 do begin
    while VDrawLonLatRect.Left + zLonR/2 <=VGridLonLatRect.Right do begin
      VDrawRect := FGeoConvert.LonLatRect2PixelRect(VDrawLonLatRect, VZoomCurr);
      ListName := LonLat2GShListName(
        ExtPoint(VDrawLonLatRect.Left + zLonR/2, VDrawLonLatRect.Top - zLatR/2),
        GState.GShScale, GSHprec
      );
      twidth := FLayer.bitmap.TextWidth(ListName);
      theight := FLayer.bitmap.TextHeight(ListName);

      VDrawScreenRect.TopLeft := MapPixel2BitmapPixel(VDrawRect.TopLeft);
      VDrawScreenRect.BottomRight := MapPixel2BitmapPixel(VDrawRect.BottomRight);

      FLayer.bitmap.RenderTextW(
        VDrawScreenRect.Left + (VDrawScreenRect.Right - VDrawScreenRect.Left) div 2 - (twidth div 2),
        VDrawScreenRect.Top + (VDrawScreenRect.Bottom - VDrawScreenRect.Top) div 2 - (theight div 2),
        ListName, 0, VColor
      );

      VDrawLonLatRect.Left := VDrawLonLatRect.Right;
      VDrawLonLatRect.Right := VDrawLonLatRect.Right + zLonR;
    end;
    VDrawLonLatRect.Left := VGridLonLatRect.Left;
    VDrawLonLatRect.Right := VDrawLonLatRect.Left + zLonR;
    VDrawLonLatRect.Top := VDrawLonLatRect.Bottom;
    VDrawLonLatRect.Bottom := VDrawLonLatRect.Bottom-zLatR;
  end;
end;

procedure TMapMainLayer.generate_granica;
var
    i,j:integer;
    drawcolor:TColor32;
    textoutx,textouty:string;
    Sz1,Sz2: TSize;
    VLoadedRect: TRect;
    VLoadedRelativeRect: TExtendedRect;
    VCurrentZoom: Byte;
    VTilesRect: TRect;
    VTileRelativeRect: TExtendedRect;
    VTileRect: TRect;
    VTileIndex: TPoint;
    VTileScreenRect: TRect;
    VTileCenter: TPoint;
    VTileSize: TPoint;
    VGridZoom: Byte;
    VTilesLineRect: TRect;
begin
  VCurrentZoom := GState.zoom_size - 1;
  if GState.TileGridZoom=99 then begin
    VGridZoom := VCurrentZoom;
  end else begin
    VGridZoom := GState.TileGridZoom - 1;
  end;
  if (VGridZoom < VCurrentZoom) or (VGridZoom - VCurrentZoom > 5) then exit;

  VLoadedRect.TopLeft := BitmapPixel2MapPixel(Point(0, 0));
  VLoadedRect.BottomRight := BitmapPixel2MapPixel(GetBitmapSizeInPixel);

  GState.sat_map_both.GeoConvert.CheckPixelRect(VLoadedRect, VCurrentZoom, False);
  VLoadedRelativeRect := GState.sat_map_both.GeoConvert.PixelRect2RelativeRect(VLoadedRect, VCurrentZoom);
  VTilesRect := GState.sat_map_both.GeoConvert.RelativeRect2TileRect(VLoadedRelativeRect, VGridZoom);

  drawcolor:=SetAlpha(Color32(GState.BorderColor),GState.BorderAlpha);

  VTilesLineRect.Left := VTilesRect.Left;
  VTilesLineRect.Right := VTilesRect.Right;
  for i := VTilesRect.Top to VTilesRect.Bottom do begin
    VTilesLineRect.Top := i;
    VTilesLineRect.Bottom := i;

    VTileRelativeRect := GState.sat_map_both.GeoConvert.TileRect2RelativeRect(VTilesLineRect, VGridZoom);
    VTileRect := GState.sat_map_both.GeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
    VTileScreenRect.TopLeft := MapPixel2BitmapPixel(VTileRect.TopLeft);
    VTileScreenRect.BottomRight := MapPixel2BitmapPixel(VTileRect.BottomRight);
    FLayer.bitmap.LineAS(VTileScreenRect.Left, VTileScreenRect.Top,
      VTileScreenRect.Right, VTileScreenRect.Top, drawcolor);
  end;

  VTilesLineRect.Top := VTilesRect.Top;
  VTilesLineRect.Bottom := VTilesRect.Bottom;
  for j := VTilesRect.Left to VTilesRect.Right do begin
    VTilesLineRect.Left := j;
    VTilesLineRect.Right := j;

    VTileRelativeRect := GState.sat_map_both.GeoConvert.TileRect2RelativeRect(VTilesLineRect, VGridZoom);
    VTileRect := GState.sat_map_both.GeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
    VTileScreenRect.TopLeft := MapPixel2BitmapPixel(VTileRect.TopLeft);
    VTileScreenRect.BottomRight := MapPixel2BitmapPixel(VTileRect.BottomRight);
    FLayer.bitmap.LineAS(VTileScreenRect.Left, VTileScreenRect.Top,
      VTileScreenRect.Left, VTileScreenRect.Bottom, drawcolor);
  end;

  if not (GState.ShowBorderText) then exit;
  if VGridZoom - VCurrentZoom > 2 then exit;

  for i := VTilesRect.Top to VTilesRect.Bottom do begin
    VTileIndex.Y := i;
    for j := VTilesRect.Left to VTilesRect.Right do begin
      VTileIndex.X := j;
      VTileRelativeRect := GState.sat_map_both.GeoConvert.TilePos2RelativeRect(VTileIndex, VGridZoom);
      VTileRect := GState.sat_map_both.GeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
      VTileScreenRect.TopLeft := MapPixel2BitmapPixel(VTileRect.TopLeft);
      VTileScreenRect.BottomRight := MapPixel2BitmapPixel(VTileRect.BottomRight);

      VTileSize.X := VTileRect.Right - VTileRect.Left;
      VTileSize.Y := VTileRect.Bottom - VTileRect.Top;
      VTileCenter.X := VTileScreenRect.Left + VTileSize.X div 2;
      VTileCenter.Y := VTileScreenRect.Top + VTileSize.Y div 2;
      textoutx := 'x='+inttostr(VTileIndex.X);
      textouty := 'y='+inttostr(VTileIndex.Y);
      Sz1 := FLayer.bitmap.TextExtent(textoutx);
      Sz2 := FLayer.bitmap.TextExtent(textouty);
      if (Sz1.cx < VTileSize.X) and (Sz2.cx < VTileSize.X) then begin
        FLayer.bitmap.RenderText(VTileCenter.X-(Sz1.cx div 2)+1,VTileCenter.Y-Sz2.cy,textoutx,0, drawcolor);
        FLayer.bitmap.RenderText(VTileCenter.X-(Sz2.cx div 2)+1,VTileCenter.Y,textouty,0,drawcolor);
      end;
    end;
  end;
end;

end.
 