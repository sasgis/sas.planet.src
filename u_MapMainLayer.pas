unit u_MapMainLayer;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  t_CommonTypes,
  u_MapViewPortState,
  u_TileDownloaderUI,
  UMapType,
  u_MapLayerBasic;

type
  TMapMainLayer = class(TMapLayerBasic)
  private
    FUIDownLoader: TTileDownloaderUI;
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(const Value: TTileSource);
  protected
    procedure DrawGenShBorders;
    procedure generate_granica;
    procedure DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
    property UseDownload: TTileSource read GetUseDownload write SetUseDownload;
  end;

const
  GSHprec = 100000000;


implementation

uses
  ActiveX,
  SysUtils,
  i_ICoordConverter,
  i_MapTypes,
  Ugeofun,
  Uimgfun,
  u_GeoToStr,
  u_GlobalState,
  u_WindowLayerBasic;

{ TMapMainLayer }

constructor TMapMainLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FUIDownLoader := TTileDownloaderUI.Create;
end;

destructor TMapMainLayer.Destroy;
var
  VWaitResult: DWORD;
begin
  VWaitResult := WaitForSingleObject(FUIDownLoader.Handle, 10000);
  if VWaitResult = WAIT_TIMEOUT then begin
    TerminateThread(FUIDownLoader.Handle, 0);
  end;
  FreeAndNil(FUIDownLoader);
  inherited;
end;

procedure TMapMainLayer.DoRedraw;
var
  i: Cardinal;
  VMapType: TMapType;
  VGUID: TGUID;
  VItem: IMapType;
  VEnum: IEnumGUID;
  VHybrList: IMapTypeList;
begin
  inherited;
  FUIDownLoader.change_scene := true;
  FLayer.Bitmap.Clear(Color32(GState.BGround));
  DrawMap(GState.ViewState.GetCurrentMap, dmOpaque);

  VHybrList := GState.ViewState.HybrList;
  VEnum := VHybrList.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    if GState.ViewState.IsHybrGUIDSelected(VGUID) then begin
      VItem := VHybrList.GetMapTypeByGUID(VGUID);
      VMapType := VItem.GetMapType;
      if VMapType.IsBitmapTiles then begin
        DrawMap(VMapType, dmBlend);
      end;
    end;
  end;
  generate_granica;
  DrawGenShBorders;
end;

procedure TMapMainLayer.DrawGenShBorders;
var
  zLonR, zLatR: extended;
  twidth, theight: integer;
  ListName: WideString;
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
  if GState.GShScale = 0 then begin
    exit;
  end;
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

  VGridLonLatRect.Left := VLoadedLonLatRect.Left - zLonR;
  VGridLonLatRect.Top := VLoadedLonLatRect.Top + zLatR;
  VGridLonLatRect.Right := VLoadedLonLatRect.Right + zLonR;
  VGridLonLatRect.Bottom := VLoadedLonLatRect.Bottom - zLatR;
  FGeoConvert.CheckLonLatRect(VGridLonLatRect);

  VGridLonLatRect.Left := VGridLonLatRect.Left - (round(VGridLonLatRect.Left * GSHprec) mod round(zLonR * GSHprec)) / GSHprec;
  VGridLonLatRect.Top := VGridLonLatRect.Top - (round(VGridLonLatRect.Top * GSHprec) mod round(zLatR * GSHprec)) / GSHprec;
  VGridLonLatRect.Bottom := VGridLonLatRect.Bottom - (round(VGridLonLatRect.Bottom * GSHprec) mod round(zLatR * GSHprec)) / GSHprec;

  VGridRect := FGeoConvert.LonLatRect2PixelRect(VGridLonLatRect, VZoomCurr);

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.BottomRight := ExtPoint(VGridLonLatRect.Left + zLonR, VGridLonLatRect.Bottom);
  VDrawRect := FGeoConvert.LonLatRect2PixelRect(VDrawLonLatRect, VZoomCurr);

  if abs(VDrawRect.Right - VDrawRect.Left) < 4 then begin
    exit;
  end;

  if (abs(VDrawRect.Right - VDrawRect.Left) > 30) and (GState.ShowBorderText) then begin
    VShowText := true;
  end else begin
    VShowText := false;
  end;

  VColor := SetAlpha(Color32(GState.BorderColor), GState.BorderAlpha);

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.Right := VGridLonLatRect.Left;
  VDrawLonLatRect.Bottom := VGridLonLatRect.Bottom;

  while VDrawLonLatRect.Left <= VGridLonLatRect.Right do begin
    VDrawRect := FGeoConvert.LonLatRect2PixelRect(VDrawLonLatRect, VZoomCurr);

    VDrawScreenRect.TopLeft := MapPixel2BitmapPixel(VDrawRect.TopLeft);
    VDrawScreenRect.BottomRight := MapPixel2BitmapPixel(VDrawRect.BottomRight);
    FLayer.bitmap.LineAS(
      VDrawScreenRect.Left, VDrawScreenRect.Top,
      VDrawScreenRect.Right, VDrawScreenRect.Bottom, VColor
    );

    VDrawLonLatRect.Left := VDrawLonLatRect.Left + zLonR;
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


    VDrawLonLatRect.Top := VDrawLonLatRect.Top - zLatR;
    VDrawLonLatRect.Bottom := VDrawLonLatRect.Top;
  end;

  if not VShowText then begin
    exit;
  end;

  VDrawLonLatRect.TopLeft := VGridLonLatRect.TopLeft;
  VDrawLonLatRect.Right := VDrawLonLatRect.Left + zLonR;
  VDrawLonLatRect.Bottom := VDrawLonLatRect.Top - zLatR;
  while VDrawLonLatRect.Top - VGridLonLatRect.Bottom > -0.000001 do begin
    while VDrawLonLatRect.Left + zLonR / 2 <= VGridLonLatRect.Right do begin
      VDrawRect := FGeoConvert.LonLatRect2PixelRect(VDrawLonLatRect, VZoomCurr);
      ListName := LonLat2GShListName(
        ExtPoint(VDrawLonLatRect.Left + zLonR / 2, VDrawLonLatRect.Top - zLatR / 2),
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
    VDrawLonLatRect.Bottom := VDrawLonLatRect.Bottom - zLatR;
  end;
end;

procedure TMapMainLayer.DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
var
  VZoom: Byte;
  VSourceMapType: TMapType;
  VBmp: TBitmap32;

  {
    Прямоугольник пикселей растра в координатах текущей основной карты
  }
  VBitmapOnMapPixelRect: TRect;

  {
    Географические координаты растра
  }
  VSourceLonLatRect: TExtendedRect;

  {
    Прямоугольник пикселов текущего зума, покрывающий растр, в кооординатах
    карты для которой строится слой
  }
  VPixelSourceRect: TRect;

  {
    Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    карты, для которой строится слой
  }
  VTileSourceRect: TRect;

  {
    Текущий тайл в кооординатах карты, для которой строится слой
  }
  VTile: TPoint;

  {
    Прямоугольник пикслов текущего тайла в кооординатах карты,
    для которой строится слой
  }
  VCurrTilePixelRectSource: TRect;

  {
    Прямоугольник пикслов текущего тайла в кооординатах текущей карты
  }
  VCurrTilePixelRect: TRect;

  {
    Прямоугольник пикслов текущего тайла в кооординатах текущего растра
  }
  VCurrTilePixelRectAtBitmap: TRect;

  {
    Прямоугольник тайла подлежащий отображению на текущий растр
  }
  VTilePixelsToDraw: TRect;


  VSourceGeoConvert: ICoordConverter;
  VGeoConvert: ICoordConverter;
  i, j: integer;
  VUsePre: Boolean;
begin
  if AMapType.asLayer then begin
    VUsePre := GState.UsePrevZoomLayer;
  end else begin
    VUsePre := GState.UsePrevZoom;
  end;

  VBmp := TBitmap32.Create;
  try
    VZoom := FZoom;
    VSourceMapType := AMapType;
    VSourceGeoConvert := VSourceMapType.GeoConvert;
    VGeoConvert := FGeoConvert;
    VBitmapOnMapPixelRect.TopLeft := BitmapPixel2MapPixel(Point(0, 0));
    VBitmapOnMapPixelRect.BottomRight := BitmapPixel2MapPixel(GetBitmapSizeInPixel);
    VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom, False);
    VSourceLonLatRect := VGeoConvert.PixelRect2LonLatRect(VBitmapOnMapPixelRect, VZoom);
    VPixelSourceRect := VSourceGeoConvert.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);
    VTileSourceRect := VSourceGeoConvert.PixelRect2TileRect(VPixelSourceRect, VZoom);

    for i := VTileSourceRect.Left to VTileSourceRect.Right do begin
      VTile.X := i;
      for j := VTileSourceRect.Top to VTileSourceRect.Bottom do begin
        VTile.Y := j;
        VCurrTilePixelRectSource := VSourceGeoConvert.TilePos2PixelRect(VTile, VZoom);
        VTilePixelsToDraw.TopLeft := Point(0, 0);
        VTilePixelsToDraw.Right := VCurrTilePixelRectSource.Right - VCurrTilePixelRectSource.Left + 1;
        VTilePixelsToDraw.Bottom := VCurrTilePixelRectSource.Bottom - VCurrTilePixelRectSource.Top + 1;

        if VCurrTilePixelRectSource.Left < VPixelSourceRect.Left then begin
          VTilePixelsToDraw.Left := VPixelSourceRect.Left - VCurrTilePixelRectSource.Left;
          VCurrTilePixelRectSource.Left := VPixelSourceRect.Left;
        end;

        if VCurrTilePixelRectSource.Top < VPixelSourceRect.Top then begin
          VTilePixelsToDraw.Top := VPixelSourceRect.Top - VCurrTilePixelRectSource.Top;
          VCurrTilePixelRectSource.Top := VPixelSourceRect.Top;
        end;

        if VCurrTilePixelRectSource.Right > VPixelSourceRect.Right then begin
          VTilePixelsToDraw.Right := VTilePixelsToDraw.Right - (VCurrTilePixelRectSource.Right - VPixelSourceRect.Right);
          VCurrTilePixelRectSource.Right := VPixelSourceRect.Right;
        end;

        if VCurrTilePixelRectSource.Bottom > VPixelSourceRect.Bottom then begin
          VTilePixelsToDraw.Bottom := VTilePixelsToDraw.Bottom - (VCurrTilePixelRectSource.Bottom - VPixelSourceRect.Bottom);
          VCurrTilePixelRectSource.Bottom := VPixelSourceRect.Bottom;
        end;

        VCurrTilePixelRect.TopLeft := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.TopLeft, VZoom, VGeoConvert);
        VCurrTilePixelRect.BottomRight := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.BottomRight, VZoom, VGeoConvert);

        VCurrTilePixelRectAtBitmap.TopLeft := MapPixel2BitmapPixel(VCurrTilePixelRect.TopLeft);
        VCurrTilePixelRectAtBitmap.BottomRight := MapPixel2BitmapPixel(VCurrTilePixelRect.BottomRight);
        Inc(VCurrTilePixelRectAtBitmap.Bottom);
        Inc(VCurrTilePixelRectAtBitmap.Right);
        if VSourceMapType.LoadTileOrPreZ(VBmp, VTile, VZoom, true, False, VUsePre) then begin
          Gamma(VBmp);
        end;
        FLayer.Bitmap.Lock;
        try
          VBmp.DrawMode := ADrawMode;
          FLayer.Bitmap.Draw(VCurrTilePixelRectAtBitmap, VTilePixelsToDraw, Vbmp);
        finally
          FLayer.Bitmap.UnLock;
        end;
      end;
    end;
  finally
    VBmp.Free;
  end;
end;

procedure TMapMainLayer.generate_granica;
var
  i, j: integer;
  drawcolor: TColor32;
  textoutx, textouty: string;
  Sz1, Sz2: TSize;
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
  VCurrentZoom := FZoom;
  if GState.TileGridZoom = 99 then begin
    VGridZoom := VCurrentZoom;
  end else begin
    VGridZoom := GState.TileGridZoom - 1;
  end;
  if (VGridZoom < VCurrentZoom) or (VGridZoom - VCurrentZoom > 5) then begin
    exit;
  end;

  VLoadedRect.TopLeft := BitmapPixel2MapPixel(Point(0, 0));
  VLoadedRect.BottomRight := BitmapPixel2MapPixel(GetBitmapSizeInPixel);

  FGeoConvert.CheckPixelRect(VLoadedRect, VCurrentZoom, False);
  VLoadedRelativeRect := FGeoConvert.PixelRect2RelativeRect(VLoadedRect, VCurrentZoom);
  VTilesRect := FGeoConvert.RelativeRect2TileRect(VLoadedRelativeRect, VGridZoom);

  drawcolor := SetAlpha(Color32(GState.BorderColor), GState.BorderAlpha);

  VTilesLineRect.Left := VTilesRect.Left;
  VTilesLineRect.Right := VTilesRect.Right;
  for i := VTilesRect.Top to VTilesRect.Bottom do begin
    VTilesLineRect.Top := i;
    VTilesLineRect.Bottom := i;

    VTileRelativeRect := FGeoConvert.TileRect2RelativeRect(VTilesLineRect, VGridZoom);
    VTileRect := FGeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
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

    VTileRelativeRect := FGeoConvert.TileRect2RelativeRect(VTilesLineRect, VGridZoom);
    VTileRect := FGeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
    VTileScreenRect.TopLeft := MapPixel2BitmapPixel(VTileRect.TopLeft);
    VTileScreenRect.BottomRight := MapPixel2BitmapPixel(VTileRect.BottomRight);
    FLayer.bitmap.LineAS(VTileScreenRect.Left, VTileScreenRect.Top,
      VTileScreenRect.Left, VTileScreenRect.Bottom, drawcolor);
  end;

  if not (GState.ShowBorderText) then begin
    exit;
  end;
  if VGridZoom - VCurrentZoom > 2 then begin
    exit;
  end;

  for i := VTilesRect.Top to VTilesRect.Bottom do begin
    VTileIndex.Y := i;
    for j := VTilesRect.Left to VTilesRect.Right do begin
      VTileIndex.X := j;
      VTileRelativeRect := FGeoConvert.TilePos2RelativeRect(VTileIndex, VGridZoom);
      VTileRect := FGeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
      VTileScreenRect.TopLeft := MapPixel2BitmapPixel(VTileRect.TopLeft);
      VTileScreenRect.BottomRight := MapPixel2BitmapPixel(VTileRect.BottomRight);

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

function TMapMainLayer.GetUseDownload: TTileSource;
begin
  Result := FUIDownLoader.UseDownload;
end;

procedure TMapMainLayer.SendTerminateToThreads;
begin
  inherited;
  FUIDownLoader.Terminate;
end;

procedure TMapMainLayer.SetUseDownload(const Value: TTileSource);
begin
  FUIDownLoader.UseDownload := Value;
  FUIDownLoader.change_scene := True;
end;

procedure TMapMainLayer.StartThreads;
begin
  inherited;
  FUIDownLoader.Resume;
end;

end.
