unit u_MapMainLayer;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  t_CommonTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState,
  u_TileDownloaderUI,
  u_MapLayerShowError,
  UMapType,
  u_MapLayerBasic;

type
  TMapMainLayer = class(TMapLayerBasic)
  private
    FUIDownLoader: TTileDownloaderUI;
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(const Value: TTileSource);
    procedure SetErrorShowLayer(const Value: TTileErrorInfoLayer);
    function GetUseDownloadChangeNotifier: IJclNotifier;
    procedure SetKmlLayer(const Value: TMapLayerBasic);
  protected
    procedure DrawGenShBorders;
    procedure generate_granica;
    procedure DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
    property UseDownload: TTileSource read GetUseDownload write SetUseDownload;
    property UseDownloadChangeNotifier: IJclNotifier read GetUseDownloadChangeNotifier;
    property ErrorShowLayer: TTileErrorInfoLayer write SetErrorShowLayer;
    property KmlLayer: TMapLayerBasic write SetKmlLayer;
  end;

const
  GSHprec = 100000000;


implementation

uses
  ActiveX,
  SysUtils,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  i_ITileIterator,
  i_MapTypes,
  Ugeofun,
  Uimgfun,
  u_TileIteratorByRect,
  u_GeoToStr,
  u_GlobalState,
  u_WindowLayerBasic;

{ TMapMainLayer }

constructor TMapMainLayer.Create(
  AParentMap: TImage32;
  AViewPortState: TMapViewPortState
);
begin
  inherited;
  FUIDownLoader := TTileDownloaderUI.Create(AViewPortState);
  FUIDownLoader.MainLayer := Self;
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

procedure TMapMainLayer.DrawGenShBorders;
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
  VBitmapRect: TDoubleRect;
  VGeoConvert: ICoordConverter;
begin
  if GState.GShScale = 0 then begin
    exit;
  end;
  z := GetGhBordersStepByScale(GState.GShScale);
  VLocalConverter := FBitmapCoordConverter;
  VGeoConvert := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VBitmapRect := DoubleRect(DoublePoint(0, 0), DoublePoint(FMapViewSize));
  VLoadedRect := VLocalConverter.LocalRectFloat2MapRectFloat(VBitmapRect);

  VGeoConvert.CheckPixelPosFloat(VLoadedRect.TopLeft, VZoom, False);
  VGeoConvert.CheckPixelPosFloat(VLoadedRect.BottomRight, VZoom, False);

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

procedure TMapMainLayer.DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
var
  VZoom: Byte;
  VSourceMapType: TMapType;
  VBmp: TCustomBitmap32;

  {
    Прямоугольник пикселей растра в координатах текущей основной карты
  }
  VBitmapOnMapPixelRect: TDoubleRect;

  {
    Географические координаты растра
  }
  VSourceLonLatRect: TDoubleRect;

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
  VLocalConverter: ILocalCoordConverter;
  VTileIterator: ITileIterator;
  VBitmapRect: TDoubleRect;
begin
  if AMapType.asLayer then begin
    VUsePre := GState.UsePrevZoomLayer;
  end else begin
    VUsePre := GState.UsePrevZoom;
  end;

  VLocalConverter := FBitmapCoordConverter;
  VGeoConvert := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VSourceMapType := AMapType;
  VSourceGeoConvert := VSourceMapType.GeoConvert;

  VBitmapRect := DoubleRect(DoublePoint(0, 0), DoublePoint(FMapViewSize));
  VBitmapOnMapPixelRect := FVisualCoordConverter.LocalRectFloat2MapRectFloat(VBitmapRect);
  VGeoConvert.CheckPixelPosFloat(VBitmapOnMapPixelRect.TopLeft, VZoom, False);
  VGeoConvert.CheckPixelPosFloat(VBitmapOnMapPixelRect.BottomRight, VZoom, False);

  VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
  VPixelSourceRect := VSourceGeoConvert.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);
  VTileSourceRect := VSourceGeoConvert.PixelRect2TileRect(VPixelSourceRect, VZoom);

  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);

  VBmp := TCustomBitmap32.Create;
  try
    while VTileIterator.Next(VTile) do begin
        VCurrTilePixelRectSource := VSourceGeoConvert.TilePos2PixelRect(VTile, VZoom);
        VTilePixelsToDraw.TopLeft := Point(0, 0);
        VTilePixelsToDraw.Right := VCurrTilePixelRectSource.Right - VCurrTilePixelRectSource.Left;
        VTilePixelsToDraw.Bottom := VCurrTilePixelRectSource.Bottom - VCurrTilePixelRectSource.Top;

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

        VCurrTilePixelRectAtBitmap.TopLeft := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
        VCurrTilePixelRectAtBitmap.BottomRight := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);
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

  VBitmapRect := DoubleRect(DoublePoint(0, 0), DoublePoint(FMapViewSize));
  VLoadedRect := FVisualCoordConverter.LocalRectFloat2MapRectFloat(VBitmapRect);
  VGeoConvert.CheckPixelPosFloat(VLoadedRect.TopLeft, VCurrentZoom, False);
  VGeoConvert.CheckPixelPosFloat(VLoadedRect.BottomRight, VCurrentZoom, False);

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

function TMapMainLayer.GetUseDownload: TTileSource;
begin
  Result := FUIDownLoader.UseDownload;
end;

function TMapMainLayer.GetUseDownloadChangeNotifier: IJclNotifier;
begin
  Result := FUIDownLoader.UseDownloadChangeNotifier;
end;

procedure TMapMainLayer.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    case VConfigProvider.ReadInteger('TileSource',1) of
      0: UseDownload := tsInternet;
      2: UseDownload := tsCacheInternet;
    else
      UseDownload := tsCache;
    end;
  end else begin
    UseDownload := tsCache;
  end;
end;

procedure TMapMainLayer.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('VIEW');
  case UseDownload of
    tsInternet: VConfigProvider.WriteInteger('TileSource', 0);
    tsCache: VConfigProvider.WriteInteger('TileSource', 1);
    tsCacheInternet: VConfigProvider.WriteInteger('TileSource', 2);
  end;
end;

procedure TMapMainLayer.SendTerminateToThreads;
begin
  inherited;
  FUIDownLoader.Terminate;
end;

procedure TMapMainLayer.SetErrorShowLayer(const Value: TTileErrorInfoLayer);
begin
  FUIDownLoader.ErrorShowLayer := Value;
end;

procedure TMapMainLayer.SetKmlLayer(const Value: TMapLayerBasic);
begin
  FUIDownLoader.KmlLayer := Value;
end;

procedure TMapMainLayer.SetUseDownload(const Value: TTileSource);
begin
  FUIDownLoader.UseDownload := Value;
end;

procedure TMapMainLayer.StartThreads;
begin
  inherited;
  FUIDownLoader.Resume;
end;

end.


