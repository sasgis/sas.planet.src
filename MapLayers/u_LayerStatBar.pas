unit u_LayerStatBar;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_ILocalCoordConverter,
  i_IStatBarConfig,
  u_MapViewPortState,
  u_WindowLayerWithPos;

type
  TLayerStatBar = class(TWindowLayerWithBitmap)
  private
    FConfig: IStatBarConfig;
    FLastUpdateTick: DWORD;
    function GetTimeInLonLat(ALonLat: TDoublePoint): TDateTime;
    procedure OnConfigChange(Sender: TObject);
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoRedraw; override;
    function GetLayerSizeForViewSize(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    procedure DoUpdateLayerSize(ANewSize: TPoint); override;
    procedure DoHide; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState; AConfig: IStatBarConfig);
  end;

implementation

uses
  SysUtils,
  u_GeoToStr,
  i_ICoordConverter,
  i_IValueToStringConverter,
  u_NotifyEventListener,
  UResStrings,
  UTimeZones,
  Unit1,
  uMapType,
  u_GlobalState;

const
  D2R: Double = 0.017453292519943295769236907684886;//  онстанта дл€ преобразовани€ градусов в радианы

{ TLayerStatBar }

constructor TLayerStatBar.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState; AConfig: IStatBarConfig);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  FLastUpdateTick := 0;
  OnConfigChange(nil);
end;

function TLayerStatBar.GetLayerSizeForViewSize(ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  Result.X := ANewVisualCoordConverter.GetLocalRectSize.X;
  Result.Y := FConfig.Height;
end;

function TLayerStatBar.GetMapLayerLocationRect: TFloatRect;
begin
  Result.Left := 0;
  Result.Bottom := FVisualCoordConverter.GetLocalRectSize.Y;
  Result.Right := Result.Left + LayerSize.X;
  Result.Top := Result.Bottom - LayerSize.Y;
end;

function TLayerStatBar.GetTimeInLonLat(ALonLat: TDoublePoint): TDateTime;
var
  prH, prM: integer;
  tz: real;
  st: TSystemTime;
begin
  tz := GetTZ_(ALonLat);
  GetSystemTime(st);
  prH := trunc(tz);
  prM := round(60 * frac(TZ));
  result := EncodeTime(abs(st.wHour + prH + 24) mod 24, abs(st.wMinute + prM + 60) mod 60, 0, 0);
end;

procedure TLayerStatBar.OnConfigChange(Sender: TObject);
begin
  FLayer.Bitmap.Font.Name := FConfig.FontName;
  FLayer.Bitmap.Font.Size := FConfig.FontSize;
  if FConfig.Visible then begin
    Redraw;
    Show;
  end else begin
    Hide;
  end;
end;

procedure TLayerStatBar.DoHide;
begin
  inherited;
  UpdateLayerSize(Point(0,0));
end;

procedure TLayerStatBar.DoRedraw;
var
  ll: TDoublePoint;
  subs2: string;
  posnext: integer;
  TameTZ: TDateTime;
  VMapPoint: TDoublePoint;
  VZoomCurr: Byte;
  VLonLatStr: String;
  VSize: TPoint;
  VRad: Extended;
  VTile: TPoint;
  VMap: TMapType;
  VConverter: ICoordConverter;
  VPixelsAtZoom: Double;
  VCurrentTick: DWORD;
  VMousePos: TPoint;
  VVisualCoordConverter: ILocalCoordConverter;
  VMinUpdate: Cardinal;
  VBgColor: TColor32;
  VTextColor: TColor32;
  VValueConverter: IValueToStringConverter;
begin
  inherited;
  FConfig.LockRead;
  try
    VMinUpdate := FConfig.MinUpdateTickCount;
    VBgColor := FConfig.BgColor;
    VTextColor := FConfig.TextColor;
  finally
    FConfig.UnlockRead;
  end;
  VCurrentTick := GetTickCount;
  if (VCurrentTick < FLastUpdateTick) or (VCurrentTick > FLastUpdateTick + VMinUpdate) then begin
    VValueConverter := GState.ValueToStringConverterConfig.GetStaticConverter;
    VVisualCoordConverter := FVisualCoordConverter;
    VMousePos := Fmain.MouseCursorPos;
    VZoomCurr := VVisualCoordConverter.GetZoom;
    VConverter := VVisualCoordConverter.GetGeoConverter;
    VSize := LayerSize;
    VMap := GState.ViewState.GetCurrentMap;

    VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
    VMap.GeoConvert.CheckPixelPosFloatStrict(VMapPoint, VZoomCurr, True);
    VTile := VMap.GeoConvert.PixelPosFloat2TilePos(VMapPoint, VZoomCurr);

    VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
    VConverter.CheckPixelPosFloatStrict(VMapPoint, VZoomCurr, True);
    ll := VConverter.PixelPosFloat2LonLat(VMapPoint, VZoomCurr);
    VLonLatStr:= VValueConverter.LonLatConvert(ll);

    FLayer.Bitmap.Clear(VBgColor);
    FLayer.Bitmap.Line(0, 0, VSize.X, 0, SetAlpha(clBlack32, 256));
    FLayer.Bitmap.RenderText(4, 1, 'z' + inttostr(VZoomCurr + 1), 0, VTextColor);
    FLayer.Bitmap.RenderText(29, 1, '| ' + SAS_STR_coordinates + ' ' + VLonLatStr, 0, VTextColor);

    VRad := VConverter.Datum.GetSpheroidRadiusA;
    VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoomCurr);
    subs2 := VValueConverter.DistConvert(1 / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(ll.y * D2R)))) + SAS_UNITS_mperp;
    FLayer.Bitmap.RenderText(278, 1, ' | ' + SAS_STR_Scale + ' ' + subs2, 0, VTextColor);
    posnext := 273 + FLayer.Bitmap.TextWidth(subs2) + 70;
    TameTZ := GetTimeInLonLat(ll);
    FLayer.Bitmap.RenderText(posnext, 1, ' | ' + SAS_STR_time + ' ' + TimeToStr(TameTZ), 0, VTextColor);
    posnext := posnext + FLayer.Bitmap.TextWidth(SAS_STR_time + ' ' + TimeToStr(TameTZ)) + 10;
    subs2 := VMap.GetTileShowName(VTile, VZoomCurr);
    FLayer.Bitmap.RenderText(
      posnext, 1,
      ' | ' + SAS_STR_load + ' ' +
      inttostr(GState.DownloadInfo.TileCount) + ' (' +
      VValueConverter.DataSizeConvert(GState.DownloadInfo.Size/1024) +
      ') | ' + SAS_STR_file + ' ' + subs2,
       0, VTextColor
    );
    FLastUpdateTick := GetTickCount;
  end;
end;

procedure TLayerStatBar.DoUpdateLayerSize(ANewSize: TPoint);
var
  VBitmapSizeInPixel: TPoint;
begin
  inherited;
  VBitmapSizeInPixel := LayerSize;
  FLayer.Bitmap.Lock;
  try
    if (FLayer.Bitmap.Width <> VBitmapSizeInPixel.X) or (FLayer.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
      FLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
    end;
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

end.
