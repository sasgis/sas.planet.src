unit u_LayerStatBar;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_StatBarConfig,
  i_ViewPortState,
  i_ActiveMapsConfig,
  i_ValueToStringConverter,
  i_DownloadInfoSimple,
  u_WindowLayerWithPos;

type
  TLayerStatBar = class(TWindowLayerWithBitmap)
  private
    FConfig: IStatBarConfig;
    FMainMapsConfig: IMainMapsConfig;
    FDownloadInfo: IDownloadInfoSimple;
    FValueToStringConverterConfig: IValueToStringConverterConfig;

    FLastUpdateTick: DWORD;
    FMinUpdate: Cardinal;
    FBgColor: TColor32;
    FTextColor: TColor32;
    function GetTimeInLonLat(ALonLat: TDoublePoint): TDateTime;
    procedure OnConfigChange(Sender: TObject);
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoRedraw; override;
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    procedure SetViewCoordConverter(AValue: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IStatBarConfig;
      AValueToStringConverterConfig: IValueToStringConverterConfig;
      ADownloadInfo: IDownloadInfoSimple;
      AMainMapsConfig: IMainMapsConfig
    );
  end;

implementation

uses
  SysUtils,
  i_CoordConverter,
  u_NotifyEventListener,
  u_ResStrings,
  u_TimeZones,
  frm_Main,
  u_MapType;

const
  D2R: Double = 0.017453292519943295769236907684886;//  онстанта дл€ преобразовани€ градусов в радианы

{ TLayerStatBar }

constructor TLayerStatBar.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IStatBarConfig;
  AValueToStringConverterConfig: IValueToStringConverterConfig;
  ADownloadInfo: IDownloadInfoSimple;
  AMainMapsConfig: IMainMapsConfig
);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FDownloadInfo := ADownloadInfo;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  FMainMapsConfig := AMainMapsConfig;
  FLastUpdateTick := 0;
end;

function TLayerStatBar.GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  Result.X := ANewVisualCoordConverter.GetLocalRectSize.X;
  Result.Y := FConfig.Height;
end;

function TLayerStatBar.GetMapLayerLocationRect: TFloatRect;
begin
  Result.Left := 0;
  Result.Bottom := ViewCoordConverter.GetLocalRectSize.Y;
  Result.Right := Result.Left + FLayer.Bitmap.Width;
  Result.Top := Result.Bottom - FLayer.Bitmap.Height;
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
var
  VVisible: Boolean;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      FLayer.Bitmap.Font.Name := FConfig.FontName;
      FLayer.Bitmap.Font.Size := FConfig.FontSize;
      FMinUpdate := FConfig.MinUpdateTickCount;
      FBgColor := FConfig.BgColor;
      FTextColor := FConfig.TextColor;
      VVisible := FConfig.Visible;
    finally
      FConfig.UnlockRead;
    end;
    SetNeedRedraw;
    SetVisible(VVisible);
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TLayerStatBar.SetViewCoordConverter(AValue: ILocalCoordConverter);
begin
  inherited;
  SetNeedUpdateLayerSize;
  SetNeedUpdateLocation;
end;

procedure TLayerStatBar.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
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
  VValueConverter: IValueToStringConverter;
begin
  inherited;
  VCurrentTick := GetTickCount;
  if (VCurrentTick < FLastUpdateTick) or (VCurrentTick > FLastUpdateTick + FMinUpdate) then begin
    VValueConverter := FValueToStringConverterConfig.GetStaticConverter;
    VVisualCoordConverter := ViewCoordConverter;
    VMousePos := frmMain.MouseCursorPos;
    VZoomCurr := VVisualCoordConverter.GetZoom;
    VConverter := VVisualCoordConverter.GetGeoConverter;
    VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
    VMap := FMainMapsConfig.GetSelectedMapType.MapType;

    VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
    VMap.GeoConvert.CheckPixelPosFloatStrict(VMapPoint, VZoomCurr, True);
    VTile := VMap.GeoConvert.PixelPosFloat2TilePos(VMapPoint, VZoomCurr);

    VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
    VConverter.CheckPixelPosFloatStrict(VMapPoint, VZoomCurr, True);
    ll := VConverter.PixelPosFloat2LonLat(VMapPoint, VZoomCurr);
    VLonLatStr:= VValueConverter.LonLatConvert(ll);

    FLayer.Bitmap.Clear(FBgColor);
    FLayer.Bitmap.Line(0, 0, VSize.X, 0, SetAlpha(clBlack32, 256));
    FLayer.Bitmap.RenderText(4, 1, 'z' + inttostr(VZoomCurr + 1), 0, FTextColor);
    FLayer.Bitmap.RenderText(29, 1, '| ' + SAS_STR_coordinates + ' ' + VLonLatStr, 0, FTextColor);

    VRad := VConverter.Datum.GetSpheroidRadiusA;
    VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoomCurr);
    subs2 := VValueConverter.DistConvert(1 / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(ll.y * D2R)))) + SAS_UNITS_mperp;
    FLayer.Bitmap.RenderText(278, 1, ' | ' + SAS_STR_Scale + ' ' + subs2, 0, FTextColor);
    posnext := 273 + FLayer.Bitmap.TextWidth(subs2) + 70;
    TameTZ := GetTimeInLonLat(ll);
    FLayer.Bitmap.RenderText(posnext, 1, ' | ' + SAS_STR_time + ' ' + TimeToStr(TameTZ), 0, FTextColor);
    posnext := posnext + FLayer.Bitmap.TextWidth(SAS_STR_time + ' ' + TimeToStr(TameTZ)) + 10;
    subs2 := VMap.GetTileShowName(VTile, VZoomCurr);
    FLayer.Bitmap.RenderText(
      posnext, 1,
      ' | ' + SAS_STR_load + ' ' +
      inttostr(FDownloadInfo.TileCount) + ' (' +
      VValueConverter.DataSizeConvert(FDownloadInfo.Size/1024) +
      ') | ' + SAS_STR_file + ' ' + subs2,
       0, FTextColor
    );
    FLastUpdateTick := GetTickCount;
  end;
end;

end.
