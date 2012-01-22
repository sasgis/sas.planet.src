unit u_LayerStatBar;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_TimeZoneDiffByLonLat,
  i_StatBarConfig,
  i_ViewPortState,
  i_MouseState,
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
    FMouseState: IMouseState;
    FTimeZoneDiff: ITimeZoneDiffByLonLat;
    FValueToStringConverterConfig: IValueToStringConverterConfig;

    FLastUpdateTick: DWORD;
    FMinUpdate: Cardinal;
    FBgColor: TColor32;
    FTextColor: TColor32;
    FAALevel: Integer;
    FCoordsMaxWidth: Integer;
    function GetTimeInLonLat(ALonLat: TDoublePoint): TDateTime;
    procedure OnConfigChange;
    procedure OnTimerEvent;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoRedraw; override;
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    procedure SetViewCoordConverter(AValue: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IStatBarConfig;
      AValueToStringConverterConfig: IValueToStringConverterConfig;
      AMouseState: IMouseState;
      ATimerNoifier: IJclNotifier;
      ATimeZoneDiff: ITimeZoneDiffByLonLat;
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
  u_MapType;

const
  D2R: Double = 0.017453292519943295769236907684886;//  онстанта дл€ преобразовани€ градусов в радианы

{ TLayerStatBar }

constructor TLayerStatBar.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IStatBarConfig;
  AValueToStringConverterConfig: IValueToStringConverterConfig;
  AMouseState: IMouseState;
  ATimerNoifier: IJclNotifier;
  ATimeZoneDiff: ITimeZoneDiffByLonLat;
  ADownloadInfo: IDownloadInfoSimple;
  AMainMapsConfig: IMainMapsConfig
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FTimeZoneDiff := ATimeZoneDiff;
  FDownloadInfo := ADownloadInfo;
  FMouseState := AMouseState;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimerEvent),
    ATimerNoifier
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
  tz: TDateTime;
  st: TSystemTime;
begin
  tz := FTimeZoneDiff.GetTimeDiff(ALonLat);
  GetSystemTime(st);
  Result := EncodeTime(st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);
  Result := Result + tz;
  Result := Frac(Result);
end;

procedure TLayerStatBar.OnConfigChange;
var
  VVisible: Boolean;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      FLayer.Bitmap.Font.Name := FConfig.FontName;
      FLayer.Bitmap.Font.Size := FConfig.FontSize;
      FCoordsMaxWidth := FLayer.Bitmap.TextWidth('N 99" 99" 99.99" W 99" 99" 99.99"') + 10;

      FMinUpdate := FConfig.MinUpdateTickCount;
      FBgColor := FConfig.BgColor;
      FTextColor := FConfig.TextColor;
      VVisible := FConfig.Visible;
      FAALevel := 0;
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

procedure TLayerStatBar.OnTimerEvent;
begin
  Redraw;
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
  OnConfigChange;
end;

procedure TLayerStatBar.DoRedraw;
var
  VLonLat: TDoublePoint;
  VString: string;
  VTimeTZ: TDateTime;
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
  VOffset: TPoint;
begin
  inherited;
  VCurrentTick := GetTickCount;
  if (VCurrentTick < FLastUpdateTick) or (VCurrentTick > FLastUpdateTick + FMinUpdate) then begin
    VValueConverter := FValueToStringConverterConfig.GetStatic;
    VVisualCoordConverter := ViewCoordConverter;
    VMousePos := FMouseState.CurentPos;
    VZoomCurr := VVisualCoordConverter.GetZoom;
    VConverter := VVisualCoordConverter.GetGeoConverter;
    VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
    VMap := FMainMapsConfig.GetSelectedMapType.MapType;

    VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
    VMap.GeoConvert.CheckPixelPosFloatStrict(VMapPoint, VZoomCurr, True);
    VTile := VMap.GeoConvert.PixelPosFloat2TilePos(VMapPoint, VZoomCurr);

    VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
    VConverter.CheckPixelPosFloatStrict(VMapPoint, VZoomCurr, True);
    VLonLat := VConverter.PixelPosFloat2LonLat(VMapPoint, VZoomCurr);
    VLonLatStr:= VValueConverter.LonLatConvert(VLonLat);

    FLayer.Bitmap.Clear(FBgColor);
    FLayer.Bitmap.Line(0, 0, VSize.X, 0, SetAlpha(clBlack32, 255));

    VOffset.Y := 1;
    VOffset.X := 4;
    FLayer.Bitmap.RenderText(VOffset.X, VOffset.Y, 'z' + inttostr(VZoomCurr + 1), FAALevel, FTextColor);

    VOffset.X := 29;
    FLayer.Bitmap.RenderText(VOffset.X, VOffset.Y, '| ' + VLonLatStr, FAALevel, FTextColor);

    VRad := VConverter.Datum.GetSpheroidRadiusA;
    VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoomCurr);
    VString := VValueConverter.DistPerPixelConvert(1 / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(VLonLat.y * D2R))));
    VOffset.X := VOffset.X + FCoordsMaxWidth;
    FLayer.Bitmap.RenderText(VOffset.X, VOffset.Y, ' | ' + VString, FAALevel, FTextColor);
    VOffset.X := VOffset.X + FLayer.Bitmap.TextWidth(VString) + 10;
    VTimeTZ := GetTimeInLonLat(VLonLat);
    VString := ' | ' + TimeToStr(VTimeTZ);
    FLayer.Bitmap.RenderText(VOffset.X, VOffset.Y, VString, FAALevel, FTextColor);
    VOffset.X := VOffset.X + FLayer.Bitmap.TextWidth(VString) + 10;
    VString := VMap.GetTileShowName(VTile, VZoomCurr);
    FLayer.Bitmap.RenderText(
      VOffset.X, VOffset.Y,
      ' | ' + SAS_STR_load + ' ' +
      inttostr(FDownloadInfo.TileCount) + ' (' +
      VValueConverter.DataSizeConvert(FDownloadInfo.Size/1024) +
      ') | ' + SAS_STR_file + ' ' + VString,
       FAALevel, FTextColor
    );
    FLastUpdateTick := GetTickCount;
  end;
end;

end.
