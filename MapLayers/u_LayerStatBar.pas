unit u_LayerStatBar;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  i_Notify,
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
  i_GlobalInternetState,
  u_WindowLayerWithPos;

type
  TLayerStatBar = class(TWindowLayerWithBitmap)
  private
    FConfig: IStatBarConfig;
    FMainMapsConfig: IMainMapsConfig;
    FDownloadInfo: IDownloadInfoSimple;
    FGlobalInternetState: IGlobalInternetState;
    FMouseState: IMouseState;
    FTimeZoneDiff: ITimeZoneDiffByLonLat;
    FValueToStringConverterConfig: IValueToStringConverterConfig;

    FLastUpdateTick: DWORD;
    FMinUpdate: Cardinal;
    FBgColor: TColor32;
    FTextColor: TColor32;
    FAALevel: Integer;
    function GetTimeInLonLat(const ALonLat: TDoublePoint): TDateTime;
    procedure OnConfigChange;
    procedure OnTimerEvent;
  protected
    function GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect; override;
    procedure DoRedraw; override;
    function GetLayerSizeForView(
      const ANewVisualCoordConverter: ILocalCoordConverter
    ): TPoint; override;
    procedure SetViewCoordConverter(const AValue: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AConfig: IStatBarConfig;
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const AMouseState: IMouseState;
      const ATimerNoifier: INotifier;
      const ATimeZoneDiff: ITimeZoneDiffByLonLat;
      const ADownloadInfo: IDownloadInfoSimple;
      const AGlobalInternetState: IGlobalInternetState;
      const AMainMapsConfig: IMainMapsConfig
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  Graphics,
  i_CoordConverter,
  u_NotifyEventListener,
  u_ResStrings,
  u_MapType;

const
  D2R: Double = 0.017453292519943295769236907684886;//  онстанта дл€ преобразовани€ градусов в радианы

{ TLayerStatBar }

constructor TLayerStatBar.Create(
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AConfig: IStatBarConfig;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const AMouseState: IMouseState;
  const ATimerNoifier: INotifier;
  const ATimeZoneDiff: ITimeZoneDiffByLonLat;
  const ADownloadInfo: IDownloadInfoSimple;
  const AGlobalInternetState: IGlobalInternetState;
  const AMainMapsConfig: IMainMapsConfig
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  FGlobalInternetState := AGlobalInternetState;
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

function TLayerStatBar.GetLayerSizeForView(
  const ANewVisualCoordConverter: ILocalCoordConverter
): TPoint;
begin
  Result.X := ANewVisualCoordConverter.GetLocalRectSize.X;
  Result.Y := FConfig.Height;
end;

function TLayerStatBar.GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect;
begin
  if ANewVisualCoordConverter <> nil then begin
    Result.Left := 0;
    Result.Bottom := ANewVisualCoordConverter.GetLocalRectSize.Y;
    Result.Right := Result.Left + Layer.Bitmap.Width;
    Result.Top := Result.Bottom - Layer.Bitmap.Height;
  end;
end;

function TLayerStatBar.GetTimeInLonLat(const ALonLat: TDoublePoint): TDateTime;
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

  procedure SetValidFontSize(
    AFont: TFont;
    ASize: Integer;
    AMaxHeight: Integer
  );
  begin
    if abs(AFont.Height) < AMaxHeight then begin
      AFont.Size := ASize;
    end;
    while abs(AFont.Height) >= AMaxHeight do begin
      AFont.Size := AFont.Size - 1;
    end;
  end;

var
  VVisible: Boolean;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      Layer.Bitmap.Font.Name := FConfig.FontName;
      SetValidFontSize(Layer.Bitmap.Font, FConfig.FontSize, (FConfig.Height - 2));

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
end;

procedure TLayerStatBar.OnTimerEvent;
begin
  Redraw;
end;

procedure TLayerStatBar.SetViewCoordConverter(const AValue: ILocalCoordConverter);
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

  procedure RenderText(
  const AOffset: TPoint;
  const AText: string;
    ADrawLine: Boolean = True
  );
  begin
    Layer.Bitmap.RenderText(AOffset.X, AOffset.Y, AText, FAALevel, FTextColor);
    if ADrawLine then begin
      Layer.Bitmap.LineS(AOffset.X - 10, 0, AOffset.X - 10, Layer.Bitmap.Height, SetAlpha(clBlack32, 125));
    end;
  end;

var
  VLonLat: TDoublePoint;
  VString: string;
  VTimeTZ: TDateTime;
  VMapPoint: TDoublePoint;
  VZoomCurr: Byte;
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
  VTileName: string;
  VShortTileName: string;
  VTileNameWidth: Integer;
  VTileNameWidthAviable: Integer;
  VNeedSeparator: Boolean;
begin
  inherited;
  VCurrentTick := GetTickCount;
  if (VCurrentTick < FLastUpdateTick) or (VCurrentTick > FLastUpdateTick + FMinUpdate) then begin
    VValueConverter := FValueToStringConverterConfig.GetStatic;
    VVisualCoordConverter := ViewCoordConverter;
    VMousePos := FMouseState.CurentPos;
    VZoomCurr := VVisualCoordConverter.GetZoom;
    VConverter := VVisualCoordConverter.GetGeoConverter;
    VSize := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
    VMap := FMainMapsConfig.GetSelectedMapType.MapType;

    VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
    VMap.GeoConvert.CheckPixelPosFloatStrict(VMapPoint, VZoomCurr, True);
    VTile := VMap.GeoConvert.PixelPosFloat2TilePos(VMapPoint, VZoomCurr);

    VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
    VConverter.CheckPixelPosFloatStrict(VMapPoint, VZoomCurr, True);
    VLonLat := VConverter.PixelPosFloat2LonLat(VMapPoint, VZoomCurr);

    Layer.Bitmap.Clear(FBgColor);
    Layer.Bitmap.LineS(0, 0, VSize.X, 0, SetAlpha(clBlack32, 255));

    VOffset.Y := 1;
    VOffset.X := -10;
    VString := '';
    VNeedSeparator := False;

    if FConfig.ViewZoomInfo then begin
      VOffset.X := VOffset.X + 20;
      VString := 'z' + inttostr(VZoomCurr + 1);
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewLonLatInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VString := VValueConverter.LonLatConvert(VLonLat);
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewMetrPerPixInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VRad := VConverter.Datum.GetSpheroidRadiusA;
      VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoomCurr);
      VString := VValueConverter.DistPerPixelConvert(1 / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(VLonLat.y * D2R))));
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewTimeZoneTimeInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VTimeTZ := GetTimeInLonLat(VLonLat);
      VString := TimeToStr(VTimeTZ);
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewDownloadedInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VTileName := VMap.GetTileShowName(VTile, VZoomCurr);
      VString := SAS_STR_load + ' ' +
        inttostr(FDownloadInfo.TileCount) +
        ' (' + VValueConverter.DataSizeConvert(FDownloadInfo.Size / 1024) + ')';
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewHttpQueueInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VString := SAS_STR_queue + ' ' + IntToStr(FGlobalInternetState.QueueCount);
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewTilePathInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VTileNameWidthAviable := Layer.Bitmap.Width - VOffset.X;
      if Length(VTileName) > 0 then begin
        if VTileNameWidthAviable > 30 then begin
          VTileNameWidth := Layer.Bitmap.TextWidth(VTileName);
          if VTileNameWidthAviable < VTileNameWidth + 40 then begin
            SetLength(VShortTileName, 6);
            StrLCopy(PAnsiChar(VShortTileName), PAnsiChar(VTileName), 6);
            VShortTileName :=
              VShortTileName + '...' +
              RightStr(
              VTileName,
              Trunc(
                (Length(VTileName) / VTileNameWidth) * (VTileNameWidthAviable - Layer.Bitmap.TextWidth(VShortTileName) - 40)
              )
            );
            VTileName := VShortTileName;
          end;
        end;
      end;
      VString := SAS_STR_file + ' ' + VTileName;
      RenderText(VOffset, VString, VNeedSeparator);
    end;

    FLastUpdateTick := GetTickCount;
  end;
end;

end.

