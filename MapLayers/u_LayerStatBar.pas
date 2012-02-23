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
  StrUtils,
  Graphics,
  i_CoordConverter,
  u_GlobalInternetState,
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

  procedure SetValidFontSize(AFont: TFont; ASize: Integer; AMaxHeight: Integer);
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
      FLayer.Bitmap.Font.Name := FConfig.FontName;
      SetValidFontSize(FLayer.Bitmap.Font, FConfig.FontSize, (FConfig.Height - 2) );

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

  procedure RenderText(AOffset: TPoint; AText: string; ADrawLine: Boolean = True);
  begin
    FLayer.Bitmap.RenderText(AOffset.X, AOffset.Y, AText, FAALevel, FTextColor);
    if ADrawLine then begin
      FLayer.Bitmap.Line(AOffset.X - 10, 0, AOffset.X - 10, FLayer.Bitmap.Height, SetAlpha(clBlack32, 125));
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

    FLayer.Bitmap.Clear(FBgColor);
    FLayer.Bitmap.Line(0, 0, VSize.X, 0, SetAlpha(clBlack32, 255));

    VOffset.Y := 1;

    // zoom
    VOffset.X := 10;
    VString := 'z' + inttostr(VZoomCurr + 1);
    RenderText(VOffset, VString, False);

    // degrees
    VOffset.X := VOffset.X + FLayer.Bitmap.TextWidth(VString) + 20;
    VString := VValueConverter.LonLatConvert(VLonLat);
    RenderText(VOffset, VString);

    // meters per degree
    VOffset.X := VOffset.X + FLayer.Bitmap.TextWidth(VString) + 20;
    VRad := VConverter.Datum.GetSpheroidRadiusA;
    VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoomCurr);
    VString := VValueConverter.DistPerPixelConvert(1 / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(VLonLat.y * D2R))));
    RenderText(VOffset, VString);

    // time
    VOffset.X := VOffset.X + FLayer.Bitmap.TextWidth(VString) + 20;
    VTimeTZ := GetTimeInLonLat(VLonLat);
    VString := TimeToStr(VTimeTZ);
    RenderText(VOffset, VString);

    // downloaded
    VOffset.X := VOffset.X + FLayer.Bitmap.TextWidth(VString) + 20;
    VTileName := VMap.GetTileShowName(VTile, VZoomCurr);
    VString := SAS_STR_load + ' ' +
      inttostr(FDownloadInfo.TileCount) +
      ' (' + VValueConverter.DataSizeConvert(FDownloadInfo.Size/1024) + ')';
    RenderText(VOffset, VString);

    // internet queue
    VOffset.X := VOffset.X + FLayer.Bitmap.TextWidth(VString) + 20;
    VString := 'Queue ' + IntToStr(GInternetState.TaskCount);
    RenderText(VOffset, VString);

    // file name
    VOffset.X := VOffset.X + FLayer.Bitmap.TextWidth(VString) + 20;
    VTileNameWidthAviable := FLayer.Bitmap.Width - VOffset.X;
    if Length(VTileName) > 0 then begin
      if VTileNameWidthAviable > 30 then begin
        VTileNameWidth := FLayer.Bitmap.TextWidth(VTileName);
        if VTileNameWidthAviable < VTileNameWidth + 40 then begin
          SetLength(VShortTileName, 6);
          StrLCopy(PAnsiChar(VShortTileName), PAnsiChar(VTileName), 6);
          VShortTileName := VShortTileName + '...' +
            RightStr(
              VTileName,
              Trunc(
                (Length(VTileName)/VTileNameWidth) * (VTileNameWidthAviable - FLayer.Bitmap.TextWidth(VShortTileName) - 40 )
              )
            );
          VTileName := VShortTileName;
        end;
      end;
    end;
    VString := SAS_STR_file + ' ' + VTileName;
    RenderText(VOffset, VString);

    FLastUpdateTick := GetTickCount;
  end;
end;

end.
