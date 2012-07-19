unit u_LocalConverterChangeableOfMiniMap;

interface

uses
  i_Listener,
  i_Notifier,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_LocalCoordConverterChangeable,
  i_MiniMapLayerConfig;

type
  TLocalConverterChangeableOfMiniMap = class(TInterfacedObject, ILocalCoordConverterChangeable)
  private
    FInternal: ILocalCoordConverterChangeableInternal;
    FSoruce: ILocalCoordConverterChangeable;
    FConfig: IMiniMapLayerConfig;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FSourceListener: IListener;
    FConfigListener: IListener;
    procedure OnSourceChange;
    procedure OnConfigChange;
    function GetActualZoom(const AVisualCoordConverter: ILocalCoordConverter): Byte;
    function GetConverterForSource(
      const AVisualCoordConverter: ILocalCoordConverter
    ): ILocalCoordConverter;
  private
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
    function GetStatic: ILocalCoordConverter;
  public
    constructor Create(
      const AChangeCounter: IInternalPerformanceCounter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const ASoruce: ILocalCoordConverterChangeable;
      const AConfig: IMiniMapLayerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_LocalCoordConverterChangeable,
  u_GeoFun;

{ TLocalConverterChangeableOfMiniMap }

constructor TLocalConverterChangeableOfMiniMap.Create(
  const AChangeCounter: IInternalPerformanceCounter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ASoruce: ILocalCoordConverterChangeable;
  const AConfig: IMiniMapLayerConfig
);
begin
  inherited Create;
  FSoruce := ASoruce;
  FConfig := AConfig;
  FConverterFactory := AConverterFactory;

  FInternal :=
    TLocalCoordConverterChangeable.Create(
      TSimpleFlagWithInterlock.Create,
      FSoruce.GetStatic,
      AChangeCounter
    );
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);
  FSoruce.ChangeNotifier.Add(FSourceListener);
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;
end;

destructor TLocalConverterChangeableOfMiniMap.Destroy;
begin
  if FSoruce <> nil then begin
    FSoruce.ChangeNotifier.Remove(FSourceListener);
    FSoruce := nil;
    FSourceListener := nil;
  end;
  if FConfig <> nil then begin
    FConfig.ChangeNotifier.Remove(FConfigListener);
    FConfig := nil;
    FConfigListener := nil;
  end;
  inherited;
end;

function TLocalConverterChangeableOfMiniMap.GetActualZoom(
  const AVisualCoordConverter: ILocalCoordConverter): Byte;
var
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
  VZoomDelta: Integer;
begin
  VZoom := AVisualCoordConverter.GetZoom;
  VGeoConvert := AVisualCoordConverter.GetGeoConverter;
  VZoomDelta := FConfig.ZoomDelta;
  if VZoomDelta = 0 then begin
    Result := VZoom;
  end else if VZoomDelta > 0 then begin
    if VZoom > VZoomDelta then begin
      Result := VZoom - VZoomDelta;
    end else begin
      Result := 0;
    end;
  end else begin
    Result := VZoom - VZoomDelta;
    VGeoConvert.CheckZoom(Result);
  end;
end;

function TLocalConverterChangeableOfMiniMap.GetAfterChangeNotifier: INotifier;
begin
  Result := FInternal.AfterChangeNotifier;
end;

function TLocalConverterChangeableOfMiniMap.GetBeforeChangeNotifier: INotifier;
begin
  Result := FInternal.BeforeChangeNotifier;
end;

function TLocalConverterChangeableOfMiniMap.GetChangeNotifier: INotifier;
begin
  Result := FInternal.ChangeNotifier;
end;

function TLocalConverterChangeableOfMiniMap.GetConverterForSource(
  const AVisualCoordConverter: ILocalCoordConverter): ILocalCoordConverter;
var
  VVisualMapCenter: TDoublePoint;
  VZoom: Byte;
  VSourceZoom: Byte;
  VConverter: ICoordConverter;
  VVisualMapCenterInRelative: TDoublePoint;
  VVisualMapCenterInLayerMap: TDoublePoint;
  VLocalTopLeftAtMap: TPoint;
  VLocalTopLeftAtMapFloat: TDoublePoint;
  VLayerSize: TPoint;
  VVeiwSize: TPoint;
  VWidth: Integer;
  VBottomMargin: Integer;
  VLocalRect: TRect;
begin
  VWidth := FConfig.Width;
  VBottomMargin := FConfig.BottomMargin;
  VVeiwSize := AVisualCoordConverter.GetLocalRectSize;
  VVisualMapCenter := AVisualCoordConverter.GetCenterMapPixelFloat;
  VSourceZoom := AVisualCoordConverter.GetZoom;
  VConverter := AVisualCoordConverter.GetGeoConverter;
  VConverter.CheckPixelPosFloatStrict(VVisualMapCenter, VSourceZoom, True);
  VVisualMapCenterInRelative := VConverter.PixelPosFloat2Relative(VVisualMapCenter, VSourceZoom);
  VZoom := GetActualZoom(AVisualCoordConverter);
  VVisualMapCenterInLayerMap := VConverter.Relative2PixelPosFloat(VVisualMapCenterInRelative, VZoom);
  VLayerSize := Point(VWidth, VWidth);
  VLocalTopLeftAtMapFloat :=
    DoublePoint(
      VVisualMapCenterInLayerMap.X - VLayerSize.X / 2,
      VVisualMapCenterInLayerMap.Y - VLayerSize.Y / 2
    );
  VLocalTopLeftAtMap := PointFromDoublePoint(VLocalTopLeftAtMapFloat, prToTopLeft);

  VLocalRect.Right := VVeiwSize.X;
  VLocalRect.Bottom := VVeiwSize.Y - VBottomMargin;
  VLocalRect.Left := VLocalRect.Right - VLayerSize.X;
  VLocalRect.Top := VLocalRect.Bottom - VLayerSize.Y;
  Result := FConverterFactory.CreateConverterNoScale(
    VLocalRect,
    VZoom,
    VConverter,
    VLocalTopLeftAtMap
  );
end;

function TLocalConverterChangeableOfMiniMap.GetStatic: ILocalCoordConverter;
begin
  Result := FInternal.GetStatic;
end;

procedure TLocalConverterChangeableOfMiniMap.OnConfigChange;
begin
  FInternal.SetConverter(GetConverterForSource(FSoruce.GetStatic));
end;

procedure TLocalConverterChangeableOfMiniMap.OnSourceChange;
begin
  FInternal.SetConverter(GetConverterForSource(FSoruce.GetStatic));
end;

end.
