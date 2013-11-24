unit u_SourceDataUpdateInRectByFillingMap;

interface

uses
  SysUtils,
  i_ObjectWithListener,
  i_Listener,
  i_LocalCoordConverter,
  i_FillingMapLayerConfig,
  i_MapTypes,
  u_BaseInterfacedObject;

type
  TSourceDataUpdateInRectByFillingMap = class(TBaseInterfacedObject, IObjectWithListener)
  private
    FConfig: IFillingMapLayerConfig;
    FMapType: IMapTypeChangeable;

    FMapTypeListener: IListener;
    FCS: IReadWriteSync;

    FMapListened: IMapType;
    FMapListener: IListener;

    FListener: IListener;
    FListenLocalConverter: ILocalCoordConverter;
    function GetActualZoom(
      const ALocalConverter: ILocalCoordConverter
    ): Byte;
    procedure OnTileUpdate(const AMsg: IInterface);
    procedure OnMapChange;

    procedure _RemoveListener(
      const AMapListened: IMapType
    );
    procedure _SetListener(
      const AMapListened: IMapType;
      const ALocalConverter: ILocalCoordConverter
    );
  private
    procedure SetListener(
      const AListener: IListener;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure RemoveListener;

  public
    constructor Create(
      const AConfig: IFillingMapLayerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  t_GeoTypes,
  i_LonLatRect,
  i_CoordConverter,
  i_NotifierTilePyramidUpdate,
  u_ListenerByEvent,
  u_TileUpdateListenerToLonLat,
  u_GeoFun,
  u_Synchronizer;

{ TSourceDataUpdateInRectByMap }

constructor TSourceDataUpdateInRectByFillingMap.Create(
  const AConfig: IFillingMapLayerConfig
);
begin
  inherited Create;
  FConfig := AConfig;
  FCS := MakeSyncRW_Var(Self, False);

  FMapTypeListener := TNotifyNoMmgEventListener.Create(Self.OnMapChange);
  FConfig.ChangeNotifier.Add(FMapTypeListener);
  OnMapChange;
end;

destructor TSourceDataUpdateInRectByFillingMap.Destroy;
begin
  if Assigned(FMapType) and Assigned(FMapTypeListener) then begin
    FMapType.ChangeNotifier.Remove(FMapTypeListener);
    FMapType := nil;
    FMapTypeListener := nil;
  end;
  if Assigned(FMapListened) and Assigned(FMapListener) then begin
    _RemoveListener(FMapListened);
  end;
  inherited;
end;

function TSourceDataUpdateInRectByFillingMap.GetActualZoom(
  const ALocalConverter: ILocalCoordConverter): Byte;
var
  VZoom: Integer;
begin
  VZoom := FConfig.GetStatic.Zoom;
  if FConfig.GetStatic.UseRelativeZoom then begin
    VZoom := VZoom + ALocalConverter.GetZoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    ALocalConverter.GetGeoConverter.CheckZoom(Result);
  end;
end;

procedure TSourceDataUpdateInRectByFillingMap.OnMapChange;
var
  VMap: IMapType;
begin
  VMap := FConfig.GetStatic.SourceMap;
  FCS.BeginWrite;
  try
    if Assigned(FMapListened) and not (FMapListened = VMap)  then begin
      if Assigned(FListener) and Assigned(FListenLocalConverter) then begin
        _RemoveListener(FMapListened);
      end;
      FMapListener := nil;
    end;
    if Assigned(VMap) and not (VMap = FMapListened) then begin
      if Assigned(FListener) and Assigned(FListenLocalConverter) then begin
        _SetListener(VMap, FListenLocalConverter);
      end;
    end;
    FMapListened := VMap;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSourceDataUpdateInRectByFillingMap.OnTileUpdate(const AMsg: IInterface);
var
  VListener: IListener;
  VLonLatRect: ILonLatRect;
begin
  FCS.BeginRead;
  try
    VListener := FListener;
  finally
    FCS.EndRead;
  end;
  if VListener <> nil then begin
    if Supports(AMsg, ILonLatRect, VLonLatRect) then begin
      VListener.Notification(VLonLatRect);
    end else begin
      VListener.Notification(nil);
    end;
  end;
end;

procedure TSourceDataUpdateInRectByFillingMap.RemoveListener;
begin
  FCS.BeginWrite;
  try
    if Assigned(FListener) and Assigned(FListenLocalConverter) and Assigned(FMapListened) then begin
      _RemoveListener(FMapListened);
    end;
    FListener := nil;
    FListenLocalConverter := nil;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSourceDataUpdateInRectByFillingMap._RemoveListener(
  const AMapListened: IMapType
);
var
  VNotifier: INotifierTilePyramidUpdate;
begin
  Assert(Assigned(FMapListener));
  if Assigned(AMapListened) and Assigned(FMapListener)then begin
    VNotifier := AMapListened.MapType.TileStorage.TileNotifier;
    if VNotifier <> nil then begin
      VNotifier.Remove(FMapListener);
    end;
  end;
end;

procedure TSourceDataUpdateInRectByFillingMap.SetListener(const AListener: IListener;
  const ALocalConverter: ILocalCoordConverter);
begin
  FCS.BeginWrite;
  try
    if not Assigned(AListener) or not Assigned(ALocalConverter) then begin
      if Assigned(FListener) and Assigned(FListenLocalConverter) and Assigned(FMapListened) then begin
        _RemoveListener(FMapListened);
      end;
      FListener := nil;
      FListenLocalConverter := nil;
    end else begin
      if not ALocalConverter.GetIsSameConverter(FListenLocalConverter) then begin
        if Assigned(FListener) and Assigned(FListenLocalConverter) and Assigned(FMapListened) then begin
          _RemoveListener(FMapListened);
        end;
        if Assigned(FMapListened) then begin
          _SetListener(FMapListened, ALocalConverter);
        end;
        FListenLocalConverter := ALocalConverter;
      end;
      FListener := AListener;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSourceDataUpdateInRectByFillingMap._SetListener(
  const AMapListened: IMapType; const ALocalConverter: ILocalCoordConverter);
var
  VZoom: Byte;
  VSourceZoom: Byte;
  VTileRect: TRect;
  VLonLatRect: TDoubleRect;
  VMapRect: TDoubleRect;
  VConverter: ICoordConverter;
  VMapLonLatRect: TDoubleRect;
  VNotifier: INotifierTilePyramidUpdate;
begin
  if AMapListened <> nil then begin
    if not Assigned(FMapListener) then begin
      FMapListener := TTileUpdateListenerToLonLat.Create(AMapListened.MapType.GeoConvert, Self.OnTileUpdate);
    end;
    VZoom := ALocalConverter.Zoom;
    VConverter := ALocalConverter.GeoConverter;
    VMapRect := ALocalConverter.GetRectInMapPixelFloat;
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VNotifier := AMapListened.MapType.TileStorage.TileNotifier;
    if VNotifier <> nil then begin
      VConverter := AMapListened.MapType.GeoConvert;
      VMapLonLatRect := VLonLatRect;
      VConverter.CheckLonLatRect(VMapLonLatRect);
      VSourceZoom := GetActualZoom(ALocalConverter);
      VTileRect :=
        RectFromDoubleRect(
          VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, VSourceZoom),
          rrToTopLeft
        );
      VNotifier.AddListenerByRect(FMapListener, VSourceZoom, VTileRect);
    end;
  end;
end;

end.
