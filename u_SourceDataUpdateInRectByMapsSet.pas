unit u_SourceDataUpdateInRectByMapsSet;

interface

uses
  SysUtils,
  i_ObjectWithListener,
  i_Listener,
  i_LocalCoordConverter,
  i_MapTypeSet,
  i_MapTypeSetChangeable,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TSourceDataUpdateInRectByMapsSet = class(TBaseInterfacedObject, IObjectWithListener)
  private
    FMapTypeSet: IMapTypeSetChangeable;

    FMapTypeSetListener: IListener;
    FCS: IReadWriteSync;

    FMapsListened: IMapTypeSet;
    FMapListeners: IInterfaceListStatic;

    FListener: IListener;
    FListenLocalConverter: ILocalCoordConverter;
    procedure OnTileUpdate(const AMsg: IInterface);
    procedure OnMapSetChange;

    procedure _RemoveListeners(
      const AMapsListened: IMapTypeSet
    );
    procedure _SetListeners(
      const AMapsListened: IMapTypeSet;
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
      const AMapTypeSet: IMapTypeSetChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  t_GeoTypes,
  i_InterfaceListSimple,
  i_LonLatRect,
  i_CoordConverter,
  i_NotifierTilePyramidUpdate,
  i_MapTypes,
  u_InterfaceListSimple,
  u_ListenerByEvent,
  u_TileUpdateListenerToLonLat,
  u_GeoFun,
  u_Synchronizer;

{ TSourceDataUpdateInRectByMapsSet }

constructor TSourceDataUpdateInRectByMapsSet.Create(
  const AMapTypeSet: IMapTypeSetChangeable
);
begin
  inherited Create;
  FMapTypeSet := AMapTypeSet;
  FMapTypeSetListener := TNotifyNoMmgEventListener.Create(Self.OnMapSetChange);
  FCS := MakeSyncRW_Var(Self, False);
  FMapTypeSet.ChangeNotifier.Add(FMapTypeSetListener);
  OnMapSetChange;
end;

destructor TSourceDataUpdateInRectByMapsSet.Destroy;
begin
  if Assigned(FMapTypeSet) and Assigned(FMapTypeSetListener) then begin
    FMapTypeSet.ChangeNotifier.Remove(FMapTypeSetListener);
    FMapTypeSet := nil;
    FMapTypeSetListener := nil;
  end;
  if Assigned(FMapsListened) and Assigned(FMapListeners) then begin
    _RemoveListeners(FMapsListened);
  end;
  inherited;
end;

procedure TSourceDataUpdateInRectByMapsSet.OnMapSetChange;
var
  VMapSet: IMapTypeSet;
begin
  VMapSet := FMapTypeSet.GetStatic;
  FCS.BeginWrite;
  try
    if Assigned(FMapsListened) and not FMapsListened.IsEqual(VMapSet)  then begin
      if Assigned(FListener) and Assigned(FListenLocalConverter) then begin
        _RemoveListeners(FMapsListened);
      end;
      FMapListeners := nil;
    end;
    if Assigned(VMapSet) and not VMapSet.IsEqual(FMapsListened) then begin
      if Assigned(FListener) and Assigned(FListenLocalConverter) then begin
        _SetListeners(VMapSet, FListenLocalConverter);
      end;
    end;
    FMapsListened := VMapSet;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSourceDataUpdateInRectByMapsSet.OnTileUpdate(const AMsg: IInterface);
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

procedure TSourceDataUpdateInRectByMapsSet.RemoveListener;
begin
  FCS.BeginWrite;
  try
    if Assigned(FListener) and Assigned(FListenLocalConverter) and Assigned(FMapsListened) then begin
      _RemoveListeners(FMapsListened);
    end;
    FListener := nil;
    FListenLocalConverter := nil;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSourceDataUpdateInRectByMapsSet._RemoveListeners(
  const AMapsListened: IMapTypeSet
);
var
  i: Integer;
  VMap: IMapType;
  VListener: IListener;
  VNotifier: INotifierTilePyramidUpdate;
begin
  Assert(Assigned(FMapListeners));
  if Assigned(AMapsListened) and Assigned(FMapListeners)then begin
    Assert(AMapsListened.Count = FMapListeners.Count);
    for i := 0 to AMapsListened.Count - 1 do begin
      VMap := AMapsListened.Items[i];
      VListener := IListener(FMapListeners.Items[i]);
      if Assigned(VMap) and Assigned(VListener) then begin
        VNotifier := VMap.MapType.TileStorage.TileNotifier;
        if VNotifier <> nil then begin
          VNotifier.Remove(VListener);
        end;
      end;
    end;
  end;
end;

procedure TSourceDataUpdateInRectByMapsSet.SetListener(
  const AListener: IListener; const ALocalConverter: ILocalCoordConverter);
begin
  FCS.BeginWrite;
  try
    if not Assigned(AListener) or not Assigned(ALocalConverter) then begin
      if Assigned(FListener) and Assigned(FListenLocalConverter) and Assigned(FMapsListened) then begin
        _RemoveListeners(FMapsListened);
      end;
      FListener := nil;
      FListenLocalConverter := nil;
    end else begin
      if not ALocalConverter.GetIsSameConverter(FListenLocalConverter) then begin
        if Assigned(FListener) and Assigned(FListenLocalConverter) and Assigned(FMapsListened) then begin
          _RemoveListeners(FMapsListened);
        end;
        if Assigned(FMapsListened) then begin
          _SetListeners(FMapsListened, ALocalConverter);
        end;
        FListenLocalConverter := ALocalConverter;
      end;
      FListener := AListener;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSourceDataUpdateInRectByMapsSet._SetListeners(
  const AMapsListened: IMapTypeSet;
  const ALocalConverter: ILocalCoordConverter
);
var
  VListeners: IInterfaceListSimple;
  i: Integer;
  VMap: IMapType;
  VListener: IListener;
  VZoom: Byte;
  VTileRect: TRect;
  VLonLatRect: TDoubleRect;
  VMapRect: TRect;
  VConverter: ICoordConverter;
  VMapLonLatRect: TDoubleRect;
  VNotifier: INotifierTilePyramidUpdate;
begin
  if not Assigned(FMapListeners) then begin
    VListeners := TInterfaceListSimple.Create;
    VListeners.Capacity := AMapsListened.Count;
    for i := 0 to AMapsListened.Count - 1 do begin
      VMap := AMapsListened.Items[i];
      if VMap <> nil then begin
        VListener := TTileUpdateListenerToLonLat.Create(VMap.MapType.GeoConvert, Self.OnTileUpdate);
        VListeners.Add(VListener);
      end;
    end;
    FMapListeners := VListeners.MakeStaticAndClear;
  end;
  VZoom := ALocalConverter.Zoom;
  VConverter := ALocalConverter.GeoConverter;
  VMapRect := ALocalConverter.GetRectInMapPixel;
  VConverter.CheckPixelRect(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRect2LonLatRect(VMapRect, VZoom);
  for i := 0 to AMapsListened.Count - 1 do begin
    VMap := AMapsListened.Items[i];
    if VMap <> nil then begin
      VNotifier := VMap.MapType.TileStorage.TileNotifier;
      if VNotifier <> nil then begin
        VConverter := VMap.MapType.GeoConvert;
        VMapLonLatRect := VLonLatRect;
        VConverter.CheckLonLatRect(VMapLonLatRect);
        VTileRect :=
          RectFromDoubleRect(
            VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, VZoom),
            rrToTopLeft
          );
        VListener := IListener(FMapListeners[i]);
        VNotifier.AddListenerByRect(VListener, VZoom, VTileRect);
      end;
    end;
  end;
end;

end.
