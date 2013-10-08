unit u_BitmapLayerProviderFillingMap;

interface

uses
  Types,
  SysUtils,
  i_Listener,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_MapTypes,
  i_BitmapLayerProvider,
  i_FillingMapColorer,
  i_ObjectWithListener,
  u_MapType,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderFillingMap = class(
    TBaseInterfacedObject,
    IBitmapLayerProvider,
    IObjectWithListener
    )
  private
    FMapType: IMapType;
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
    FColorer: IFillingMapColorer;

    FListener: IListener;
    FListenLocalConverter: ILocalCoordConverter;
    FListenerCS: IReadWriteSync;
    FMapListener: IListener;
    FVersionListener: IListener;
    procedure OnTileUpdate(const AMsg: IInterface);
    procedure OnMapVersionChange;
    function GetActualZoom(
      const ALocalConverter: ILocalCoordConverter
    ): Byte;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  private
    procedure SetListener(
      const AListener: IListener;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure RemoveListener;
  public
    constructor Create(
      const AMapType: IMapType;
      AUseRelativeZoom: Boolean;
      AZoom: Integer;
      const AColorer: IFillingMapColorer
    );
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  i_CoordConverter,
  i_LonLatRect,
  i_NotifierTilePyramidUpdate,
  u_ListenerByEvent,
  u_TileUpdateListenerToLonLat,
  u_GeoFun,
  u_Synchronizer;

{ TBitmapLayerProviderFillingMap }

constructor TBitmapLayerProviderFillingMap.Create(
  const AMapType: IMapType;
  AUseRelativeZoom: Boolean;
  AZoom: Integer;
  const AColorer: IFillingMapColorer
);
begin
  inherited Create;
  FMapType := AMapType;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FColorer := AColorer;
  Assert(FMapType <> nil);
  Assert(FMapType.MapType <> nil);
  Assert(FColorer <> nil);
  FListenerCS := MakeSyncRW_Var(Self, False);
end;

destructor TBitmapLayerProviderFillingMap.Destroy;
begin
  if Assigned(FListenerCS) then begin
    RemoveListener;
  end;
  inherited;
end;

function TBitmapLayerProviderFillingMap.GetActualZoom(
  const ALocalConverter: ILocalCoordConverter): Byte;
var
  VZoom: Integer;
begin
  VZoom := FZoom;
  if FUseRelativeZoom then begin
    VZoom := VZoom + ALocalConverter.GetZoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    ALocalConverter.GetGeoConverter.CheckZoom(Result);
  end;
end;

function TBitmapLayerProviderFillingMap.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VSourceZoom: Byte;
begin
  VSourceZoom := GetActualZoom(ALocalConverter);
  if ALocalConverter.Zoom > VSourceZoom then begin
    Result := nil;
  end else begin
    Result :=
      FMapType.MapType.GetFillingMapBitmap(
        AOperationID,
        ACancelNotifier,
        ALocalConverter,
        VSourceZoom,
        FMapType.MapType.VersionConfig.Version,
        FColorer
      );
  end;
end;

procedure TBitmapLayerProviderFillingMap.OnMapVersionChange;
var
  VListener: IListener;
begin
  FListenerCS.BeginRead;
  try
    VListener := FListener;
  finally
    FListenerCS.EndRead;
  end;
  if VListener <> nil then begin
    VListener.Notification(nil);
  end;
end;

procedure TBitmapLayerProviderFillingMap.OnTileUpdate(const AMsg: IInterface);
var
  VListener: IListener;
  VLonLatRect: ILonLatRect;
begin
  FListenerCS.BeginRead;
  try
    VListener := FListener;
  finally
    FListenerCS.EndRead;
  end;
  if VListener <> nil then begin
    if Supports(AMsg, ILonLatRect, VLonLatRect) then begin
      VListener.Notification(VLonLatRect);
    end else begin
      VListener.Notification(nil);
    end;
  end;
end;

procedure TBitmapLayerProviderFillingMap.RemoveListener;
var
  VNotifier: INotifierTilePyramidUpdate;
begin
  FListenerCS.BeginWrite;
  try
    if (FListener <> nil) and (FListenLocalConverter <> nil) then begin
      VNotifier := FMapType.MapType.TileStorage.TileNotifier;
      if VNotifier <> nil then begin
        VNotifier.Remove(FMapListener);
      end;
      FMapType.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
    end;
    FListener := nil;
    FListenLocalConverter := nil;
  finally
    FListenerCS.EndWrite;
  end;
end;

procedure TBitmapLayerProviderFillingMap.SetListener(
  const AListener: IListener;
  const ALocalConverter: ILocalCoordConverter
);
var
  VNotifier: INotifierTilePyramidUpdate;
  VZoom: Byte;
  VConverter: ICoordConverter;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VMapLonLatRect: TDoubleRect;
  VTileRect: TRect;
  VSourceZoom: Byte;
begin
  VSourceZoom := GetActualZoom(ALocalConverter);
  FListenerCS.BeginWrite;
  try
    if (AListener = nil) or (ALocalConverter = nil) then begin
      RemoveListener;
    end else begin
      if FMapListener = nil then begin
        FMapListener := TTileUpdateListenerToLonLat.Create(FMapType.MapType.GeoConvert, Self.OnTileUpdate);
      end;
      if FVersionListener = nil then begin
        FVersionListener := TNotifyNoMmgEventListener.Create(Self.OnMapVersionChange);
      end;
      if (FListener = nil) or (FListenLocalConverter = nil) then begin
        FMapType.MapType.VersionConfig.ChangeNotifier.Add(FVersionListener);
      end;
      if not ALocalConverter.GetIsSameConverter(FListenLocalConverter) then begin
        VZoom := ALocalConverter.Zoom;
        VConverter := ALocalConverter.GeoConverter;
        VMapRect := ALocalConverter.GetRectInMapPixelFloat;
        VConverter.CheckPixelRectFloat(VMapRect, VZoom);
        VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
        VNotifier := FMapType.MapType.TileStorage.TileNotifier;
        if VNotifier <> nil then begin
          VConverter := FMapType.MapType.GeoConvert;
          VMapLonLatRect := VLonLatRect;
          VConverter.CheckLonLatRect(VMapLonLatRect);
          VTileRect :=
            RectFromDoubleRect(
              VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, VSourceZoom),
              rrToTopLeft
            );
          VNotifier.AddListenerByRect(FMapListener, VSourceZoom, VTileRect);
        end;
      end;
      FListener := AListener;
      FListenLocalConverter := ALocalConverter;
    end;
  finally
    FListenerCS.EndWrite;
  end;
end;

end.
