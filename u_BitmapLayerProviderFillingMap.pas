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
  i_BitmapLayerProviderWithListener,
  u_MapType,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderFillingMap = class(
    TBaseInterfacedObject,
    IBitmapLayerProvider,
    IBitmapLayerProviderWithListener
    )
  private
    FMapType: IMapType;
    FSourceZoom: Byte;
    FColorer: IFillingMapColorer;

    FListener: IListener;
    FListenLocalConverter: ILocalCoordConverter;
    FListenerCS: IReadWriteSync;
    FMapListener: IListener;
    FVersionListener: IListener;
    procedure OnTileUpdate(const AMsg: IInterface);
    procedure OnMapVersionChange;
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
      ASourceZoom: Byte;
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
  ASourceZoom: Byte;
  const AColorer: IFillingMapColorer
);
begin
  inherited Create;
  FMapType := AMapType;
  FSourceZoom := ASourceZoom;
  FColorer := AColorer;
  Assert(FMapType <> nil);
  Assert(FMapType.MapType <> nil);
  Assert(FColorer <> nil);
  FListenerCS := MakeSyncRW_Var(Self, False);
end;

destructor TBitmapLayerProviderFillingMap.Destroy;
begin
  RemoveListener;
  inherited;
end;

function TBitmapLayerProviderFillingMap.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
begin
  if ALocalConverter.Zoom > FSourceZoom then begin
    Result := nil;
  end else begin
    Result :=
      FMapType.MapType.GetFillingMapBitmap(
        AOperationID,
        ACancelNotifier,
        ALocalConverter,
        FSourceZoom,
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
      VNotifier := FMapType.MapType.TileNotifier;
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
begin
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
        VNotifier := FMapType.MapType.TileNotifier;
        if VNotifier <> nil then begin
          VConverter := FMapType.MapType.GeoConvert;
          VMapLonLatRect := VLonLatRect;
          VConverter.CheckLonLatRect(VMapLonLatRect);
          VTileRect :=
            RectFromDoubleRect(
              VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, FSourceZoom),
              rrToTopLeft
            );
          VNotifier.AddListenerByRect(FMapListener, FSourceZoom, VTileRect);
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
