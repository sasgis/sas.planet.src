unit u_BitmapLayerProviderForViewMaps;

interface

uses
  Types,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_NotifierOperation,
  i_CoordConverter,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_MapTypes,
  i_BitmapLayerProvider,
  i_BitmapPostProcessingConfig,
  i_BitmapLayerProviderWithListener,
  i_TileError,
  u_MapType,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderForViewMaps = class(
    TBaseInterfacedObject,
    IBitmapLayerProvider,
    IBitmapLayerProviderWithListener
    )
  private
    FMainMap: IMapType;
    FLayersList: IMapTypeListStatic;
    FBitmapFactory: IBitmap32StaticFactory;

    FListener: IListener;
    FListenLocalConverter: ILocalCoordConverter;
    FListenerCS: IReadWriteSync;
    FMainMapListener: IListener;
    FLayerListeners: array of IListener;
    FVersionListener: IListener;

    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    FUseCache: Boolean;
    FPostProcessingConfig: IBitmapPostProcessing;
    FErrorLogger: ITileErrorLogger;
    procedure OnMapVersionChange;
    procedure OnTileUpdate(const AMsg: IInterface);
    function GetBitmapByMapType(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint;
      AZoom: byte;
      const ACoordConverterTarget: ICoordConverter;
      const ASource: IBitmap32Static;
      AUsePrevZoom: Boolean;
      const AMapType: IMapType
    ): IBitmap32Static;
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
      const ABitmapFactory: IBitmap32StaticFactory;
      const AMainMap: IMapType;
      const ALayersList: IMapTypeListStatic;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean;
      AUseCache: Boolean;
      const APostProcessingConfig: IBitmapPostProcessing;
      const AErrorLogger: ITileErrorLogger
    );
    destructor Destroy; override;
  end;

implementation

uses
  GR32,
  t_GeoTypes,
  i_LonLatRect,
  i_TileObjCache,
  i_NotifierTilePyramidUpdate,
  u_Bitmap32ByStaticBitmap,
  u_ListenerByEvent,
  u_TileUpdateListenerToLonLat,
  u_Synchronizer,
  u_BitmapFunc,
  u_GeoFun,
  u_TileErrorInfo;

{ TBitmapLayerProviderForViewMaps }

constructor TBitmapLayerProviderForViewMaps.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  const AMainMap: IMapType;
  const ALayersList: IMapTypeListStatic;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer, AUseCache: Boolean;
  const APostProcessingConfig: IBitmapPostProcessing;
  const AErrorLogger: ITileErrorLogger
);
begin
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FMainMap := AMainMap;
  FLayersList := ALayersList;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;
  FUseCache := AUseCache;
  FPostProcessingConfig := APostProcessingConfig;
  FErrorLogger := AErrorLogger;
  FListenerCS := MakeSyncRW_Var(Self, False);
end;

destructor TBitmapLayerProviderForViewMaps.Destroy;
begin
  RemoveListener;
  inherited;
end;

function TBitmapLayerProviderForViewMaps.GetBitmapByMapType(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint;
  AZoom: byte;
  const ACoordConverterTarget: ICoordConverter;
  const ASource: IBitmap32Static;
  AUsePrevZoom: Boolean;
  const AMapType: IMapType
): IBitmap32Static;
var
  VCache: ITileObjCacheBitmap;
  VLayer: IBitmap32Static;
  VBitmap: TBitmap32ByStaticBitmap;
  VError: ITileErrorInfo;
begin
  Result := ASource;
  VLayer := nil;
  try
    VCache := nil;
    if FUseCache then begin
      VCache := AMapType.MapType.CacheBitmap;
    end;
    VLayer :=
      AMapType.MapType.LoadTileUni(
        ATile,
        AZoom,
        ACoordConverterTarget,
        AUsePrevZoom,
        True,
        False,
        VCache
      );
  except
    on E: Exception do begin
      if FErrorLogger <> nil then begin
        VError :=
          TTileErrorInfo.Create(
            AMapType.MapType,
            AZoom,
            ATile,
            E.Message
          );
        FErrorLogger.LogError(VError);
      end else begin
        raise;
      end;
    end;
    else if FErrorLogger <> nil then begin
        FErrorLogger.LogError(
          TTileErrorInfo.Create(
          AMapType.MapType,
          AZoom,
          ATile,
          'Unexpected read tile error'
          )
        );
      end else begin
        raise;
      end;
  end;

  if VLayer <> nil then begin
    if Result = nil then begin
      Result := VLayer;
    end else begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
      try
        AssignStaticToBitmap32(VBitmap, Result);
        BlockTransferFull(
          VBitmap,
          0, 0,
          VLayer,
          dmBlend
        );
        Result := VBitmap.BitmapStatic;
      finally
        VBitmap.Free;
      end;
    end;
  end;
end;

function TBitmapLayerProviderForViewMaps.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VTile: TPoint;
  Vzoom: byte;
  VCoordConverterTarget: ICoordConverter;
  VPixelRect: TRect;
  i: Integer;
begin
  Vzoom := ALocalConverter.Zoom;
  VCoordConverterTarget := ALocalConverter.GeoConverter;
  VPixelRect := ALocalConverter.GetRectInMapPixel;
  VTile := VCoordConverterTarget.PixelRect2TileRect(VPixelRect, Vzoom).TopLeft;
  Assert(Types.EqualRect(VPixelRect, VCoordConverterTarget.TilePos2PixelRect(VTile, Vzoom)));

  Result :=
    GetBitmapByMapType(
      AOperationID,
      ACancelNotifier,
      VTile,
      Vzoom,
      VCoordConverterTarget,
      nil,
      FUsePrevZoomAtMap,
      FMainMap
    );
  if FLayersList <> nil then begin
    for i := 0 to FLayersList.Count - 1 do begin
      Result :=
        GetBitmapByMapType(
          AOperationID,
          ACancelNotifier,
          VTile,
          Vzoom,
          VCoordConverterTarget,
          Result,
          FUsePrevZoomAtLayer,
          FLayersList.Items[i]
        );
    end;
  end;
  if FPostProcessingConfig <> nil then begin
    Result := FPostProcessingConfig.Process(Result);
  end;
end;

procedure TBitmapLayerProviderForViewMaps.OnMapVersionChange;
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

procedure TBitmapLayerProviderForViewMaps.OnTileUpdate(const AMsg: IInterface);
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

procedure TBitmapLayerProviderForViewMaps.RemoveListener;
var
  VNotifier: INotifierTilePyramidUpdate;
  i: Integer;
  VMap: IMapType;
begin
  FListenerCS.BeginWrite;
  try
    if (FListener <> nil) and (FListenLocalConverter <> nil) then begin
      VMap := FMainMap;
      if VMap <> nil then begin
        VNotifier := VMap.MapType.TileNotifier;
        if VNotifier <> nil then begin
          VNotifier.Remove(FMainMapListener);
        end;
        VMap.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
      end;
      if FLayersList <> nil then begin
        for i := 0 to FLayersList.Count - 1 do begin
          VMap := FLayersList.Items[i];
          if VMap <> nil then begin
            VNotifier := VMap.MapType.TileNotifier;
            if VNotifier <> nil then begin
              VNotifier.Remove(FLayerListeners[i]);
            end;
            VMap.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
          end;
        end;
      end;
    end;
    FListener := nil;
    FListenLocalConverter := nil;
  finally
    FListenerCS.EndWrite;
  end;
end;

procedure TBitmapLayerProviderForViewMaps.SetListener(
  const AListener: IListener;
  const ALocalConverter: ILocalCoordConverter
);
var
  VNotifier: INotifierTilePyramidUpdate;
  i: Integer;
  VMap: IMapType;
  VZoom: Byte;
  VTileRect: TRect;
  VLonLatRect: TDoubleRect;
  VMapRect: TRect;
  VConverter: ICoordConverter;
  VMapLonLatRect: TDoubleRect;
begin
  FListenerCS.BeginWrite;
  try
    if (AListener = nil) or (ALocalConverter = nil) then begin
      RemoveListener;
    end else begin
      if (FListener <> nil) and (FListenLocalConverter <> nil) then begin
        VZoom := FListenLocalConverter.Zoom;
        if (VZoom <> ALocalConverter.Zoom) then begin
          VMap := FMainMap;
          if VMap <> nil then begin
            VNotifier := VMap.MapType.TileNotifier;
            if VNotifier <> nil then begin
              VNotifier.Remove(FMainMapListener);
            end;
          end;
          if FLayersList <> nil then begin
            for i := 0 to FLayersList.Count - 1 do begin
              VMap := FLayersList.Items[i];
              if VMap <> nil then begin
                VNotifier := VMap.MapType.TileNotifier;
                if VNotifier <> nil then begin
                  VNotifier.Remove(FLayerListeners[i]);
                end;
              end;
            end;
          end;
        end;
      end;
      if (FMainMap <> nil) and (FMainMapListener = nil) then begin
        VMap := FMainMap;
        FMainMapListener := TTileUpdateListenerToLonLat.Create(VMap.MapType.GeoConvert, Self.OnTileUpdate);
      end;
      if (FLayersList <> nil) and (FLayersList.Count > 0) and (Length(FLayerListeners) = 0) then begin
        SetLength(FLayerListeners, FLayersList.Count);
        for i := 0 to FLayersList.Count - 1 do begin
          VMap := FLayersList.Items[i];
          if VMap <> nil then begin
            FLayerListeners[i] := TTileUpdateListenerToLonLat.Create(VMap.MapType.GeoConvert, Self.OnTileUpdate);
          end;
        end;
      end;
      if FVersionListener = nil then begin
        FVersionListener := TNotifyNoMmgEventListener.Create(Self.OnMapVersionChange);
      end;
      if (FListener = nil) or (FListenLocalConverter = nil) then begin
        VMap := FMainMap;
        if VMap <> nil then begin
          VMap.MapType.VersionConfig.ChangeNotifier.Add(FVersionListener);
        end;
        if FLayersList <> nil then begin
          for i := 0 to FLayersList.Count - 1 do begin
            VMap := FLayersList.Items[i];
            if VMap <> nil then begin
              VMap.MapType.VersionConfig.ChangeNotifier.Add(FVersionListener);
            end;
          end;
        end;
      end;
      if not ALocalConverter.GetIsSameConverter(FListenLocalConverter) then begin
        VZoom := ALocalConverter.Zoom;
        VConverter := ALocalConverter.GeoConverter;
        VMapRect := ALocalConverter.GetRectInMapPixel;
        VConverter.CheckPixelRect(VMapRect, VZoom);
        VLonLatRect := VConverter.PixelRect2LonLatRect(VMapRect, VZoom);
        VMap := FMainMap;
        if VMap <> nil then begin
          VNotifier := VMap.MapType.TileNotifier;
          if VNotifier <> nil then begin
            VConverter := VMap.MapType.GeoConvert;
            VMapLonLatRect := VLonLatRect;
            VConverter.CheckLonLatRect(VMapLonLatRect);
            VTileRect :=
              RectFromDoubleRect(
                VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, VZoom),
                rrToTopLeft
              );
            VNotifier.AddListenerByRect(FMainMapListener, VZoom, VTileRect);
          end;
        end;
        if FLayersList <> nil then begin
          for i := 0 to FLayersList.Count - 1 do begin
            VMap := FLayersList.Items[i];
            if VMap <> nil then begin
              VNotifier := VMap.MapType.TileNotifier;
              if VNotifier <> nil then begin
                VConverter := VMap.MapType.GeoConvert;
                VMapLonLatRect := VLonLatRect;
                VConverter.CheckLonLatRect(VMapLonLatRect);
                VTileRect :=
                  RectFromDoubleRect(
                    VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, VZoom),
                    rrToTopLeft
                  );
                VNotifier.AddListenerByRect(FLayerListeners[i], VZoom, VTileRect);
              end;
            end;
          end;
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
