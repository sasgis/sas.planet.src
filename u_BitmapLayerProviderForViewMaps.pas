unit u_BitmapLayerProviderForViewMaps;

interface

uses
  Types,
  i_NotifierOperation,
  i_CoordConverter,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_MapTypes,
  i_MapTypeListStatic,
  i_BitmapLayerProvider,
  i_BitmapPostProcessing,
  i_TileError,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderForViewMaps = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FMainMap: IMapType;
    FLayersList: IMapTypeListStatic;
    FBitmapFactory: IBitmap32StaticFactory;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    FUseCache: Boolean;
    FPostProcessingConfig: IBitmapPostProcessing;
    FErrorLogger: ITileErrorLogger;
    
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
  end;

implementation

uses
  SysUtils,
  GR32,
  i_TileObjCache,
  u_Bitmap32ByStaticBitmap,
  u_BitmapFunc,
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
        AMapType.MapType.VersionConfig.Version,
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
            AMapType.MapType.Zmp.GUID,
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
          AMapType.MapType.Zmp.GUID,
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

end.
