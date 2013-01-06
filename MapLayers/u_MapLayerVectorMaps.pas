unit u_MapLayerVectorMaps;

interface

uses
  Types,
  Classes,
  SysUtils,
  GR32_Image,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_MapTypes,
  i_KmlLayerConfig,
  i_Bitmap32StaticFactory,
  i_TileError,
  i_VectorDataItemSimple,
  i_VectorItemsFactory,
  i_ImageResamplerConfig,
  i_IdCacheSimple,
  i_FindVectorItems,
  u_MapType,
  u_TiledLayerWithThreadBase;

type
  TMapLayerVectorMaps = class(TTiledLayerWithThreadBase, IFindVectorItems)
  private
    FConfig: IKmlLayerConfig;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FLayersSet: IMapTypeSetChangeable;
    FErrorLogger: ITileErrorLogger;

    FProjectedCache: IIdCacheSimple;

    FAllElements: IVectorDataItemList;
    FAllElementsCS: IReadWriteSync;

    FVectorMapsSet: IMapTypeSet;
    FVectorMapsSetCS: IReadWriteSync;

    procedure OnConfigChange;
    procedure OnLayerSetChange;

    procedure AddWikiElement(
      const AElments: IInterfaceList;
      const AData: IVectorDataItemSimple;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure AddElementsFromMap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AElments: IInterfaceList;
      Alayer: TMapType;
      const ALocalConverter: ILocalCoordConverter
    );
    function PrepareWikiElements(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IVectorDataItemList;
    function MouseOnElements(
      const AVisualConverter: ILocalCoordConverter;
      const ACopiedElements: IVectorDataItemList;
      const xy: TPoint;
      var AItemS: Double
    ): IVectorDataItemSimple;
  protected
    function CreateLayerProvider(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; override;
    procedure DelicateRedrawWithFullUpdate; override;
    procedure StartThreads; override;
  private
    function FindItem(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint;
      out AMarkS: Double
    ): IVectorDataItemSimple;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ATimerNoifier: INotifierTime;
      const AErrorLogger: ITileErrorLogger;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AConfig: IKmlLayerConfig;
      const ALayersSet: IMapTypeSetChangeable
    );
  end;

implementation


uses
  GR32,
  ActiveX,
  t_GeoTypes,
  i_CoordConverter,
  i_TileMatrix,
  i_TileIterator,
  i_VectorItemProjected,
  i_LonLatRect,
  i_VectorItemDrawConfig,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_TileErrorInfo,
  u_IdCacheSimpleThreadSafe,
  u_VectorDataItemList,
  u_GeoFun,
  u_TileIteratorByRect,
  u_Synchronizer,
  u_ResStrings,
  u_BitmapLayerProviderByVectorSubset;

{ TWikiLayerNew }

constructor TMapLayerVectorMaps.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ATimerNoifier: INotifierTime;
  const AErrorLogger: ITileErrorLogger;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AConfig: IKmlLayerConfig;
  const ALayersSet: IMapTypeSetChangeable
);
var
  VTileMatrixFactory: ITileMatrixFactory;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      ATileMatrixDraftResamplerConfig,
      ABitmapFactory,
      AConverterFactory
    );
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState.Position,
    AViewPortState.View,
    VTileMatrixFactory,
    ATimerNoifier,
    True,
    AConfig.ThreadConfig
  );
  FConfig := AConfig;
  FVectorItemsFactory := AVectorItemsFactory;
  FBitmapFactory := ABitmapFactory;
  FLayersSet := ALayersSet;
  FErrorLogger := AErrorLogger;

  FProjectedCache := TIdCacheSimpleThreadSafe.Create;
  FVectorMapsSetCS := MakeSyncRW_Var(Self);
  FAllElementsCS := MakeSyncRW_Var(Self, False);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FLayersSet.GetChangeNotifier
  );
end;

function TMapLayerVectorMaps.CreateLayerProvider(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALayerConverter: ILocalCoordConverter
): IBitmapLayerProvider;
var
  VConfig: IVectorItemDrawConfigStatic;
  VLinesClipRect: TDoubleRect;
  VMapPixelRect: TDoubleRect;
  VList: IVectorDataItemList;
  VVectorMapsSet: IMapTypeSet;
begin
  Result := nil;
  VConfig := FConfig.DrawConfig.GetStatic;

  VList := PrepareWikiElements(AOperationID, ACancelNotifier, ALayerConverter);
  FProjectedCache.Clear;
  FAllElementsCS.BeginWrite;
  try
    FAllElements := VList;
  finally
    FAllElementsCS.EndWrite;
  end;

  VMapPixelRect := ALayerConverter.GetRectInMapPixelFloat;
  VLinesClipRect.Left := VMapPixelRect.Left - 10;
  VLinesClipRect.Top := VMapPixelRect.Top - 10;
  VLinesClipRect.Right := VMapPixelRect.Right + 10;
  VLinesClipRect.Bottom := VMapPixelRect.Bottom + 10;
  FVectorMapsSetCS.BeginRead;
  try
    VVectorMapsSet := FVectorMapsSet;
  finally
    FVectorMapsSetCS.EndRead;
  end;
  Result :=
    TBitmapLayerProviderByVectorSubset.Create(
      VVectorMapsSet,
      VConfig.MainColor,
      VConfig.ShadowColor,
      VConfig.PointColor,
      FVectorItemsFactory,
      FBitmapFactory,
      ALayerConverter.ProjectionInfo,
      FProjectedCache,
      VLinesClipRect,
      VList
    );
end;

procedure TMapLayerVectorMaps.DelicateRedrawWithFullUpdate;
begin
  UpdateLayerProviderFlag.SetFlag;
  inherited;
end;

function TMapLayerVectorMaps.FindItem(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint;
  out AMarkS: Double
): IVectorDataItemSimple;
var
  VElements: IVectorDataItemList;
begin
  Result := nil;
  AMarkS := 0;
  if Visible then begin
    FAllElementsCS.BeginRead;
    try
      VElements := FAllElements;
    finally
      FAllElementsCS.EndRead;
    end;
    if VElements <> nil then begin
      Result := MouseOnElements(AVisualConverter, VElements, ALocalPoint, AMarkS);
    end;
  end;
end;

function TMapLayerVectorMaps.MouseOnElements(
  const AVisualConverter: ILocalCoordConverter;
  const ACopiedElements: IVectorDataItemList;
  const xy: TPoint;
  var AItemS: Double
): IVectorDataItemSimple;
var
  VRect: TRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  i: integer;
  VItem: IVectorDataItemSimple;
  VProjectdPath: IProjectedPath;
  VItemLine: IVectorDataItemLine;
  VItemPoly: IVectorDataItemPoly;
  VProjectdPolygon: IProjectedPolygon;
  VSquare: Double;
begin
  Result := nil;

  if ACopiedElements.Count > 0 then begin
    VRect.Left := xy.X - 3;
    VRect.Top := xy.Y - 3;
    VRect.Right := xy.X + 3;
    VRect.Bottom := xy.Y + 3;

    VConverter := AVisualConverter.GetGeoConverter;
    VZoom := AVisualConverter.GetZoom;
    VMapRect := AVisualConverter.LocalRect2MapRectFloat(VRect);
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(xy);

    // check element
    for i := 0 to ACopiedElements.Count - 1 do begin
      VItem := ACopiedElements.GetItem(i);
      if VItem.LLRect.IsIntersecWithRect(VLonLatRect) then begin
        if Supports(VItem, IVectorDataItemPoint) then begin
          Result := VItem;
          AItemS := 0;
          Exit;
        end else if Supports(VItem, IVectorDataItemLine, VItemLine) then begin
          if Supports(FProjectedCache.GetByID(Integer(VItemLine)), IProjectedPath, VProjectdPath) then begin
            if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
              Result := VItem;
              AItemS := 0;
              Exit;
            end;
          end;
        end else if Supports(VItem, IVectorDataItemPoly, VItemPoly) then begin
          if Supports(FProjectedCache.GetByID(Integer(VItemPoly)), IProjectedPolygon, VProjectdPolygon) then begin
            if VProjectdPolygon.IsPointInPolygon(VPixelPos) then begin
              VSquare := VProjectdPolygon.CalcArea;
              if (Result = nil) or (VSquare < AItemS) then begin
                Result := VItem;
                AItemS := VSquare;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMapLayerVectorMaps.OnConfigChange;
var
  VVectorMapsSet: IMapTypeSet;
begin
  FVectorMapsSetCS.BeginRead;
  try
    VVectorMapsSet := FVectorMapsSet;
  finally
    FVectorMapsSetCS.EndRead;
  end;
  ViewUpdateLock;
  try
    Visible := (VVectorMapsSet <> nil) and (VVectorMapsSet.GetCount > 0);
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerVectorMaps.OnLayerSetChange;
var
  VNewLayersSet: IMapTypeSet;
begin
  ViewUpdateLock;
  try
    VNewLayersSet := FLayersSet.GetStatic;

    FVectorMapsSetCS.BeginWrite;
    try
      FVectorMapsSet := VNewLayersSet;
    finally
      FVectorMapsSetCS.EndWrite;
    end;
    OnConfigChange;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerVectorMaps.AddElementsFromMap(AOperationID: Integer;
  const ACancelNotifier: INotifierOperation; const AElments: IInterfaceList;
  Alayer: TMapType; const ALocalConverter: ILocalCoordConverter);
var
  ii: integer;
  kml: IVectorDataItemList;
  VTileIterator: ITileIterator;
  VZoom: Byte;
  VSourceGeoConvert: ICoordConverter;
  VGeoConvert: ICoordConverter;
  VBitmapOnMapPixelRect: TDoubleRect;
  VSourceLonLatRect: TDoubleRect;
  VTileSourceRect: TRect;
  VTile: TPoint;
  VErrorString: string;
  VError: ITileErrorInfo;
begin
  VZoom := ALocalConverter.GetZoom;
  VSourceGeoConvert := Alayer.GeoConvert;
  VGeoConvert := ALocalConverter.GetGeoConverter;

  VBitmapOnMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VBitmapOnMapPixelRect, VZoom);

  VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
  VTileSourceRect :=
    RectFromDoubleRect(
      VSourceGeoConvert.LonLatRect2TileRectFloat(VSourceLonLatRect, VZoom),
      rrToTopLeft
    );
  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);

  while VTileIterator.Next(VTile) do begin
    VErrorString := '';
    try
      kml := Alayer.LoadTileVector(VTile, VZoom, False, Alayer.CacheVector);
      if kml <> nil then begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end else begin
          for ii := 0 to KML.Count - 1 do begin
            AddWikiElement(AElments, KML.GetItem(ii), ALocalConverter);
          end;
        end;
      end;
    except
      on E: Exception do begin
        VErrorString := E.Message;
      end;
      else
        VErrorString := SAS_ERR_TileDownloadUnexpectedError;
    end;
    if VErrorString <> '' then begin
      VError :=
        TTileErrorInfo.Create(
          Alayer,
          VZoom,
          VTile,
          VErrorString
        );
      FErrorLogger.LogError(VError);
    end;
    kml := nil;
  end;
end;

procedure TMapLayerVectorMaps.AddWikiElement(const AElments: IInterfaceList;
  const AData: IVectorDataItemSimple;
  const ALocalConverter: ILocalCoordConverter);
var
  VConverter: ICoordConverter;
  VSize: TPoint;
  VRect: ILonLatRect;
  VLLRect: TDoubleRect;
  VBounds: TDoubleRect;
begin
  if AData <> nil then begin
    VSize := ALocalConverter.GetLocalRectSize;
    VConverter := ALocalConverter.GetGeoConverter;
    VRect := AData.LLRect;
    if VRect <> nil then begin
      VLLRect := VRect.Rect;
      VConverter.CheckLonLatRect(VLLRect);
      VBounds := ALocalConverter.LonLatRect2LocalRectFloat(VLLRect);
      if ((VBounds.Top < VSize.Y) and (VBounds.Bottom > 0) and (VBounds.Left < VSize.X) and (VBounds.Right > 0)) then begin
        if Supports(AData, IVectorDataItemPoint) or (((VBounds.Right - VBounds.Left) > 1) and ((VBounds.Bottom - VBounds.Top) > 1)) then begin
          AElments.Add(AData);
        end;
      end;
    end;
  end;
end;

function TMapLayerVectorMaps.PrepareWikiElements(AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter): IVectorDataItemList;
var
  VVectorMapsSet: IMapTypeSet;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
  VItem: IMapType;
  VMapType: TMapType;
  VElements: IInterfaceList;
begin
  FVectorMapsSetCS.BeginRead;
  try
    VVectorMapsSet := FVectorMapsSet;
  finally
    FVectorMapsSetCS.EndRead;
  end;
  VElements := TInterfaceList.Create;
  VElements.Lock;
  try
    if VVectorMapsSet <> nil then begin
      VEnum := VVectorMapsSet.GetIterator;
      while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
        VItem := VVectorMapsSet.GetMapTypeByGUID(VGUID);
        VMapType := VItem.GetMapType;
        if VMapType.IsKmlTiles then begin
          AddElementsFromMap(AOperationID, ACancelNotifier, VElements, VMapType, ALocalConverter);
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            Break;
          end;
        end;
      end;
    end;
  finally
    VElements.Unlock;
  end;
  Result := TVectorDataItemList.Create(VElements);
end;

procedure TMapLayerVectorMaps.StartThreads;
begin
  inherited;
  OnLayerSetChange;
  OnConfigChange;
end;

end.
