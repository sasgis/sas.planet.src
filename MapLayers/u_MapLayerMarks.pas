unit u_MapLayerMarks;

interface

uses
  Types,
  SysUtils,
  GR32_Image,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_InternalPerformanceCounter,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarksLayerConfig,
  i_VectorDataItemSimple,
  i_VectorItemsFactory,
  i_Bitmap32StaticFactory,
  i_ImageResamplerConfig,
  i_IdCacheSimple,
  i_FindVectorItems,
  i_VectorItemSubset,
  i_MarkerDrawable,
  i_Mark,
  i_MarkSystem,
  u_TiledLayerWithThreadBase;

type
  TMapLayerMarks = class(TTiledLayerWithThreadBase, IFindVectorItems)
  private
    FConfig: IMarksLayerConfig;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FMarkDB: IMarkSystem;
    FMarkIconDefault: IMarkerDrawableChangeable;

    FGetMarksCounter: IInternalPerformanceCounter;
    FMouseOnRegCounter: IInternalPerformanceCounter;

    FProjectedCache: IIdCacheSimple;
    FMarkerCache: IIdCacheSimple;

    FMarksSubset: IVectorItemSubset;
    FMarksSubsetCS: IReadWriteSync;

    procedure OnConfigChange;
    procedure OnMarksDbChange;
    function GetMarksSubset(
      const AConfig: IUsedMarksConfigStatic;
      const ALocalConverter: ILocalCoordConverter
    ): IVectorItemSubset;
  protected
    function CreateLayerProvider(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; override;
    procedure StartThreads; override;
  private
    function FindItems(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AView: ILocalCoordConverterChangeable;
      const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AVectorItemsFactory: IVectorItemsFactory;
      const AMarkIconDefault: IMarkerDrawableChangeable;
      const ATimerNoifier: INotifierTime;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AConfig: IMarksLayerConfig;
      const AMarkDB: IMarkSystem
    );
  end;

implementation

uses
  ActiveX,
  Classes,
  t_GeoTypes,
  i_CoordConverter,
  i_TileMatrix,
  i_VectorItemProjected,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_MarkerProviderForVectorItem,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_InterfaceListSimple,
  u_IdCacheSimpleThreadSafe,
  u_Synchronizer,
  u_MarkerProviderForVectorItemWithCache,
  u_MarkerProviderForVectorItemForMarkPoints,
  u_VectorDataItemSubset,
  u_BitmapLayerProviderByMarksSubset;

{ TMapMarksLayerNew }

constructor TMapLayerMarks.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AMarkIconDefault: IMarkerDrawableChangeable;
  const ATimerNoifier: INotifierTime;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AConfig: IMarksLayerConfig;
  const AMarkDB: IMarkSystem
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
    APosition,
    AView,
    VTileMatrixFactory,
    ATimerNoifier,
    True,
    AConfig.ThreadConfig
  );
  FConfig := AConfig;
  FMarkDB := AMarkDB;
  FVectorItemsFactory := AVectorItemsFactory;
  FMarkIconDefault := AMarkIconDefault;
  FBitmapFactory := ABitmapFactory;


  FProjectedCache := TIdCacheSimpleThreadSafe.Create;
  FMarkerCache := TIdCacheSimpleThreadSafe.Create;

  FMarksSubsetCS := MakeSyncRW_Var(Self);
  FGetMarksCounter := PerfList.CreateAndAddNewCounter('GetMarks');
  FMouseOnRegCounter := PerfList.CreateAndAddNewCounter('MouseOnReg');

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.MarksShowConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.MarksDrawConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMarksDbChange),
    FMarkDB.MarkDb.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMarksDbChange),
    FMarkDB.CategoryDB.ChangeNotifier
  );
end;

function TMapLayerMarks.CreateLayerProvider(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALayerConverter: ILocalCoordConverter
): IBitmapLayerProvider;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VMarksSubset: IVectorItemSubset;
  VMapRect: TDoubleRect;
  VLinesClipRect: TDoubleRect;
  VMarksDrawConfig: IMarksDrawConfigStatic;
  VMarkerProvider: IMarkerProviderForVectorItem;
begin
  Result := nil;
  VCounterContext := FGetMarksCounter.StartOperation;
  try
    VMarksSubset := GetMarksSubset(FConfig.MarksShowConfig.GetStatic, ALayerConverter);
    FMarksSubsetCS.BeginWrite;
    try
      FMarksSubset := VMarksSubset;
    finally
      FMarksSubsetCS.EndWrite;
    end;
  finally
    FGetMarksCounter.FinishOperation(VCounterContext);
  end;
  FProjectedCache.Clear;
  FMarkerCache.Clear;
  if (VMarksSubset <> nil) and (not VMarksSubset.IsEmpty) then begin
    VMapRect := ALayerConverter.GetRectInMapPixelFloat;
    VLinesClipRect.Left := VMapRect.Left - 10;
    VLinesClipRect.Top := VMapRect.Top - 10;
    VLinesClipRect.Right := VMapRect.Right + 10;
    VLinesClipRect.Bottom := VMapRect.Bottom + 10;
    VMarksDrawConfig := FConfig.MarksDrawConfig.GetStatic;
    VMarkerProvider :=
      TMarkerProviderForVectorItemWithCache.Create(
        FMarkerCache,
        TMarkerProviderForVectorItemForMarkPoints.Create(FBitmapFactory, FMarkIconDefault, VMarksDrawConfig)
      );
    Result :=
      TBitmapLayerProviderByMarksSubset.Create(
        VMarksDrawConfig,
        FVectorItemsFactory,
        FBitmapFactory,
        ALayerConverter.ProjectionInfo,
        FProjectedCache,
        VMarkerProvider,
        VLinesClipRect,
        VMarksSubset
      );
  end;
end;

function TMapLayerMarks.FindItems(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;
var
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VPixelPos: TDoublePoint;
  VZoom: Byte;
  VMark: IMark;
  VMapRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
  VMarksSubset: IVectorItemSubset;
  VMarksEnum: IEnumUnknown;
  i: Cardinal;
  VCounterContext: TInternalPerformanceCounterContext;
  VMarkLine: IVectorDataItemLine;
  VMarkPoly: IVectorDataItemPoly;
  VProjectdPath: IProjectedPath;
  VProjectdPolygon: IProjectedPolygon;
  Vtmp: IInterfaceListSimple;
begin
  Result := nil;
  Vtmp := TInterfaceListSimple.Create;
  VCounterContext := FMouseOnRegCounter.StartOperation;
  try
    FMarksSubsetCS.BeginRead;
    try
      VMarksSubset := FMarksSubset;
    finally
      FMarksSubsetCS.EndRead;
    end;

    if VMarksSubset <> nil then begin
      if not VMarksSubset.IsEmpty then begin
        VRect.Left := ALocalPoint.X - 8;
        VRect.Top := ALocalPoint.Y - 16;
        VRect.Right := ALocalPoint.X + 8;
        VRect.Bottom := ALocalPoint.Y + 16;
        VLocalConverter := TileMatrix.LocalConverter;
        VConverter := VLocalConverter.GetGeoConverter;
        VZoom := VLocalConverter.GetZoom;
        VMapRect := AVisualConverter.LocalRect2MapRectFloat(VRect);
        VConverter.CheckPixelRectFloat(VMapRect, VZoom);
        VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
        VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(ALocalPoint);
        VMarksEnum := VMarksSubset.GetEnum;
        while VMarksEnum.Next(1, VMark, @i) = S_OK do begin
          if VMark.LLRect.IsIntersecWithRect(VLonLatRect) then begin
            if Supports(VMark, IMarkPoint) then begin
              Vtmp.add(VMark);
            end else begin
              if Supports(VMark, IVectorDataItemLine, VMarkLine) then begin
                if Supports(FProjectedCache.GetByID(Integer(VMarkLine)), IProjectedPath, VProjectdPath) then begin
                  if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
                    Vtmp.Add(VMark);
                  end;
                end;
              end else if Supports(VMark, IVectorDataItemPoly, VMarkPoly) then begin
                if Supports(FProjectedCache.GetByID(Integer(VMarkPoly)), IProjectedPolygon, VProjectdPolygon) then begin
                  if VProjectdPolygon.IsPointInPolygon(VPixelPos) or
                    VProjectdPolygon.IsPointOnBorder(VPixelPos, 3) then begin
                    Vtmp.Add(VMark);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    FMouseOnRegCounter.FinishOperation(VCounterContext);
  end;
  Result := TVectorItemSubset.Create(Vtmp.MakeStaticAndClear);
end;

function TMapLayerMarks.GetMarksSubset(
  const AConfig: IUsedMarksConfigStatic;
  const ALocalConverter: ILocalCoordConverter
): IVectorItemSubset;
var
  VList: IInterfaceListStatic;
  VZoom: Byte;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VGeoConverter: ICoordConverter;
begin
  VList := nil;
  Result := nil;
  if AConfig.IsUseMarks then begin
    VZoom := ALocalConverter.GetZoom;
    if not AConfig.IgnoreCategoriesVisible then begin
      VList := FMarkDB.GetVisibleCategories(VZoom);
    end;
    try
      if (VList <> nil) and (VList.Count = 0) then begin
        Result := nil;
      end else begin
        VGeoConverter := ALocalConverter.GetGeoConverter;
        VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
        VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
        VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);
        Result :=
          FMarkDB.MarkDb.GetMarkSubsetByCategoryListInRect(
            VLonLatRect,
            VList,
            AConfig.IgnoreMarksVisible
          );
      end;
    finally
      VList := nil;
    end;
  end;
end;

procedure TMapLayerMarks.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.MarksShowConfig.IsUseMarks;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerMarks.OnMarksDbChange;
begin
  if Visible then begin
    ViewUpdateLock;
    try
      SetNeedUpdateLayerProvider;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerMarks.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
