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
  i_VectorItemSubsetBuilder,
  i_Bitmap32StaticFactory,
  i_ImageResamplerConfig,
  i_MarkerProviderForVectorItem,
  i_FindVectorItems,
  i_VectorItemSubset,
  i_ProjectedGeometryProvider,
  i_Mark,
  i_MarkSystem,
  u_TiledLayerWithThreadBase;

type
  TMapLayerMarks = class(TTiledLayerWithThreadBase, IFindVectorItems)
  private
    FConfig: IMarksLayerConfig;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FMarkDB: IMarkSystem;
    FProjectedCache: IProjectedGeometryProvider;
    FMarkerProvider: IMarkerProviderForVectorItem;

    FGetMarksCounter: IInternalPerformanceCounter;
    FMouseOnRegCounter: IInternalPerformanceCounter;

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
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AProjectedCache: IProjectedGeometryProvider;
      const AMarkerProvider: IMarkerProviderForVectorItem;
      const ATimerNoifier: INotifierTime;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AConfig: IMarksLayerConfig;
      const AMarkDB: IMarkSystem
    );
  end;

implementation

uses
  ActiveX,
  t_GeoTypes,
  i_CoordConverter,
  i_TileMatrix,
  i_VectorItemProjected,
  i_InterfaceListStatic,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_Synchronizer,
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
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjectedCache: IProjectedGeometryProvider;
  const AMarkerProvider: IMarkerProviderForVectorItem;
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
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FMarkerProvider := AMarkerProvider;
  FBitmapFactory := ABitmapFactory;
  FProjectedCache := AProjectedCache;

  FMarksSubsetCS := MakeSyncRW_Var(Self);
  FGetMarksCounter := APerfList.CreateAndAddNewCounter('GetMarks');
  FMouseOnRegCounter := APerfList.CreateAndAddNewCounter('MouseOnReg');

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
  VMarksDrawConfig: IMarksDrawConfigStatic;
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
  if (VMarksSubset <> nil) and (not VMarksSubset.IsEmpty) then begin
    VMarksDrawConfig := FConfig.MarksDrawConfig.GetStatic;
    Result :=
      TBitmapLayerProviderByMarksSubset.Create(
        VMarksDrawConfig,
        FBitmapFactory,
        FProjectedCache,
        FMarkerProvider,
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
  VMarksSubset: IVectorItemSubset;
  VMarksEnum: IEnumUnknown;
  i: Cardinal;
  VCounterContext: TInternalPerformanceCounterContext;
  VMarkLine: IVectorDataItemLine;
  VMarkPoly: IVectorDataItemPoly;
  VProjectdPath: IProjectedPath;
  VProjectdPolygon: IProjectedPolygon;
  Vtmp: IVectorItemSubsetBuilder;
begin
  Result := nil;
  Vtmp := FVectorItemSubsetBuilderFactory.Build;
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
        VConverter := AVisualConverter.GetGeoConverter;
        VZoom := AVisualConverter.GetZoom;
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
                VProjectdPath := FProjectedCache.GetProjectedPath(AVisualConverter.ProjectionInfo, VMarkLine.Line);
                if Assigned(VProjectdPath) then begin
                  if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
                    Vtmp.Add(VMark);
                  end;
                end;
              end else if Supports(VMark, IVectorDataItemPoly, VMarkPoly) then begin
                VProjectdPolygon := FProjectedCache.GetProjectedPolygon(AVisualConverter.ProjectionInfo, VMarkPoly.Line);
                if Assigned(VProjectdPolygon) then begin
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
  Result := Vtmp.MakeStaticAndClear;
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
    if AConfig.IgnoreCategoriesVisible or (Assigned(VList) and (VList.Count > 0)) then begin
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
