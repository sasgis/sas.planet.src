unit u_SelectionLayer;

interface

uses
  Windows,
  GR32,
  GR32_Polygons,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_ViewPortState,
  i_LocalCoordConverter,
  i_DoublePointsAggregator,
  i_InternalPerformanceCounter,
  i_LastSelectionLayerConfig,
  i_LastSelectionInfo,
  i_VectorItemProjected,
  i_VectorItmesFactory,
  u_MapLayerBasic;

type
  TSelectionLayer = class(TMapLayerBasicNoBitmap)
  private
    FConfig: ILastSelectionLayerConfig;
    FLastSelectionInfo: ILastSelectionInfo;
    FVectorItmesFactory: IVectorItmesFactory;

    FLineColor: TColor32;
    FLineWidth: Integer;

    FSourceChangeCount: Integer;
    FSourceProjected: IProjectedPolygon;
    FPolygon: TPolygon32;
    FLinePolygon: TPolygon32;
    FPreparedPointsAggreagtor: IDoublePointsAggregator;

    procedure PreparePolygon(ALocalConverter: ILocalCoordConverter);
    procedure OnChangeSelection;
    procedure OnConfigChange;
  protected
    procedure PaintLayer(Buffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AVectorItmesFactory: IVectorItmesFactory;
      AConfig: ILastSelectionLayerConfig;
      ALastSelectionInfo: ILastSelectionInfo
    );
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  u_NotifyEventListener,
  i_EnumDoublePoint,
  i_ProjectionInfo,
  u_GeoFun,
  u_DoublePointsAggregator,
  u_EnumDoublePointsByArray,
  u_EnumDoublePointLonLatToMapPixel,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterFirstSegment,
  u_EnumDoublePointFilterEqual;

{ TSelectionLayer }

constructor TSelectionLayer.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AVectorItmesFactory: IVectorItmesFactory;
  AConfig: ILastSelectionLayerConfig;
  ALastSelectionInfo: ILastSelectionInfo
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  FLastSelectionInfo := ALastSelectionInfo;
  FVectorItmesFactory := AVectorItmesFactory;

  FPolygon := TPolygon32.Create;
  FPolygon.Closed := True;
  FPolygon.Antialiased := true;
  FPolygon.AntialiasMode := am4times;

  FLinePolygon := TPolygon32.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnChangeSelection),
    FLastSelectionInfo.GetChangeNotifier
  );
  FPreparedPointsAggreagtor := TDoublePointsAggregator.Create;
end;

destructor TSelectionLayer.Destroy;
begin
  FPreparedPointsAggreagtor := nil;
  FreeAndNil(FLinePolygon);
  FreeAndNil(FPolygon);
  inherited;
end;

procedure TSelectionLayer.OnConfigChange;
var
  VVisible: Boolean;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      VVisible := FConfig.Visible;
      FLineWidth := FConfig.LineWidth;
      FLineColor := FConfig.LineColor
    finally
      FConfig.UnlockRead;
    end;
    SetNeedRedraw;
    SetVisible(VVisible);
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TSelectionLayer.OnChangeSelection;
begin
  ViewUpdateLock;
  try
    InterlockedIncrement(FSourceChangeCount);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TSelectionLayer.PaintLayer(Buffer: TBitmap32; ALocalConverter: ILocalCoordConverter);
var
  VPointsCount: Integer;
  VProjection: IProjectionInfo;
begin
  VProjection := ALocalConverter.ProjectionInfo;
  if
    (InterlockedExchange(FSourceChangeCount, 0) > 0) or
    (FSourceProjected = nil) or
    (not FSourceProjected.Projection.GetIsSameProjectionInfo(VProjection))
  then begin
    FSourceProjected :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        FLastSelectionInfo.Polygon,
        FPreparedPointsAggreagtor
      );
  end;
  PreparePolygon(ViewCoordConverter);
  VPointsCount := FPreparedPointsAggreagtor.Count;
  if VPointsCount > 0 then begin
    if FLinePolygon <> nil then begin
      FLinePolygon.DrawFill(Buffer, FLineColor);
    end;
  end;
end;

procedure TSelectionLayer.PreparePolygon(ALocalConverter: ILocalCoordConverter);
var
  VPolygonOutline: TPolygon32;
  VPolygonGrow: TPolygon32;
  i: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VPointsProcessedCount: Integer;
  VLocalRect: TDoubleRect;
  VRectWithDelta: TDoubleRect;
  VEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
  VSourceProjected: IProjectedPolygon;
begin
  VSourceProjected := FSourceProjected;
  if VSourceProjected <> nil then begin
    VLocalRect := DoubleRect(ALocalConverter.GetLocalRect);
    VRectWithDelta.Left := VLocalRect.Left - 10;
    VRectWithDelta.Top := VLocalRect.Top - 10;
    VRectWithDelta.Right := VLocalRect.Right + 10;
    VRectWithDelta.Bottom := VLocalRect.Bottom + 10;

    VEnum :=
      TEnumDoublePointFilterEqual.Create(
        TEnumDoublePointClipByRect.Create(
          True,
          VRectWithDelta,
          TEnumDoublePointMapPixelToLocalPixel.Create(
            ALocalConverter,
            VSourceProjected.GetEnum
          )
        )
      );
    FPreparedPointsAggreagtor.Clear;
    while VEnum.Next(VPoint) do begin
      FPreparedPointsAggreagtor.Add(VPoint);
    end;

    FPolygon.Clear;
    FLinePolygon.Clear;
    VPointsProcessedCount := FPreparedPointsAggreagtor.Count;
    if VPointsProcessedCount > 0 then begin
      SetLength(VPathFixedPoints, VPointsProcessedCount);
      for i := 0 to VPointsProcessedCount - 1 do begin
        VPoint := FPreparedPointsAggreagtor.Points[i];
        VPathFixedPoints[i] := FixedPoint(VPoint.X, VPoint.Y);
      end;
      FPolygon.AddPoints(VPathFixedPoints[0], VPointsProcessedCount);

      VPolygonOutline := FPolygon.Outline;
      try
        VPolygonGrow := VPolygonOutline.Grow(Fixed(FLineWidth / 2), 0.5);
        try
          FLinePolygon.Assign(VPolygonGrow);
        finally
          VPolygonGrow.Free;
        end;
      finally
        VPolygonOutline.Free;
      end;
    end;
  end;
end;

procedure TSelectionLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
