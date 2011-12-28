unit u_SelectionLayer;

interface

uses
  
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
  u_MapLayerBasic;

type
  TSelectionLayer = class(TMapLayerBasicNoBitmap)
  private
    FConfig: ILastSelectionLayerConfig;
    FLastSelectionInfo: ILastSelectionInfo;

    FLineColor: TColor32;
    FLineWidth: Integer;

    FSourcePolygon: TArrayOfDoublePoint;
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
  u_GeoFun,
  u_DoublePointsAggregator,
  u_EnumDoublePointsByArray,
  u_EnumDoublePointLonLatToMapPixel,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterFirstPoly,
  u_EnumDoublePointFilterEqual;

{ TSelectionLayer }

constructor TSelectionLayer.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: ILastSelectionLayerConfig;
  ALastSelectionInfo: ILastSelectionInfo
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  FLastSelectionInfo := ALastSelectionInfo;

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
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TSelectionLayer.PaintLayer(Buffer: TBitmap32; ALocalConverter: ILocalCoordConverter);
var
  VPointsCount: Integer;
begin
  FSourcePolygon := Copy(FLastSelectionInfo.Polygon);
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
  VPointsCount: Integer;
  VPolygonOutline: TPolygon32;
  VPolygonGrow: TPolygon32;
  i: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VPointsProcessedCount: Integer;
  VLocalRect: TDoubleRect;
  VRectWithDelta: TDoubleRect;
  VEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  VPointsCount := Length(FSourcePolygon);
  if VPointsCount > 0 then begin
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
            TEnumDoublePointFilterEqual.Create(
              TEnumDoublePointLonLatToMapPixel.Create(
                ALocalConverter.GetZoom,
                ALocalConverter.GetGeoConverter,
                TEnumDoublePointFilterFirstPoly.Create(
                  TEnumDoublePointsByArray.Create(
                    @(FSourcePolygon[0]),
                    VPointsCount
                  )
                )
              )
            )
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
