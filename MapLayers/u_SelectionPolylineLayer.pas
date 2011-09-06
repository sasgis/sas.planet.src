unit u_SelectionPolylineLayer;

interface

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_ViewPortState,
  i_LineOnMapEdit,
  i_SelectionPolylineLayerConfig,
  u_PolyLineLayerBase,
  u_ClipPolygonByRect,
  u_MapLayerBasic;

type
  TSelectionPolylineLayer = class(TPolyLineLayerBase)
  private
    FConfig: ISelectionPolylineLayerConfig;
    FFillColor: TColor32;
    FPolygonFill: TPolygon32;
    FShadowPointsOnBitmap: TArrayOfDoublePoint;
    FShadowPolygon: TPolygon32;
    FShadowLinePolygon: TPolygon32;
  protected
    procedure SetSourcePolygon(const Value: TArrayOfDoublePoint); override;
    procedure DoConfigChange; override;
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
    procedure PrepareShadowPolygon(ALocalConverter: ILocalCoordConverter);
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ALineOnMapEdit: ILineOnMapEdit;
      AConfig: ISelectionPolylineLayerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener,
  u_GeoFun;

{ TMarkPolyLineLayer }

constructor TSelectionPolylineLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ALineOnMapEdit: ILineOnMapEdit;
  AConfig: ISelectionPolylineLayerConfig
);
begin
  FPolygonFill := TPolygon32.Create;
  FPolygonFill.Closed := false;
  FPolygonFill.Antialiased := true;
  FPolygonFill.AntialiasMode := am4times;

  FShadowPolygon := TPolygon32.Create;
  FShadowPolygon.Closed := True;
  FShadowPolygon.Antialiased := true;
  FShadowPolygon.AntialiasMode := am4times;

  FShadowLinePolygon := TPolygon32.Create;

  inherited Create(AParentMap, AViewPortState, ALineOnMapEdit, AConfig, FPolygonFill);
  FConfig := AConfig;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

destructor TSelectionPolylineLayer.Destroy;
begin
  FreeAndNil(FShadowPolygon);
  FreeAndNil(FShadowLinePolygon);
  inherited;
end;

procedure TSelectionPolylineLayer.DoConfigChange;
begin
  inherited;
  ViewUpdateLock;
  try
    SetNeedRedraw;
    SetNeedUpdateLocation;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TSelectionPolylineLayer.PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter);
var
  VPosOnBitmap: TDoublePoint;
  VPointsCount: Integer;
begin
  inherited;
  PrepareShadowPolygon(ALocalConverter);
  VPointsCount := Length(FShadowPointsOnBitmap);
  if VPointsCount > 0 then begin
    if FShadowLinePolygon <> nil then begin
      FShadowLinePolygon.DrawFill(ABuffer, FConfig.GetShadowPolygonColor);
    end;
  end;
end;

procedure TSelectionPolylineLayer.PrepareShadowPolygon(ALocalConverter: ILocalCoordConverter);
var
  VPointsCount: Integer;
  VPolygonOutline: TPolygon32;
  VPolygonGrow: TPolygon32;
  i: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VBitmapClip: IPolygonClip;
  VPointsProcessedCount: Integer;
  VPointsOnBitmapPrepared: TArrayOfDoublePoint;
  VIndex: Integer;
  VLocalRect: TRect;
  VSourcePolygon: TArrayOfDoublePoint;
begin
  VSourcePolygon := u_GeoFun.ConveryPolyline2Polygon(SourcePolygon, FConfig.GetRadius, ALocalConverter);
  VPointsCount := Length(VSourcePolygon);
  if VPointsCount > 0 then begin
    VLocalRect := ALocalConverter.GetLocalRect;
    Dec(VLocalRect.Left, 10);
    Dec(VLocalRect.Top, 10);
    Inc(VLocalRect.Right, 10);
    Inc(VLocalRect.Bottom, 10);
    VBitmapClip := TPolygonClipByRect.Create(VLocalRect);
    FShadowPointsOnBitmap := ALocalConverter.LonLatArrayToVisualFloatArray(VSourcePolygon);

    VPointsProcessedCount := VBitmapClip.Clip(FShadowPointsOnBitmap[0], VPointsCount, VPointsOnBitmapPrepared);
    if VPointsProcessedCount > 0 then begin
      SetLength(VPathFixedPoints, VPointsProcessedCount);
      VIndex := 0;
      FShadowPolygon.Clear;
      for i := 0 to VPointsProcessedCount - 1 do begin
        if PointIsEmpty(VPointsOnBitmapPrepared[i]) then begin
          FShadowPolygon.AddPoints(VPathFixedPoints[0], VIndex);
          FShadowPolygon.NewLine;
          VIndex := 0;
        end else begin
          VPathFixedPoints[VIndex] := FixedPoint(VPointsOnBitmapPrepared[i].X, VPointsOnBitmapPrepared[i].Y);
          Inc(VIndex);
        end;
      end;
      FShadowPolygon.AddPoints(VPathFixedPoints[0], VIndex);

      VPolygonOutline := FShadowPolygon.Outline;
      try
        VPolygonGrow := VPolygonOutline.Grow(Fixed(1 / 2), 0.5);
        try
          FShadowLinePolygon.Assign(VPolygonGrow);
        finally
          VPolygonGrow.Free;
        end;
      finally
        VPolygonOutline.Free;
      end;
    end;
  end;
end;

procedure TSelectionPolylineLayer.SetSourcePolygon(
  const Value: TArrayOfDoublePoint);
var
  VPathLonLat: TArrayOfDoublePoint;
  VPointsCount: Integer;
  i: Integer;
begin
  VPointsCount := Length(Value);
  if VPointsCount > 2 then begin
    if DoublePointsEqual(Value[0], Value[VPointsCount - 1]) then begin
      VPathLonLat := Value;
    end else begin
      SetLength(VPathLonLat, VPointsCount + 1);
      for i := 0 to VPointsCount - 1 do begin
        VPathLonLat[i] := Value[i];
      end;
      VPathLonLat[VPointsCount] := VPathLonLat[0];
    end;
  end else begin
    VPathLonLat := Value;
  end;
  inherited SetSourcePolygon(VPathLonLat);
end;

end.
