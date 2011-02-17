unit u_PolyLineLayerBase;

interface

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  t_GeoTypes,
  i_IViewPortState,
  i_ILocalCoordConverter,
  i_IPolyLineLayerConfig,
  u_ClipPolygonByRect,
  u_MapLayerBasic;

type
  TPolyLineLayerBase = class(TMapLayerBasicFullView)
  private
    FConfig: IPolyLineLayerConfig;

    FLineColor: TColor32;
    FLineWidth: Integer;
    FPointFillColor: TColor32;
    FPointRectColor: TColor32;
    FPointFirstColor: TColor32;
    FPointActiveColor: TColor32;
    FPointSize: integer;

    FSourcePolygon: TDoublePointArray;
    FPolyActivePointIndex: integer;

    FBitmapSize: TPoint;
    FPointsOnBitmap: TDoublePointArray;
    FPolygon: TPolygon32;
    FLinePolygon: TPolygon32;


    function LonLatArrayToVisualFloatArray(ALocalConverter: ILocalCoordConverter; APolygon: TDoublePointArray): TDoublePointArray;

    procedure DrawPolyPoint(
      ABuffer: TBitmap32;
      const ABitmapSize: TPoint;
      const APosOnBitmap: TDoublePoint;
      const ASize: Integer;
      const AFillColor: TColor32;
      const ARectColor: TColor32
    );
  protected
    procedure OnConfigChange(Sender: TObject); virtual;
    procedure DoConfigChange; virtual;
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32); virtual;
    procedure PreparePolygon(ALocalConverter: ILocalCoordConverter); virtual;
    property BitmapSize: TPoint read FBitmapSize;
    property PointsOnBitmap: TDoublePointArray read FPointsOnBitmap;
    property SourcePolygon: TDoublePointArray read FSourcePolygon;
  protected
    procedure DoRedraw; override;
    procedure DoScaleChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IPolyLineLayerConfig;
      APolygon: TPolygon32
    );
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure DrawLine(APathLonLat: TDoublePointArray; AActiveIndex: Integer); virtual;
    procedure DrawNothing; virtual;
  end;

implementation

uses
  SysUtils,
  GR32_Layers,
  i_ICoordConverter,
  u_NotifyEventListener;

{ TPolyLineLayerBase }

constructor TPolyLineLayerBase.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IPolyLineLayerConfig;
  APolygon: TPolygon32
);
begin
  inherited Create(TPositionedLayer.Create(AParentMap.Layers), AViewPortState);
  FConfig := AConfig;
  FPolygon := APolygon;

  FLinePolygon := TPolygon32.Create;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

destructor TPolyLineLayerBase.Destroy;
begin
  FreeAndNil(FLinePolygon);
  FreeAndNil(FPolygon);
  inherited;
end;

procedure TPolyLineLayerBase.DoConfigChange;
begin
  FLineColor := FConfig.LineColor;
  FLineWidth := FConfig.LineWidth;
  FPointFillColor := FConfig.PointFillColor;
  FPointRectColor := FConfig.PointRectColor;
  FPointFirstColor := FConfig.PointFirstColor;
  FPointActiveColor := FConfig.PointActiveColor;
  FPointSize := FConfig.PointSize;
end;

procedure TPolyLineLayerBase.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  Redraw;
end;

procedure TPolyLineLayerBase.DoRedraw;
begin
  inherited;
  PreparePolygon(VisualCoordConverter);
  LayerPositioned.Changed;
end;

procedure TPolyLineLayerBase.DoScaleChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  Redraw;
end;

procedure TPolyLineLayerBase.DrawLine(APathLonLat: TDoublePointArray;
  AActiveIndex: Integer);
var
  VPointsCount: Integer;
begin
  FSourcePolygon := Copy(APathLonLat);
  FPolyActivePointIndex := AActiveIndex;

  VPointsCount := Length(FSourcePolygon);
  if VPointsCount > 0 then begin
    Redraw;
    Show;
  end else begin
    Hide;
  end;
end;

procedure TPolyLineLayerBase.DrawNothing;
begin
  Hide;
end;

procedure TPolyLineLayerBase.DrawPolyPoint(
  ABuffer: TBitmap32;
  const ABitmapSize: TPoint;
  const APosOnBitmap: TDoublePoint;
  const ASize: Integer;
  const AFillColor, ARectColor: TColor32
);
var
  VHalfSize: Double;
  VRect: TRect;
begin
  if
    (APosOnBitmap.x > 0) and
    (APosOnBitmap.y > 0) and
    (APosOnBitmap.x < ABitmapSize.X) and
    (APosOnBitmap.y < ABitmapSize.Y)
  then begin
    VHalfSize := ASize / 2;
    VRect.Left := Trunc(APosOnBitmap.X - VHalfSize);
    VRect.Top := Trunc(APosOnBitmap.Y - VHalfSize);
    VRect.Right := VRect.Left + ASize;
    VRect.Bottom := VRect.Top + ASize;
    ABuffer.FillRectTS(VRect, ARectColor);
    if AFillColor <> ARectColor then begin
      Inc(VRect.Left);
      Inc(VRect.Top);
      Dec(VRect.Right);
      Dec(VRect.Bottom);
      ABuffer.FillRectS(VRect, AFillColor);
    end;
  end;
end;

function TPolyLineLayerBase.LonLatArrayToVisualFloatArray(
  ALocalConverter: ILocalCoordConverter;
  APolygon: TDoublePointArray
): TDoublePointArray;
var
  i: Integer;
  VPointsCount: Integer;
  VLonLat: TDoublePoint;
  VGeoConvert: ICoordConverter;
begin
  VPointsCount := Length(APolygon);
  SetLength(Result, VPointsCount);

  VGeoConvert := ALocalConverter.GetGeoConverter;
  for i := 0 to VPointsCount - 1 do begin
    VLonLat := APolygon[i];
    VGeoConvert.CheckLonLatPos(VLonLat);
    Result[i] := ALocalConverter.LonLat2LocalPixelFloat(VLonLat);
  end;
end;

procedure TPolyLineLayerBase.OnConfigChange(Sender: TObject);
begin
  FConfig.LockRead;
  try
    DoConfigChange;
  finally
    FConfig.UnlockRead;
  end;
  Redraw;
end;

procedure TPolyLineLayerBase.PaintLayer(Sender: TObject; Buffer: TBitmap32);
var
  VIndex: integer;
  VPosOnBitmap: TDoublePoint;
  VPointsCount: Integer;
begin
  VPointsCount := Length(FSourcePolygon);
  if VPointsCount > 0 then begin
    if FLinePolygon <> nil then begin
      FLinePolygon.DrawFill(Buffer, FLineColor);
    end;

    for VIndex := 1 to VPointsCount - 2 do begin
      VPosOnBitmap := FPointsOnBitmap[VIndex];
      DrawPolyPoint(Buffer, FBitmapSize, VPosOnBitmap, FPointSize, FPointFillColor, FPointRectColor);
    end;
    DrawPolyPoint(Buffer, FBitmapSize, FPointsOnBitmap[VPointsCount - 1], FPointSize, FPointFillColor, FPointRectColor);
    DrawPolyPoint(Buffer, FBitmapSize, FPointsOnBitmap[0], FPointSize, FPointFirstColor, FPointFirstColor);
    DrawPolyPoint(Buffer, FBitmapSize, FPointsOnBitmap[FPolyActivePointIndex], FPointSize, FPointActiveColor, FPointActiveColor);
  end;
end;

procedure TPolyLineLayerBase.PreparePolygon(ALocalConverter: ILocalCoordConverter);
var
  VPointsCount: Integer;
  VPolygonOutline: TPolygon32;
  VPolygonGrow: TPolygon32;
  i: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VBitmapClip: IPolyClip;
  VPointsProcessedCount: Integer;
  VPointsOnBitmapPrepared: TDoublePointArray;
begin
  VPointsCount := Length(FSourcePolygon);
  if VPointsCount > 0 then begin
    VBitmapClip := TPolyClipByRect.Create(ALocalConverter.GetLocalRect);

    FPointsOnBitmap := LonLatArrayToVisualFloatArray(ALocalConverter, FSourcePolygon);
    FBitmapSize := ALocalConverter.GetLocalRectSize;

    VPointsProcessedCount := VBitmapClip.Clip(FPointsOnBitmap, VPointsCount, VPointsOnBitmapPrepared);
    if VPointsProcessedCount > 0 then begin
      SetLength(VPathFixedPoints, VPointsProcessedCount);
      for i := 0 to VPointsProcessedCount - 1 do begin
        VPathFixedPoints[i] := FixedPoint(VPointsOnBitmapPrepared[i].X, VPointsOnBitmapPrepared[i].Y);
      end;

      FPolygon.Clear;
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

procedure TPolyLineLayerBase.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
  LayerPositioned.OnPaint := PaintLayer;
end;

end.
