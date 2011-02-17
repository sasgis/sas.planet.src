unit u_SelectionLayer;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_IViewPortState,
  i_ILastSelectionLayerConfig,
  i_ILastSelectionInfo,
  u_MapLayerBasic;

type
  TSelectionLayer = class(TMapLayerBasicFullView)
  private
    FConfig: ILastSelectionLayerConfig;
    FLastSelectionInfo: ILastSelectionInfo;
    FPolygon: TDoublePointArray;
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32);
    function LonLatArrayToVisualFloatArray(APolygon: TDoublePointArray): TDoublePointArray;
    procedure OnChangeSelection(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState; AConfig: ILastSelectionLayerConfig; ALastSelectionInfo: ILastSelectionInfo);
  end;


implementation

uses
  GR32_PolygonsEx,
  GR32_Layers,
  GR32_VectorUtils,
  i_ILocalCoordConverter,
  u_NotifyEventListener,
  u_GlobalState,
  Ugeofun;

{ TSelectionLayer }

constructor TSelectionLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: ILastSelectionLayerConfig;
  ALastSelectionInfo: ILastSelectionInfo
);
begin
  inherited Create(TPositionedLayer.Create(AParentMap.Layers), AViewPortState);
  FConfig := AConfig;
  FLastSelectionInfo := ALastSelectionInfo;
  LayerPositioned.OnPaint := PaintLayer;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnChangeSelection),
    FLastSelectionInfo.GetChangeNotifier
  );
end;

procedure TSelectionLayer.DoRedraw;
begin
  inherited;
  FPolygon := Copy(GState.LastSelectionInfo.Polygon);
end;

function TSelectionLayer.LonLatArrayToVisualFloatArray(
  APolygon: TDoublePointArray): TDoublePointArray;
var
  i: Integer;
  VPointsCount: Integer;
  VViewRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
begin
  VPointsCount := Length(APolygon);
  SetLength(Result, VPointsCount);
  VLocalConverter := ViewPortState.GetVisualCoordConverter;

  for i := 0 to VPointsCount - 1 do begin
    Result[i] := VLocalConverter.LonLat2LocalPixelFloat(APolygon[i]);
  end;
  VViewRect := DoubleRect(VLocalConverter.GetLocalRect);
end;

procedure TSelectionLayer.OnConfigChange(Sender: TObject);
begin
  if FConfig.Visible then begin
    Redraw;
    Show;
  end else begin
    Hide;
  end;
end;

procedure TSelectionLayer.OnChangeSelection(Sender: TObject);
begin
  FPolygon := GState.LastSelectionInfo.Polygon;
  LayerPositioned.Changed;
end;

procedure TSelectionLayer.PaintLayer(Sender: TObject; Buffer: TBitmap32);
var
  VVisualPolygon: TDoublePointArray;
  VFloatPoints: TArrayOfFloatPoint;
  VPointCount: Integer;
  i: Integer;
  VLineColor: TColor32;
  VLineWidth: Integer;
begin
  VPointCount := Length(FPolygon);
  if VPointCount > 0 then begin
    FConfig.LockRead;
    try
      VLineColor := FConfig.LineColor;
      VLineWidth := FConfig.LineWidth;
    finally
      FConfig.UnlockRead;
    end;
    VVisualPolygon := LonLatArrayToVisualFloatArray(FPolygon);

    SetLength(VFloatPoints, VPointCount);
    for i := 0 to VPointCount - 1 do begin
      VFloatPoints[i] := FloatPoint(VVisualPolygon[i].X, VVisualPolygon[i].Y);
    end;
    PolylineFS(Buffer, VFloatPoints, VLineColor, True, VLineWidth, jsBevel);
  end;
end;

end.
