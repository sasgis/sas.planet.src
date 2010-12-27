unit u_SelectionLayer;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_ClipPolygonByRect,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TSelectionLayer = class(TMapLayerBasicFullView)
  protected
    FBitmapClip: IPolyClip;
    FLineColor: TColor32;
    FLineWidth: Integer;
    FPolygon: TDoublePointArray;
    FSelectionChangeListener: IJclListener;
    procedure DoRedraw; override;
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32);
    function LonLatArrayToVisualFloatArray(APolygon: TDoublePointArray): TDoublePointArray;
    procedure ChangeSelection(Sender: TObject);
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
  end;


implementation

uses
  Classes,
  Graphics,
  GR32_PolygonsEx,
  GR32_Layers,
  GR32_VectorUtils,
  u_ConfigProviderHelpers,
  u_NotifyEventListener,
  u_GlobalState,
  Ugeofun;

{ TSelectionLayer }

procedure TSelectionLayer.ChangeSelection(Sender: TObject);
begin
  FPolygon := GState.LastSelectionInfo.Polygon;
  LayerPositioned.Changed;
end;

constructor TSelectionLayer.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited Create(TPositionedLayer.Create(AParentMap.Layers), AViewPortState);
  FLineColor := SetAlpha(Color32(clBlack), 210);
  FLineWidth := 2;
  FBitmapClip := TPolyClipByRect.Create(MakeRect(-1000, -1000, 10000, 10000));
  LayerPositioned.OnPaint := PaintLayer;
  FSelectionChangeListener := TNotifyEventListener.Create(ChangeSelection);
  GState.LastSelectionInfo.ChangeNotifier.Add(FSelectionChangeListener);
end;

destructor TSelectionLayer.Destroy;
begin
  GState.LastSelectionInfo.ChangeNotifier.Remove(FSelectionChangeListener);
  FSelectionChangeListener := nil;
  FBitmapClip := nil;
  inherited;
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
begin
  VPointsCount := Length(APolygon);
  SetLength(Result, VPointsCount);
  FViewPortState.LockRead;
  try
    for i := 0 to VPointsCount - 1 do begin
      Result[i] := FViewPortState.LonLat2VisiblePixel(APolygon[i]);
    end;
    VViewRect := DoubleRect(FViewPortState.GetViewRectInVisualPixel);
  finally
    FViewPortState.UnLockRead;
  end;
end;

procedure TSelectionLayer.PaintLayer(Sender: TObject; Buffer: TBitmap32);
var
  VVisualPolygon: TDoublePointArray;
  VFloatPoints: TArrayOfFloatPoint;
  VPointCount: Integer;
  i: Integer;
begin
  VPointCount := Length(FPolygon);
  if VPointCount > 0 then begin
    VVisualPolygon := LonLatArrayToVisualFloatArray(FPolygon);

    SetLength(VFloatPoints, VPointCount);
    for i := 0 to VPointCount - 1 do begin
      VFloatPoints[i] := FloatPoint(VVisualPolygon[i].X, VVisualPolygon[i].Y);
    end;
    PolylineFS(Buffer, VFloatPoints, FLineColor, True, FLineWidth, jsBevel);
  end;
end;

procedure TSelectionLayer.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    VConfigProvider := VConfigProvider.GetSubItem('LastSelection');
    if VConfigProvider <> nil then begin
      FLineColor := LoadColor32(VConfigProvider, 'LineColor', FLineColor);
      FLineWidth := VConfigProvider.ReadInteger('LineWidth', FLineWidth);
      Visible := VConfigProvider.ReadBool('Visible',false);
    end;
  end;
end;

procedure TSelectionLayer.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('VIEW');
  VConfigProvider := VConfigProvider.GetOrCreateSubItem('LastSelection');
  VConfigProvider.WriteBool('Visible', Visible);
  WriteColor32(VConfigProvider, 'LineColor', FLineColor);
  VConfigProvider.WriteInteger('LineWidth', FLineWidth);
end;

end.
