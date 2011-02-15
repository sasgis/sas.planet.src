unit u_SelectionPolygonLayer;

interface

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  i_IViewPortState,
  i_ISelectionPolygonLayerConfig,
  u_PolyLineLayerBase,
  u_MapLayerBasic;

type
  TSelectionPolygonLayer = class(TPolyLineLayerBase)
  private
    FConfig: ISelectionPolygonLayerConfig;
    FFillColor: TColor32;
    FPolygonFill: TPolygon32;
  protected
    procedure DoConfigChange; override;
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: ISelectionPolygonLayerConfig
    );
  end;

implementation

uses
  SysUtils;

{ TMarkPolyLineLayer }

constructor TSelectionPolygonLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: ISelectionPolygonLayerConfig
);
begin
  FPolygonFill := TPolygon32.Create;
  FPolygonFill.Closed := True;
  FPolygonFill.Antialiased := true;
  FPolygonFill.AntialiasMode := am4times;
  inherited Create(AParentMap, AViewPortState, AConfig, FPolygonFill);
  FConfig := AConfig;
end;

procedure TSelectionPolygonLayer.DoConfigChange;
begin
  inherited;
  FFillColor := FConfig.FillColor;
end;

procedure TSelectionPolygonLayer.PaintLayer(Sender: TObject; Buffer: TBitmap32);
begin
  FPolygonFill.DrawFill(Buffer, FFillColor);
  inherited PaintLayer(Sender, Buffer);
end;

end.
