unit u_MarkPolygonLayer;

interface

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  i_IViewPortState,
  i_IMarkPolygonLayerConfig,
  u_PolyLineLayerBase,
  u_MapLayerBasic;

type
  TMarkPolygonLayer = class(TPolyLineLayerBase)
  private
    FConfig: IMarkPolygonLayerConfig;
    FFillColor: TColor32;
    FPolygonFill: TPolygon32;
  protected
    procedure DoConfigChange; override;
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IMarkPolygonLayerConfig
    );
  end;

implementation

uses
  SysUtils;

{ TMarkPolyLineLayer }

constructor TMarkPolygonLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IMarkPolygonLayerConfig
);
begin
  FPolygonFill := TPolygon32.Create;
  FPolygonFill.Closed := True;
  FPolygonFill.Antialiased := true;
  FPolygonFill.AntialiasMode := am4times;
  inherited Create(AParentMap, AViewPortState, AConfig, FPolygonFill);
  FConfig := AConfig;
end;

procedure TMarkPolygonLayer.DoConfigChange;
begin
  inherited;
  FFillColor := FConfig.FillColor;
end;

procedure TMarkPolygonLayer.PaintLayer(Sender: TObject; Buffer: TBitmap32);
begin
  FPolygonFill.DrawFill(Buffer, FFillColor);
  inherited PaintLayer(Sender, Buffer);
end;

end.
