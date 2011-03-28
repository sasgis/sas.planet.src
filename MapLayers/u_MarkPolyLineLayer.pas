unit u_MarkPolyLineLayer;

interface

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  i_IViewPortState,
  i_MarkPolyLineLayerConfig,
  u_PolyLineLayerBase,
  u_MapLayerBasic;

type
  TMarkPolyLineLayer = class(TPolyLineLayerBase)
  private
    FConfig: IMarkPolyLineLayerConfig;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IMarkPolyLineLayerConfig
    );
  end;

implementation

uses
  SysUtils;

{ TMarkPolyLineLayer }

constructor TMarkPolyLineLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IMarkPolyLineLayerConfig
);
var
  VPolygon: TPolygon32;
begin
  VPolygon := TPolygon32.Create;
  VPolygon.Closed := False;
  VPolygon.Antialiased := true;
  VPolygon.AntialiasMode := am4times;
  inherited Create(AParentMap, AViewPortState, AConfig, VPolygon);
  FConfig := AConfig;
end;

end.
