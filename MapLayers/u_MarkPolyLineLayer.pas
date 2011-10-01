unit u_MarkPolyLineLayer;

interface

uses
  Types,
  GR32_Image,
  i_ViewPortState,
  i_LineOnMapEdit,
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
      ALineOnMapEdit: ILineOnMapEdit;
      AConfig: IMarkPolyLineLayerConfig
    );
  end;

implementation

uses
  SysUtils,
  GR32_Polygons;

{ TMarkPolyLineLayer }

constructor TMarkPolyLineLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ALineOnMapEdit: ILineOnMapEdit;
  AConfig: IMarkPolyLineLayerConfig
);
var
  VPolygon: TPolygon32;
begin
  VPolygon := TPolygon32.Create;
  VPolygon.Closed := False;
  VPolygon.Antialiased := true;
  VPolygon.AntialiasMode := am4times;
  inherited Create(AParentMap, AViewPortState, ALineOnMapEdit, AConfig, VPolygon, false);
  FConfig := AConfig;
end;

end.
