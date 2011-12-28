unit u_MarkPolyLineLayer;

interface

uses
  
  GR32_Image,
  i_ViewPortState,
  i_InternalPerformanceCounter,
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
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ALineOnMapEdit: ILineOnMapEdit;
      AConfig: IMarkPolyLineLayerConfig
    );
  end;

implementation

uses
  
  GR32_Polygons;

{ TMarkPolyLineLayer }

constructor TMarkPolyLineLayer.Create(
  APerfList: IInternalPerformanceCounterList;
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
  inherited Create(
    APerfList,
    AParentMap,
    AViewPortState,
    ALineOnMapEdit,
    AConfig,
    VPolygon,
    false
  );
  FConfig := AConfig;
end;

end.
