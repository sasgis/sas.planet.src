unit u_SelectionPolygonLayer;

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
  i_SelectionPolygonLayerConfig,
  u_PolyLineLayerBase,
  u_MapLayerBasic;

type
  TSelectionPolygonLayer = class(TPolyLineLayerBase)
  private
    FConfig: ISelectionPolygonLayerConfig;
    FFillColor: TColor32;
    FPolygonFill: TPolygon32;
  protected
    procedure SetSourcePolygon(const Value: TArrayOfDoublePoint); override;
    procedure DoConfigChange; override;
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ALineOnMapEdit: ILineOnMapEdit;
      AConfig: ISelectionPolygonLayerConfig
    );
  end;

implementation

uses
  SysUtils,
  u_GeoFun;

{ TMarkPolyLineLayer }

constructor TSelectionPolygonLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ALineOnMapEdit: ILineOnMapEdit;
  AConfig: ISelectionPolygonLayerConfig
);
begin
  FPolygonFill := TPolygon32.Create;
  FPolygonFill.Closed := True;
  FPolygonFill.Antialiased := true;
  FPolygonFill.AntialiasMode := am4times;
  inherited Create(AParentMap, AViewPortState, ALineOnMapEdit, AConfig, FPolygonFill);
  FConfig := AConfig;
end;

procedure TSelectionPolygonLayer.DoConfigChange;
begin
  inherited;
  FFillColor := FConfig.FillColor;
end;

procedure TSelectionPolygonLayer.PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter);
begin
  FPolygonFill.DrawFill(ABuffer, FFillColor);
  inherited;
end;

procedure TSelectionPolygonLayer.SetSourcePolygon(
  const Value: TArrayOfDoublePoint);
var
  VPathLonLat: TArrayOfDoublePoint;
  VPointsCount: Integer;
  i: Integer;
begin
  VPointsCount := Length(Value);
  if VPointsCount > 2 then begin
    if DoublePoitnsEqual(Value[0], Value[VPointsCount - 1]) then begin
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
