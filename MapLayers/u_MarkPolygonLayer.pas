unit u_MarkPolygonLayer;

interface

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_LineOnMapEdit,
  i_MarkPolygonLayerConfig,
  u_PolyLineLayerBase,
  u_MapLayerBasic;

type
  TMarkPolygonLayer = class(TPolyLineLayerBase)
  private
    FConfig: IMarkPolygonLayerConfig;
    FFillColor: TColor32;
    FPolygonFill: TPolygon32;
  protected
    procedure SetSourcePolygon(const Value: TArrayOfDoublePoint); override;
    procedure DoConfigChange; override;
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ALineOnMapEdit: ILineOnMapEdit;
      AConfig: IMarkPolygonLayerConfig
    );
  end;

implementation

uses
  SysUtils,
  u_GeoFun;

{ TMarkPolyLineLayer }

constructor TMarkPolygonLayer.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ALineOnMapEdit: ILineOnMapEdit;
  AConfig: IMarkPolygonLayerConfig
);
begin
  FPolygonFill := TPolygon32.Create;
  FPolygonFill.Closed := True;
  FPolygonFill.Antialiased := true;
  FPolygonFill.AntialiasMode := am4times;
  inherited Create(APerfList, AParentMap, AViewPortState, ALineOnMapEdit, AConfig, FPolygonFill, true);
  FConfig := AConfig;
end;

procedure TMarkPolygonLayer.DoConfigChange;
begin
  inherited;
  FFillColor := FConfig.FillColor;
end;

procedure TMarkPolygonLayer.PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter);
begin
  FPolygonFill.DrawFill(ABuffer, FFillColor);
  inherited;
end;

procedure TMarkPolygonLayer.SetSourcePolygon(const Value: TArrayOfDoublePoint);
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
