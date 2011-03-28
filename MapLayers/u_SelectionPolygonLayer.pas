unit u_SelectionPolygonLayer;

interface

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  t_GeoTypes,
  i_ViewPortState,
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
    procedure DoConfigChange; override;
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: ISelectionPolygonLayerConfig
    );
    procedure DrawLine(APathLonLat: TArrayOfDoublePoint; AActiveIndex: Integer); override;
  end;

implementation

uses
  SysUtils,
  Ugeofun;

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

procedure TSelectionPolygonLayer.DrawLine(APathLonLat: TArrayOfDoublePoint;
  AActiveIndex: Integer);
var
  VPathLonLat: TArrayOfDoublePoint;
  VPointsCount: Integer;
  i: Integer;
begin
  VPointsCount := Length(APathLonLat);
  if VPointsCount > 2 then begin
    if compare2EP(APathLonLat[0], APathLonLat[VPointsCount - 1]) then begin
      VPathLonLat := APathLonLat;
    end else begin
      SetLength(VPathLonLat, VPointsCount + 1);
      for i := 0 to VPointsCount - 1 do begin
        VPathLonLat[i] := APathLonLat[i];
      end;
      VPathLonLat[VPointsCount] := VPathLonLat[0]; 
    end;
  end else begin
    VPathLonLat := APathLonLat;
  end;
  inherited DrawLine(VPathLonLat, AActiveIndex);
end;

procedure TSelectionPolygonLayer.PaintLayer(Sender: TObject; Buffer: TBitmap32);
begin
  FPolygonFill.DrawFill(Buffer, FFillColor);
  inherited PaintLayer(Sender, Buffer);
end;

end.
