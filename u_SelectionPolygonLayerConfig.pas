unit u_SelectionPolygonLayerConfig;

interface

uses
  i_SelectionPolygonLayerConfig,
  u_PolygonLayerConfig;

type
  TSelectionPolygonLayerConfig = class(TPolygonLayerConfig, ISelectionPolygonLayerConfig)
  public
    constructor Create;
  end;

implementation

uses
  GR32;

{ TSelectionPolygonLayerConfig }

constructor TSelectionPolygonLayerConfig.Create;
begin
  inherited;
  LockWrite;
  try
    SetLineColor(SetAlpha(clBlue32, 180));

    SetPointFillColor(SetAlpha(clYellow32, 150));
    SetPointRectColor(SetAlpha(ClRed32, 150));
    SetPointFirstColor(SetAlpha(ClGreen32, 255));
    SetPointActiveColor(SetAlpha(ClRed32, 255));

    SetFillColor(SetAlpha(clWhite32, 40));
  finally
    UnlockWrite;
  end;
end;

end.
