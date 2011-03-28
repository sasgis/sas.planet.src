unit u_MarkPolygonLayerConfig;

interface

uses
  i_MarkPolygonLayerConfig,
  u_PolygonLayerConfig;

type
  TMarkPolygonLayerConfig = class(TPolygonLayerConfig, IMarkPolygonLayerConfig)
  public
    constructor Create;
  end;

implementation

uses
  GR32;

{ TMarkPolygonLayerConfig }

constructor TMarkPolygonLayerConfig.Create;
begin
  inherited;
  LockWrite;
  try
    SetLineColor(SetAlpha(ClRed32, 150));
    SetLineWidth(3);

    SetPointFillColor(SetAlpha(clYellow32, 150));
    SetPointRectColor(SetAlpha(ClRed32, 150));
    SetPointFirstColor(SetAlpha(ClGreen32, 255));
    SetPointActiveColor(SetAlpha(ClRed32, 255));
    SetPointSize(8);

    SetFillColor(SetAlpha(ClWhite32, 50));
  finally
    UnlockWrite;
  end;
end;

end.
