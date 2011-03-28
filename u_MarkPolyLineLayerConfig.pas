unit u_MarkPolyLineLayerConfig;

interface
uses
  i_MarkPolyLineLayerConfig,
  u_PolyLineLayerConfig;

type
  TMarkPolyLineLayerConfig = class(TPolyLineLayerConfig, IMarkPolyLineLayerConfig)
  public
    constructor Create;
  end;

implementation

uses
  GR32;

{ TMarkPolyLineLayerConfig }

constructor TMarkPolyLineLayerConfig.Create;
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
  finally
    UnlockWrite;
  end;
end;

end.
