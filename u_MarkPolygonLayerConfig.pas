unit u_MarkPolygonLayerConfig;

interface

uses
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IMarkPolygonLayerConfig,
  u_ConfigDataElementBase,
  u_PolyLineLayerConfig;

type
  TMarkPolygonLayerConfig = class(TPolyLineLayerConfig, IMarkPolygonLayerConfig)
  private
    FFillColor: TColor32;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetFillColor: TColor32;
    procedure SetFillColor(AValue: TColor32);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TCalcLineLayerConfig }

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

procedure TMarkPolygonLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FFillColor := LoadColor32(AConfigData, 'FillColor', FFillColor);
    SetChanged;
  end;
end;

procedure TMarkPolygonLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  WriteColor32(AConfigData, 'FillColor', FFillColor);
end;

function TMarkPolygonLayerConfig.GetFillColor: TColor32;
begin
  LockRead;
  try
    Result := FFillColor;
  finally
    UnlockRead;
  end;
end;

procedure TMarkPolygonLayerConfig.SetFillColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FFillColor <> AValue then begin
      FFillColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
