unit u_MarksLayerConfig;

interface

uses
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarksLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TMarksLayerConfig = class(TConfigDataElementComplexBase, IMarksLayerConfig)
  private
    FMarksShowConfig: IUsedMarksConfig;
    FMarksDrawConfig: IMarksDrawConfig;
  protected
    function GetMarksShowConfig: IUsedMarksConfig;
    function GetMarksDrawConfig: IMarksDrawConfig;
  public
    constructor Create();
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_UsedMarksConfig,
  u_MarksDrawConfig;

{ TMainFormLayersConfig }

constructor TMarksLayerConfig.Create;
begin
  inherited Create;

  FMarksShowConfig := TUsedMarksConfig.Create;
  Add(FMarksShowConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FMarksDrawConfig := TMarksDrawConfig.Create;
  Add(FMarksDrawConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TMarksLayerConfig.GetMarksDrawConfig: IMarksDrawConfig;
begin
  Result := FMarksDrawConfig;
end;

function TMarksLayerConfig.GetMarksShowConfig: IUsedMarksConfig;
begin
  Result := FMarksShowConfig;
end;

end.
