unit u_MainMapLayerConfig;

interface

uses
  i_ThreadConfig,
  i_MainMapLayerConfig,
  i_UseTilePrevZoomConfig,
  u_ConfigDataElementComplexBase;

type
  TMainMapLayerConfig = class(TConfigDataElementComplexBase, IMainMapLayerConfig)
  private
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FThreadConfig: IThreadConfig;
  private
    function GetUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    function GetThreadConfig: IThreadConfig;
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_UseTilePrevZoomConfig,
  u_ThreadConfig;

{ TMainMapLayerConfig }

constructor TMainMapLayerConfig.Create;
begin
  inherited Create;

  FUseTilePrevZoomConfig := TUseTilePrevZoomConfig.Create;
  Add(FUseTilePrevZoomConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FThreadConfig := TThreadConfig.Create(tpNormal);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TMainMapLayerConfig.GetThreadConfig: IThreadConfig;
begin
  Result := FThreadConfig;
end;

function TMainMapLayerConfig.GetUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
begin
  Result := FUseTilePrevZoomConfig;
end;

end.
