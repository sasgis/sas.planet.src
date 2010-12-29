unit u_MainFormConfig;

interface

uses
  i_IConfigDataElement,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  i_MainFormConfig,
  u_ConfigDataElementComplexBase;

type
  TMainFormConfig = class(TConfigDataElementComplexBase, IMainFormConfig)
  private
    FMainConfig: IMainFormMainConfig;
    FToolbarsLock: IMainWindowToolbarsLock;
    FMapLayerGridsConfig: IMapLayerGridsConfig;
  protected
    function GetMainConfig: IMainFormMainConfig;
    function GetToolbarsLock: IMainWindowToolbarsLock;
    function GetMapLayerGridsConfig: IMapLayerGridsConfig;
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_MainWindowToolbarsLock,
  u_MapLayerGridsConfig,
  u_MainFormMainConfig;

{ TMainFormConfig }

constructor TMainFormConfig.Create;
begin
  inherited;
  FMainConfig := TMainFormMainConfig.Create;
  Add(FMainConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('View'));
  FToolbarsLock := TMainWindowToolbarsLock.Create;
  Add(FToolbarsLock, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PANEL'));
  FMapLayerGridsConfig := TMapLayerGridsConfig.Create;
  Add(FMapLayerGridsConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TMainFormConfig.GetMainConfig: IMainFormMainConfig;
begin
  Result := FMainConfig;
end;

function TMainFormConfig.GetMapLayerGridsConfig: IMapLayerGridsConfig;
begin
  Result := FMapLayerGridsConfig;
end;

function TMainFormConfig.GetToolbarsLock: IMainWindowToolbarsLock;
begin
  Result := FToolbarsLock;
end;

end.
