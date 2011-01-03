unit u_MainFormConfig;

interface

uses
  i_IConfigDataElement,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  i_INavigationToPoint,
  i_IStatBarConfig,
  i_IMapLayerGPSMarkerConfig,
  i_IMapLayerGPSTrackConfig,
  i_MainFormConfig,
  u_ConfigDataElementComplexBase;

type
  TMainFormConfig = class(TConfigDataElementComplexBase, IMainFormConfig)
  private
    FMainConfig: IMainFormMainConfig;
    FToolbarsLock: IMainWindowToolbarsLock;
    FMapLayerGridsConfig: IMapLayerGridsConfig;
    FNavToPoint: INavigationToPoint;
    FStatBar: IStatBarConfig;
    FGPSMarker: IMapLayerGPSMarkerConfig;
    FGPSTrackConfig: IMapLayerGPSTrackConfig;
  protected
    function GetMainConfig: IMainFormMainConfig;
    function GetToolbarsLock: IMainWindowToolbarsLock;
    function GetMapLayerGridsConfig: IMapLayerGridsConfig;
    function GetNavToPoint: INavigationToPoint;
    function GetStatBar: IStatBarConfig;
    function GetGPSMarker: IMapLayerGPSMarkerConfig;
    function GetGPSTrackConfig: IMapLayerGPSTrackConfig;
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_MainWindowToolbarsLock,
  u_MapLayerGridsConfig,
  u_NavigationToPoint,
  u_StatBarConfig,
  u_MapLayerGPSMarkerConfig,
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
  FNavToPoint := TNavigationToPoint.Create;
  Add(FNavToPoint, TConfigSaveLoadStrategyBasicProviderSubItem.Create('NavToPoint'));
  FStatBar := TStatBarConfig.Create;
  Add(FStatBar, TConfigSaveLoadStrategyBasicProviderSubItem.Create('StatusBar'));
  FGPSMarker := TMapLayerGPSMarkerConfig.Create;
  Add(FGPSMarker, TConfigSaveLoadStrategyBasicProviderSubItem.Create('GPSMarker'));
end;

function TMainFormConfig.GetGPSMarker: IMapLayerGPSMarkerConfig;
begin
  Result := FGPSMarker;
end;

function TMainFormConfig.GetGPSTrackConfig: IMapLayerGPSTrackConfig;
begin
  Result := FGPSTrackConfig;
end;

function TMainFormConfig.GetMainConfig: IMainFormMainConfig;
begin
  Result := FMainConfig;
end;

function TMainFormConfig.GetMapLayerGridsConfig: IMapLayerGridsConfig;
begin
  Result := FMapLayerGridsConfig;
end;

function TMainFormConfig.GetNavToPoint: INavigationToPoint;
begin
  Result := FNavToPoint;
end;

function TMainFormConfig.GetStatBar: IStatBarConfig;
begin
  Result := FStatBar;
end;

function TMainFormConfig.GetToolbarsLock: IMainWindowToolbarsLock;
begin
  Result := FToolbarsLock;
end;

end.
