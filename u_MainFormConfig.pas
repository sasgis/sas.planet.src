unit u_MainFormConfig;

interface

uses
  i_MapTypes,
  i_ActiveMapsConfig,
  i_IViewPortState,
  i_NavigationToPoint,
  i_MainFormConfig,
  i_MainFormBehaviourByGPSConfig,
  i_MainGeoCoderConfig,
  i_GeoCoderList,
  i_DownloadUIConfig,
  u_ConfigDataElementComplexBase;

type
  TMainFormConfig = class(TConfigDataElementComplexBase, IMainFormConfig)
  private
    FMainConfig: IMainFormMainConfig;
    FLayersConfig: IMainFormLayersConfig;
    FToolbarsLock: IMainWindowToolbarsLock;
    FNavToPoint: INavigationToPoint;
    FGPSBehaviour: IMainFormBehaviourByGPSConfig;
    FMainGeoCoderConfig: IMainGeoCoderConfig;
    FMainMapsConfig: IMainMapsConfig;
    FViewPortState: IViewPortState;
    FDownloadUIConfig: IDownloadUIConfig;
  protected
    function GetMainConfig: IMainFormMainConfig;
    function GetLayersConfig: IMainFormLayersConfig;
    function GetToolbarsLock: IMainWindowToolbarsLock;
    function GetNavToPoint: INavigationToPoint;
    function GetGPSBehaviour: IMainFormBehaviourByGPSConfig;
    function GetMainGeoCoderConfig: IMainGeoCoderConfig;
    function GetMainMapsConfig: IMainMapsConfig;
    function GetViewPortState: IViewPortState;
    function GetDownloadUIConfig: IDownloadUIConfig;
  public
    constructor Create(AGeoCoderList: IGeoCoderList; AMapsList, ALayersList: IMapTypeList; ADefaultMapGUID: TGUID);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_MainMapsConfig,
  u_MapViewPortStateNew,
  u_MainWindowToolbarsLock,
  u_NavigationToPoint,
  u_MainFormLayersConfig,
  u_MainFormBehaviourByGPSConfig,
  u_MainGeoCoderConfig,
  u_DownloadUIConfig,
  u_MainFormMainConfig;

{ TMainFormConfig }

constructor TMainFormConfig.Create(AGeoCoderList: IGeoCoderList; AMapsList, ALayersList: IMapTypeList; ADefaultMapGUID: TGUID);
begin
  inherited Create;
  FMainConfig := TMainFormMainConfig.Create;
  Add(FMainConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('View'));
  FToolbarsLock := TMainWindowToolbarsLock.Create;
  Add(FToolbarsLock, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PANEL'));
  FNavToPoint := TNavigationToPoint.Create;
  Add(FNavToPoint, TConfigSaveLoadStrategyBasicProviderSubItem.Create('NavToPoint'));
  FGPSBehaviour := TMainFormBehaviourByGPSConfig.Create;
  Add(FGPSBehaviour, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MainFormGPSEvents'));
  FMainGeoCoderConfig := TMainGeoCoderConfig.Create(AGeoCoderList);
  Add(FMainGeoCoderConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('View'));
  FMainMapsConfig := TMainMapsConfig.Create(AMapsList, ALayersList, ADefaultMapGUID);
  Add(FMainMapsConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Maps'));
  FViewPortState := TMapViewPortStateNew.Create(FMainMapsConfig);
  Add(FViewPortState, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Position'));
  FLayersConfig := TMainFormLayersConfig.Create(FMainMapsConfig);
  Add(FLayersConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FDownloadUIConfig := TDownloadUIConfig.Create;
  Add(FDownloadUIConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ViewDownload'));
end;

function TMainFormConfig.GetDownloadUIConfig: IDownloadUIConfig;
begin
  Result := FDownloadUIConfig;
end;

function TMainFormConfig.GetGPSBehaviour: IMainFormBehaviourByGPSConfig;
begin
  Result := FGPSBehaviour;
end;

function TMainFormConfig.GetLayersConfig: IMainFormLayersConfig;
begin
  Result := FLayersConfig;
end;

function TMainFormConfig.GetMainConfig: IMainFormMainConfig;
begin
  Result := FMainConfig;
end;

function TMainFormConfig.GetMainGeoCoderConfig: IMainGeoCoderConfig;
begin
  Result := FMainGeoCoderConfig;
end;

function TMainFormConfig.GetMainMapsConfig: IMainMapsConfig;
begin
  Result := FMainMapsConfig;
end;

function TMainFormConfig.GetNavToPoint: INavigationToPoint;
begin
  Result := FNavToPoint;
end;

function TMainFormConfig.GetToolbarsLock: IMainWindowToolbarsLock;
begin
  Result := FToolbarsLock;
end;

function TMainFormConfig.GetViewPortState: IViewPortState;
begin
  Result := FViewPortState;
end;

end.
