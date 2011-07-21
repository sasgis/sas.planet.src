unit u_MainFormConfig;

interface

uses
  i_MapTypes,
  i_ActiveMapsConfig,
  i_LocalCoordConverterFactorySimpe,
  i_ViewPortState,
  i_NavigationToPoint,
  i_MainFormConfig,
  i_MainFormBehaviourByGPSConfig,
  i_MainGeoCoderConfig,
  i_KeyMovingConfig,
  i_MapZoomingConfig,
  i_GeoCoderList,
  i_DownloadUIConfig,
  i_InternalPerformanceCounter,
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
    FKeyMovingConfig: IKeyMovingConfig;
    FMapZoomingConfig: IMapZoomingConfig;
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
    function GetKeyMovingConfig: IKeyMovingConfig;
    function GetMapZoomingConfig: IMapZoomingConfig;
  public
    constructor Create(
      ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      AGeoCoderList: IGeoCoderList;
      AMapsList, ALayersList: IMapTypeList;
      ADefaultMapGUID: TGUID;
      APerfCounterList: IInternalPerformanceCounterList
    );
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
  u_MapZoomingConfig,
  u_DownloadUIConfig,
  u_KeyMovingConfig,
  u_MainFormMainConfig;

{ TMainFormConfig }

constructor TMainFormConfig.Create(
  ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  AGeoCoderList: IGeoCoderList;
  AMapsList, ALayersList: IMapTypeList;
  ADefaultMapGUID: TGUID;
  APerfCounterList: IInternalPerformanceCounterList
);
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
  FViewPortState := TMapViewPortState.Create(ACoordConverterFactory, FMapZoomingConfig, FMainMapsConfig, APerfCounterList);
  Add(FViewPortState, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Position'));
  FLayersConfig := TMainFormLayersConfig.Create(FMainMapsConfig);
  Add(FLayersConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FDownloadUIConfig := TDownloadUIConfig.Create;
  Add(FDownloadUIConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ViewDownload'));
  FKeyMovingConfig := TKeyMovingConfig.Create;
  Add(FKeyMovingConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('KeyMoving'));
  FMapZoomingConfig := TMapZoomingConfig.Create;
  Add(FMapZoomingConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Zooming'));
end;

function TMainFormConfig.GetDownloadUIConfig: IDownloadUIConfig;
begin
  Result := FDownloadUIConfig;
end;

function TMainFormConfig.GetGPSBehaviour: IMainFormBehaviourByGPSConfig;
begin
  Result := FGPSBehaviour;
end;

function TMainFormConfig.GetKeyMovingConfig: IKeyMovingConfig;
begin
  Result := FKeyMovingConfig;
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

function TMainFormConfig.GetMapZoomingConfig: IMapZoomingConfig;
begin
  Result := FMapZoomingConfig;
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
