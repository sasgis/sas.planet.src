unit u_MainFormLayersConfig;

interface

uses
  i_IActiveMapsConfig,
  i_MapLayerGridsConfig,
  i_IStatBarConfig,
  i_IMapLayerGPSMarkerConfig,
  i_IMapLayerGPSTrackConfig,
  i_IMapLayerNavToPointMarkerConfig,
  i_IUsedMarksConfig,
  i_IKmlLayerConfig,
  i_IMiniMapLayerConfig,
  i_ICenterScaleConfig,
  i_IScaleLineConfig,
  i_ILastSelectionLayerConfig,
  i_ICalcLineLayerConfig,
  i_MainFormConfig,
  u_ConfigDataElementComplexBase;

type
  TMainFormLayersConfig = class(TConfigDataElementComplexBase, IMainFormLayersConfig)
  private
    FMapLayerGridsConfig: IMapLayerGridsConfig;
    FStatBar: IStatBarConfig;
    FGPSMarker: IMapLayerGPSMarkerConfig;
    FGPSTrackConfig: IMapLayerGPSTrackConfig;
    FNavToPointMarkerConfig: IMapLayerNavToPointMarkerConfig;
    FMarksShowConfig: IUsedMarksConfig;
    FKmlLayerConfig: IKmlLayerConfig;
    FMiniMapLayerConfig: IMiniMapLayerConfig;
    FCenterScaleConfig: ICenterScaleConfig;
    FScaleLineConfig: IScaleLineConfig;
    FLastSelectionLayerConfig: ILastSelectionLayerConfig;
    FCalcLineLayerConfig: ICalcLineLayerConfig;
  protected
    function GetMapLayerGridsConfig: IMapLayerGridsConfig;
    function GetStatBar: IStatBarConfig;
    function GetGPSMarker: IMapLayerGPSMarkerConfig;
    function GetGPSTrackConfig: IMapLayerGPSTrackConfig;
    function GetNavToPointMarkerConfig: IMapLayerNavToPointMarkerConfig;
    function GetMarksShowConfig: IUsedMarksConfig;
    function GetKmlLayerConfig: IKmlLayerConfig;
    function GetMiniMapLayerConfig: IMiniMapLayerConfig;
    function GetCenterScaleConfig: ICenterScaleConfig;
    function GetScaleLineConfig: IScaleLineConfig;
    function GetLastSelectionLayerConfig: ILastSelectionLayerConfig;
    function GetCalcLineLayerConfig: ICalcLineLayerConfig;
  public
    constructor Create(AMapsConfig: IMainMapsConfig);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_MapLayerGridsConfig,
  u_StatBarConfig,
  u_MapLayerGPSMarkerConfig,
  u_MapLayerGPSTrackConfig,
  u_UsedMarksConfig,
  u_KmlLayerConfig,
  u_MiniMapLayerConfig,
  u_CenterScaleConfig,
  u_ScaleLineConfig,
  u_LastSelectionLayerConfig,
  u_CalcLineLayerConfig,
  u_MapLayerNavToPointMarkerConfig;

{ TMainFormLayersConfig }

constructor TMainFormLayersConfig.Create(AMapsConfig: IMainMapsConfig);
begin
  inherited Create;
  FMapLayerGridsConfig := TMapLayerGridsConfig.Create;
  Add(FMapLayerGridsConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FStatBar := TStatBarConfig.Create;
  Add(FStatBar, TConfigSaveLoadStrategyBasicProviderSubItem.Create('StatusBar'));
  FGPSMarker := TMapLayerGPSMarkerConfig.Create;
  Add(FGPSMarker, TConfigSaveLoadStrategyBasicProviderSubItem.Create('GPSMarker'));
  FGPSTrackConfig := TMapLayerGPSTrackConfig.Create;
  Add(FGPSTrackConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('GPSTrack'));
  FNavToPointMarkerConfig := TMapLayerNavToPointMarkerConfig.Create;
  Add(FNavToPointMarkerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('NavToPointMarker'));
  FMarksShowConfig := TUsedMarksConfig.Create;
  Add(FMarksShowConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarksShow'));
  FKmlLayerConfig := TKmlLayerConfig.Create;
  Add(FKmlLayerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('WikiLayer'));
  FMiniMapLayerConfig := TMiniMapLayerConfig.Create(AMapsConfig);
  Add(FMiniMapLayerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MiniMap'));
  FCenterScaleConfig := TCenterScaleConfig.Create;
  Add(FCenterScaleConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('CenterScale'));
  FScaleLineConfig := TScaleLineConfig.Create;
  Add(FScaleLineConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ScaleLine'));
  FLastSelectionLayerConfig := TLastSelectionLayerConfig.Create;
  Add(FLastSelectionLayerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('LastSelection'));
  FCalcLineLayerConfig := TCalcLineLayerConfig.Create;
  Add(FCalcLineLayerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('CalcLine'));
end;

function TMainFormLayersConfig.GetCalcLineLayerConfig: ICalcLineLayerConfig;
begin
  Result := FCalcLineLayerConfig;
end;

function TMainFormLayersConfig.GetCenterScaleConfig: ICenterScaleConfig;
begin
  Result := FCenterScaleConfig;
end;

function TMainFormLayersConfig.GetGPSMarker: IMapLayerGPSMarkerConfig;
begin
  Result := FGPSMarker;
end;

function TMainFormLayersConfig.GetGPSTrackConfig: IMapLayerGPSTrackConfig;
begin
  Result := FGPSTrackConfig;
end;

function TMainFormLayersConfig.GetKmlLayerConfig: IKmlLayerConfig;
begin
  Result := FKmlLayerConfig;
end;

function TMainFormLayersConfig.GetLastSelectionLayerConfig: ILastSelectionLayerConfig;
begin
  Result := FLastSelectionLayerConfig;
end;

function TMainFormLayersConfig.GetMapLayerGridsConfig: IMapLayerGridsConfig;
begin
  Result := FMapLayerGridsConfig;
end;

function TMainFormLayersConfig.GetMarksShowConfig: IUsedMarksConfig;
begin
  Result := FMarksShowConfig;
end;

function TMainFormLayersConfig.GetMiniMapLayerConfig: IMiniMapLayerConfig;
begin
  Result := FMiniMapLayerConfig;
end;

function TMainFormLayersConfig.GetNavToPointMarkerConfig: IMapLayerNavToPointMarkerConfig;
begin
  Result := FNavToPointMarkerConfig;
end;

function TMainFormLayersConfig.GetScaleLineConfig: IScaleLineConfig;
begin
  Result := FScaleLineConfig;
end;

function TMainFormLayersConfig.GetStatBar: IStatBarConfig;
begin
  Result := FStatBar;
end;

end.
