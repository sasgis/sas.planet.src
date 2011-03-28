unit u_MainFormLayersConfig;

interface

uses
  i_ActiveMapsConfig,
  i_MapLayerGridsConfig,
  i_IStatBarConfig,
  i_MapLayerGPSMarkerConfig,
  i_MapLayerGPSTrackConfig,
  i_MapLayerNavToPointMarkerConfig,
  i_IUsedMarksConfig,
  i_KmlLayerConfig,
  i_IMiniMapLayerConfig,
  i_CenterScaleConfig,
  i_IScaleLineConfig,
  i_LastSelectionLayerConfig,
  i_CalcLineLayerConfig,
  i_ISelectionRectLayerConfig,
  i_ISelectionPolygonLayerConfig,
  i_MarkPolygonLayerConfig,
  i_MarkPolyLineLayerConfig,
  i_FillingMapLayerConfig,
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
    FSelectionRectLayerConfig: ISelectionRectLayerConfig;
    FSelectionPolygonLayerConfig: ISelectionPolygonLayerConfig;
    FMarkPolygonLayerConfig: IMarkPolygonLayerConfig;
    FMarkPolyLineLayerConfig: IMarkPolyLineLayerConfig;
    FFillingMapLayerConfig: IFillingMapLayerConfig;
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
    function GetSelectionRectLayerConfig: ISelectionRectLayerConfig;
    function GetSelectionPolygonLayerConfig: ISelectionPolygonLayerConfig;
    function GetMarkPolygonLayerConfig: IMarkPolygonLayerConfig;
    function GetMarkPolyLineLayerConfig: IMarkPolyLineLayerConfig;
    function GetFillingMapLayerConfig: IFillingMapLayerConfig;
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
  u_SelectionRectLayerConfig,
  u_SelectionPolygonLayerConfig,
  u_MarkPolygonLayerConfig,
  u_MarkPolyLineLayerConfig,
  u_FillingMapLayerConfig,
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
  FSelectionRectLayerConfig := TSelectionRectLayerConfig.Create;
  Add(FSelectionRectLayerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('SelectionRect'));
  FSelectionPolygonLayerConfig := TSelectionPolygonLayerConfig.Create;
  Add(FSelectionPolygonLayerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('SelectionPolygon'));
  FMarkPolygonLayerConfig := TMarkPolygonLayerConfig.Create;
  Add(FMarkPolygonLayerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('EditMarkPolygon'));
  FMarkPolyLineLayerConfig := TMarkPolyLineLayerConfig.Create;
  Add(FMarkPolyLineLayerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('EditMarkPolyLine'));
  FFillingMapLayerConfig := TFillingMapLayerConfig.Create(AMapsConfig);
  Add(FFillingMapLayerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('FillingLayer'));
end;

function TMainFormLayersConfig.GetCalcLineLayerConfig: ICalcLineLayerConfig;
begin
  Result := FCalcLineLayerConfig;
end;

function TMainFormLayersConfig.GetCenterScaleConfig: ICenterScaleConfig;
begin
  Result := FCenterScaleConfig;
end;

function TMainFormLayersConfig.GetFillingMapLayerConfig: IFillingMapLayerConfig;
begin
  Result := FFillingMapLayerConfig;
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

function TMainFormLayersConfig.GetMarkPolygonLayerConfig: IMarkPolygonLayerConfig;
begin
  Result := FMarkPolygonLayerConfig;
end;

function TMainFormLayersConfig.GetMarkPolyLineLayerConfig: IMarkPolyLineLayerConfig;
begin
  Result := FMarkPolyLineLayerConfig;
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

function TMainFormLayersConfig.GetSelectionPolygonLayerConfig: ISelectionPolygonLayerConfig;
begin
  Result := FSelectionPolygonLayerConfig;
end;

function TMainFormLayersConfig.GetSelectionRectLayerConfig: ISelectionRectLayerConfig;
begin
  Result := FSelectionRectLayerConfig;
end;

function TMainFormLayersConfig.GetStatBar: IStatBarConfig;
begin
  Result := FStatBar;
end;

end.
