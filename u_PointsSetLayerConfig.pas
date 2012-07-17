unit u_PointsSetLayerConfig;

interface

uses
  i_MarkerSimpleConfig,
  i_PolyLineLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TPointsSetLayerConfig = class(TConfigDataElementComplexBase, IPointsSetLayerConfig)
  private
    FFirstPointMarker: IMarkerSimpleConfig;
    FActivePointMarker: IMarkerSimpleConfig;
    FNormalPointMarker: IMarkerSimpleConfig;
  private
    function GetFirstPointMarker: IMarkerSimpleConfig;
    function GetActivePointMarker: IMarkerSimpleConfig;
    function GetNormalPointMarker: IMarkerSimpleConfig;
  public
    constructor Create(
      const AFirstPointMarkerDefault: IMarkerSimpleConfigStatic;
      const AActivePointMarkerDefault: IMarkerSimpleConfigStatic;
      const ANormalPointMarkerDefault: IMarkerSimpleConfigStatic
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_MarkerSimpleConfig;

constructor TPointsSetLayerConfig.Create(
  const AFirstPointMarkerDefault: IMarkerSimpleConfigStatic;
  const AActivePointMarkerDefault: IMarkerSimpleConfigStatic;
  const ANormalPointMarkerDefault: IMarkerSimpleConfigStatic
);
begin
  inherited Create;
  FFirstPointMarker := TMarkerSimpleConfig.Create(AFirstPointMarkerDefault);
  Add(
    FFirstPointMarker,
    TConfigSaveLoadStrategyBasicProviderSubItem.Create('FirstPoint')
  );

  FActivePointMarker := TMarkerSimpleConfig.Create(AActivePointMarkerDefault);
  Add(
    FActivePointMarker,
    TConfigSaveLoadStrategyBasicProviderSubItem.Create('ActivePoint')
  );

  FNormalPointMarker := TMarkerSimpleConfig.Create(ANormalPointMarkerDefault);
  Add(
    FNormalPointMarker,
    TConfigSaveLoadStrategyBasicProviderSubItem.Create('NormalPoint')
  );
end;

function TPointsSetLayerConfig.GetActivePointMarker: IMarkerSimpleConfig;
begin
  Result := FActivePointMarker;
end;

function TPointsSetLayerConfig.GetFirstPointMarker: IMarkerSimpleConfig;
begin
  Result := FFirstPointMarker;
end;

function TPointsSetLayerConfig.GetNormalPointMarker: IMarkerSimpleConfig;
begin
  Result := FNormalPointMarker;
end;

end.
