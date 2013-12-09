unit u_ExportProviderAUX;

interface

uses
  Forms,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_CoordConverterFactory,
  i_VectorGeometryProjectedFactory,
  i_MapTypeGUIConfigList,
  i_GeometryLonLat,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract;

type
  TExportProviderAUX = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon); override;
  end;


implementation

uses
  Classes,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_VectorItemProjected,
  u_ThreadExportToAUX,
  u_ResStrings,
  u_MapType,
  fr_ExportAUX;

{ TExportProviderKml }

constructor TExportProviderAUX.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
end;

function TExportProviderAUX.CreateFrame: TFrame;
begin
  Result :=
    TfrExportAUX.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameOneZoom));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderAUX.GetCaption: string;
begin
  Result := SAS_STR_ExportAUXGeoServerCaption;
end;

procedure TExportProviderAUX.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VPath: string;
  VMapType: TMapType;
  VZoom: byte;
  VProjectedPolygon: IProjectedPolygon;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
begin
  inherited;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;

  VProjectedPolygon :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      FProjectionFactory.GetByConverterAndZoom(VMapType.GeoConvert, VZoom),
      APolygon
    );

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportToAUX.Create(
      VProgressInfo,
      APolygon,
      VProjectedPolygon,
      VZoom,
      VMapType.TileStorage,
      VMapType.VersionConfig.Version,
      VPath
    );
  VThread.Resume;
end;

end.


