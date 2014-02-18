unit u_ExportProviderGEKml;

interface

uses
  Forms,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_ExportGEKml;

type
  TExportProviderGEKml = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon); override;
  end;


implementation

uses
  Types,
  Classes,
  SysUtils,
  i_MapTypes,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadExportKML,
  u_ResStrings;

{ TExportProviderKml }

constructor TExportProviderGEKml.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
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

function TExportProviderGEKml.CreateFrame: TFrame;
begin
  Result :=
    TfrExportGEKml.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameKmlExport));
end;

function TExportProviderGEKml.GetCaption: string;
begin
  Result := SAS_STR_ExportGEKmlExportCaption;
end;

procedure TExportProviderGEKml.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VMapType: IMapType;
  NotSaveNotExists: boolean;
  RelativePath: Boolean;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  RelativePath := (ParamsFrame as IRegionProcessParamsFrameKmlExport).RelativePath;
  NotSaveNotExists := (ParamsFrame as IRegionProcessParamsFrameKmlExport).NotSaveNotExists;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportKML.Create(
      VProgressInfo,
      VPath,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      APolygon,
      VZoomArr,
      VMapType.TileStorage,
      VMapType.VersionRequestConfig.GetStatic.BaseVersion,
      NotSaveNotExists,
      RelativePath
    );
  VThread.Resume;
end;

end.


