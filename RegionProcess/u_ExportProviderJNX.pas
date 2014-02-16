unit u_ExportProviderJNX;

interface

uses
  Forms,
  i_GeometryLonLat,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_BitmapTileSaveLoadFactory,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_ExportToJNX;

type
  TExportProviderJNX = class(TExportProviderAbstract)
  private
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon); override;
  end;

implementation

uses
  Types,
  Classes,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ExportToJnxTask,
  u_ThreadExportToJNX,
  u_ResStrings;


{ TExportProviderJNX }

constructor TExportProviderJNX.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ACoordConverterFactory: ICoordConverterFactory
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
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FCoordConverterFactory := ACoordConverterFactory;
end;

function TExportProviderJNX.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToJNX.Create(
      Self.LanguageManager,
      FBitmapTileSaveLoadFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'JNX |*.jnx',
      'jnx'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToJNX));
end;

function TExportProviderJNX.GetCaption: string;
begin
  Result := SAS_STR_ExportJNXPackCaption;
end;

procedure TExportProviderJNX.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VPath: string;
  VProductName: string;
  VMapName: string;
  VJNXVersion: integer;
  VZorder: integer;
  VProductID: integer;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VTasks: TExportTaskJnxArray;
  VThread: TThread;
begin
  inherited;

  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VProductName := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ProductName;
  VMapName := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).MapName;
  VJNXVersion := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).JNXVersion;
  VZorder := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ZOrder;
  VProductID := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ProductID;
  VTasks := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).Tasks;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportToJnx.Create(
      VProgressInfo,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      VPath,
      APolygon,
      VTasks,
      VProductName,
      VMapName,
      VJNXVersion,
      VZorder,
      VProductID
    );
  VThread.Resume;
end;

end.


