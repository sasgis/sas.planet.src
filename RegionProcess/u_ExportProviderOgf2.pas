unit u_ExportProviderOgf2;

interface

uses
  Forms,
  i_GeometryLonLat,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_VectorGeometryProjectedFactory,
  i_Bitmap32StaticFactory,
  i_BitmapTileSaveLoadFactory,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_ExportToOgf2;

type
  TExportProviderOgf2 = class(TExportProviderAbstract)
  private
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
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
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
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
  i_BitmapLayerProvider,
  i_BitmapTileSaveLoad,
  u_ThreadExportToOgf2,
  u_ResStrings;

{ TExportProviderOgf2 }

constructor TExportProviderOgf2.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
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
  FBitmapFactory := ABitmapFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
end;

function TExportProviderOgf2.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToOgf2.Create(
      Self.LanguageManager,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      FBitmapTileSaveLoadFactory,
      FBitmapFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'OGF2 (*.ogf2) |*.ogf2',
      'ogf2'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneZoom));
  Assert(Supports(Result, IRegionProcessParamsFrameImageProvider));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToOgf2));
end;

function TExportProviderOgf2.GetCaption: string;
begin
  Result := SAS_STR_ExportOgf2PackCaption;
end;

procedure TExportProviderOgf2.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VTargetFile: string;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VImageProvider: IBitmapLayerProvider;
  VZoom: Byte;
  VSaver: IBitmapTileSaver;
  VTileSize: TPoint;
  VThread: TThread;
begin
  inherited;

  VTargetFile := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
  VImageProvider := (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider;
  VSaver := (ParamsFrame as IRegionProcessParamsFrameExportToOgf2).Saver;
  VTileSize := (ParamsFrame as IRegionProcessParamsFrameExportToOgf2).TileSize;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportToOgf2.Create(
      VProgressInfo,
      FCoordConverterFactory,
      FLocalConverterFactory,
      FProjectionFactory,
      FBitmapFactory,
      FVectorGeometryProjectedFactory,
      VTargetFile,
      APolygon,
      VImageProvider,
      VZoom,
      VTileSize,
      VSaver
    );
  VThread.Resume;
end;

end.


