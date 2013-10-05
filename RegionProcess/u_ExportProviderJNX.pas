unit u_ExportProviderJNX;

interface

uses
  Forms,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_StringListStatic,
  i_LanguageManager,
  i_MapTypeSet,
  i_MapTypeListBuilder,
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
    FVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
    FMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
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
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;

implementation

uses
  Types,
  SysUtils,
  i_MapTypeListStatic,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadExportToJNX,
  u_ResStrings;


{ TExportProviderJNX }

constructor TExportProviderJNX.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
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
  FMapTypeListBuilderFactory := AMapTypeListBuilderFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FCoordConverterFactory := ACoordConverterFactory;
end;

function TExportProviderJNX.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToJNX.Create(
      Self.LanguageManager,
      FMapTypeListBuilderFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'JNX |*.jnx',
      'jnx'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToJNX));
end;

function TExportProviderJNX.GetCaption: string;
begin
  Result := SAS_STR_ExportJNXPackCaption;
end;

procedure TExportProviderJNX.StartProcess(const APolygon: ILonLatPolygon);
var
  VPath: string;
  Zoomarr: TByteDynArray;
  VProductName: string;
  VMapName: string;
  VJNXVersion: integer;
  VZorder: integer;
  VProductID: integer;
  VJpgQuality: IStringListStatic;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VLevelsDesc: IStringListStatic;
  VMapList: IMapTypeListStatic;
  VLayerList: IMapTypeListStatic;
  VScaleArr: TByteDynArray;
  VRecompressArr: TBooleanDynArray;
begin
  inherited;

  Zoomarr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;

  VLevelsDesc := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).LevelsDesc;
  VProductName := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ProductName;
  VMapName := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).MapName;
  VJpgQuality := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).JpgQuality;
  VJNXVersion := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).JNXVersion;
  VZorder := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ZOrder;
  VProductID := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ProductID;
  VScaleArr := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ScaleArray;
  VMapList := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).MapList;
  VRecompressArr := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).Recompress;

  VProgressInfo := ProgressFactory.Build(APolygon);

  TThreadExportToJnx.Create(
    VProgressInfo,
    FCoordConverterFactory,
    FProjectionFactory,
    FVectorGeometryProjectedFactory,
    FBitmapTileSaveLoadFactory,
    VPath,
    APolygon,
    Zoomarr,
    VProductName,
    VMapName,
    VJNXVersion,
    VZorder,
    VProductID,
    VJpgQuality,
    VLevelsDesc,
    VMapList,
    VLayerList,
    VScaleArr,
    VRecompressArr
  );
end;

end.


