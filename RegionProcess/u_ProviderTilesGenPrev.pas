unit u_ProviderTilesGenPrev;

interface

uses
  Types,
  Forms,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ImageResamplerConfig,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_Bitmap32StaticFactory,
  i_GlobalViewMainConfig,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_TilesGenPrev;

type
  TProviderTilesGenPrev = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FImageResamplerConfig: IImageResamplerConfig;
    FViewConfig: IGlobalViewMainConfig;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AImageResamplerConfig: IImageResamplerConfig
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon); override;
  end;


implementation

uses
  Classes,
  SysUtils,
  GR32,
  i_ImageResamplerFactory,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadGenPrevZoom,
  u_ResStrings,
  u_MapType;

{ TProviderTilesGenPrev }

constructor TProviderTilesGenPrev.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AImageResamplerConfig: IImageResamplerConfig
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
  FViewConfig := AViewConfig;
  FImageResamplerConfig := AImageResamplerConfig;
end;

function TProviderTilesGenPrev.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesGenPrev.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      FImageResamplerConfig
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTilesGenPrev));
end;

function TProviderTilesGenPrev.GetCaption: string;
begin
  Result := SAS_STR_OperationGenPrevCaption;
end;

procedure TProviderTilesGenPrev.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VInZooms: TByteDynArray;
  VMapType: TMapType;
  VResampler: IImageResamplerFactory;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VBgColor: TColor32;
  VThread: TThread;
begin
  inherited;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VInZooms := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VResampler := (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).Resampler;
  if VMapType.Zmp.IsLayer then begin
    VBgColor := 0;
  end else begin
    VBgColor := Color32(FViewConfig.BackGroundColor);
  end;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadGenPrevZoom.Create(
      VProgressInfo,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      FBitmapFactory,
      VInZooms,
      APolygon,
      VMapType,
      VMapType.VersionConfig.Version,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsReplace,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsSaveFullOnly,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsCreateAllFromFirstZoom,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsUseTilesFromPrevZoom,
      VBgColor,
      VResampler
    );
  VThread.Resume;
end;

end.


