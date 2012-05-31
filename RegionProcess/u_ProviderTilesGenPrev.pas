unit u_ProviderTilesGenPrev;

interface

uses
  Types,
  Controls,
  Forms,
  i_JclNotify,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ImageResamplerConfig,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_GlobalViewMainConfig,
  u_ExportProviderAbstract,
  fr_TilesGenPrev;

type
  TProviderTilesGenPrev = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FImageResamplerConfig: IImageResamplerConfig;
    FViewConfig: IGlobalViewMainConfig;
    FAppClosingNotifier: IJclNotifier;
    FTimerNoifier: IJclNotifier;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: IJclNotifier;
      const ATimerNoifier: IJclNotifier;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const AImageResamplerConfig: IImageResamplerConfig
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  SysUtils,
  GR32,
  i_ImageResamplerFactory,
  i_RegionProcessParamsFrame,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  u_ThreadGenPrevZoom,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TProviderTilesGenPrev }

constructor TProviderTilesGenPrev.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: IJclNotifier;
  const ATimerNoifier: IJclNotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const AImageResamplerConfig: IImageResamplerConfig
);
begin
  inherited Create(
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FViewConfig := AViewConfig;
  FImageResamplerConfig := AImageResamplerConfig;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
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

procedure TProviderTilesGenPrev.StartProcess(const APolygon: ILonLatPolygon);
var
  VInZooms: TByteDynArray;
  VMapType: TMapType;
  VResampler: IImageResamplerFactory;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
  VBgColor: TColor32;
begin
  inherited;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VInZooms := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VResampler := (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).Resampler;
  if VMapType.IsHybridLayer then begin
    VBgColor := 0;
  end else begin
    VBgColor := Color32(FViewConfig.BackGroundColor);
  end;

  VCancelNotifierInternal := TOperationNotifier.Create;
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo
  );

  TThreadGenPrevZoom.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    FProjectionFactory,
    FVectorItmesFactory,
    VInZooms,
    APolygon,
    VMapType,
    (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsReplace,
    (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsSaveFullOnly,
    (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsCreateAllFromFirstZoom,
    (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsUseTilesFromPrevZoom,
    VBgColor,
    VResampler
  );
end;

end.
