unit u_ProviderTilesGenPrev;

interface

uses
  Types,
  Forms,
  i_NotifierTime,
  i_NotifierOperation,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_MapViewGoto,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ImageResamplerConfig,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_Bitmap32StaticFactory,
  i_GlobalViewMainConfig,
  u_ExportProviderAbstract,
  fr_TilesGenPrev;

type
  TProviderTilesGenPrev = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FImageResamplerConfig: IImageResamplerConfig;
    FViewConfig: IGlobalViewMainConfig;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AImageResamplerConfig: IImageResamplerConfig
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto ); override;
  end;


implementation

uses
  SysUtils,
  GR32,
  i_ImageResamplerFactory,
  i_RegionProcessParamsFrame,
  u_Notifier,
  u_NotifierOperation,
  u_RegionProcessProgressInfo,
  u_ThreadGenPrevZoom,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TProviderTilesGenPrev }

constructor TProviderTilesGenPrev.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
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
  FVectorItemsFactory := AVectorItemsFactory;
  FBitmapFactory := ABitmapFactory;
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

procedure TProviderTilesGenPrev.StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto );
var
  VInZooms: TByteDynArray;
  VMapType: TMapType;
  VResampler: IImageResamplerFactory;
  VCancelNotifierInternal: INotifierOperationInternal;
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

  VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create(VCancelNotifierInternal, VOperationID);

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo,
    AMapGoto,
    APolygon
  );

  TThreadGenPrevZoom.Create(
    VProgressInfo,
    FProjectionFactory,
    FVectorItemsFactory,
    FBitmapFactory,
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


