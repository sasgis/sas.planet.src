unit u_ExportProviderCE;

interface

uses
  Forms,
  i_NotifierTime,
  i_NotifierOperation,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_LanguageManager,
  i_MapTypes,
  i_mapViewGoto,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_ExportProviderAbstract;

type
  TExportProviderCE = class(TExportProviderAbstract)
  private
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
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
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto ); override;
  end;

implementation

uses
  Types,
  SysUtils,
  i_RegionProcessParamsFrame,
  u_Notifier,
  u_NotifierOperation,
  u_RegionProcessProgressInfo,
  u_ThreadExportToCE,
  u_ResStrings,
  u_MapType,
  fr_ExportToCE,
  frm_ProgressSimple;

{ TExportProviderCE }

constructor TExportProviderCE.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ACoordConverterFactory: ICoordConverterFactory
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
  FCoordConverterFactory := ACoordConverterFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderCE.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToCE.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'd00 |*.d00',
      'd00'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToCE));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderCE.GetCaption: string;
begin
  Result := SAS_STR_ExportCEPackCaption;
end;

procedure TExportProviderCE.StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto );
var
  VPath: string;
  Zoomarr: TByteDynArray;
  VMapType: TMapType;

  VMaxSize: integer;
  VComent: string;
  VRecoverInfo: boolean;

  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  Zoomarr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMaxSize := (ParamsFrame as IRegionProcessParamsFrameExportToCE).MaxSize;
  VComent := (ParamsFrame as IRegionProcessParamsFrameExportToCE).Coment;
  VRecoverInfo := (ParamsFrame as IRegionProcessParamsFrameExportToCE).IsAddRecoverInfo;

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

  TThreadExportToCE.Create(
    VProgressInfo,
    FCoordConverterFactory,
    FProjectionFactory,
    FVectorItemsFactory,
    VPath,
    APolygon,
    Zoomarr,
    VMapType,
    VMaxSize,
    VComent,
    VRecoverInfo
  );
end;

end.


