unit u_ExportProviderGEKml;

interface

uses
  Forms,
  i_NotifierTime,
  i_NotifierOperation,
  i_VectorItemLonLat,
  i_VectorItemsFactory,
  i_LanguageManager,
  i_MapTypes,
  i_MapViewGoto,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  u_ExportProviderAbstract,
  fr_ExportGEKml;

type
  TExportProviderGEKml = class(TExportProviderAbstract)
  private
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
      const AVectorItemsFactory: IVectorItemsFactory
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
  u_ThreadExportKML,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderKml }

constructor TExportProviderGEKml.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory
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
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
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

procedure TExportProviderGEKml.StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto );
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VMapType: TMapType;
  NotSaveNotExists: boolean;
  RelativePath: Boolean;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  RelativePath := (ParamsFrame as IRegionProcessParamsFrameKmlExport).RelativePath;
  NotSaveNotExists := (ParamsFrame as IRegionProcessParamsFrameKmlExport).NotSaveNotExists;

  VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo,
    AMapGoto,
    APolygon
  );

  TThreadExportKML.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    VPath,
    FProjectionFactory,
    FVectorItemsFactory,
    APolygon,
    VZoomArr,
    VMapType,
    NotSaveNotExists,
    RelativePath
  );
end;

end.


