unit u_ExportProviderRMapsSQLite;

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
  i_RegionProcessProgressInfoInternalFactory,
  i_CoordConverterFactory,
  u_ExportProviderAbstract,
  fr_ExportRMapsSQLite;

type
  TExportProviderRMapsSQLite = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
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
//    procedure StartProcess(const APolygon: ILonLatPolygon); override;
    procedure StartProcess(
      const APolygon: ILonLatPolygon;
      const AMapGoto: IMapViewGoto
    ); override;

  end;


implementation

uses
  Types,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadExportToSQLite,
  u_ResStrings;

{ TExportProviderKml }

constructor TExportProviderRMapsSQLite.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
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
    AProgressFactory,
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

function TExportProviderRMapsSQLite.CreateFrame: TFrame;
begin
  Result :=
    TfrExportRMapsSQLite.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameSQLiteExport));
end;

function TExportProviderRMapsSQLite.GetCaption: string;
begin
  Result := SAS_STR_ExportRMapsSQLiteExportCaption;
end;

procedure TExportProviderRMapsSQLite.StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto);

var
  VPath: string;
  VZoomArr: TByteDynArray;
  VMapTypeList: IMapTypeListStatic;
  VForceDropTarget: boolean;
  VReplaceExistingTiles: Boolean;
  VProgressInfo: IRegionProcessProgressInfoInternal;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;

  with (ParamsFrame as IRegionProcessParamsFrameSQLiteExport) do begin
    VForceDropTarget := ForceDropTarget;
    VReplaceExistingTiles := ReplaceExistingTiles;
    VMapTypeList := MapTypeList;
  end;

  VProgressInfo := ProgressFactory.Build(APolygon);

  TThreadExportRMapsSQLite.Create(
    VProgressInfo,
    '',
    VPath,
    FProjectionFactory,
    FVectorItemsFactory,
    APolygon,
    VZoomArr,
    VMapTypeList,
    FALSE,
    '',
    VForceDropTarget,
    FALSE,
    VReplaceExistingTiles
  );
end;

end.


