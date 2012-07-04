unit u_ExportProviderYaMobileV4;

interface

uses
  Forms,
  i_Notifier,
  i_NotifierOperation,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItmesFactory,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  u_ExportProviderAbstract,
  fr_ExportYaMobileV4;

type
  TExportProviderYaMobileV4 = class(TExportProviderAbstract)
  private
    FFrame: TfrExportYaMobileV4;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifier;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifier;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  Types,
  SysUtils,
  i_RegionProcessParamsFrame,
  u_NotifierOperation,
  u_RegionProcessProgressInfo,
  u_ThreadExportYaMobileV4,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderYaMaps }

constructor TExportProviderYaMobileV4.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ACoordConverterFactory: ICoordConverterFactory
);
begin
  inherited Create(
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderYaMobileV4.CreateFrame: TFrame;
begin
  FFrame :=
    TfrExportYaMobileV4.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Result := FFrame;
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderYaMobileV4.GetCaption: string;
begin
  Result := SAS_STR_ExportYaMobileV4Caption;
end;

procedure TExportProviderYaMobileV4.StartProcess(const APolygon: ILonLatPolygon);
var
  path: string;
  Zoomarr: TByteDynArray;
  typemaparr: array of TMapType;
  comprSat, comprMap: byte;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  inherited;
  Zoomarr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  path := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  setlength(typemaparr, 3);
  typemaparr[0] := TMapType(FFrame.cbbSat.Items.Objects[FFrame.cbbSat.ItemIndex]);
  typemaparr[1] := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  typemaparr[2] := TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  comprSat := FFrame.seSatCompress.Value;
  comprMap := FFrame.seMapCompress.Value;

  VCancelNotifierInternal := TNotifierOperation.Create;
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo
  );

  TThreadExportYaMobileV4.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    FCoordConverterFactory,
    FLocalConverterFactory,
    FProjectionFactory,
    FVectorItmesFactory,
    path,
    APolygon,
    ZoomArr,
    typemaparr,
    FFrame.chkReplaseTiles.Checked,
    comprSat,
    comprMap
  );
end;

end.
