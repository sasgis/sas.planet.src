unit u_ExportProviderYaMobileV3;

interface

uses
  Forms,
  i_Notifier,
  i_NotifierOperation,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_VectorItemsFactory,
  i_VectorItemLonLat,
  i_BitmapTileSaveLoadFactory,
  u_ExportProviderAbstract,
  fr_ExportYaMobileV3;

type
  TExportProviderYaMobileV3 = class(TExportProviderAbstract)
  private
    FFrame: TfrExportYaMobileV3;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItemsFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
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
      const AVectorItmesFactory: IVectorItemsFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
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
  u_Notifier,
  u_NotifierOperation,
  u_RegionProcessProgressInfo,
  u_ThreadExportYaMobileV3,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderYaMobileV3 }

constructor TExportProviderYaMobileV3.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItemsFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
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
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderYaMobileV3.CreateFrame: TFrame;
begin
  FFrame :=
    TfrExportYaMobileV3.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Result := FFrame;
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderYaMobileV3.GetCaption: string;
begin
  Result := SAS_STR_ExportYaMobileV3Caption;
end;

procedure TExportProviderYaMobileV3.StartProcess(const APolygon: ILonLatPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  typemaparr: array of TMapType;
  comprSat, comprMap: byte;
  Replace: boolean;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;

  setlength(typemaparr, 3);
  typemaparr[0] := TMapType(FFrame.cbbSat.Items.Objects[FFrame.cbbSat.ItemIndex]);
  typemaparr[1] := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  typemaparr[2] := TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  comprSat := FFrame.seSatCompress.Value;
  comprMap := FFrame.seMapCompress.Value;
  Replace := FFrame.chkReplaseTiles.Checked;

  VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo
  );

  TThreadExportYaMobileV3.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    FCoordConverterFactory,
    FLocalConverterFactory,
    FProjectionFactory,
    FVectorItmesFactory,
    FBitmapTileSaveLoadFactory,
    VPath,
    APolygon,
    VZoomArr,
    typemaparr,
    Replace,
    comprSat,
    comprMap
  );
end;

end.
