unit u_ExportProviderIPhone;

interface

uses
  Forms,
  i_NotifierTime,
  i_NotifierOperation,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_Bitmap32StaticFactory,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_VectorItemsFactory,
  i_MapTypeGUIConfigList,
  i_BitmapTileSaveLoadFactory,
  u_ExportProviderAbstract,
  fr_ExportIPhone;

type
  TExportProviderIPhone = class(TExportProviderAbstract)
  private
    FFrame: TfrExportIPhone;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FNewFormat: Boolean;
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
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      ANewFormat: Boolean
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
  u_ThreadExportIPhone,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderIPhone }

constructor TExportProviderIPhone.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  ANewFormat: Boolean
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
  FVectorItemsFactory := AVectorItemsFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FBitmapFactory := ABitmapFactory;
  FNewFormat := ANewFormat;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderIPhone.CreateFrame: TFrame;
begin
  FFrame :=
    TfrExportIPhone.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Result := FFrame;
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderIPhone.GetCaption: string;
begin
  if FNewFormat then begin
    Result := SAS_STR_ExportIPhone128Caption;
  end else begin
    Result := SAS_STR_ExportIPhone64Caption;
  end;
end;

procedure TExportProviderIPhone.StartProcess(const APolygon: ILonLatPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  typemaparr: array of TMapType;
  comprSat, comprMap, comprHyb: byte;
  Replace: boolean;
  VActiveMapIndex: Integer;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  setlength(typemaparr, 3);
  VActiveMapIndex := 0;
  typemaparr[0] := TMapType(FFrame.cbbSat.Items.Objects[FFrame.cbbSat.ItemIndex]);
  if typemaparr[0] <> nil then begin
    if FFrame.rbSat.Checked then begin
      VActiveMapIndex := 0;
    end;
  end;
  typemaparr[1] := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  if typemaparr[1] <> nil then begin
    if FFrame.rbMap.Checked then begin
      VActiveMapIndex := 1;
    end;
  end;
  typemaparr[2] := TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  if typemaparr[2] <> nil then begin
    if FFrame.rbHybr.Checked then begin
      VActiveMapIndex := 2;
    end;
  end;
  comprSat := FFrame.seSatCompress.Value;
  comprMap := FFrame.seMapCompress.Value;
  comprHyb := FFrame.seHybrCompress.Value;
  Replace := FFrame.chkAppendTilse.Checked;

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

  TThreadExportIPhone.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    FCoordConverterFactory,
    FLocalConverterFactory,
    FProjectionFactory,
    FVectorItemsFactory,
    FBitmapFactory,
    FBitmapTileSaveLoadFactory,
    VPath,
    APolygon,
    VZoomArr,
    typemaparr,
    VActiveMapIndex,
    Replace,
    FNewFormat,
    comprSat,
    comprMap,
    comprHyb
  );
end;

end.


