unit u_ExportProviderYaMobileV3;

interface

uses
  Controls,
  i_JclNotify,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
  u_ExportProviderAbstract,
  fr_ExportYaMobileV3;

type
  TExportProviderYaMobileV3 = class(TExportProviderAbstract)
  private
    FFrame: TfrExportYaMobileV3;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FAppClosingNotifier: IJclNotifier;
    FTimerNoifier: IJclNotifier;
  public
    constructor Create(
      AParent: TWinControl;
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: IJclNotifier;
      const ATimerNoifier: IJclNotifier;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
    procedure InitFrame(
      Azoom: byte;
      const APolygon: ILonLatPolygon
    ); override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  Forms,
  SysUtils,
  i_RegionProcessProgressInfo,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  u_ThreadExportYaMobileV3,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderYaMobileV3 }

constructor TExportProviderYaMobileV3.Create(
  AParent: TWinControl;
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: IJclNotifier;
  const ATimerNoifier: IJclNotifier;
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
    AParent,
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderYaMobileV3.GetCaption: string;
begin
  Result := SAS_STR_ExportYaMobileV3Caption;
end;

procedure TExportProviderYaMobileV3.InitFrame(
  Azoom: byte;
  const APolygon: ILonLatPolygon
);
begin
  if FFrame = nil then begin
    FFrame :=
      TfrExportYaMobileV3.Create(
        Self.LanguageManager,
        Self.MainMapsConfig,
        Self.FullMapsSet,
        Self.GUIConfigList
      );
    SetFrame(FFrame);
  end;
  FFrame.Init;
end;

procedure TExportProviderYaMobileV3.StartProcess(const APolygon: ILonLatPolygon);
var
  i: integer;
  path: string;
  Zoomarr: array [0..23] of boolean;
  typemaparr: array of TMapType;
  comprSat, comprMap: byte;
  Replace: boolean;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfo;
begin
  inherited;
  for i := 0 to 23 do begin
    ZoomArr[i] := FFrame.chklstZooms.Checked[i];
  end;
  setlength(typemaparr, 3);
  typemaparr[0] := TMapType(FFrame.cbbSat.Items.Objects[FFrame.cbbSat.ItemIndex]);
  typemaparr[1] := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  typemaparr[2] := TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  comprSat := FFrame.seSatCompress.Value;
  comprMap := FFrame.seMapCompress.Value;
  path := IncludeTrailingPathDelimiter(FFrame.edtTargetPath.Text);
  Replace := FFrame.chkReplaseTiles.Checked;

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

  TThreadExportYaMobileV3.Create(
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
    Replace,
    comprSat,
    comprMap
  );
end;

end.
