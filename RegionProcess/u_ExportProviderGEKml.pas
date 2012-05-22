unit u_ExportProviderGEKml;

interface

uses
  Controls,
  Forms,
  i_JclNotify,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  u_ExportProviderAbstract,
  fr_ExportGEKml;

type
  TExportProviderGEKml = class(TExportProviderAbstract)
  private
    FFrame: TfrExportGEKml;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
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
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  SysUtils,
  i_RegionProcessProgressInfo,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  u_ThreadExportKML,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderKml }

constructor TExportProviderGEKml.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: IJclNotifier;
  const ATimerNoifier: IJclNotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory
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
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderGEKml.CreateFrame: TFrame;
begin
  FFrame := TfrExportGEKml.Create(
    Self.LanguageManager,
    Self.MainMapsConfig,
    Self.FullMapsSet,
    Self.GUIConfigList
  );
  Result := FFrame;
end;

function TExportProviderGEKml.GetCaption: string;
begin
  Result := SAS_STR_ExportGEKmlExportCaption;
end;

procedure TExportProviderGEKml.StartProcess(const APolygon: ILonLatPolygon);
var
  i: integer;
  path: string;
  Zoomarr: array [0..23] of boolean;
  VMapType: TMapType;
  NotSaveNotExists: boolean;
  RelativePath: Boolean;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  inherited;
  for i := 0 to 23 do begin
    ZoomArr[i] := FFrame.chklstZooms.Checked[i];
  end;
  VMapType := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  path := FFrame.edtTargetFile.Text;
  RelativePath := FFrame.chkUseRelativePath.Checked;
  NotSaveNotExists := FFrame.chkNotSaveNotExists.Checked;

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

  TThreadExportKML.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    path,
    FProjectionFactory,
    FVectorItmesFactory,
    APolygon,
    ZoomArr,
    VMapType,
    NotSaveNotExists,
    RelativePath
  );
end;

end.
