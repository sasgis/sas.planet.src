unit u_ExportProviderCE;

interface

uses
  Controls,
  i_JclNotify,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_ExportProviderAbstract,
  fr_ExportToCE;

type
  TExportProviderCE = class(TExportProviderAbstract)
  private
    FFrame: TfrExportToCE;
    FCoordConverterFactory: ICoordConverterFactory;
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
  Classes,
  RegExprUtils,
  i_RegionProcessProgressInfo,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  u_ThreadExportToCE,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderCE }

constructor TExportProviderCE.Create(
  AParent: TWinControl;
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: IJclNotifier;
  const ATimerNoifier: IJclNotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ACoordConverterFactory: ICoordConverterFactory
);
begin
  inherited Create(AParent, ALanguageManager, AMainMapsConfig, AFullMapsSet, AGUIConfigList);
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderCE.GetCaption: string;
begin
  Result := SAS_STR_ExportCEPackCaption;
end;

procedure TExportProviderCE.InitFrame(
  Azoom: byte;
  const APolygon: ILonLatPolygon
);
begin
  if FFrame = nil then begin
    FFrame := TfrExportToCE.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'd00 |*.d00',
      'd00'
    );
    SetFrame(FFrame);
  end;
  FFrame.Init;
end;

procedure TExportProviderCE.StartProcess(const APolygon: ILonLatPolygon);
var
  i: integer;
  path: string;
  Zoomarr: array [0..23] of boolean;
  VMapType: TMapType;

  VMaxSize: integer;
  VComent: string;
  VRecoverInfo: boolean;

  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfo;

begin

  for i := 0 to 23 do begin
    ZoomArr[i] := FFrame.chklstZooms.Checked[i];
  end;
  VMapType := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  if FFrame.Temppath.Text <> '' then begin
    path := FFrame.edtTargetFile.Text;
  end else if copy(FFrame.edtTargetFile.Text, length(FFrame.edtTargetFile.Text), 1) <> '\' then begin
    path := FFrame.edtTargetFile.Text;
  end else begin
    path := IncludeTrailingPathDelimiter(FFrame.edtTargetFile.Text) + VMapType.GetShortFolderName;
  end;

  VMaxSize := FFrame.cbbMaxVolSize.value;

  VComent := FFrame.EmapName.Text;
  if VComent <> '' then begin
    VComent := Guidtostring(VMapType.Zmp.GUID) + #13#10 + VComent;
  end;
  if FFrame.EComent.Text <> '' then begin
    if VComent <> '' then begin
      VComent := VComent + #13#10;
    end;
    VComent := VComent + FFrame.EComent.Text;
  end;
  VRecoverInfo := FFrame.SaveRecoverInfo.Checked;
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

  TThreadExportToCE.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    FCoordConverterFactory,
    FProjectionFactory,
    FVectorItmesFactory,
    path,
    APolygon,
    Zoomarr,
    VMapType,

    VMaxSize,
    VComent,
    VRecoverInfo
  );
end;

end.
