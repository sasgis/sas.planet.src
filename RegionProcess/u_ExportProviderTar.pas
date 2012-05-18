unit u_ExportProviderTar;

interface

uses
  Controls,
  i_JclNotify,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_TileFileNameGeneratorsList,
  u_ExportProviderAbstract,
  fr_ExportToFileCont;

type
  TExportProviderTar = class(TExportProviderAbstract)
  private
    FFrame: TfrExportToFileCont;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FTileNameGenerator: ITileFileNameGeneratorsList;
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
      const ATileNameGenerator: ITileFileNameGeneratorsList
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
  i_TileFileNameGenerator,
  u_ThreadExportToTar,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderTar }

constructor TExportProviderTar.Create(
  AParent: TWinControl;
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: IJclNotifier;
  const ATimerNoifier: IJclNotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ATileNameGenerator: ITileFileNameGeneratorsList
);
begin
  inherited Create(AParent, ALanguageManager, AMainMapsConfig, AFullMapsSet, AGUIConfigList);
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FTileNameGenerator := ATileNameGenerator;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderTar.GetCaption: string;
begin
  Result := SAS_STR_ExportTarPackCaption;
end;

procedure TExportProviderTar.InitFrame(
  Azoom: byte;
  const APolygon: ILonLatPolygon
);
begin
  if FFrame = nil then begin
    FFrame := TfrExportToFileCont.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'Tar |*.tar',
      'tar'
    );
    SetFrame(FFrame);
  end;
  FFrame.Init;
end;

procedure TExportProviderTar.StartProcess(const APolygon: ILonLatPolygon);
var
  i: integer;
  path: string;
  Zoomarr: array [0..23] of boolean;
  VMapType: TMapType;
  VNameGenerator: ITileFileNameGenerator;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfo;
begin
  inherited;
  for i := 0 to 23 do begin
    ZoomArr[i] := FFrame.chklstZooms.Checked[i];
  end;
  VMapType := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  path := FFrame.edtTargetFile.Text;
  VNameGenerator := FTileNameGenerator.GetGenerator(FFrame.cbbNamesType.ItemIndex + 1);

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

  TThreadExportToTar.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    path,
    FProjectionFactory,
    FVectorItmesFactory,
    APolygon,
    Zoomarr,
    VMapType,
    VNameGenerator
  );
end;

end.
