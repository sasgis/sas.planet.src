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
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(
      Azoom: byte;
      const APolygon: ILonLatPolygon
    ); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
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

destructor TExportProviderTar.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
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
    FFrame := TfrExportToFileCont.CreateForFileType(
      nil,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'Tar |*.tar',
      'tar'
    );
    FFrame.Visible := False;
    FFrame.Parent := Self.Parent;
  end;
  FFrame.Init;
end;

procedure TExportProviderTar.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TExportProviderTar.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderTar.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
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
