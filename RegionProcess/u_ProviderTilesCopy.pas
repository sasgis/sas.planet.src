unit u_ProviderTilesCopy;

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
  fr_TilesCopy;

type
  TProviderTilesCopy = class(TExportProviderAbstract)
  private
    FFrame: TfrTilesCopy;
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
  u_ThreadExportToFileSystem,
  u_ThreadExportToBDB,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TProviderTilesDelete }

constructor TProviderTilesCopy.Create(
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

function TProviderTilesCopy.GetCaption: string;
begin
  Result := SAS_STR_OperationTilesCopyCaption;
end;

procedure TProviderTilesCopy.InitFrame(
  Azoom: byte;
  const APolygon: ILonLatPolygon
);
begin
  if FFrame = nil then begin
    FFrame := TfrTilesCopy.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
    SetFrame(FFrame);
  end;
  FFrame.Init;
end;

procedure TProviderTilesCopy.StartProcess(const APolygon: ILonLatPolygon);
var
  i: integer;
  path: string;
  Zoomarr: array [0..23] of boolean;
  typemaparr: array of TMapType;
  Replace: boolean;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfo;
begin
  for i := 0 to 23 do begin
    ZoomArr[i] := FFrame.chklstZooms.Checked[i];
  end;
  for i := 0 to FFrame.chklstMaps.Items.Count - 1 do begin
    if FFrame.chklstMaps.Checked[i] then begin
      setlength(typemaparr, length(typemaparr) + 1);
      typemaparr[length(typemaparr) - 1] := TMapType(FFrame.chklstMaps.Items.Objects[i]);
    end;
  end;
  path := IncludeTrailingPathDelimiter(FFrame.edtTargetPath.Text);
  Replace := FFrame.chkReplaseTarget.Checked;


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

  if FFrame.cbbNamesType.ItemIndex = 4 then begin
    TThreadExportToBDB.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      path,
      FProjectionFactory,
      FVectorItmesFactory,
      APolygon,
      ZoomArr,
      typemaparr,
      FFrame.chkDeleteSource.Checked,
      Replace
    );
  end else begin
    TThreadExportToFileSystem.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      path,
      FProjectionFactory,
      FVectorItmesFactory,
      APolygon,
      ZoomArr,
      typemaparr,
      FFrame.chkDeleteSource.Checked,
      Replace,
      FTileNameGenerator.GetGenerator(FFrame.cbbNamesType.ItemIndex + 1)
    );
  end;
end;

end.
