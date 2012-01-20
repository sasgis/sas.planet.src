unit u_ProviderTilesCopy;

interface

uses
  Controls,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_TileFileNameGeneratorsList,
  u_ExportProviderAbstract,
  fr_TilesCopy;

type
  TProviderTilesCopy = class(TExportProviderAbstract)
  private
    FFrame: TfrTilesCopy;
    FTileNameGenerator: ITileFileNameGeneratorsList;
  public
    constructor Create(
      AParent: TWinControl;
      ALanguageManager: ILanguageManager;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList;
      ATileNameGenerator: ITileFileNameGeneratorsList
    );
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte; APolygon: ILonLatPolygon); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  SysUtils,
  u_ThreadExportToFileSystem,
  u_ThreadExportToBDB,
  u_ResStrings,
  u_MapType;

{ TProviderTilesDelete }

constructor TProviderTilesCopy.Create(
  AParent: TWinControl;
  ALanguageManager: ILanguageManager;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList;
  ATileNameGenerator: ITileFileNameGeneratorsList
);
begin
  inherited Create(AParent, ALanguageManager, AMainMapsConfig, AFullMapsSet, AGUIConfigList);
  FTileNameGenerator := ATileNameGenerator;
end;

destructor TProviderTilesCopy.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TProviderTilesCopy.GetCaption: string;
begin
  Result := SAS_STR_OperationTilesCopyCaption;
end;

procedure TProviderTilesCopy.InitFrame(Azoom: byte; APolygon: ILonLatPolygon);
begin
  if FFrame = nil then begin
    FFrame := TfrTilesCopy.Create(
      nil,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
    FFrame.Visible := False;
    FFrame.Parent := Self.Parent;
  end;
  FFrame.Init;
end;

procedure TProviderTilesCopy.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TProviderTilesCopy.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TProviderTilesCopy.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TProviderTilesCopy.StartProcess(APolygon: ILonLatPolygon);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  typemaparr:array of TMapType;
  Replace:boolean;
begin
  for i:=0 to 23 do begin
    ZoomArr[i]:=FFrame.chklstZooms.Checked[i];
  end;
  for i:=0 to FFrame.chklstMaps.Items.Count-1 do begin
    if FFrame.chklstMaps.Checked[i] then begin
    setlength(typemaparr,length(typemaparr)+1);
    typemaparr[length(typemaparr)-1]:=TMapType(FFrame.chklstMaps.Items.Objects[i]);
    end;
  end;
  path:=IncludeTrailingPathDelimiter(FFrame.edtTargetPath.Text);
  Replace:=FFrame.chkReplaseTarget.Checked;

  if FFrame.cbbNamesType.ItemIndex = 4 then begin
    TThreadExportToBDB.Create(
      path,
      APolygon.Item[0],
      ZoomArr,
      typemaparr,
      FFrame.chkDeleteSource.Checked,
      Replace
    )
  end else begin
    TThreadExportToFileSystem.Create(
      path,
      APolygon.Item[0],
      ZoomArr,
      typemaparr,
      FFrame.chkDeleteSource.Checked,
      Replace,
      FTileNameGenerator.GetGenerator(FFrame.cbbNamesType.ItemIndex + 1)
    )
  end;
end;

end.

