unit fr_ExportToFileCont;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  ExtCtrls,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_TileFileNameGenerator,
  i_TileFileNameGeneratorsList,
  i_RegionProcessParamsFrame,
  u_MapType,
  fr_MapSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameExportToFileCont = interface(IRegionProcessParamsFrameBase)
    ['{0DB6292A-DE1D-4437-A110-3439923ED4B0}']
    function GetNameGenerator: ITileFileNameGenerator;
    property NameGenerator: ITileFileNameGenerator read GetNameGenerator;
  end;

type
  TfrExportToFileCont = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameExportToFileCont
    )
    pnlCenter: TPanel;
    pnlRight: TPanel;
    lblZooms: TLabel;
    chkAllZooms: TCheckBox;
    chklstZooms: TCheckListBox;
    pnlMain: TPanel;
    lblMap: TLabel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgSaveTargetFile: TSaveDialog;
    cbbNamesType: TComboBox;
    lblNamesType: TLabel;
    pnlFrame: TPanel;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FTileNameGeneratorList: ITileFileNameGeneratorsList;
    FfrMapSelect: TfrMapSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  private
    function GetMapType: TMapType;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
    function GetNameGenerator: ITileFileNameGenerator;
    function GetAllowExport(AMapType: TMapType): boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const ATileNameGeneratorList: ITileFileNameGeneratorsList;
      const AFileFilters: string;
      const AFileExtDefault: string
    ); reintroduce;
    destructor Destroy; override;
    procedure RefreshTranslation; override;
  end;

implementation

{$R *.dfm}

procedure TfrExportToFileCont.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrExportToFileCont.chkAllZoomsClick(Sender: TObject);
var
  i: byte;
begin
  for i:=0 to chklstZooms.Count-1 do begin
    chklstZooms.Checked[i] := TCheckBox(Sender).Checked;
  end;
end;

constructor TfrExportToFileCont.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ATileNameGeneratorList: ITileFileNameGeneratorsList;
  const AFileFilters: string;
  const AFileExtDefault: string
);
begin
  inherited Create(ALanguageManager);
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FTileNameGeneratorList := ATileNameGeneratorList;
  dlgSaveTargetFile.Filter := AFileFilters;
  dlgSaveTargetFile.DefaultExt := AFileExtDefault;
  cbbNamesType.ItemIndex := 1;
  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfAll, // show maps and layers
      False,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
end;

destructor TfrExportToFileCont.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;
 
function TfrExportToFileCont.GetAllowExport(AMapType: TMapType): boolean;
begin
  Result := True
end;

function TfrExportToFileCont.GetMapType: TMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
  Assert(Result <> nil);
end;

function TfrExportToFileCont.GetNameGenerator: ITileFileNameGenerator;
begin
  Result := FTileNameGeneratorList.GetGenerator(cbbNamesType.ItemIndex + 1);
end;

function TfrExportToFileCont.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportToFileCont.GetZoomArray: TByteDynArray;
var
  i: Integer;
  VCount: Integer;
begin
  Result := nil;
  VCount := 0;
  for i := 0 to 23 do begin
    if chklstZooms.Checked[i] then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := i;
      Inc(VCount);
    end;
  end;
end;

procedure TfrExportToFileCont.Init;
var
  i: integer;
begin
  chklstZooms.Items.Clear;
  for i := 1 to 24 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;
  FfrMapSelect.Show(pnlFrame);
end;

procedure TfrExportToFileCont.RefreshTranslation;
var
  i: Integer;
begin
  i := cbbNamesType.ItemIndex;
  inherited;
  cbbNamesType.ItemIndex := i;
end;
end.
