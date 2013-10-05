unit fr_ExportToCE;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  CheckLst,
  StdCtrls,
  ExtCtrls,
  Spin,
  Dialogs,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_MapType,
  fr_MapSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameExportToCE = interface(IRegionProcessParamsFrameBase)
    ['{00A64FCB-EFC5-4E88-B4CF-0FCCDB096FAE}']
    function GetComent: string;
    property Coment: string read GetComent;
    function GetIsAddRecoverInfo: boolean;
    property IsAddRecoverInfo: boolean read GetIsAddRecoverInfo;
    function GetMaxSize: integer;
    property MaxSize: integer read GetMaxSize;
  end;

type
  TfrExportToCE = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameExportToCE
    )
    pnlCenter: TPanel;
    pnlRight: TPanel;
    lblZooms: TLabel;
    chkAllZooms: TCheckBox;
    chklstZooms: TCheckListBox;
    lblMap: TLabel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    EMapName: TEdit;
    EComent: TEdit;
    SaveRecoverInfo: TCheckBox;
    lVolSize: TLabel;
    dlgSaveTargetFile: TSaveDialog;
    CComment: TCheckBox;
    CheckBox1: TCheckBox;
    CMapName: TCheckBox;
    TempPath: TEdit;
    cbbMaxVolSize: TSpinEdit;
    pnlMap: TPanel;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
    procedure MapChange(Sender: TObject);
    procedure chklstZoomsDblClick(Sender: TObject);
    procedure CMapNameClick(Sender: TObject);
    procedure CCommentClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
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
    function GetAllowExport(AMapType: TMapType): boolean;
    function GetComent: string;
    function GetIsAddRecoverInfo: boolean;
    function GetMaxSize: integer;
    procedure SetMapName();
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AFileFilters: string;
      const AFileExtDefault: string
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl;
  {$WARN UNIT_PLATFORM ON}

{$R *.dfm}

constructor TfrExportToCe.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AFileFilters: string;
  const AFileExtDefault: string
);
begin
  inherited Create(ALanguageManager);
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
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
  FfrMapSelect.OnMapChange := MapChange;
end;

destructor TfrExportToCE.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;

function TfrExportToCE.GetAllowExport(AMapType: TMapType): boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

procedure TfrExportToCE.btnSelectTargetFileClick(Sender: TObject);
var TempString: string;
begin
  if FfrMapSelect.GetSelectedMapType <> nil then begin
    if SelectDirectory('', '', TempString) then begin
     TempPath.text := TempString;
     edtTargetFile.Text := IncludeTrailingPathDelimiter(TempPath.text) + FfrMapSelect.GetSelectedMapType.GetShortFolderName;
    end;
  end;
end;

procedure TfrExportToCE.SetMapName();
begin
  if CMapName.checked then begin
     EMapName.enabled := true;
     if FfrMapSelect.GetSelectedMapType <> nil then begin
       EMapName.text := FfrMapSelect.GetSelectedMapType.GUIConfig.Name.Value;
     end else begin
      EMapName.text := '';
     end;
  end else begin
     EMapName.Enabled := false;
     EMapName.text := '';
  end;
end;

procedure TfrExportToCE.MapChange(Sender: TObject);
begin
  SetMapName();
  if (TempPath.text = '' ) and (edtTargetFile.Text <> '')then TempPath.text := edtTargetFile.Text;
  if (TempPath.text <> '' )then begin
    if FfrMapSelect.GetSelectedMapType <> nil then begin
      edtTargetFile.Text := IncludeTrailingPathDelimiter(TempPath.text) + FfrMapSelect.GetSelectedMapType.GetShortFolderName;
    end else begin
      edtTargetFile.Text := '';
    end;
  end;
end;

procedure TfrExportToCE.CCommentClick(Sender: TObject);
begin
  if CComment.checked then EComent.enabled := true else begin
    EComent.Enabled := false;
    EComent.text := '';
  end;
end;

procedure TfrExportToCE.chkAllZoomsClick(Sender: TObject);
var
  i: byte;
begin
  if chkAllZooms.state <> cbGrayed then
  for i := 0 to chklstZooms.items.Count - 1 do begin
    chklstZooms.Checked[i] := TCheckBox(Sender).Checked;
  end;
end;

procedure TfrExportToCE.chklstZoomsDblClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chklstZooms.ItemIndex do chklstZooms.Checked[i] := true;
  if chklstZooms.ItemIndex < chklstZooms.items.count-1 then for i := chklstZooms.ItemIndex+1 to chklstZooms.count-1 do chklstZooms.Checked[i] := false;
  if chklstZooms.ItemIndex = chklstZooms.items.count-1 then chkAllZooms.state := cbChecked else chkAllZooms.state := cbGrayed;
end;

procedure TfrExportToCE.CMapNameClick(Sender: TObject);
begin
  SetMapName();
end;

function TfrExportToCE.GetComent: string;
var
  VMapType: TMapType;
begin
  Result := EMapName.Text;
  if Result <> '' then begin
    VMapType :=  GetMapType;
    Result := Guidtostring(VMapType.Zmp.GUID) + #13#10 + Result;
  end;
  if EComent.Text <> '' then begin
    if Result <> '' then begin
      Result := Result + #13#10;
    end;
    Result := Result + EComent.Text;
  end;
end;

function TfrExportToCE.GetIsAddRecoverInfo: boolean;
begin
  Result := SaveRecoverInfo.Checked;
end;

function TfrExportToCE.GetMapType: TMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrExportToCE.GetMaxSize: integer;
begin
  Result := cbbMaxVolSize.value;
end;

function TfrExportToCE.GetPath: string;
var
  VMapType: TMapType;
begin
  Result := '';
  if TempPath.Text <> '' then begin
    Result := edtTargetFile.Text;
  end else if copy(edtTargetFile.Text, length(edtTargetFile.Text), 1) <> '\' then begin
    Result := edtTargetFile.Text;
  end else begin
    VMapType := GetMapType;
    if VMapType <> nil then begin
      Result := IncludeTrailingPathDelimiter(edtTargetFile.Text) + VMapType.GetShortFolderName;
    end;
  end;
end;

function TfrExportToCE.GetZoomArray: TByteDynArray;
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

procedure TfrExportToCE.Init;
var
  i: integer;
begin
  if chklstZooms.Items.count=0 then
  for i:=1 to 24 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;
  if CComment.checked then EComent.enabled := true else begin
      EComent.Enabled := false;
      EComent.text := '';
  end;
  FfrMapSelect.Show(pnlMap);
  SetMapName();
end;
end.
