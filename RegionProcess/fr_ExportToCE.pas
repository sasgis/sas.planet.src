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
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_MapType,
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
    cbbMap: TComboBox;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    EMapName: TEdit;
    EComent: TEdit;
    SaveRecoverInfo: TCheckBox;
    lVolSize: TLabel;
    dlgSaveTargetFile: TSaveDialog;
    LFoldersName: TListBox;
    CComment: TCheckBox;
    CheckBox1: TCheckBox;
    CMapName: TCheckBox;
    TempPath: TEdit;
    cbbMaxVolSize: TSpinEdit;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
    procedure cbbMapChange(Sender: TObject);
    procedure chklstZoomsDblClick(Sender: TObject);
    procedure CMapNameClick(Sender: TObject);
    procedure CCommentClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  private
    function GetMapType: TMapType;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
  private
    function GetComent: string;
    function GetIsAddRecoverInfo: boolean;
    function GetMaxSize: integer;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AFileFilters: string;
      const AFileExtDefault: string
    ); reintroduce;
  end;

implementation
uses
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  i_GUIDListStatic;

{$R *.dfm}

procedure TfrExportToCE.btnSelectTargetFileClick(Sender: TObject);
var TempString: string ;
begin
  if SelectDirectory('', '', TempString) then begin
   TempPath.text := TempString;
   edtTargetFile.Text := IncludeTrailingPathDelimiter(TempPath.text)+LFoldersName.items[cbbMap.itemindex];
  end;
end;

procedure TfrExportToCE.cbbMapChange(Sender: TObject);
begin
  if EMapName.enabled then  EMapName.text := cbbMap.text;
  if (TempPath.text = '' ) and (edtTargetFile.Text<>'')then TempPath.text := edtTargetFile.Text;
  if (TempPath.text <> '' )then
  edtTargetFile.Text := IncludeTrailingPathDelimiter(TempPath.text)+LFoldersName.items[cbbMap.itemindex]
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
  if chkAllZooms.state<>cbGrayed then
  for i:=0 to chklstZooms.items.Count-1 do begin
    chklstZooms.Checked[i] := TCheckBox(sender).Checked;
  end;
end;

procedure TfrExportToCE.chklstZoomsDblClick(Sender: TObject);
var
  i: Integer;
begin
  for I := 0 to chklstZooms.ItemIndex do chklstZooms.Checked[i]:=true;
  if chklstZooms.ItemIndex<chklstZooms.items.count-1 then for I := chklstZooms.ItemIndex+1 to chklstZooms.count-1 do chklstZooms.Checked[i]:=false;
  if chklstZooms.ItemIndex=chklstZooms.items.count-1 then chkAllZooms.state:=cbChecked else chkAllZooms.state:=cbGrayed;
end;

procedure TfrExportToCE.CMapNameClick(Sender: TObject);
begin
  if CMapName.checked then begin
     EMapName.enabled := true;
     EMapName.text := cbbMap.text;
  end else begin
     EMapName.Enabled := false;
     EMapName.text := '';
  end;
end;

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
  Result := nil;
  if cbbMap.ItemIndex >= 0 then begin
    Result := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end;
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
  if Temppath.Text <> '' then begin
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
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  if chklstZooms.Items.count=0 then
  for i:=1 to 24 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;

  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetStatic.GUID;
  cbbMap.items.Clear;
  LFoldersName.items.Clear;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.GUIConfig.Enabled) then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
      LFoldersName.Items.AddObject(VMapType.GetShortFolderName,VMapType);

      if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
        cbbMap.ItemIndex:=VAddedIndex;
          if TempPath.text <> '' then
           edtTargetFile.Text := IncludeTrailingPathDelimiter(TempPath.text)+LFoldersName.items[cbbMap.itemindex];
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
  if CComment.checked then EComent.enabled := true else begin
      EComent.Enabled := false;
      EComent.text := '';
  end;
  if CMapName.checked then begin
     EMapName.enabled := true;
     EMapName.text := cbbMap.text;
  end else begin
     EMapName.Enabled := false;
     EMapName.text := '';
  end;


end;

end.
