unit fr_ExportToCE;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  CheckLst,
  StdCtrls,
  ExtCtrls,
  Dialogs,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents, Spin, ComCtrls;

type
  TfrExportToCE = class(TFrame, IRegionProcessParamsFrameBase)
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
  i_GUIDListStatic,
  u_MapType;

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

  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetSelectedGUID;
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
