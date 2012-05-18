unit fr_ExportToJNX;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  ExtCtrls,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_CommonFormAndFrameParents, Spin, ComCtrls;

type
  TfrExportToJNX = class(TFrame)
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
    dlgSaveTargetFile: TSaveDialog;
    LProductID: TLabel;
    LProductName: TLabel;
    EProductName: TEdit;
    LMapName: TLabel;
    EMapName: TEdit;
    v3: TRadioButton;
    v4: TRadioButton;
    EZorder: TSpinEdit;
    LZOrder: TLabel;
    LVersion: TLabel;
    EJpgQuality: TSpinEdit;
    lblCompress: TLabel;
    EProductID: TComboBox;
    TreeView1: TTreeView;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
    procedure versionselect(Sender: TObject);
    procedure cbbMapChange(Sender: TObject);
    procedure chklstZoomsClickCheck(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AFileFilters: string;
      const AFileExtDefault: string
    ); reintroduce;
    procedure Init;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  i_GUIDListStatic,
  u_MapType;

{$R *.dfm}

procedure TfrExportToJNX.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrExportToJNX.cbbMapChange(Sender: TObject);
begin
  EProductName.text := 'SAS Palnet & '+cbbMap.text;
  EMapName.text := cbbMap.text;
end;

procedure TfrExportToJNX.chkAllZoomsClick(Sender: TObject);
var
  i: byte;
begin
  for i:=0 to chklstZooms.items.Count-1 do begin
    chklstZooms.Checked[i] := TCheckBox(sender).Checked;
  end;
end;

procedure TfrExportToJNX.chklstZoomsClickCheck(Sender: TObject);
var VCurrZoom:integer;
 i : integer;
 cnt, total : integer;
 VItemNode, VParentNode : TTreeNode;

begin
cnt := 0;
Total :=0;
VCurrZoom := chklstZooms.ItemIndex ;

for I := 0 to chklstZooms.items.Count - 1 do begin
    if (i<VCurrZoom) and (chklstZooms.Checked[i]) then inc(cnt);
    if chklstZooms.Checked[i] then inc(Total);
end;
if Total >5 then begin
  ChklstZooms.Checked[VCurrZoom] := false;
  Exit;
end;                                           

if chklstZooms.Checked[VCurrZoom] then
 begin // add items
  if (TreeView1.Items.count >cnt*3) and (cnt<>0) then begin
    VItemNode := TreeView1.Items[(cnt)*3];
    VParentNode := TreeView1.Items.insert(VItemNode, 'Level'+inttostr(VCurrZoom+1));
   end else
   begin
    if cnt=0 then
     VParentNode := TreeView1.Items.AddFirst(nil, 'Level'+inttostr(VCurrZoom+1)) else
     VParentNode := TreeView1.Items.Add(nil, 'Level'+inttostr(VCurrZoom+1));
   end;
    TreeView1.Items.AddChild(VParentNode, EMapName.text);
    TreeView1.Items.AddChild(VParentNode, '(c) '+EProductName.text);
 end else
 begin//delete items
  VItemNode := TreeView1.Items[cnt*3];
  TreeView1.Items.delete(VItemNode);
 end;
end;

constructor TfrExportToJNX.Create(
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
  dlgSaveTargetFile.Filter := AFileFilters;
  dlgSaveTargetFile.DefaultExt := AFileExtDefault;
end;

procedure TfrExportToJNX.Init;
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
//  chklstZooms.Items.Clear;
//  TreeView1.Items.Clear;
  if chklstZooms.Items.count=0 then
  for i:=1 to 24 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;

  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetSelectedGUID;
  cbbMap.items.Clear;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.GUIConfig.Enabled) then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
      if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
  EMapName.text := cbbMap.text;
  EProductName.text := 'SAS Planet & '+cbbMap.text;
  if v4.checked then EZorder.enabled := true else EZorder.Enabled := false;
  EProductID.ItemIndex :=0;
end;

procedure TfrExportToJNX.RefreshTranslation;
begin
  inherited;
end;



procedure TfrExportToJNX.versionselect(Sender: TObject);
begin
  if v4.checked then EZorder.enabled := true else EZorder.Enabled := false;
end;

end.
