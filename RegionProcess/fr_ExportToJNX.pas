unit fr_ExportToJNX;

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
  i_MapTypes,
  i_StringListStatic,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_MapType,
  u_CommonFormAndFrameParents, Spin, ComCtrls;

type
  IRegionProcessParamsFrameExportToJNX = interface(IRegionProcessParamsFrameBase)
    ['{BBF2A4A5-C6CC-45D0-A010-A122617EFBB6}']
    function GetLevelsDesc: IStringListStatic;
    property LevelsDesc: IStringListStatic read GetLevelsDesc;

    function GetProductName: string;
    property ProductName: string read GetProductName;

    function GetMapName: string;
    property MapName: string read GetMapName;

    function GetJpgQuality: Byte;
    property JpgQuality: Byte read GetJpgQuality;

    function GetJNXVersion: Integer;
    property JNXVersion: Integer read GetJNXVersion;

    function GetZOrder: Integer;
    property ZOrder: Integer read GetZOrder;

    function GetProductID: Integer;
    property ProductID: Integer read GetProductID;
  end;

type
  TfrExportToJNX = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameExportToJNX
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
    function GetLevelsDesc: IStringListStatic;
    function GetProductName: string;
    function GetMapName: string;
    function GetJpgQuality: Byte;
    function GetJNXVersion: Integer;
    function GetZOrder: Integer;
    function GetProductID: Integer;
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
  RegExprUtils,
  i_GUIDListStatic,
  u_StringListStatic;

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

function TfrExportToJNX.GetJNXVersion: Integer;
begin
  if v3.checked then begin
    Result := 3;
  end else begin
    Result := 4;
  end;
end;

function TfrExportToJNX.GetJpgQuality: Byte;
begin
  Result := EJpgQuality.Value;
end;

function TfrExportToJNX.GetLevelsDesc: IStringListStatic;
var
  VList: TStringList;
  i: Integer;
begin
  VList := TStringList.Create;
  try
    for i := 0 to TreeView1.Items.count - 1 do begin
      VList.add(TreeView1.Items[i].text);
    end;
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

function TfrExportToJNX.GetMapName: string;
begin
  Result := EmapName.Text;
end;

function TfrExportToJNX.GetMapType: TMapType;
begin
  Result := nil;
  if cbbMap.ItemIndex >= 0 then begin
    Result := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end;
  Assert(Result <> nil);
end;

function TfrExportToJNX.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportToJNX.GetProductID: Integer;
var
  VMatchSubStr: string;
begin
  try
    VMatchSubStr := RegExprGetMatchSubStr(EProductID.Text, '[0-9]+', 0);
    Result := StrToIntDef(VMatchSubStr, 0);
  except
    Result := 0;
  end;
end;

function TfrExportToJNX.GetProductName: string;
begin
  Result := EProductName.Text;
end;

function TfrExportToJNX.GetZoomArray: TByteDynArray;
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

function TfrExportToJNX.GetZOrder: Integer;
begin
  if v3.checked then begin
    Result := 0;
  end else begin
    Result := EZorder.Value;
  end;
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

procedure TfrExportToJNX.versionselect(Sender: TObject);
begin
  if v4.checked then EZorder.enabled := true else EZorder.Enabled := false;
end;

end.
