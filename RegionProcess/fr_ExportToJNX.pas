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
  ExtCtrls,
  i_LanguageManager,
  i_MapTypes,
  i_StringListStatic,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
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

    function GetJpgQuality: IStringListStatic;
    property JpgQuality: IStringListStatic read GetJpgQuality;

    function GetJNXVersion: Integer;
    property JNXVersion: Integer read GetJNXVersion;

    function GetZOrder: Integer;
    property ZOrder: Integer read GetZOrder;

    function GetProductID: Integer;
    property ProductID: Integer read GetProductID;

    function GetScaleArray: TByteDynArray;
    property ScaleArray: TByteDynArray read GetScaleArray;

    function GetMapList: IMapTypeListStatic;
    property MapList: IMapTypeListStatic read GetMapList;

    function GetRecompress: TBooleanDynArray;
    property Recompress: TBooleanDynArray read GetRecompress;

  end;

type
  TfrExportToJNX = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameExportToJNX
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgSaveTargetFile: TSaveDialog;
    EJpgQuality: TSpinEdit;
    lblCompress: TLabel;
    PageControl1: TPageControl;
    Map: TTabSheet;
    Info: TTabSheet;
    TreeView1: TTreeView;
    EZorder: TSpinEdit;
    LVersion: TLabel;
    LZOrder: TLabel;
    LProductID: TLabel;
    EProductID: TComboBox;
    EProductName: TEdit;
    LProductName: TLabel;
    LMapName: TLabel;
    EMapName: TEdit;
    Label1: TLabel;
    CbbZoom: TComboBox;
    EJpgQuality2: TSpinEdit;
    CbbZoom2: TComboBox;
    EJpgQuality4: TSpinEdit;
    CbbZoom4: TComboBox;
    EJpgQuality5: TSpinEdit;
    CbbZoom5: TComboBox;
    EJpgQuality3: TSpinEdit;
    CbbZoom3: TComboBox;
    Label2: TLabel;
    cbbscale: TComboBox;
    cbbscale2: TComboBox;
    cbbscale3: TComboBox;
    cbbscale4: TComboBox;
    cbbscale5: TComboBox;
    ChRecompress1: TCheckBox;
    ChRecompress2: TCheckBox;
    ChRecompress3: TCheckBox;
    ChRecompress4: TCheckBox;
    ChRecompress5: TCheckBox;
    cbbMap4: TComboBox;
    cbbMap3: TComboBox;
    cbbMap2: TComboBox;
    cbbMap: TComboBox;
    cbbMap5: TComboBox;
    lblMap: TLabel;
    ChMap5: TCheckBox;
    ChMap4: TCheckBox;
    ChMap3: TCheckBox;
    ChMap2: TCheckBox;
    ChMap1: TCheckBox;
    MapsPanel: TPanel;
    PnlInfo: TPanel;
    cbbVersion: TComboBox;
    chkUseRecolor: TCheckBox;
    chkUseMapMarks: TCheckBox;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure cbbMapChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ChMap1Click(Sender: TObject);
    procedure ChMap2Click(Sender: TObject);
    procedure ChMap3Click(Sender: TObject);
    procedure ChMap4Click(Sender: TObject);
    procedure ChMap5Click(Sender: TObject);
    procedure cbbMap2Change(Sender: TObject);
    procedure cbbMap3Change(Sender: TObject);
    procedure cbbMap4Change(Sender: TObject);
    procedure cbbMap5Change(Sender: TObject);
    procedure CbbZoomChange(Sender: TObject);
    procedure CbbZoom2Change(Sender: TObject);
    procedure CbbZoom3Change(Sender: TObject);
    procedure CbbZoom4Change(Sender: TObject);
    procedure CbbZoom5Change(Sender: TObject);
    procedure cbbVersionChange(Sender: TObject);
    procedure ChRecompress1Click(Sender: TObject);
    procedure ChRecompress2Click(Sender: TObject);
    procedure ChRecompress3Click(Sender: TObject);
    procedure ChRecompress4Click(Sender: TObject);
    procedure ChRecompress5Click(Sender: TObject);
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
    function GetZoomArray: TByteDynArray;
    function GetScaleArray: TByteDynArray;
    function GetPath: string;
  private
    function GetLevelsDesc: IStringListStatic;
    function GetProductName: string;
    function GetMapName: string;
    function GetJpgQuality: IStringListStatic;
    function GetJNXVersion: Integer;
    function GetZOrder: Integer;
    function GetProductID: Integer;
    function GetMapList: IMapTypeListStatic;
    function GetRecompress: TBooleanDynArray;

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
  u_StringListStatic,
  u_MapType,
  u_MapTypeListStatic;

{$R *.dfm}

procedure TfrExportToJNX.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrExportToJNX.cbbMap2Change(Sender: TObject);
var
 cnt : integer;
begin
  cnt := 0;
  if ChMap1.Checked then inc(cnt);
  TreeView1.Items[cnt*3+1].Text :=cbbMap2.text;
end;

procedure TfrExportToJNX.cbbMap3Change(Sender: TObject);
var
 cnt : integer;
begin
  cnt := 0;
  if ChMap1.Checked then inc(cnt);
  if ChMap2.Checked then inc(cnt);
  TreeView1.Items[cnt*3+1].Text :=cbbMap3.text;
end;

procedure TfrExportToJNX.cbbMap4Change(Sender: TObject);
var
 cnt : integer;
begin
  cnt := 0;
  if ChMap1.Checked then inc(cnt);
  if ChMap2.Checked then inc(cnt);
  if ChMap3.Checked then inc(cnt);
  TreeView1.Items[cnt*3+1].Text :=cbbMap4.text;
end;

procedure TfrExportToJNX.cbbMap5Change(Sender: TObject);
var
 cnt : integer;
begin
  cnt := 0;
  if ChMap1.Checked then inc(cnt);
  if ChMap2.Checked then inc(cnt);
  if ChMap3.Checked then inc(cnt);
  if ChMap4.Checked then inc(cnt);
  TreeView1.Items[cnt*3+1].Text :=cbbMap5.text;
end;

procedure TfrExportToJNX.cbbMapChange(Sender: TObject);
begin
  if ChMap1.Checked then begin
    EProductName.text := 'SAS Palnet';
    EMapName.text := cbbMap.text;
    TreeView1.Items[1].Text :=cbbMap.text;
  end;
end;

procedure TfrExportToJNX.cbbVersionChange(Sender: TObject);
begin
  if cbbVersion.ItemIndex = 1 then begin
    EZorder.visible := true;
    LZOrder.visible := true;
  end else begin
    EZorder.visible := false;
    LZOrder.visible := false;
  end;
end;

const
  ZoomIndexToScaleIndex: array [0..23] of integer = (
    0, 1, 2, 3, 4, 5, 6, 8,
    9, 11, 12, 14, 15, 17, 18, 20,
    21, 23, 24, 26, 27, 29, 30, 32
  );

procedure TfrExportToJNX.CbbZoom2Change(Sender: TObject);
begin
  cbbscale2.ItemIndex := ZoomIndexToScaleIndex[CbbZoom2.itemindex];
end;

procedure TfrExportToJNX.CbbZoom3Change(Sender: TObject);
begin
  cbbscale3.ItemIndex := ZoomIndexToScaleIndex[CbbZoom3.itemindex];
end;

procedure TfrExportToJNX.CbbZoom4Change(Sender: TObject);
begin
  cbbscale4.ItemIndex := ZoomIndexToScaleIndex[CbbZoom4.itemindex];
end;

procedure TfrExportToJNX.CbbZoom5Change(Sender: TObject);
begin
  cbbscale5.ItemIndex := ZoomIndexToScaleIndex[CbbZoom5.itemindex];
end;

procedure TfrExportToJNX.CbbZoomChange(Sender: TObject);
begin
  cbbscale.ItemIndex := ZoomIndexToScaleIndex[CbbZoom.itemindex];
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

procedure TfrExportToJNX.Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VActiveLayers: IMapTypeSet;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  if CbbZoom.Items.count=0 then begin
    for i:=1 to 24 do begin
      CbbZoom.Items.Add(inttostr(i));
    end;

    VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetStatic.GUID;
    VActiveLayers := FMainMapsConfig.GetActiveLayersSet.GetStatic;

    CbbZoom2.items := CbbZoom.Items;
    CbbZoom3.items := CbbZoom.Items;
    CbbZoom4.items := CbbZoom.Items;
    CbbZoom5.items := CbbZoom.Items;

    cbbscale2.items := cbbscale.Items;
    cbbscale3.items := cbbscale.Items;
    cbbscale4.items := cbbscale.Items;
    cbbscale5.items := cbbscale.Items;

    cbbMap.items.Clear;
    cbbMap2.items.Clear;
    cbbMap3.items.Clear;
    cbbMap4.items.Clear;
    cbbMap5.items.Clear;

    CbbZoom.ItemIndex := AZoom;
    CbbZoom2.ItemIndex := AZoom;
    CbbZoom3.ItemIndex := AZoom;
    CbbZoom4.ItemIndex := AZoom;
    CbbZoom5.ItemIndex := AZoom;

    cbbscale.ItemIndex := ZoomIndexToScaleIndex[AZoom];
    cbbscale2.ItemIndex := ZoomIndexToScaleIndex[AZoom];
    cbbscale3.ItemIndex := ZoomIndexToScaleIndex[AZoom];
    cbbscale4.ItemIndex := ZoomIndexToScaleIndex[AZoom];
    cbbscale5.ItemIndex := ZoomIndexToScaleIndex[AZoom];

  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.GUIConfig.Enabled) then begin
     if (not(VMapType.Abilities.IsLayer)) then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
      if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
     end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
  if cbbMap.ItemIndex=-1 then cbbMap.ItemIndex:=0;

  cbbMap2.Items := cbbMap.Items;
  cbbMap3.Items := cbbMap.Items;
  cbbMap4.Items := cbbMap.Items;
  cbbMap5.Items := cbbMap.Items;

  If cbbMap2.ItemIndex=-1 then cbbMap2.ItemIndex := cbbMap.ItemIndex;
  If cbbMap3.ItemIndex=-1 then cbbMap3.ItemIndex := cbbMap.ItemIndex;
  If cbbMap4.ItemIndex=-1 then cbbMap4.ItemIndex := cbbMap.ItemIndex;
  If cbbMap5.ItemIndex=-1 then cbbMap5.ItemIndex := cbbMap.ItemIndex;
  end;

  EMapName.text := cbbMap.text;
  EProductName.text := 'SAS Planet';
  EProductID.ItemIndex :=0;

  if cbbVersion.ItemIndex = -1 then cbbVersion.ItemIndex := 0;
  if cbbVersion.ItemIndex = 1 then begin
    EZorder.visible := true;
    LZOrder.visible := true;
  end else begin
    EZorder.visible := false;
    LZOrder.visible := false;
  end;
end;

procedure TfrExportToJNX.ChMap1Click(Sender: TObject);
var
 VItemNode, VParentNode : TTreeNode;
begin
  cbbMap.Enabled := ChMap1.Checked;
  EJpgQuality.Enabled := ChMap1.Checked;
  CbbZoom.Enabled := ChMap1.Checked;
  cbbscale.Enabled := ChMap1.Checked;
  ChMap2.Enabled := ChMap1.Checked;
  ChRecompress1.Enabled := ChMap1.Checked;

if  ChMap1.Checked then begin
    VParentNode := TreeView1.Items.AddFirst(nil, 'Level'+inttostr(1));
    TreeView1.Items.AddChild(VParentNode, cbbMap.text);
    TreeView1.Items.AddChild(VParentNode, '(c) '+EProductName.text);
  end else begin
    VItemNode := TreeView1.Items[0];
    TreeView1.Items.delete(VItemNode);

    ChMap2.Checked := False;
  end;
end;

procedure TfrExportToJNX.ChMap2Click(Sender: TObject);
var
 VItemNode, VParentNode : TTreeNode;
 cnt : integer;
begin
  cbbMap2.Enabled := ChMap2.Checked;
  EJpgQuality2.Enabled := ChMap2.Checked;
  CbbZoom2.Enabled := ChMap2.Checked;
  cbbscale2.Enabled := ChMap2.Checked;
  ChMap3.Enabled := ChMap2.Checked;
  ChRecompress2.Enabled := ChMap2.Checked;

cnt := 0;
if ChMap1.Checked then inc(cnt);
if ChMap2.Checked then begin
  if (TreeView1.Items.count >cnt*3) then begin
    VItemNode := TreeView1.Items[(cnt)*3];
    VParentNode := TreeView1.Items.insert(VItemNode, 'Level'+inttostr(2));
   end else
    if TreeView1.Items.Count=0 then
       VParentNode := TreeView1.Items.AddFirst(nil, 'Level'+inttostr(2)) else
       VParentNode := TreeView1.Items.Add(nil, 'Level'+inttostr(2));
    TreeView1.Items.AddChild(VParentNode, cbbMap2.text);
    TreeView1.Items.AddChild(VParentNode, '(c) '+EProductName.text);
  end else begin
    VItemNode := TreeView1.Items[cnt*3];
    TreeView1.Items.delete(VItemNode);

    ChMap3.Checked := False;
  end;
end;

procedure TfrExportToJNX.ChMap3Click(Sender: TObject);
var
 VItemNode, VParentNode : TTreeNode;
 cnt : integer;
begin
  cbbMap3.Enabled := ChMap3.Checked;
  EJpgQuality3.Enabled := ChMap3.Checked;
  CbbZoom3.Enabled := ChMap3.Checked;
  cbbscale3.Enabled := ChMap3.Checked;
  ChMap4.Enabled := ChMap3.Checked;
  ChRecompress3.Enabled := ChMap3.Checked;

cnt := 0;
if ChMap1.Checked then inc(cnt);
if ChMap2.Checked then inc(cnt);
if ChMap3.Checked then begin
  if (TreeView1.Items.count >cnt*3) then begin
    VItemNode := TreeView1.Items[(cnt)*3];
    VParentNode := TreeView1.Items.insert(VItemNode, 'Level'+inttostr(3));
   end else
    if TreeView1.Items.Count=0 then
       VParentNode := TreeView1.Items.AddFirst(nil, 'Level'+inttostr(3)) else
       VParentNode := TreeView1.Items.Add(nil, 'Level'+inttostr(3));
    TreeView1.Items.AddChild(VParentNode, cbbMap3.text);
    TreeView1.Items.AddChild(VParentNode, '(c) '+EProductName.text);
  end else begin
    VItemNode := TreeView1.Items[cnt*3];
    TreeView1.Items.delete(VItemNode);

    ChMap4.Checked := False;
  end;
end;

procedure TfrExportToJNX.ChMap4Click(Sender: TObject);
var
 VItemNode, VParentNode : TTreeNode;
 cnt : integer;
begin
  cbbMap4.Enabled := ChMap4.Checked;
  EJpgQuality4.Enabled := ChMap4.Checked;
  CbbZoom4.Enabled := ChMap4.Checked;
  cbbscale4.Enabled := ChMap4.Checked;
  ChMap5.Enabled := ChMap4.Checked;
  ChRecompress4.Enabled := ChMap4.Checked;

cnt := 0;
if ChMap1.Checked then inc(cnt);
if ChMap2.Checked then inc(cnt);
if ChMap3.Checked then inc(cnt);
if ChMap4.Checked then begin
  if (TreeView1.Items.count >cnt*3) then begin
    VItemNode := TreeView1.Items[(cnt)*3];
    VParentNode := TreeView1.Items.insert(VItemNode, 'Level'+inttostr(4));
   end else
    if TreeView1.Items.Count=0 then
       VParentNode := TreeView1.Items.AddFirst(nil, 'Level'+inttostr(4)) else
       VParentNode := TreeView1.Items.Add(nil, 'Level'+inttostr(4));
    TreeView1.Items.AddChild(VParentNode, cbbMap4.text);
    TreeView1.Items.AddChild(VParentNode, '(c) '+EProductName.text);
  end else begin
    VItemNode := TreeView1.Items[cnt*3];
    TreeView1.Items.delete(VItemNode);

    ChMap5.Checked := False;
  end;
end;

procedure TfrExportToJNX.ChMap5Click(Sender: TObject);
var
 VItemNode, VParentNode : TTreeNode;
 cnt : integer;
begin
  cbbMap5.Enabled := ChMap5.Checked;
  EJpgQuality5.Enabled := ChMap5.Checked;
  CbbZoom5.Enabled := ChMap5.Checked;
  cbbscale5.Enabled := ChMap5.Checked;
  ChRecompress5.Enabled := ChMap5.Checked;

cnt := 0;
if ChMap1.Checked then inc(cnt);
if ChMap2.Checked then inc(cnt);
if ChMap3.Checked then inc(cnt);
if ChMap4.Checked then inc(cnt);
if ChMap5.Checked then begin
  if (TreeView1.Items.count >cnt*3) then begin
    VItemNode := TreeView1.Items[(cnt)*3];
    VParentNode := TreeView1.Items.insert(VItemNode, 'Level'+inttostr(5));
   end else
    if TreeView1.Items.Count=0 then
       VParentNode := TreeView1.Items.AddFirst(nil, 'Level'+inttostr(5)) else
       VParentNode := TreeView1.Items.Add(nil, 'Level'+inttostr(5));
    TreeView1.Items.AddChild(VParentNode, cbbMap5.text);
    TreeView1.Items.AddChild(VParentNode, '(c) '+EProductName.text);
  end else begin
    VItemNode := TreeView1.Items[cnt*3];
    TreeView1.Items.delete(VItemNode);
  end;
end;

procedure TfrExportToJNX.PageControl1Change(Sender: TObject);
begin
  if PageControl1.TabIndex >=2 then begin
      MapsPanel.Visible := false;
  end else begin
      MapsPanel.Visible := true;
  end;


end;

function TfrExportToJNX.GetJNXVersion: Integer;
begin
  if cbbVersion.ItemIndex=0 then begin
    Result := 3;
  end else begin
    Result := 4;
  end;
end;

function TfrExportToJNX.GetJpgQuality: IStringListStatic;
var VJPGList: TStringList;

begin
  VJPGList := TStringList.Create;
  VJPGList.Clear;

  if ChMap1.Checked then  begin
    VJPGList.Add(inttostr(EJpgQuality.Value));
  end;
  if ChMap2.Checked then  begin
    VJPGList.Add(inttostr(EJpgQuality2.Value));
  end;
  if ChMap3.Checked then  begin
    VJPGList.Add(inttostr(EJpgQuality3.Value));
  end;
  if ChMap4.Checked then  begin
    VJPGList.Add(inttostr(EJpgQuality4.Value));
  end;
  if ChMap5.Checked then  begin
    VJPGList.Add(inttostr(EJpgQuality5.Value));
  end;

  Result := TStringListStatic.CreateWithOwn(VJPGList);
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
  Result := EMapName.Text;
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

function TfrExportToJNX.GetMapList: IMapTypeListStatic;
var
  VMaps: array of IMapType;
  i: integer;
begin
  i := 0;
  if ChMap1.Checked then  begin
    SetLength(VMaps, i+1);
    VMaps[i] := FFullMapsSet.GetMapTypeByGUID(TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]).Zmp.GUID);
    inc(i);
  end;
  if ChMap2.Checked then  begin
    SetLength(VMaps, i+1);
    VMaps[i] := FFullMapsSet.GetMapTypeByGUID(TMapType(cbbMap2.Items.Objects[cbbMap2.ItemIndex]).Zmp.GUID);
    inc(i);
  end;
  if ChMap3.Checked then  begin
    SetLength(VMaps, i+1);
    VMaps[i] := FFullMapsSet.GetMapTypeByGUID(TMapType(cbbMap3.Items.Objects[cbbMap3.ItemIndex]).Zmp.GUID);
    inc(i);
  end;
  if ChMap4.Checked then  begin
    SetLength(VMaps, i+1);
    VMaps[i] := FFullMapsSet.GetMapTypeByGUID(TMapType(cbbMap4.Items.Objects[cbbMap4.ItemIndex]).Zmp.GUID);
    inc(i);
  end;
  if ChMap5.Checked then  begin
    SetLength(VMaps, i+1);
    VMaps[i] := FFullMapsSet.GetMapTypeByGUID(TMapType(cbbMap5.Items.Objects[cbbMap5.ItemIndex]).Zmp.GUID);
  end;
  Result := TMapTypeListStatic.Create(VMaps);
end;

function TfrExportToJNX.GetScaleArray: TByteDynArray;
var
  VCount: Integer;
begin
  Result := nil;
  VCount := 0;
  if ChMap1.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := cbbscale.ItemIndex;
      Inc(VCount);
  end;
  if ChMap2.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := cbbscale2.ItemIndex;
      Inc(VCount);
  end;
  if ChMap3.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := cbbscale3.ItemIndex;
      Inc(VCount);
  end;
  if ChMap4.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := cbbscale4.ItemIndex;
      Inc(VCount);
  end;
  if ChMap5.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := cbbscale5.ItemIndex;
  end;

end;
function TfrExportToJNX.GetZoomArray: TByteDynArray;
var
  VCount: Integer;
begin
  Result := nil;
  VCount := 0;
  if ChMap1.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := CbbZoom.ItemIndex;
      Inc(VCount);
  end;
  if ChMap2.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := CbbZoom2.ItemIndex;
      Inc(VCount);
  end;
  if ChMap3.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := CbbZoom3.ItemIndex;
      Inc(VCount);
  end;
  if ChMap4.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := CbbZoom4.ItemIndex;
      Inc(VCount);
  end;
  if ChMap5.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := CbbZoom5.ItemIndex;
  end;
end;

function TfrExportToJNX.GetZOrder: Integer;
begin
  if cbbVersion.ItemIndex=0 then begin
    Result := 0;
  end else begin
    Result := EZorder.Value;
  end;
end;

function TfrExportToJNX.GetRecompress: TBooleanDynArray;
var
  VCount: Integer;
begin
  Result := nil;
  VCount := 0;
  if ChMap1.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := ChRecompress1.Checked;
      Inc(VCount);
  end;
  if ChMap2.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := ChRecompress2.Checked;
      Inc(VCount);
  end;
  if ChMap3.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := ChRecompress3.Checked;
      Inc(VCount);
  end;
  if ChMap4.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := ChRecompress4.Checked;
      Inc(VCount);
  end;
  if ChMap5.Checked then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := ChRecompress5.Checked;
  end;

end;

procedure TfrExportToJNX.ChRecompress1Click(Sender: TObject);
begin
  EJpgQuality.Enabled := ChRecompress1.Checked;
end;

procedure TfrExportToJNX.ChRecompress2Click(Sender: TObject);
begin
  EJpgQuality2.Enabled := ChRecompress2.Checked;
end;

procedure TfrExportToJNX.ChRecompress3Click(Sender: TObject);
begin
  EJpgQuality3.Enabled := ChRecompress3.Checked;
end;

procedure TfrExportToJNX.ChRecompress4Click(Sender: TObject);
begin
  EJpgQuality4.Enabled := ChRecompress4.Checked;
end;

procedure TfrExportToJNX.ChRecompress5Click(Sender: TObject);
begin
  EJpgQuality5.Enabled := ChRecompress5.Checked;
end;

end.
