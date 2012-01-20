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
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_CommonFormAndFrameParents, Spin;

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
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
    procedure versionselect(Sender: TObject);
    procedure cbbMapChange(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  public
    constructor CreateForFileType(
      AOwner : TComponent;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList;
      AFileFilters: string;
      AFileExtDefault: string
    );
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
  EMapName.text := cbbMap.text;
end;

procedure TfrExportToJNX.chkAllZoomsClick(Sender: TObject);
var
  i: byte;
begin
  for i:=0 to chklstZooms.Count-1 do begin
    chklstZooms.Checked[i] := TCheckBox(sender).Checked;
  end;
end;

constructor TfrExportToJNX.CreateForFileType(
  AOwner : TComponent;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList;
  AFileFilters: string;
  AFileExtDefault: string
);
begin
  inherited Create(AOwner);
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
  chklstZooms.Items.Clear;
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
