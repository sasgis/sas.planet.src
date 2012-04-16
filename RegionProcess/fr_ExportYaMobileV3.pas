unit fr_ExportYaMobileV3;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  CheckLst,
  Spin,
  ExtCtrls,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_CommonFormAndFrameParents;

type
  TfrExportYaMobileV3 = class(TFrame)
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetPath: TLabel;
    edtTargetPath: TEdit;
    btnSelectTargetPath: TButton;
    chkReplaseTiles: TCheckBox;
    pnlRight: TPanel;
    lblZooms: TLabel;
    chklstZooms: TCheckListBox;
    pnlMapsSelect: TPanel;
    grdpnlMaps: TGridPanel;
    lblMapCompress: TLabel;
    seMapCompress: TSpinEdit;
    seSatCompress: TSpinEdit;
    cbbHybr: TComboBox;
    cbbMap: TComboBox;
    cbbSat: TComboBox;
    lblSatCompress: TLabel;
    lblCompress: TLabel;
    lblHybr: TLabel;
    lblMap: TLabel;
    lblSat: TLabel;
    lblMaps: TLabel;
    procedure btnSelectTargetPathClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  public
    constructor Create(
      AOwner : TComponent;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
    procedure Init;
  end;

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  i_GUIDListStatic,
  u_ResStrings,
  u_MapType;

{$R *.dfm}

procedure TfrExportYaMobileV3.btnSelectTargetPathClick(Sender: TObject);
var
  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then begin
    edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath);
  end;
end;

constructor TfrExportYaMobileV3.Create(
  AOwner: TComponent;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList
);
begin
  inherited Create(AOwner);
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
end;

procedure TfrExportYaMobileV3.Init;
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
  cbbSat.items.Clear;
  cbbMap.items.Clear;
  cbbHybr.items.Clear;
  cbbSat.Items.AddObject(SAS_STR_No,nil);
  cbbMap.Items.AddObject(SAS_STR_No,nil);
  cbbHybr.Items.AddObject(SAS_STR_No,nil);
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.IsBitmapTiles)and(VMapType.GUIConfig.Enabled) then begin
      if (not(VMapType.Abilities.IsLayer)) then begin
        VAddedIndex := cbbSat.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
        if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
          cbbSat.ItemIndex:=VAddedIndex;
        end;
        VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
        if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
          cbbMap.ItemIndex:=VAddedIndex;
        end;
      end else if(VMapType.IsHybridLayer) then begin
        VAddedIndex := cbbHybr.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
        if (cbbHybr.ItemIndex=-1) then begin
          if FMainMapsConfig.GetActiveLayersSet.IsGUIDSelected(VGUID) then begin
            cbbHybr.ItemIndex:=VAddedIndex;
          end;
        end;
      end;
    end;
  end;
  if cbbSat.ItemIndex=-1 then cbbSat.ItemIndex:=1;
  if cbbMap.ItemIndex=-1 then cbbMap.ItemIndex:=0;
  if cbbHybr.ItemIndex=-1 then cbbHybr.ItemIndex:=0;
end;

end.
