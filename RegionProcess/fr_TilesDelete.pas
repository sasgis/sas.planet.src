unit fr_TilesDelete;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  Spin,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_CommonFormAndFrameParents;

type
  TfrTilesDelete = class(TFrame)
    cbbMap: TComboBox;
    seDelSize: TSpinEdit;
    chkDelBySize: TCheckBox;
    lblMap: TLabel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    pnlCenter: TPanel;
    lblStat: TLabel;
    flwpnlDelBySize: TFlowPanel;
    lblDelSize: TLabel;
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  public
    constructor Create(
      AOwner : TComponent;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
    procedure Init(AZoom: Byte);
  end;

implementation

uses
  i_GUIDListStatic,
  u_MapType;

{$R *.dfm}

{ TFrame3 }

constructor TfrTilesDelete.Create(AOwner: TComponent;
  AMainMapsConfig: IMainMapsConfig; AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList);
begin
  inherited Create(AOwner);
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
end;

procedure TfrTilesDelete.Init(AZoom: Byte);
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;

  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetSelectedGUID;
  cbbMap.items.Clear;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.StorageConfig.AllowDelete)and(VMapType.GUIConfig.Enabled) then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
      if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
end;

end.
