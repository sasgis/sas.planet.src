unit fr_ExportYaMobileV4;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  CheckLst,
  Spin,
  ExtCtrls,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents;

type
  TfrExportYaMobileV4 = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetPath: TLabel;
    edtTargetPath: TEdit;
    btnSelectTargetPath: TButton;
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
    chkReplaseTiles: TCheckBox;
    rgTileSize: TRadioGroup;
    procedure btnSelectTargetPathClick(Sender: TObject);
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
    function GetPath: string;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
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

procedure TfrExportYaMobileV4.btnSelectTargetPathClick(Sender: TObject);
var
  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then begin
    edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath);
  end;
end;

constructor TfrExportYaMobileV4.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList
);
begin
  inherited Create(ALanguageManager);
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
end;

function TfrExportYaMobileV4.GetPath: string;
begin
  Result := IncludeTrailingPathDelimiter(edtTargetPath.Text);
end;

function TfrExportYaMobileV4.GetZoomArray: TByteDynArray;
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

procedure TfrExportYaMobileV4.Init;
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VActiveLayers: IMapTypeSet;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  chklstZooms.Items.Clear;
  for i:=1 to 24 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;

  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetStatic.GUID;
  VActiveLayers := FMainMapsConfig.GetActiveLayersSet.GetStatic;
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
          if VActiveLayers.GetMapTypeByGUID(VGUID) <> nil then begin
            cbbHybr.ItemIndex:=VAddedIndex;
          end;
        end;
      end;
    end;
  end;
  if cbbSat.ItemIndex=-1 then cbbSat.ItemIndex:=1;
  if cbbMap.ItemIndex=-1 then cbbMap.ItemIndex:=0;
  if cbbHybr.ItemIndex=-1 then cbbHybr.ItemIndex:=0;
  if rgTileSize.ItemIndex = -1 then rgTileSize.ItemIndex := 0;
end;

end.
