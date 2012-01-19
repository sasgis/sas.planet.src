unit fr_TilesDownload;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  t_GeoTypes,
  i_MapTypes,
  i_VectorItemLonLat,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_CommonFormAndFrameParents;

type
  TfrTilesDownload = class(TFrame)
    lblZoom: TLabel;
    lblStat: TLabel;
    chkReplace: TCheckBox;
    chkReplaceIfDifSize: TCheckBox;
    chkReplaceOlder: TCheckBox;
    dtpReplaceOlderDate: TDateTimePicker;
    cbbMap: TComboBox;
    cbbZoom: TComboBox;
    chkTryLoadIfTNE: TCheckBox;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    pnlMain: TPanel;
    pnlTileReplaceCondition: TPanel;
    pnlReplaceOlder: TPanel;
    lblReplaceOlder: TLabel;
    lblMap: TLabel;
    Bevel1: TBevel;
    procedure chkReplaceClick(Sender: TObject);
    procedure chkReplaceOlderClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
  private
    FPolygLL: ILonLatPolygon;
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
    procedure Init(AZoom: Byte; APolygLL: ILonLatPolygon);
  end;

implementation

uses
  i_GUIDListStatic,
  u_GeoFun,
  u_ResStrings,
  u_MapType;

{$R *.dfm}

procedure TfrTilesDownload.cbbZoomChange(Sender: TObject);
var
  polyg:TArrayOfPoint;
  VRect: TRect;
  numd:int64 ;
  Vmt: TMapType;
  VZoom: byte;
  VPolyLL: ILonLatPolygon;
  VLen: Integer;
begin
  if cbbMap.ItemIndex >= 0 then begin
    Vmt := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
    VZoom := cbbZoom.ItemIndex;
    VPolyLL := FPolygLL;
    VLen := VPolyLL.Item[0].Count;
    Vmt.GeoConvert.CheckZoom(VZoom);
    SetLength(Polyg, VLen);
    Vmt.GeoConvert.LonLatArray2PixelArray(VPolyLL.Item[0].Points, VLen, @Polyg[0], VZoom);
    numd:=GetDwnlNum(VRect, @Polyg[0], VLen,true);
    lblStat.Caption:=SAS_STR_filesnum+': '+inttostr((VRect.Right-VRect.Left)div 256+1)+'x'
                    +inttostr((VRect.Bottom-VRect.Top)div 256+1)+'('+inttostr(numd)+')';
    GetMinMax(VRect, @Polyg[0], VLen,false);
    lblStat.Caption:=lblStat.Caption+', '+SAS_STR_Resolution+' '+inttostr(VRect.Right-VRect.Left)+'x'
                  +inttostr(VRect.Bottom-VRect.Top);
  end;
end;

procedure TfrTilesDownload.chkReplaceClick(Sender: TObject);
var
  VEnabled: Boolean;
begin
  VEnabled := chkReplace.Checked;
  chkReplaceIfDifSize.Enabled := VEnabled;
  chkReplaceOlder.Enabled := VEnabled;
  chkReplaceOlderClick(chkReplaceOlder);
end;

procedure TfrTilesDownload.chkReplaceOlderClick(Sender: TObject);
begin
  dtpReplaceOlderDate.Enabled := chkReplaceOlder.Enabled and chkReplaceOlder.Checked;
end;

constructor TfrTilesDownload.Create(AOwner: TComponent;
  AMainMapsConfig: IMainMapsConfig; AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList);
begin
  inherited Create(AOwner);
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
end;

procedure TfrTilesDownload.Init(AZoom: Byte; APolygLL: ILonLatPolygon);
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  FPolygLL := APolygLL;
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
    if (VMapType.TileDownloadSubsystem.State.GetStatic.Enabled)and(VMapType.GUIConfig.Enabled) then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
      if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
  dtpReplaceOlderDate.Date:=now;
  cbbZoomChange(nil);
end;

end.
