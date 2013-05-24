unit fr_TilesDownload;

interface

uses
  Classes,
  Controls,
  ComCtrls,
  ExtCtrls,
  Forms,
  Menus,
  SysUtils,
  StdCtrls,
  TBX,
  TB2Item,
  TB2ExtItems,
  TBXExtItems,
  Windows,
  i_MapTypes,
  i_CoordConverterFactory,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_VectorItemsFactory,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_RegionProcessParamsFrame,
  u_MapType,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameTilesDownload = interface(IRegionProcessParamsFrameBase)
    ['{70B48431-5383-4CD2-A1EF-AF9291F6ABB0}']
    function GetIsStartPaused: Boolean;
    property IsStartPaused: Boolean read GetIsStartPaused;

    function GetIsIgnoreTne: Boolean;
    property IsIgnoreTne: Boolean read GetIsIgnoreTne;

    function GetIsReplace: Boolean;
    property IsReplace: Boolean read GetIsReplace;

    function GetIsReplaceIfDifSize: Boolean;
    property IsReplaceIfDifSize: Boolean read GetIsReplaceIfDifSize;

    function GetIsReplaceIfOlder: Boolean;
    property IsReplaceIfOlder: Boolean read GetIsReplaceIfOlder;

    function GetReplaceDate: TDateTime;
    property ReplaceDate: TDateTime read GetReplaceDate;
  end;

type
  TfrTilesDownload = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameOneZoom,
      IRegionProcessParamsFrameTilesDownload
    )
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
    chkStartPaused: TCheckBox;
    MainPopupMenu: TTBXPopupMenu;
    TBX_Layers: TTBXItem;
    TBX_Maps: TTBXItem;
    TBX_All: TTBXItem;
    TBX_active: TTBXItem;
    TBSeparatorItem1: TTBSeparatorItem;
    TBX_Filter: TTBXItem;
    TBX_AFilter: TTBXEditItem;
    procedure chkReplaceClick(Sender: TObject);
    procedure chkReplaceOlderClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
    procedure RefreshList(Sender: TObject);
    procedure ApplyFilter(Sender: TObject);
  private
    FVectorFactory: IVectorItemsFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FPolygLL: ILonLatPolygon;
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
    function GetZoom: Byte;
  private
    function GetIsStartPaused: Boolean;
    function GetIsIgnoreTne: Boolean;
    function GetIsReplace: Boolean;
    function GetIsReplaceIfDifSize: Boolean;
    function GetIsReplaceIfOlder: Boolean;
    function GetReplaceDate: TDateTime;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorFactory: IVectorItemsFactory;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
  end;

implementation

uses
  StrUtils,
  t_GeoTypes,
  i_GUIDListStatic,
  i_VectorItemProjected,
  u_GeoFun,
  u_ResStrings;

{$R *.dfm}

procedure TfrTilesDownload.cbbZoomChange(Sender: TObject);
var
  numd:int64 ;
  Vmt: TMapType;
  VZoom: byte;
  VPolyLL: ILonLatPolygon;
  VProjected: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
begin
  if cbbMap.ItemIndex >= 0 then begin
    Vmt := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end else begin
    Vmt := nil;
  end;

  if Vmt <> nil then begin
    VZoom := cbbZoom.ItemIndex;
    Vmt.GeoConvert.CheckZoom(VZoom);
    VPolyLL := FPolygLL;
    if VPolyLL <> nil then begin
      VProjected :=
        FVectorFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(Vmt.GeoConvert, VZoom),
          VPolyLL
        );
      if VProjected.Count > 0 then begin
        VLine := VProjected.Item[0];
        VBounds := VLine.Bounds;
        VPixelRect := RectFromDoubleRect(VBounds, rrOutside);
        VTileRect := Vmt.GeoConvert.PixelRect2TileRect(VPixelRect, VZoom);
        numd := (VTileRect.Right - VTileRect.Left);
        numd := numd * (VTileRect.Bottom - VTileRect.Top);
        lblStat.Caption :=
          SAS_STR_filesnum+': '+
          inttostr(VTileRect.Right - VTileRect.Left)+'x'+
          inttostr(VTileRect.Bottom - VTileRect.Top)+
          '('+inttostr(numd)+')' +
          ', '+SAS_STR_Resolution + ' ' +
          inttostr(VPixelRect.Right - VPixelRect.Left)+'x'+
          inttostr(VPixelRect.Bottom - VPixelRect.Top);
      end;
    end;
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

constructor TfrTilesDownload.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorFactory: IVectorItemsFactory;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList
);
begin
  inherited Create(ALanguageManager);
  FProjectionFactory := AProjectionFactory;
  FVectorFactory := AVectorFactory;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
end;

function TfrTilesDownload.GetIsIgnoreTne: Boolean;
begin
  Result := chkTryLoadIfTNE.Checked
end;

function TfrTilesDownload.GetIsReplace: Boolean;
begin
  Result := chkReplace.Checked;
end;

function TfrTilesDownload.GetIsReplaceIfDifSize: Boolean;
begin
  Result := chkReplaceIfDifSize.Checked;
end;

function TfrTilesDownload.GetIsReplaceIfOlder: Boolean;
begin
  Result := chkReplaceOlder.Checked;
end;

function TfrTilesDownload.GetIsStartPaused: Boolean;
begin
  Result := chkStartPaused.Checked;
end;

function TfrTilesDownload.GetMapType: TMapType;
begin
  Result := nil;
  if cbbMap.ItemIndex >= 0 then begin
    Result := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end;
end;

function TfrTilesDownload.GetReplaceDate: TDateTime;
begin
  Result := dtpReplaceOlderDate.DateTime;
end;

function TfrTilesDownload.GetZoom: Byte;
begin
  if cbbZoom.ItemIndex < 0 then begin
    cbbZoom.ItemIndex := 0;
  end;
  Result := cbbZoom.ItemIndex;
end;

procedure TfrTilesDownload.Init(const AZoom: Byte; const APolygon: ILonLatPolygon);
var
  i: integer;
begin
  FPolygLL := APolygon;
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;
  cbbMap.items.Clear;
  RefreshList(TBX_All); // set items
  dtpReplaceOlderDate.Date:=now;
  cbbZoomChange(nil);
end;

procedure TfrTilesDownload.ApplyFilter(Sender: TObject);
begin
 RefreshList(TBX_Filter);
end;

procedure TfrTilesDownload.RefreshList(Sender: TObject);
var
  VMode: Integer; // 1 All  2 Maps  3 Layers  4 Active   5 Filter
  VCurNewIndex: Integer;
  VActiveMapGUID: TGUID;
  i: integer;
  VNewMapType: TMapType;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
  VAdd: Boolean;
  VLayers: IMapTypeSet;
  VMapName: string;
  VFilter: string;
begin
  VMode := TTBXItem(Sender).Tag;
  TTBXItem(Sender).checked := True;
  VCurNewIndex := 0;
  VLayers := nil;
  VFilter := AnsiUpperCase(TBX_AFilter.Text);
  // get active map
  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetStatic.GUID;
  // refresh list
  cbbMap.items.BeginUpdate;
  try
    cbbMap.items.Clear;
    VGUIDList := FGUIConfigList.OrderedMapGUIDList;
    for i := 0 to VGUIDList.Count-1 do begin
      VGUID := VGUIDList.Items[i];
      VNewMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
      // check if allow to download
      if (VNewMapType.TileDownloadSubsystem.State.GetStatic.Enabled) then
      if (VNewMapType.GUIConfig.Enabled) then begin
        // check if allow to add map to list
        case VMode of
          1: begin
            // all maps
            VAdd := True;
          end;
          2: begin
            // only maps
            VAdd := (not VNewMapType.Abilities.IsLayer);
          end;
          3: begin
            // only layers
            VAdd := (VNewMapType.Abilities.IsLayer);
            // update layers list
            if (nil=VLayers) then begin
               VLayers := FMainMapsConfig.GetActiveLayersSet.GetStatic;
            end;
            // select first active layer
            if (VLayers.GetMapTypeByGUID(VGUID) <> nil) and (VCurNewIndex = 0) then
            begin
              VCurNewIndex := cbbMap.Items.Count;
            end;
          end;
          4: begin
            // only visible items: main map or visible layer
            if VNewMapType.Abilities.IsLayer then begin
              if (nil=VLayers) then begin
                VLayers := FMainMapsConfig.GetActiveLayersSet.GetStatic;
              end;
              VAdd := VLayers.GetMapTypeByGUID(VGUID) <> nil
            end else begin
                VAdd := IsEqualGUID(VActiveMapGUID, VGUID);
            end;
          end;
          5: begin // Filter by name
            if VFilter <> '' then begin
              VMapName := AnsiUpperCase(FFullMapsSet.GetMapTypeByGUID(VGUID).MapType.GUIConfig.Name.Value);
              if posex(VFilter,VMapName) <> 0 then begin
                VAdd := True
              end else begin
                VAdd := False
              end
            end else begin
              VAdd := true;
            end;
            end else begin VAdd := False;
          end;
        end;
        if VAdd then begin
          VAddedIndex := cbbMap.Items.AddObject(VNewMapType.GUIConfig.Name.Value, VNewMapType);
          // select current map by default
          if IsEqualGUID(VNewMapType.Zmp.GUID, VActiveMapGUID) then begin
           cbbMap.ItemIndex:=VAddedIndex;
          end;
        end;
      end;
    end;
    // if not selected - select some item
    if (cbbMap.Items.Count > 0) then begin
      if (cbbMap.ItemIndex < 0) and (VCurNewIndex>=0) then begin
        cbbMap.ItemIndex := VCurNewIndex;
      end;
      // last chance
      if (cbbMap.ItemIndex < 0) then begin
        cbbMap.ItemIndex := 0;
      end;
    end;
  finally
    cbbMap.items.EndUpdate;
  end;
end;
end.
