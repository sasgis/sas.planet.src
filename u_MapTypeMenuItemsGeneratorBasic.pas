unit u_MapTypeMenuItemsGeneratorBasic;

interface

uses
  Classes,
  TB2Item,
  TBX,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_MapTypeIconsList,
  u_MapType;

type
  TMapMenuGeneratorBasic = class
  private
    FGUIConfigList: IMapTypeGUIConfigList;
    FIconsList: IMapTypeIconsList;
    FRootMenu: TTBCustomItem;
    FMapsSet: IActiveMapsSet;
    FOnClick: TNotifyEvent;
    procedure ClearLists; virtual;
    procedure ProcessSubItemsCreate; virtual;
    procedure ProcessSubItemGUID(AGUID: TGUID); virtual;
    function CreateSubMenuItem(AName: string): TTBCustomItem; virtual;
    function GetParentMenuItem(AName: string): TTBCustomItem; virtual;
    function CreateMenuItem(AMapActive: IActiveMapSingle): TTBXCustomItem; virtual;
  public
    constructor Create(
      AGUIConfigList: IMapTypeGUIConfigList;
      AMapsSet: IActiveMapsSet;
      ARootMenu: TTBCustomItem;
      AOnClick: TNotifyEvent;
      AIconsList: IMapTypeIconsList
    );
    procedure BuildControls;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  i_GUIDListStatic,
  u_TBXSubmenuItemWithIndicator,
  u_ActiveMapTBXItem,
  u_ResStrings,
  u_GlobalState;

{ TMapMenuGeneratorBasic }

constructor TMapMenuGeneratorBasic.Create(
  AGUIConfigList: IMapTypeGUIConfigList;
  AMapsSet: IActiveMapsSet;
  ARootMenu: TTBCustomItem;
  AOnClick: TNotifyEvent;
  AIconsList: IMapTypeIconsList
);
begin
  FGUIConfigList := AGUIConfigList;
  FMapsSet := AMapsSet;
  FRootMenu := ARootMenu;
  FIconsList := AIconsList;
  FOnClick := AOnClick;
end;

function TMapMenuGeneratorBasic.CreateMenuItem(
  AMapActive: IActiveMapSingle): TTBXCustomItem;
var
  VGUID: TGUID;
  VMapType: TMapType;
begin
  Result := TActiveMapTBXItem.Create(FRootMenu, AMapActive);
  VMapType := nil;
  if AMapActive.GetMapType <> nil then begin
    VMapType := AMapActive.GetMapType.MapType;
  end;
  if VMapType <> nil then begin
    VGUID := VMapType.Zmp.GUID;
    Result.Caption := VMapType.GUIConfig.Name.Value;
  end else begin
    VGUID := CGUID_Zero;
    Result.Caption := SAS_STR_MiniMapAsMainMap;
  end;
  Result.ImageIndex := FIconsList.GetIconIndexByGUID(VGUID);
  Result.Tag := Integer(AMapActive);
  Result.OnClick := FOnClick;
end;

function TMapMenuGeneratorBasic.CreateSubMenuItem(
  AName: string): TTBCustomItem;
begin
  Result := TTBXSubmenuItemWithIndicator.Create(FRootMenu);
  Result.Caption := AName;
  Result.Images := FIconsList.GetImageList;
  Result.tag := -1;
end;

function TMapMenuGeneratorBasic.GetParentMenuItem(
  AName: string): TTBCustomItem;
var
  i: Integer;
begin
  if (AName = '') then begin
    Result := FRootMenu;
  end else begin
    Result := nil;
    for i := 0 to FRootMenu.Count - 1 do begin
      if SameText(FRootMenu.Items[i].Caption, AName) then begin
        Result := FRootMenu.Items[i];
      end;
    end;
    if Result = nil then begin
      Result := CreateSubMenuItem(AName);
      FRootMenu.Add(Result);
    end;
  end;
end;

procedure TMapMenuGeneratorBasic.BuildControls;
begin
  ClearLists;
  ProcessSubItemsCreate;
end;

procedure TMapMenuGeneratorBasic.ClearLists;
var
  i: integer;
begin
  for i := FRootMenu.Count - 1 downto 0 do begin
    if FRootMenu.Items[i].Tag <> 0 then begin
      FRootMenu.Items[i].Free;
    end;
  end;
end;

procedure TMapMenuGeneratorBasic.ProcessSubItemGUID(AGUID: TGUID);
var
  VActiveMap: IActiveMapSingle;
  VSubMenu: TTBCustomItem;
  VMenuItem: TTBXCustomItem;
  VSubMenuName: string;
  VMapType: TMapType;
  VEnabled: Boolean;
begin
  VActiveMap := FMapsSet.GetMapSingle(AGUID);
  if VActiveMap <> nil then begin
    VSubMenuName := '';
    VEnabled := True;
    if VActiveMap.GetMapType <> nil then begin
      if VActiveMap.GetMapType.MapType <> nil then begin
        VEnabled := VActiveMap.GetMapType.MapType.GUIConfig.Enabled;
        VSubMenuName := VActiveMap.GetMapType.MapType.GUIConfig.ParentSubMenu.Value;
      end;
    end;
    if VEnabled then begin
      VSubMenu := GetParentMenuItem(VSubMenuName);
      Assert(VSubMenu <> nil);
      VMenuItem := CreateMenuItem(VActiveMap);
      VSubMenu.Add(VMenuItem);
      VMapType := nil;
      if VActiveMap.GetMapType <> nil then begin
        VMapType:=VActiveMap.GetMapType.MapType;
      end;
      if (VMapType<>nil)and(VActiveMap.GetMapType.MapType.GUIConfig.Separator) then begin
        VSubMenu.Add(TTBSeparatorItem.Create(FRootMenu));
      end;
    end;
  end;
end;

procedure TMapMenuGeneratorBasic.ProcessSubItemsCreate;
var
  i: Integer;
  VStaticList: IGUIDListStatic;
begin
  ProcessSubItemGUID(CGUID_Zero);
  VStaticList := FGUIConfigList.OrderedMapGUIDList;
  for i := 0 to VStaticList.Count - 1 do begin
    ProcessSubItemGUID(VStaticList.Items[i]);
  end;
end;

end.
