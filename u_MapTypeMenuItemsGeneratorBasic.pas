unit u_MapTypeMenuItemsGeneratorBasic;

interface

uses
  Classes,
  TB2Item,
  TBX,
  i_MapTypes,
  i_IActiveMapsConfig,
  i_MapTypeIconsList,
  UMapType;

type
  TMapMenuGeneratorBasic = class
  private
    FIconsList: IMapTypeIconsList;
    FRootMenu: TTBCustomItem;
    FMapsSet: IActiveMapsSet;
    FOnClick: TNotifyEvent;
    FShortCut: boolean;
    procedure ClearLists; virtual;
    procedure ProcessSubItemsCreate; virtual;
    procedure ProcessSubItemGUID(AGUID: TGUID); virtual;
    function CreateSubMenuItem(AName: string): TTBCustomItem; virtual;
    function GetParentMenuItem(AName: string): TTBCustomItem; virtual;
    function CreateMenuItem(AMapActive: IActiveMapSingle): TTBXCustomItem; virtual;
  public
    constructor Create(
      AMapsSet: IActiveMapsSet;
      ARootMenu: TTBCustomItem;
      AOnClick: TNotifyEvent;
      AIconsList: IMapTypeIconsList;
      AShortCut: boolean
    );
    procedure BuildControls;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  u_ActiveMapTBXItem,
  UResStrings,
  u_GlobalState;

{ TMapMenuGeneratorBasic }

constructor TMapMenuGeneratorBasic.Create(
  AMapsSet: IActiveMapsSet;
  ARootMenu: TTBCustomItem;
  AOnClick: TNotifyEvent;
  AIconsList: IMapTypeIconsList;
  AShortCut: boolean
);
begin
  FMapsSet := AMapsSet;
  FRootMenu := ARootMenu;
  FIconsList := AIconsList;
  FOnClick := AOnClick;
  FShortCut:=AShortCut;
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
    VGUID := VMapType.GUID;
    Result.Caption := VMapType.name;
    if FShortCut then begin
      Result.ShortCut:= VMapType.HotKey;
    end;
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
  Result := TTBXSubmenuItem.Create(FRootMenu);
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
begin
  VActiveMap := FMapsSet.GetMapSingle(AGUID);
  if VActiveMap <> nil then begin
    VSubMenuName := '';
    if VActiveMap.GetMapType <> nil then begin
      if VActiveMap.GetMapType.MapType <> nil then begin
        VSubMenuName := VActiveMap.GetMapType.MapType.ParentSubMenu;
      end;
    end;
    VSubMenu := GetParentMenuItem(VSubMenuName);
    Assert(VSubMenu <> nil);
    VMenuItem := CreateMenuItem(VActiveMap);
    VSubMenu.Add(VMenuItem);
  end;
end;

procedure TMapMenuGeneratorBasic.ProcessSubItemsCreate;
var
  i: Integer;
begin
  ProcessSubItemGUID(CGUID_Zero);
  for i := 0 to GState.MapType.Count - 1 do begin
    if GState.MapType[i].Enabled then begin
      ProcessSubItemGUID(GState.MapType[i].GUID);
    end;
  end;
end;

end.
