unit u_MiniMapMenuItemsFactory;

interface

uses
  Classes,
  TB2Item,
  TBX,
  i_MapTypes,
  UMapType,
  i_IJclListenerNotifierLinksList,
  i_MapTypeIconsList,
  i_IActiveMapsConfig,
  i_IMapTypeMenuItem;

type
  TMiniMapMenuItemsFactory = class(TInterfacedObject, IMapTypeMenuItemFactory)
  private
    FRootMenu: TTBCustomItem;
    FIconsList: IMapTypeIconsList;
  protected
    function CreateSubMenuItem(AMapType: IMapType): TTBCustomItem; virtual;
    function GetParentMenuItem(AMapType: IMapType): TTBCustomItem; virtual;
    function CreateMenuItem(AMapActive: IActiveMapSingle): TTBXItem; virtual;
  protected
    function CreateItem(AMapActive: IActiveMapSingle): TTBCustomItem;
  public
    constructor Create(
      ARootMenu: TTBCustomItem;
      AIconsList: IMapTypeIconsList);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  UResStrings,
  u_MapTypeMenuItemBasic;

{ TMiniMapMenuItemsFactory }

constructor TMiniMapMenuItemsFactory.Create(
  ARootMenu: TTBCustomItem;
  AIconsList: IMapTypeIconsList);
begin
  FRootMenu := ARootMenu;
  FIconsList := AIconsList;
end;

function TMiniMapMenuItemsFactory.CreateItem(
  AMapActive: IActiveMapSingle): TTBCustomItem;
var
  VSubMenu: TTBCustomItem;
  VMenuItem: TTBXItem;
begin
  VSubMenu := GetParentMenuItem(AMapActive.GetMapType);
  VMenuItem := CreateMenuItem(AMapActive);
  VSubMenu.Add(VMenuItem);
  Result := VMenuItem;
end;

function TMiniMapMenuItemsFactory.CreateMenuItem(
  AMapActive: IActiveMapSingle): TTBXItem;
var
  VGUID: TGUID;
  VMapType: TMapType;
begin
  Result := TMiniMapTBXITem.Create(FRootMenu, AMapActive);
  VMapType := AMapActive.GetMapType.MapType;
  if VMapType <> nil then begin
    VGUID := VMapType.GUID;
    Result.Caption := VMapType.name;
  end else begin
    VGUID := CGUID_Zero;
    Result.Caption := SAS_STR_MiniMapAsMainMap;
  end;
  Result.ImageIndex := FIconsList.GetIconIndexByGUID(VGUID);
  Result.Tag := Integer(AMapActive);
end;

function TMiniMapMenuItemsFactory.CreateSubMenuItem(
  AMapType: IMapType): TTBCustomItem;
begin
  Result := TTBXSubmenuItem.Create(FRootMenu);
  Result.Caption := AMapType.MapType.ParentSubMenu;
  Result.Images := FIconsList.GetImageList;
end;

destructor TMiniMapMenuItemsFactory.Destroy;
begin
  inherited;
end;

function TMiniMapMenuItemsFactory.GetParentMenuItem(
  AMapType: IMapType): TTBCustomItem;
var
  i: Integer;
begin
  if (AMapType.MapType = nil) or (AMapType.MapType.ParentSubMenu = '') then begin
    Result := FRootMenu;
  end else begin
    Result := nil;
    for i := 0 to FRootMenu.Count - 1 do begin
      if SameText(FRootMenu.Items[i].Caption, AMapType.MapType.ParentSubMenu) then begin
        Result := FRootMenu.Items[i];
      end;
    end;
    if Result = nil then begin
      Result := CreateSubMenuItem(AMapType);
      FRootMenu.Add(Result);
    end;
  end;
end;

end.
