unit u_MiniMapMenuItemsFactory;

interface

uses
  Classes,
  ImgList,
  TB2Item,
  TBX,
  UMapType,
  i_MapTypeIconsList,
  i_IActiveMapsConfig,
  i_IMapTypeMenuItem;

type
  TMiniMapMenuItemsFactory = class(TInterfacedObject, IMapTypeMenuItemFactory)
  private
    FRootMenu: TTBCustomItem;
    FItemOnAdjustFont: TAdjustFontEvent;
    FIconsList: IMapTypeIconsList;
    FMapsActive: IActiveMapWithHybrConfig;
  protected
    function CreateSubMenuItem(AMapType: TMapType): TTBCustomItem; virtual;
    function GetParentMenuItem(AMapType: TMapType): TTBCustomItem; virtual;
    function CreateMenuItem(AMapType: TMapType): TTBCustomItem; virtual;
    function CreateItem(AMap: TMapType): IMapTypeMenuItem;
  public
    constructor Create(
      AMapsActive: IActiveMapWithHybrConfig;
      ARootMenu: TTBCustomItem;
      AItemOnAdjustFont: TAdjustFontEvent;
      AIconsList: IMapTypeIconsList);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_MapTypeMenuItemBasic;

{ TMiniMapMenuItemsFactory }

constructor TMiniMapMenuItemsFactory.Create(
  AMapsActive: IActiveMapWithHybrConfig;
  ARootMenu: TTBCustomItem;
  AItemOnAdjustFont: TAdjustFontEvent;
  AIconsList: IMapTypeIconsList);
begin
  FMapsActive := AMapsActive;
  FRootMenu := ARootMenu;
  FItemOnAdjustFont := AItemOnAdjustFont;
  FIconsList := AIconsList;
end;

function TMiniMapMenuItemsFactory.CreateItem(
  AMap: TMapType): IMapTypeMenuItem;
var
  VSubMenu: TTBCustomItem;
  VMenuItem: TTBCustomItem;
begin
  VSubMenu := GetParentMenuItem(AMap);
  VMenuItem := CreateMenuItem(AMap);
  VSubMenu.Add(VMenuItem);
  Result := TMapTypeMenuItemBasic.Create(FMapsActive, AMap, VMenuItem);
end;

function TMiniMapMenuItemsFactory.CreateMenuItem(
  AMapType: TMapType): TTBCustomItem;
var
  VItem: TTBXItem;
begin
  VItem := TTBXItem.Create(FRootMenu);
  VItem.ImageIndex:= FIconsList.GetMapTypeByGUID(AMapType.GUID).GetIconIndex;
  VItem.Caption:=AMapType.name;
  VItem.OnAdjustFont:=FItemOnAdjustFont;
  VItem.Tag := Integer(AMapType);
  Result := VItem;
end;

function TMiniMapMenuItemsFactory.CreateSubMenuItem(
  AMapType: TMapType): TTBCustomItem;
begin
  Result := TTBXSubmenuItem.Create(FRootMenu);
  Result.Caption := AMapType.ParentSubMenu;
  Result.Images := FIconsList.GetImageList;
  Result.Tag := Integer(AMapType);
end;

destructor TMiniMapMenuItemsFactory.Destroy;
begin
  FMapsActive := nil;
  inherited;
end;

function TMiniMapMenuItemsFactory.GetParentMenuItem(
  AMapType: TMapType): TTBCustomItem;
var
  i: Integer;
begin
  if AMapType.ParentSubMenu='' then begin
    Result := FRootMenu;
  end else begin
    Result := nil;
    for i := 0 to FRootMenu.Count - 1 do begin
      if SameText(FRootMenu.Items[i].Caption, AMapType.ParentSubMenu) then begin
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
