unit u_MapTypeMenuItemsGeneratorBasic;

interface

uses
  Classes,
  ImgList,
  TB2Item,
  TBX,
  i_MapTypes,
  i_IMapTypeMenuItem,
  i_IMapTypeMenuItmesList,
  i_IActiveMapsConfig,
  u_MapTypeMenuItmesList,
  UMapType;

type
  TMapMenuGeneratorBasic = class
  protected
    FList: IMapTypeList;
    FRootMenu: TTBCustomItem;
    FItemOnAdjustFont: TAdjustFontEvent;
    FItemOnClick: TNotifyEvent;
    FImages: TCustomImageList;
    FItemsFactory: IMapTypeMenuItemFactory;
    FMapsActive: IActiveMapsConfig;
    procedure ClearLists; virtual;
    procedure ProcessSubItemsCreate(AList: TMapTypeMenuItmesList); virtual;
    function CreateSubMenuItem(AMapType: TMapType): TTBCustomItem; virtual;
    function CreateItem(AMapType: TMapType; AMapIndex: Integer): TTBCustomItem; virtual;
    function GetParentMenuItem(AMapType: TMapType; AGroupsList: TStringList): TTBCustomItem; virtual;
  public
    function BuildControls: IMapTypeMenuItmesList;
    property List: IMapTypeList read FList write FList;
    property RootMenu: TTBCustomItem read FRootMenu write FRootMenu;
    property ItemOnAdjustFont: TAdjustFontEvent read FItemOnAdjustFont write FItemOnAdjustFont;
    property ItemOnClick: TNotifyEvent read FItemOnClick write FItemOnClick;
    property Images: TCustomImageList read FImages write FImages;
    property ItemsFactory: IMapTypeMenuItemFactory read FItemsFactory write FItemsFactory;
    property MapsActive: IActiveMapsConfig read FMapsActive write FMapsActive;
  end;

implementation

uses
  SysUtils,
  u_GlobalState,
  u_MapTypeMenuItemBasic;

{ TMapMenuGeneratorBasic }

function TMapMenuGeneratorBasic.BuildControls: IMapTypeMenuItmesList;
var
  VList: TMapTypeMenuItmesList;
begin
  VList := TMapTypeMenuItmesList.Create;
  Result := VList;
  ClearLists;
  ProcessSubItemsCreate(VList);
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

function TMapMenuGeneratorBasic.CreateItem(
  AMapType: TMapType; AMapIndex: Integer): TTBCustomItem;
var
  VItem: TTBXItem;
begin
  VItem := TTBXItem.Create(FRootMenu);
  VItem.ShortCut:=AMapType.HotKey;
  VItem.ImageIndex:= AMapIndex;
  VItem.Caption:=AMapType.name;
  VItem.OnAdjustFont:=FItemOnAdjustFont;
  VItem.OnClick:=FItemOnClick;
  VItem.Tag := Integer(AMapType);
  Result := VItem;
end;

function TMapMenuGeneratorBasic.CreateSubMenuItem(
  AMapType: TMapType): TTBCustomItem;
begin
  Result := TTBXSubmenuItem.Create(FRootMenu);
  Result.Caption := AMapType.ParentSubMenu;
  Result.Images := FImages;
  Result.Tag := Integer(AMapType);
end;

function TMapMenuGeneratorBasic.GetParentMenuItem(
  AMapType: TMapType; AGroupsList: TStringList): TTBCustomItem;
var
  VGroupIndex: Integer;
begin
  if AMapType.ParentSubMenu='' then begin
    Result := FRootMenu;
  end else begin
    Result := nil;
    if AGroupsList.Find(AMapType.ParentSubMenu, VGroupIndex) then begin
      Result := TTBCustomItem(AGroupsList.Objects[VGroupIndex]);
    end;
    if Result = nil then begin
      Result := CreateSubMenuItem(AMapType);
      FRootMenu.Add(Result);
      AGroupsList.AddObject(AMapType.ParentSubMenu, Result);
    end;
  end;
end;

procedure TMapMenuGeneratorBasic.ProcessSubItemsCreate(AList: TMapTypeMenuItmesList);
var
  i: Integer;
  VGroupsList: TStringList;
  VMapType: TMapType;
  VMenuItem: TTBCustomItem;
  VSubMenu: TTBCustomItem;
  VGUID: TGUID;
begin
  VGroupsList := TStringList.Create;
  VGroupsList.Sorted := True;
  VGroupsList.Duplicates := dupIgnore;
  try
    for i := 0 to Length(GState.MapType) - 1 do begin
      VMapType := GState.MapType[i];
      VGUID := VMapType.GUID;
      if FList.GetMapTypeByGUID(VGUID) <> nil then begin
        VSubMenu := GetParentMenuItem(VMapType, VGroupsList);
        VMenuItem := CreateItem(VMapType, i);
        VSubMenu.Add(VMenuItem);
        AList.Add(TMapTypeMenuItemBasic.Create(FMapsActive, VMapType, VMenuItem));
      end;
    end;
  finally
    FreeAndNil(VGroupsList);
  end;
end;

end.
