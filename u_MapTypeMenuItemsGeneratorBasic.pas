unit u_MapTypeMenuItemsGeneratorBasic;

interface

uses
  Classes,
  TB2Item,
  i_MapTypes,
  i_IActiveMapsConfig,
  i_IMapTypeMenuItem,
  i_IMapTypeMenuItmesList,
  u_MapTypeMenuItmesList,
  UMapType;

type
  TMapMenuGeneratorBasic = class
  protected
    FList: IMapTypeList;
    FRootMenu: TTBCustomItem;
    FItemsFactory: IMapTypeMenuItemFactory;
    FMapsSet: IActiveMapsSet;
    procedure ClearLists; virtual;
    procedure ProcessSubItemsCreate; virtual;
  public
    constructor Create(AList: IMapTypeList; ARootMenu: TTBCustomItem; AItemsFactory: IMapTypeMenuItemFactory);
    procedure BuildControls;
  end;

implementation

uses
  SysUtils,
  u_GlobalState;

{ TMapMenuGeneratorBasic }

procedure TMapMenuGeneratorBasic.BuildControls;
begin
//  VList := TMapTypeMenuItmesList.Create;
//  Result := VList;
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

constructor TMapMenuGeneratorBasic.Create(AList: IMapTypeList;
  ARootMenu: TTBCustomItem; AItemsFactory: IMapTypeMenuItemFactory);
begin
  FList := AList;
  FRootMenu := ARootMenu;
  FItemsFactory := AItemsFactory;
end;

procedure TMapMenuGeneratorBasic.ProcessSubItemsCreate;
var
  i: Integer;
  VMapType: TMapType;
  VGUID: TGUID;
  VMap: IActiveMapSingle;
begin
  for i := 0 to GState.MapType.Count - 1 do begin
    VMapType := GState.MapType[i];
    VGUID := VMapType.GUID;
    VMap := FMapsSet.GetMapSingle(VGUID);
    if VMap <> nil then begin
//      AList.Add(FItemsFactory.CreateItem(VMap));
    end;
  end;
end;

end.
