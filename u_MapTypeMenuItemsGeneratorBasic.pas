unit u_MapTypeMenuItemsGeneratorBasic;

interface

uses
  Classes,
  TB2Item,
  i_MapTypes,
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
    procedure ClearLists; virtual;
    procedure ProcessSubItemsCreate(AList: TMapTypeMenuItmesList); virtual;
  public
    function BuildControls: IMapTypeMenuItmesList;
    property List: IMapTypeList read FList write FList;
    property RootMenu: TTBCustomItem read FRootMenu write FRootMenu;
    property ItemsFactory: IMapTypeMenuItemFactory read FItemsFactory write FItemsFactory;
  end;

implementation

uses
  SysUtils,
  u_GlobalState;

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

procedure TMapMenuGeneratorBasic.ProcessSubItemsCreate(AList: TMapTypeMenuItmesList);
var
  i: Integer;
  VMapType: TMapType;
  VGUID: TGUID;
begin
  for i := 0 to Length(GState.MapType) - 1 do begin
    VMapType := GState.MapType[i];
    VGUID := VMapType.GUID;
    if FList.GetMapTypeByGUID(VGUID) <> nil then begin
      AList.Add(FItemsFactory.CreateItem(VMapType));
    end;
  end;
end;

end.
