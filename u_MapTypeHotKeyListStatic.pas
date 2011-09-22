unit u_MapTypeHotKeyListStatic;

interface

uses
  Classes,
  i_MapTypes,
  i_MapTypeHotKeyListStatic,
  i_IDList;

type
  TMapTypeHotKeyListStatic = class(TInterfacedObject, IMapTypeHotKeyListStatic)
  private
    FList: IIDInterfaceList;
  protected
    function GetMapTypeGUIDByHotKey(AHotKey: TShortCut): IMapType;
  public
    constructor Create(
      AMapsSet: IMapTypeSet
    );
  end;

implementation

uses
  ActiveX,
  u_IDInterfaceList;

{ TMapTypeHotKeyListStatic }

constructor TMapTypeHotKeyListStatic.Create(AMapsSet: IMapTypeSet);
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  VGetCount: Cardinal;
  VMap: IMapType;
  VHotKey: TShortCut;
begin
  FList := TIDInterfaceList.Create(False);
  VEnum := AMapsSet.GetIterator;
  while VEnum.Next(1, VGUID, VGetCount) = S_OK do begin
    VMap := AMapsSet.GetMapTypeByGUID(VGUID);
    VHotKey := VMap.MapType.GUIConfig.HotKey;
    if VHotKey <> 0 then begin
      FList.Add(VHotKey, VMap);
    end;
  end;
end;

function TMapTypeHotKeyListStatic.GetMapTypeGUIDByHotKey(
  AHotKey: TShortCut): IMapType;
begin
  Result := IMapType(FList.GetByID(AHotKey));
end;

end.
