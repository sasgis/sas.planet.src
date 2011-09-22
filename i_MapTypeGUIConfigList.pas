unit i_MapTypeGUIConfigList;

interface

uses
  i_GUIDListStatic,
  i_MapTypeHotKeyListStatic,
  i_ConfigDataElement;

type
  IMapTypeGUIConfigList = interface(IConfigDataElement)
    ['{6EAFA879-3A76-40CA-89A7-598D45E2C92E}']
    function GetOrderedMapGUIDList: IGUIDListStatic;
    property OrderedMapGUIDList: IGUIDListStatic read GetOrderedMapGUIDList;

    function GetHotKeyList: IMapTypeHotKeyListStatic;
    property HotKeyList: IMapTypeHotKeyListStatic read GetHotKeyList;
  end;

implementation

end.
