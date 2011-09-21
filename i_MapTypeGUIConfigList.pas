unit i_MapTypeGUIConfigList;

interface

uses
  i_GUIDListStatic,
  i_ConfigDataElement;

type
  IMapTypeGUIConfigList = interface(IConfigDataElement)
    ['{6EAFA879-3A76-40CA-89A7-598D45E2C92E}']
    function GetOrderedMapGUIDList: IGUIDListStatic;
    property OrderedMapGUIDList: IGUIDListStatic read GetOrderedMapGUIDList;
  end;

implementation

end.
