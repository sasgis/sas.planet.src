unit i_MapTypes;

interface

uses
  ActiveX,
  UMapType;

type
  IMapType = interface
    ['{85957D2C-19D7-4F44-A183-F3679B2A5973}']
    function GetMapType: TMapType;
    property MapType: TMapType read GetMapType;

    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;
  end;

  IMapTypeFactory = interface
    ['{80DDF8C1-5B2A-4939-A791-C6B6D5CD7028}']
    function CreateItem(AMap: TMapType): IMapType;
  end;

  IMapTypeList = interface
    ['{45EF5080-01DC-4FE1-92E1-E93574439718}']
    function GetMapTypeByGUID(AGUID: TGUID): IMapType;
    function GetIterator: IEnumGUID;
  end;

  IMapTypeListFactory = interface
    ['{9A89A8AF-ABF9-4CA6-ABF7-293A20071323}']
    function CreateList: IMapTypeList;
  end;

implementation

end.
 