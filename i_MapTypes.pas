unit i_MapTypes;

interface

uses
  ActiveX,
  u_MapType;

type
  IMapType = interface
    ['{85957D2C-19D7-4F44-A183-F3679B2A5973}']
    function GetMapType: TMapType;
    property MapType: TMapType read GetMapType;

    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;
  end;

  IMapTypeSet = interface
    ['{45EF5080-01DC-4FE1-92E1-E93574439718}']
    function GetMapTypeByGUID(AGUID: TGUID): IMapType;
    function GetIterator: IEnumGUID;
  end;

implementation

end.
 