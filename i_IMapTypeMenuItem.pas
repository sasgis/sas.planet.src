unit i_IMapTypeMenuItem;

interface

uses
  TB2Item,
  UMapType;

type
  IMapTypeMenuItem = interface
    ['{4168C52C-7A99-4CF2-AEEA-EE9710A272BD}']
    function GetMapType: TMapType;
    function GetMenuItem: TTBCustomItem;
  end;

  IMapTypeMenuItemFactory = interface
    ['{74E9D8BD-91D2-4A25-94D1-C707D068A7EC}']
    function CreateItem(AMap: TMapType): IMapTypeMenuItem;
  end;

implementation

end.
 