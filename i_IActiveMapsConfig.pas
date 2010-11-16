unit i_IActiveMapsConfig;

interface

uses
  i_JclNotify,
  i_MapTypes;

type
  IActiveMapConfig = interface
    ['{D33FE9FA-B243-4783-9D55-F15B813BADF9}']
    procedure SelectMapByGUID(AMapGUID: TGUID);
    function GetSelectedMapGUID: TGUID;
    function GetMapsList: IMapTypeList;
    function GetMapChangeNotifier: IJclNotifier;

    property SelectedMapGUID: TGUID read GetSelectedMapGUID;
    property MapsList: IMapTypeList read GetMapsList;
    property MapChangeNotifier: IJclNotifier read GetMapChangeNotifier;
  end;

  IActiveMapWithHybrConfig = interface(IActiveMapConfig)
    ['{74D5CD4A-75DC-4926-B9D9-5AA7FD13CA2C}']
    procedure SelectHybrByGUID(AMapGUID: TGUID);
    procedure UnSelectHybrByGUID(AMapGUID: TGUID);
    function IsHybrGUIDSelected(AMapGUID: TGUID): Boolean;
    function GetHybrList: IMapTypeList;
    function GetHybrChangeNotifier: IJclNotifier;

    property HybrList: IMapTypeList read GetHybrList;
    property HybrChangeNotifier: IJclNotifier read GetHybrChangeNotifier;
  end;

const
  CGUID_Zero: TGUID = '{00000000-0000-0000-0000-000000000000}';

implementation

end.
 