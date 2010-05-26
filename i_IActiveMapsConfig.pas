unit i_IActiveMapsConfig;

interface

uses
  i_JclNotify,
  i_MapTypes,
  UMapType;

type
  IActiveMapsConfig = interface
    ['{D33FE9FA-B243-4783-9D55-F15B813BADF9}']
    procedure SelectMap(AMap: TMapType);
    function GetSelectedMap: TMapType;
    procedure SelectHybr(AMap: TMapType);
    procedure UnSelectHybr(AMap: TMapType);
    function IsHybrSelected(AMap: TMapType): Boolean;
    function IsHybrGUIDSelected(AMapGUID: TGUID): Boolean;
    function GetMapsList: IMapTypeList;
    function GetHybrList: IMapTypeList;
    function GetMapChangeNotifier: IJclNotifier;
    function GetHybrChangeNotifier: IJclNotifier;

    property SelectedMap: TMapType read GetSelectedMap;
    property MapsList: IMapTypeList read GetMapsList;
    property HybrList: IMapTypeList read GetHybrList;
    property MapChangeNotifier: IJclNotifier read GetMapChangeNotifier;
    property HybrChangeNotifier: IJclNotifier read GetHybrChangeNotifier;
  end;

const
  CGUID_Zero: TGUID = '{00000000-0000-0000-0000-000000000000}';

implementation

end.
 