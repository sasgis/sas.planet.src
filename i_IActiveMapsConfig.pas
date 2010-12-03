unit i_IActiveMapsConfig;

interface

uses
  i_JclNotify,
  i_MapTypes,
  i_IConfigDataElement;

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

  IActiveMap = interface(IConfigDataElement)
    ['{6BAD8743-D50B-4342-9A68-DA5FBDDFDB04}']
    procedure SelectByGUID(AMapGUID: TGUID);
    function GetSelectedGUID: TGUID;
    function GetMapsList: IMapTypeList;

    property SelectedMapGUID: TGUID read GetSelectedGUID;
    property MapsList: IMapTypeList read GetMapsList;
  end;

  IActiveMapsSet = interface(IConfigDataElement)
    ['{09F8FEE4-984C-4D1F-A240-BD8FF3333F85}']
    procedure SelectByGUID(AMapGUID: TGUID);
    procedure UnSelectByGUID(AMapGUID: TGUID);
    function IsGUIDSelected(AMapGUID: TGUID): Boolean;
    function GetSelectedMapsList: IMapTypeList;
    function GetMapsList: IMapTypeList;

    property MapsList: IMapTypeList read GetMapsList;
  end;

const
  CGUID_Zero: TGUID = '{00000000-0000-0000-0000-000000000000}';

implementation

end.
 