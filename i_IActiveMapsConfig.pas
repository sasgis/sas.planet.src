unit i_IActiveMapsConfig;

interface

uses
  JclNotify,
  UMapType;

type
  IActiveMapsConfig = interface
    ['{D33FE9FA-B243-4783-9D55-F15B813BADF9}']
    procedure SelectMap(AMap: TMapType);
    function GetSelectedMap: TMapType;
    procedure SelectHybr(AMap: TMapType);
    procedure UnSelectHybr(AMap: TMapType);
    function IsHybrSelected(AMap: TMapType): Boolean;
    function GetMapChangeNotifier: IJclNotifier;
    function GetHybrChangeNotifier: IJclNotifier;

    property SelectedMap: TMapType read GetSelectedMap;
    property MapChangeNotifier: IJclNotifier read GetMapChangeNotifier;
    property HybrChangeNotifier: IJclNotifier read GetHybrChangeNotifier;
  end;

implementation

end.
 