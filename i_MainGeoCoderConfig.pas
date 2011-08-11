unit i_MainGeoCoderConfig;

interface

uses
  i_GeoCoderList,
  i_StringHistory,
  i_ConfigDataElement;

type
  IMainGeoCoderConfig = interface(IConfigDataElement)
    ['{9C11F955-42C4-4AED-A398-796DC5ABC936}']
    function GetList: IGeoCoderList;

    function GetSearchHistory: IStringHistory;
    property SearchHistory: IStringHistory read GetSearchHistory;

    function GetActiveGeoCoderGUID: TGUID;
    procedure SetActiveGeoCoderGUID(AValue: TGUID);
    property ActiveGeoCoderGUID: TGUID read GetActiveGeoCoderGUID write SetActiveGeoCoderGUID;

    function GetActiveGeoCoder: IGeoCoderListEntity;
  end;

implementation

end.
