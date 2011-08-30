unit i_LastSearchResultConfig;

interface
uses
  i_ConfigDataElement,
  i_GeoCoder;

type
  ILastSearchResultConfig = interface(IConfigDataElement)
    ['{BB987DBD-8DC2-409E-BFD9-145478AAAF8F}']
    function GetIsActive: Boolean;
    property IsActive: Boolean read GetIsActive;

    function GetGeoCodeResult:IGeoCodeResult;
    procedure SetGeoCodeResult(const AValue: IGeoCodeResult);
    property GeoCodeResult: IGeoCodeResult read GetGeoCodeResult write SetGeoCodeResult;

    procedure ClearGeoCodeResult;
  end;
implementation

end.
