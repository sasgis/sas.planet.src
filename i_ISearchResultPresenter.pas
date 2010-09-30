unit i_ISearchResultPresenter;

interface

uses
  i_GeoCoder;

type
  ISearchResultPresenter = interface
    ['{AAB90C64-76B3-4125-9C32-3F5FDFD127AF}']
    procedure ShowSearchResults(ASearchResult: IGeoCodeResult; AZoom: Byte);
  end;

implementation

end.
 