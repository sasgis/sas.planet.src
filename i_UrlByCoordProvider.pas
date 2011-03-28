unit i_UrlByCoordProvider;

interface

uses
  t_GeoTypes;

type
  IUrlByCoordProvider = interface
    ['{F5B74D97-AA00-454B-95A7-CE8DA7690BF2}']
    function GetUrl(AZoom: Byte; ALonLat: TDoublePoint): string;
  end;

implementation

end.
