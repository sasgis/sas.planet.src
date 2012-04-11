unit i_TimeZoneDiffByLonLat;

interface

uses
  t_GeoTypes;

type
  ITimeZoneDiffByLonLat = interface
    ['{15E97BD9-B681-47CD-9488-620120CDF341}']
    function GetTimeDiff(const ALonLat: TDoublePoint): TDateTime;
  end;

implementation

end.
