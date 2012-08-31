unit i_ProjConverter;

interface

uses
  t_GeoTypes;

type
  IProjConverter = interface
    ['{F51E7967-01AF-40CA-A7A1-9BE0E2CF03AE}']
    function LonLat2XY(const AProjLP: TDoublePoint): TDoublePoint;
    function XY2LonLat(const AProjXY: TDoublePoint): TDoublePoint;
  end;

  IProjConverterFactory = interface
    ['{49DDCC5B-B9D4-471B-8247-4CA183B9C680}']
    function GetByEPSG(const AEPSG: Integer): IProjConverter;
    function GetByInitString(const AArgs: AnsiString): IProjConverter;
  end;

implementation

end.
