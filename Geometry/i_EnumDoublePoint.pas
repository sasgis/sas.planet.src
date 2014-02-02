unit i_EnumDoublePoint;

interface

uses
  t_GeoTypes;

type
  IEnumDoublePoint = interface
    ['{A821C4B3-DB65-4B93-94A2-19ADC919EDCC}']
    function Next(out APoint: TDoublePoint): Boolean;
  end;

  IEnumLonLatPoint = interface(IEnumDoublePoint)
    ['{E8365B09-9819-4372-B24F-65BBFDC84558}']
  end;

  IEnumProjectedPoint = interface(IEnumDoublePoint)
    ['{BC88EBFF-54FA-4322-BB2A-0845A5943804}']
  end;

  IEnumLocalPoint = interface(IEnumDoublePoint)
    ['{70250A89-1BA1-45BA-8A33-0FE97E714771}']
  end;

implementation

end.
