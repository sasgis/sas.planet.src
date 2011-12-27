unit i_EnumDoublePoint;

interface

uses
  t_GeoTypes;

type
  IEnumDoublePoint = interface
    ['{A821C4B3-DB65-4B93-94A2-19ADC919EDCC}']
    function Next(out APoint: TDoublePoint): Boolean;
  end;

implementation

end.
