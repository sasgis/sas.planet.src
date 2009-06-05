unit u_CoordConverterAbstract;

interface
uses
  Types;
type
  TExtendedPoint = record
   X, Y: Extended;
  end;
  ICoordConverter = interface
  ['{3EE2987F-7681-425A-8EFE-B676C506CDD4}']
    function Pos2LonLat(XY : TPoint; Azoom : byte) : TExtendedPoint;
    function LonLat2Pos(Ll : TExtendedPoint; Azoom : byte) : Tpoint;
  end;

  TCoordConverterAbstract = class(TInterfacedObject, ICoordConverter)
  public
    function Pos2LonLat(XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; abstract;
    function LonLat2Pos(Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; abstract;
  end;
implementation

end.
