unit u_CoordConverterAbstract;

interface

uses
  Types,
  t_GeoTypes;

type
  ICoordConverter = interface
  ['{3EE2987F-7681-425A-8EFE-B676C506CDD4}']
    function Pos2LonLat(XY : TPoint; Azoom : byte) : TExtendedPoint;
    function LonLat2Pos(Ll : TExtendedPoint; Azoom : byte) : Tpoint;
    function LonLat2Metr(Ll : TExtendedPoint) : TExtendedPoint;
  end;

  TCoordConverterAbstract = class(TInterfacedObject, ICoordConverter)
  public
    function Pos2LonLat(XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; abstract;
    function LonLat2Pos(Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; abstract;
    function LonLat2Metr(Ll : TExtendedPoint) : TExtendedPoint; virtual; abstract;
  end;

implementation

end.
