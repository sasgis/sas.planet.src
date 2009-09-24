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
    function LonLat2Metr(Ll : TExtendedPoint) : TExtendedPoint;
    function Pos2OtherMap(XY : TPoint; Azoom : byte; AOtherMapCoordConv: ICoordConverter):TPoint;
  end;

  TCoordConverterAbstract = class(TInterfacedObject, ICoordConverter)
  public
    function Pos2LonLat(XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; abstract;
    function LonLat2Pos(Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; abstract;
    function LonLat2Metr(Ll : TExtendedPoint) : TExtendedPoint; virtual; abstract;
    function Pos2OtherMap(XY : TPoint; Azoom : byte; AOtherMapCoordConv: ICoordConverter):TPoint; virtual;
  end;

implementation

{ TCoordConverterAbstract }

function TCoordConverterAbstract.Pos2OtherMap(XY: TPoint; Azoom: byte;
  AOtherMapCoordConv: ICoordConverter): TPoint;
begin
  if (AOtherMapCoordConv = nil) then begin
    Result := XY;
  end else begin
    Result := AOtherMapCoordConv.LonLat2Pos(Pos2LonLat(XY, Azoom), Azoom);
  end;
end;

end.
