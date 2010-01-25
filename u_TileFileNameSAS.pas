unit u_TileFileNameSAS;

interface

uses
  Types,
  i_ITileFileNameGenerator;

type
  TTileFileNameSAS = class(TInterfacedObject, ITileFileNameGenerator)
  public
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;
  end;

implementation

uses
  SysUtils;

{ TTileFileNameSAS }

function TTileFileNameSAS.GetTileFileName(AXY: TPoint;
  Azoom: byte): string;
begin
  result := format('z%d\%d\x%d\%d\y%d', [
    Azoom + 1,
    AXY.x shr 10,
    AXY.x,
    AXY.y shr 10,
    AXY.y
    ]);
end;

end.
 