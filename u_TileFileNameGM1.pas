unit u_TileFileNameGM1;

interface

uses
  Types,
  i_ITileFileNameGenerator;

type
  TTileFileNameGM1 = class(TInterfacedObject, ITileFileNameGenerator)
  public
    function GetTileFileName(AXY: TPoint; Azoom:byte): string;
  end;
implementation

uses
  SysUtils;

{ TTileFileNameGM1 }

function TTileFileNameGM1.GetTileFileName(AXY: TPoint;
  Azoom: byte): string;
begin
  result := format('z%d\%d\%d',[
    Azoom,
    AXY.y,
    AXY.x
  ]);
end;

end.

