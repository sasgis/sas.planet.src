unit i_TileFileNameGenerator;

interface

uses
  Types;

type
  ITileFileNameGenerator = interface
    ['{E702ED99-8DC5-4C42-BB51-739011CEC6EA}']
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;
  end;

implementation

end.
