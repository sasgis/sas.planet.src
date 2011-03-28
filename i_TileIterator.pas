unit i_TileIterator;

interface

uses
  Types;

type
  ITileIterator = interface
    ['{E563544C-4A6E-4A8C-B5C9-81190F1416AF}']
    function GetTilesTotal: Int64;
    function GetTilesRect: TRect;
    function Next(out ATile: TPoint): Boolean;
    procedure Reset;

    property TilesTotal: Int64 read GetTilesTotal;
    property TilesRect: TRect read GetTilesRect;
  end;

  ITileIteratorByRows = interface(ITileIterator)
    ['{E030EE2E-8AC5-4DF7-AAF5-0EB23A4CD589}']
  end;

  ITileIteratorByCols = interface(ITileIterator)
    ['{947F77CB-2368-4734-95D1-FC1D65487BD9}']
  end;

implementation

end.
