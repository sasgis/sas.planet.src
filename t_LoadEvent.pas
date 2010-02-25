unit t_LoadEvent;

interface

uses
  Types,
  UMapType;

type
  TLastLoad = record
    TilePos: TPoint;
    Zoom: byte;
    mt: TMapType;
    use: boolean;
  end;

implementation

end.
 