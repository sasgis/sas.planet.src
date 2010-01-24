unit t_LoadEvent;

interface

uses
  UMapType;

type
  TLastLoad = record
    x, y: longint;
    z: byte;
    mt: TMapType;
    use: boolean;
  end;

implementation

end.
 