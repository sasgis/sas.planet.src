unit i_TileRequestQueue;

interface

uses
  i_TileRequest;

type
  ITileRequestQueue = interface
    ['{49C05F75-E54F-4C5A-926E-67C60AF2F9EA}']
    procedure Push(const ARequest: ITileRequest);
    function Pull: ITileRequest;
  end;

implementation

end.
