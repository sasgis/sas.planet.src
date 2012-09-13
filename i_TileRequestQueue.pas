unit i_TileRequestQueue;

interface

uses
  i_TileRequestTask;

type
  ITileRequestQueue = interface
    ['{49C05F75-E54F-4C5A-926E-67C60AF2F9EA}']
    procedure Push(
      const ARequest: ITileRequestTask
    );
    function Pull: ITileRequestTask;
  end;

implementation

end.
