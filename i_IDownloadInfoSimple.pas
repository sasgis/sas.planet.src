unit i_IDownloadInfoSimple;

interface

type
  IDownloadInfoSimple = interface
    ['{5B404464-7B4B-4966-8FFD-BBDE2A13D72B}']
    function GetTileCount: UInt64;
    property TileCount: UInt64 read GetTileCount;

    function GetSize: UInt64;
    property Size: UInt64 read GetSize;

    procedure Reset;
    procedure Add(ACount: UInt64; ASize: UInt64);
  end;

implementation

end.
