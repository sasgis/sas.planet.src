unit i_TileStorageSQLiteFetcher;

interface

uses
  i_TileInfoBasic;

type
  ITileStorageSQLiteFetcher = interface
    ['{AA8E8598-A7A1-47EE-9E57-184A02DB08C3}']
    function Opened: Boolean;
    function Fetch(var ATileInfo: TTileInfo): Boolean;
  end;

implementation

end.
