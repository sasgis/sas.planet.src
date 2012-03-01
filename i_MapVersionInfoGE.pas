unit i_MapVersionInfoGE;

interface

uses
  i_MapVersionInfo;

type
  IMapVersionInfoGE = interface(IMapVersionInfo)
    ['{D7EFBAB0-D4FE-4D9A-A4C6-CEC894B913AA}']
    function GetVer: Word;
    property Ver: Word read GetVer;

    function GetGEServer: String;
    property GEServer: String read GetGEServer;

    function GetTileDate: String;
    property TileDate: String read GetTileDate;
  end;

implementation

end.
