unit i_TileRequest;

interface

uses
  Types,
  i_ZmpInfo,
  i_MapVersionInfo;

type
  ITileRequest = interface
    ['{2E7FE7D3-1343-4823-876A-BBAD4D483728}']
    function GetZmp: IZmpInfo;
    property Zmp: IZmpInfo read GetZmp;

    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetVersionInfo: IMapVersionInfo;
    property VersionInfo: IMapVersionInfo read GetVersionInfo;
  end;

implementation

end.
