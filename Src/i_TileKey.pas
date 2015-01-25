unit i_TileKey;

interface

uses
  Types,
  i_ProjectionInfo;

type
  ITileKey = interface
    ['{C9875442-9A76-4815-9E18-E129838AC83E}']
    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetProjectionInfo: IProjectionInfo;
    property ProjectionInfo: IProjectionInfo read GetProjectionInfo;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function IsSame(const AValue: ITileKey): Boolean;
  end;

implementation

end.
