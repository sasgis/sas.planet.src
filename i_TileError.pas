unit i_TileError;

interface

uses
  Types,
  u_MapType;

type
  ITileErrorInfo = interface
    ['{35CA6508-14F7-43D0-BA19-C6FE088936FB}']
    function GetMapType: TMapType;
    property MapType: TMapType read GetMapType;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetErrorText: string;
    property ErrorText: string read GetErrorText;
  end;

  ITileErrorLogger = interface
    ['{693E489D-A78E-42B8-87FB-F16EF5F53ACF}']
    procedure LogError(AValue: ITileErrorInfo);
  end;

implementation

end.
