unit i_TileDownloadResult;

interface

uses
  Types,
  u_MapType;

type
  ITileInfo = interface
    ['{FD0335D8-CB98-4F76-A4A3-E7C1FBF185EC}']
    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetXY: TPoint;
    property XY: TPoint read GetXY;

    function GetMapType: TMapType;
    property MapType: TMapType read GetMapType;
  end;

implementation

end.
