unit i_TileDownloadRequest;

interface

uses
  Types,
  i_ZmpInfo,
  i_DownloadRequest;

type
  ITileDownloadRequest = interface(IDownloadRequest)
    ['{769133CE-0BC4-45A3-8B4C-97B968D16B40}']
    function GetZmp: IZmpInfo;
    property Zmp: IZmpInfo read GetZmp;

    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;
  end;

implementation

end.
