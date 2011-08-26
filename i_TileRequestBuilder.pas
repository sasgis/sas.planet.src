unit i_TileRequestBuilder;

interface

uses
  Types,
  i_MapVersionInfo,
  i_LastResponseInfo,
  i_TileDownloadRequest;

type
  ITileRequestBuilder = interface
    ['{3F65B989-F693-460B-AE98-FD1DAECEA04B}']
    function BuildRequest(
      ATileXY: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionInfo;
      ALastResponseInfo: ILastResponseInfo
    ): ITileDownloadRequest;
  end;


implementation

end.
