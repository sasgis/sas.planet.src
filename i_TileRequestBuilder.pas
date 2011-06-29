unit i_TileRequestBuilder;

interface

uses
  Types,
  i_MapVersionConfig,
  i_LastResponseInfo;

type
  ITileRequestBuilder = interface
    ['{3F65B989-F693-460B-AE98-FD1DAECEA04B}']
    function  BuildRequestUrl(
      ATileXY: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionConfigStatic
    ): string;
    procedure BuildRequest(
      ATileXY: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionConfigStatic;
      ALastResponseInfo: ILastResponseInfo;
      out AUrl: string;
      out ARequestHeader: string
    );
  end;


implementation

end.
