unit i_TileRequestBuilder;

interface

uses
  Types;

type
  ITileRequestBuilder = interface
    ['{3F65B989-F693-460B-AE98-FD1DAECEA04B}']
    function  BuildRequestUrl(ATileXY: TPoint; AZoom: Byte): string;
    procedure BuildRequest(
      ATileXY: TPoint;
      AZoom: Byte;
      const APreviousResponseHeader: string;
      out AUrl, ARequestHeader: string
    );

    function GetResponseHead: string;
    procedure SetResponseHead(const AValue: string);
    property ResponseHead: string read GetResponseHead write SetResponseHead;
  end;


implementation

end.
