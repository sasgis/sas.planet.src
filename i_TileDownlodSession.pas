unit i_TileDownlodSession;

interface

uses
  Classes,
  Types;

type
  TDownloadTileResult = (dtrOK, dtrSameTileSize, dtrErrorInternetOpen, dtrErrorInternetOpenURL, dtrProxyAuthError, dtrErrorMIMEType, dtrDownloadError, dtrTileNotExists, dtrBanError, dtrUnknownError);

  TOnDownloadEvent = procedure (AParentThreadEvent: Pointer; ADownloadResult: TDownloadTileResult; ATile: TPoint; AZoom: Byte; AContentType: string; fileBuf: TMemoryStream) of object;

  ITileDownlodSession = interface
    ['{2F41E328-BD28-4893-AAC5-8DC93FCC2BCF}']
    function DownloadTile(AParentThreadEvent: Pointer; ATile: TPoint; AZoom: Byte; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; AOnDownloadEvent: TOnDownloadEvent): TDownloadTileResult;
    function GetLink(AXY: TPoint; AZoom: Byte): string;
  end;

  ITileDownlodSessionFactory = interface
    ['{62196012-45CC-45D1-BBEF-9959636DA479}']
    function CreateSession: ITileDownlodSession;
    function GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);
    property WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;
  end;

implementation

end.
