unit i_TileDownlodSession;

interface

uses
  Classes,
  Types;

type
  TDownloadTileResult = (dtrOK, dtrSameTileSize, dtrErrorInternetOpen, dtrErrorInternetOpenURL, dtrProxyAuthError, dtrErrorMIMEType, dtrDownloadError, dtrTileNotExists, dtrBanError, dtrUnknownError);

  ITileDownlodSession = interface
    ['{2F41E328-BD28-4893-AAC5-8DC93FCC2BCF}']
    function DownloadTile(AUrl: string; var AHead: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType: string): TDownloadTileResult;
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
