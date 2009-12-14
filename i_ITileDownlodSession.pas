unit i_ITileDownlodSession;

interface

uses
  Classes,
  Types;

type
  TDownloadTileResult = (dtrOK, dtrSameTileSize, dtrErrorInternetOpen, dtrErrorInternetOpenURL, dtrProxyAuthError, dtrErrorMIMEType, dtrDownloadError, dtrTileNotExists, dtrBanError, dtrUnknownError);

  ITileDownlodSession = interface
  ['{2F41E328-BD28-4893-AAC5-8DC93FCC2BCF}']
    function DownloadTile(AUrl: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType: string): TDownloadTileResult;
  end;

implementation

end.
