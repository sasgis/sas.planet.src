unit i_ITileDownlodSession;

interface

uses
  Classes,
  Types;

type
  ITileDownlodSession = interface
  ['']
    function DownloadTile(AUrl: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType: string): TDownloadTileResult;
  end;

implementation

end.
