unit u_TileDownloaderViaPHP;

interface

uses
  Windows,
  Classes,
  i_TileDownlodSession;

function DownloadTileViaPHP(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl, AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;

implementation

uses
  php4delphi;

function DownloadTileViaPHP(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl, AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;
begin
  //
end;

end.
