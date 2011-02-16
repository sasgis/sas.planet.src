unit u_TileDownloaderThreadBase;

interface

uses
  Windows,
  Classes,
  Types,
  i_ITileDownlodSession,
  UMapType;

type
  TTileDownloaderThreadBase = class(TThread)
  protected
    FMapType: TMapType;
    FLoadXY: TPoint;
    FZoom: byte;
    FLoadUrl: string;
    class function GetErrStr(Aerr: TDownloadTileResult): string; virtual;
  public
    property MapType: TMapType read FMapType;
  end;

implementation

uses
  SysUtils,
  UResStrings;

class function TTileDownloaderThreadBase.GetErrStr(Aerr: TDownloadTileResult): string;
begin
  case Aerr of
    dtrProxyAuthError:
    begin
      result := SAS_ERR_Authorization;
    end;
    dtrBanError:
    begin
      result := SAS_ERR_Ban;
    end;
    dtrTileNotExists:
    begin
      result := SAS_ERR_TileNotExists;
    end;
    dtrDownloadError,
    dtrErrorInternetOpen,
    dtrErrorInternetOpenURL:
    begin
      result := SAS_ERR_Noconnectionstointernet;
    end;
    dtrErrorMIMEType:
    begin
      result := SAS_ERR_TileDownloadContentTypeUnexpcted;
    end;
    dtrUnknownError:
    begin
      Result := SAS_ERR_TileDownloadUnexpectedError;
    end else begin
    result := '';
  end;
  end;
end;

end.
