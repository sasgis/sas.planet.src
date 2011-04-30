unit u_TileDownloaderThreadBase;

interface

uses
  Windows,
  Classes,
  Types,
  i_TileDownlodSession,
  u_MapType;

type
  TTileDownloaderThreadBase = class(TThread)
  private
    FRES_Authorization: string;
    FRES_Ban: string;
    FRES_TileNotExists: string;
    FRES_Noconnectionstointernet: string;
    FRES_TileDownloadContentTypeUnexpcted: string;
    FRES_TileDownloadUnexpectedError: string;
  protected
    FMapType: TMapType;
    FLoadXY: TPoint;
    FZoom: byte;
    FLoadUrl: string;
    function GetErrStr(Aerr: TDownloadTileResult): string;
  public
    constructor Create(CreateSuspended: Boolean);
    property MapType: TMapType read FMapType;
  end;

implementation

uses
  SysUtils,
  u_ResStrings;

constructor TTileDownloaderThreadBase.Create(CreateSuspended: Boolean);
begin
  FRES_Authorization := SAS_ERR_Authorization;
  FRES_Ban := SAS_ERR_Ban;
  FRES_TileNotExists := SAS_ERR_TileNotExists;
  FRES_Noconnectionstointernet := SAS_ERR_Noconnectionstointernet;
  FRES_TileDownloadContentTypeUnexpcted := SAS_ERR_TileDownloadContentTypeUnexpcted;
  FRES_TileDownloadUnexpectedError := SAS_ERR_TileDownloadUnexpectedError;

  inherited Create(CreateSuspended);
end;

function TTileDownloaderThreadBase.GetErrStr(Aerr: TDownloadTileResult): string;
begin
  Result := '';
  case Aerr of
    dtrProxyAuthError:
      result := FRES_Authorization;

    dtrBanError:
      result := FRES_Ban;

    dtrTileNotExists:
      result := FRES_TileNotExists;

    dtrDownloadError,
    dtrErrorInternetOpen,
    dtrErrorInternetOpenURL:
      result := FRES_Noconnectionstointernet;

    dtrErrorMIMEType:
      result := FRES_TileDownloadContentTypeUnexpcted;

    dtrUnknownError:
      Result := FRES_TileDownloadUnexpectedError;
  end;
end;

end.
