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
    FTypeMap: TMapType;
    FLoadXY: TPoint;
    FZoom: byte;
    FLoadUrl: string;
    procedure ban; virtual;
    class function GetErrStr(Aerr: TDownloadTileResult): string; virtual;
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
      result := 'Ошибочный тип данных';
    end; //TODO: Заменить на ресурсную строку
    dtrUnknownError:
    begin
      Result := 'Неизвестная ошибка при скачивании';
    end else begin
    result := '';
  end;
  end;
end;

procedure TTileDownloaderThreadBase.ban;
begin
  FTypeMap.ExecOnBan(FLoadUrl);
end;

end.
