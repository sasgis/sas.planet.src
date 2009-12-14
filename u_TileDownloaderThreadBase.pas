unit u_TileDownloaderThreadBase;

interface

uses
  Windows,
  Classes,
  Types,
  t_LoadEvent,
  u_TileDownloaderBase,
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
  u_GlobalState,
  UResStrings;

class function TTileDownloaderThreadBase.GetErrStr(Aerr: TDownloadTileResult): string;
begin
 case Aerr of
  dtrProxyAuthError: result:=SAS_ERR_Authorization;
  dtrBanError: result:=SAS_ERR_Ban;
  dtrTileNotExists: result:=SAS_ERR_TileNotExists;
  dtrDownloadError,
  dtrErrorInternetOpen,
  dtrErrorInternetOpenURL: result:=SAS_ERR_Noconnectionstointernet;
  dtrErrorMIMEType: result := 'Ошибочный тип данных'; //TODO: Заменить на ресурсную строку
  dtrUnknownError: Result := 'Неизвестная ошибка при скачивании'
  else result:='';
 end;
end;

procedure TTileDownloaderThreadBase.ban;
begin
  FTypeMap.ExecOnBan(FLoadUrl);
end;

end.
