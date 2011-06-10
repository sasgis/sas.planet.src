unit u_TileDownloaderThreadBase;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  Types,
  i_JclNotify,
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
    FCancelEvent: TEvent;
    FCancelNotifier: IJclNotifier;

    function GetErrStr(Aerr: TDownloadTileResult): string;
    procedure SleepCancelable(ATime: Cardinal);
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Terminate; reintroduce;
    property MapType: TMapType read FMapType;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
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
  FCancelEvent := TEvent.Create;
  FCancelNotifier := TJclBaseNotifier.Create;
end;

destructor TTileDownloaderThreadBase.Destroy;
begin
  Terminate;
  inherited;
  FreeAndNil(FCancelEvent);
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

procedure TTileDownloaderThreadBase.SleepCancelable(ATime: Cardinal);
begin
  if  ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderThreadBase.Terminate;
begin
  inherited;
  FCancelEvent.SetEvent;
  FCancelNotifier.Notify(nil);
end;

end.
