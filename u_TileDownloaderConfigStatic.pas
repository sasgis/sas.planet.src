unit u_TileDownloaderConfigStatic;

interface

uses
  i_ProxySettings,
  i_TileDownloaderConfig;

type
  TTileDownloaderConfigStatic = class(TInterfacedObject, ITileDownloaderConfigStatic)
  private
    FProxyConfigStatic: IProxyConfigStatic;
    FTimeOut: Cardinal;
    FWaitInterval: Cardinal;
    FSleepOnResetConnection: Cardinal;
    FDownloadTryCount: Integer;
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;
  protected
    function GetProxyConfigStatic: IProxyConfigStatic;
    function GetTimeOut: Cardinal;
    function GetWaitInterval: Cardinal;
    function GetSleepOnResetConnection: Cardinal;
    function GetDownloadTryCount: Integer;
    function GetIgnoreMIMEType: Boolean;
    function GetExpectedMIMETypes: string;
    function GetDefaultMIMEType: string;
  public
    constructor Create(
      AProxyConfigStatic: IProxyConfigStatic;
      ATimeOut: Cardinal;
      AWaitInterval: Cardinal;
      ASleepOnResetConnection: Cardinal;
      ADownloadTryCount: Integer;
      AIgnoreMIMEType: Boolean;
      AExpectedMIMETypes: string;
      ADefaultMIMEType: string
    );
  end;

implementation

{ TTileDownloaderConfigStatic }

constructor TTileDownloaderConfigStatic.Create(
  AProxyConfigStatic: IProxyConfigStatic; ATimeOut, AWaitInterval,
  ASleepOnResetConnection: Cardinal; ADownloadTryCount: Integer;
  AIgnoreMIMEType: Boolean; AExpectedMIMETypes, ADefaultMIMEType: string);
begin
  FProxyConfigStatic := AProxyConfigStatic;
  FTimeOut := ATimeOut;
  FWaitInterval := AWaitInterval;
  FSleepOnResetConnection := ASleepOnResetConnection;
  FDownloadTryCount := ADownloadTryCount;
  FIgnoreMIMEType := AIgnoreMIMEType;
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDefaultMIMEType := ADefaultMIMEType;
end;

function TTileDownloaderConfigStatic.GetDefaultMIMEType: string;
begin
  Result := FDefaultMIMEType;
end;

function TTileDownloaderConfigStatic.GetDownloadTryCount: Integer;
begin
  Result := FDownloadTryCount;
end;

function TTileDownloaderConfigStatic.GetExpectedMIMETypes: string;
begin
  Result := FExpectedMIMETypes;
end;

function TTileDownloaderConfigStatic.GetIgnoreMIMEType: Boolean;
begin
  Result := FIgnoreMIMEType;
end;

function TTileDownloaderConfigStatic.GetProxyConfigStatic: IProxyConfigStatic;
begin
  Result := FProxyConfigStatic;
end;

function TTileDownloaderConfigStatic.GetSleepOnResetConnection: Cardinal;
begin
  Result := FSleepOnResetConnection;
end;

function TTileDownloaderConfigStatic.GetTimeOut: Cardinal;
begin
  Result := FTimeOut;
end;

function TTileDownloaderConfigStatic.GetWaitInterval: Cardinal;
begin
  Result := FWaitInterval;
end;

end.
