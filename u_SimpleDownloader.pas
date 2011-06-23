unit u_SimpleDownloader;

interface

uses
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  Classes,
  SysUtils,
  i_InetConfig,
  i_ProxySettings,
  i_SimpleDownloader,
  i_TileDownloaderConfig;

type
  TSimpleDownloader = class (TInterfacedObject, ISimpleDownloader)
  private
    FHttpClient: TALWinInetHTTPClient;
    FResponseHeader: TALHTTPResponseHeader;
    FTileDownloaderConfig: ITileDownloaderConfig;
  public
    constructor Create(ATileDownloaderConfig: ITileDownloaderConfig);
    destructor Destroy; override;
    function GetFromInternet(
      AUrl: string;
      AAcceptEncoding: string;
      ARequestHead: string;
      ARequestBuf: TMemoryStream;
      AResponseBuf: TMemoryStream;
      out AContentType: string;
      out AResponseHead: string
    ): Cardinal;
  end;

implementation

{ TSimpleDownloader }

constructor TSimpleDownloader.Create(ATileDownloaderConfig: ITileDownloaderConfig);
begin
  inherited Create;
  FTileDownloaderConfig := ATileDownloaderConfig;
end;

destructor TSimpleDownloader.Destroy;
begin
  try
    if Assigned(FHttpClient) then begin
      FreeAndNil(FHttpClient);
    end;
    if Assigned(FResponseHeader) then begin
      FreeAndNil(FResponseHeader);
    end;
  finally
    inherited Destroy;
  end;
end;

function TSimpleDownloader.GetFromInternet(
  AUrl: string;
  AAcceptEncoding: string;
  ARequestHead: string;
  ARequestBuf: TMemoryStream;
  AResponseBuf: TMemoryStream;
  out AContentType: string;
  out AResponseHead: string
): Cardinal;
var
  VInternetConfigStatic: IInetConfigStatic;
  VProxyConfigStatic: IProxyConfigStatic;
  VTimeOut: Cardinal;
begin
  Result := 0;
  AResponseBuf.Clear;
  AContentType := '';
  AResponseHead := '';
  if AUrl <> '' then begin
    if not Assigned(FHttpClient) then begin
      FHttpClient := TALWinInetHTTPClient.Create(nil);
    end;
    if not Assigned(FResponseHeader) then begin
      FResponseHeader := TALHTTPResponseHeader.Create;
    end;
    if Assigned(FHttpClient) and Assigned(FResponseHeader) then
    try
      FResponseHeader.Clear;

      VInternetConfigStatic := FTileDownloaderConfig.GetStatic.InetConfigStatic;

      FHttpClient.RequestHeader.UserAgent := VInternetConfigStatic.UserAgentString;

      if AAcceptEncoding <> '' then begin
        FHttpClient.RequestHeader.Accept := AAcceptEncoding;
      end else begin
        FHttpClient.RequestHeader.Accept := '*/*';
      end;

      if ARequestHead <> '' then begin
        FHttpClient.RequestHeader.RawHeaderText := ARequestHead;
      end;

      VTimeOut := VInternetConfigStatic.TimeOut;
      FHttpClient.ConnectTimeout := VTimeOut;
      FHttpClient.SendTimeout := VTimeOut;
      FHttpClient.ReceiveTimeout := VTimeOut;

      FHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                        wHttpIo_Pragma_nocache,
                                        wHttpIo_No_cookies,
                                        wHttpIo_Keep_connection
                                     ];

      VProxyConfigStatic := VInternetConfigStatic.ProxyConfigStatic;
      if Assigned(VProxyConfigStatic) then begin
        if VProxyConfigStatic.UseIESettings then begin
          FHttpClient.AccessType := wHttpAt_Preconfig
        end else begin
          if VProxyConfigStatic.UseProxy then begin
            FHttpClient.AccessType := wHttpAt_Proxy;
            FHttpClient.ProxyParams.ProxyServer := Copy(VProxyConfigStatic.Host, 0, Pos(':', VProxyConfigStatic.Host) - 1);
            FHttpClient.ProxyParams.ProxyPort := StrToInt(Copy(VProxyConfigStatic.Host, Pos(':', VProxyConfigStatic.Host) + 1));
            if VProxyConfigStatic.UseLogin then begin
              FHttpClient.ProxyParams.ProxyUserName := VProxyConfigStatic.Login;
              FHttpClient.ProxyParams.ProxyPassword := VProxyConfigStatic.Password;
            end;
          end else begin
            FHttpClient.AccessType := wHttpAt_Direct;
          end;
        end;
      end;

      try
        if Assigned(ARequestBuf) then begin
          ARequestBuf.Position := 0;
          FHttpClient.Post(AUrl, ARequestBuf, AResponseBuf, FResponseHeader);
        end else begin
          FHttpClient.Get(AUrl, AResponseBuf, FResponseHeader);
        end;
      except
        on E: EALHTTPClientException do begin
          if E.StatusCode = 0 then begin
            raise Exception.Create(E.Message); // Unknown connection Error
          end else begin
            Result := E.StatusCode;            // Http Error
          end;
        end;
      end;

    finally
      if Assigned(FResponseHeader) then begin
        if FResponseHeader.RawHeaderText <> '' then begin
          AContentType := FResponseHeader.ContentType;
          AResponseHead := FResponseHeader.RawHeaderText;
          if Result = 0 then begin
            Result := StrToIntDef(FResponseHeader.StatusCode, 0);
          end;
        end;
      end;
    end;
  end;
end;

end.
