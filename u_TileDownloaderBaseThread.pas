unit u_TileDownloaderBaseThread;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  i_ProxySettings,
  i_RequestBuilderScript,
  i_TileDownloader,
  i_TileDownloaderConfig;

type
  TTileDownloaderBaseThread = class(TThread)
  private
    FHttpClient: TALWinInetHTTPClient;
    FResponseHeader: TALHTTPResponseHeader;
    FRawResponseHeader: string;
    FRequestBuilderScript: IRequestBuilderScript;
    FTileDownloaderConfig: ITileDownloaderConfigStatic;

    FEvent: ITileDownloaderEvent;
    FSemaphore: THandle;
    FParentSemaphore: THandle;
    FBusy: Boolean;
    procedure PrepareHttpClientConfig(const AAcceptEncoding, ARawHeaders: string);
    procedure DoRequest;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEvent(AEvent: ITileDownloaderEvent);
    property Busy: Boolean read FBusy default False;
    property RequestBuilderScript: IRequestBuilderScript write FRequestBuilderScript default nil;
    property TileDownloaderConfig: ITileDownloaderConfigStatic write FTileDownloaderConfig default nil;
    property RawResponseHeader: string read FRawResponseHeader write FRawResponseHeader;
    property Semaphore: THandle read FParentSemaphore write FParentSemaphore;
  end;

implementation

{ TTileDownloaderBaseThread }

constructor TTileDownloaderBaseThread.Create;
begin
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FHttpClient := TALWinInetHTTPClient.Create(nil);
  FHttpClient.RequestHeader.UserAgent := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';
  FResponseHeader := TALHTTPResponseHeader.Create;
  FRawResponseHeader := '';
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TTileDownloaderBaseThread.Destroy;
begin
  FResponseHeader.Free;
  FHttpClient.Free;
  CloseHandle(FSemaphore);
  inherited Destroy;
end;

procedure TTileDownloaderBaseThread.PrepareHttpClientConfig(const AAcceptEncoding, ARawHeaders: string);
var
  VProxyConfig: IProxyConfigStatic;
begin
  FHttpClient.RequestHeader.Accept := AAcceptEncoding;
  
  if ARawHeaders <> '' then
    FHttpClient.RequestHeader.RawHeaderText := ARawHeaders;

  FHttpClient.ConnectTimeout := FTileDownloaderConfig.TimeOut;
  FHttpClient.SendTimeout := FTileDownloaderConfig.TimeOut;
  FHttpClient.ReceiveTimeout := FTileDownloaderConfig.TimeOut;

  FHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                    wHttpIo_Pragma_nocache,
                                    wHttpIo_No_cookies,
                                    wHttpIo_Keep_connection
                                 ];

  VProxyConfig := FTileDownloaderConfig.ProxyConfigStatic;
  if Assigned(VProxyConfig) then
  begin
    if VProxyConfig.UseIESettings then
      FHttpClient.AccessType := wHttpAt_Preconfig
    else
      if VProxyConfig.UseProxy then
      begin
        FHttpClient.AccessType := wHttpAt_Proxy;
        FHttpClient.ProxyParams.ProxyServer := Copy(VProxyConfig.Host, 0, Pos(':', VProxyConfig.Host));
        FHttpClient.ProxyParams.ProxyPort := StrToInt(Copy(VProxyConfig.Host, Pos(':', VProxyConfig.Host) + 1));
        if VProxyConfig.UseLogin then
        begin
          FHttpClient.ProxyParams.ProxyUserName := VProxyConfig.Login;
          FHttpClient.ProxyParams.ProxyPassword := VProxyConfig.Password;
        end;
      end
      else
        FHttpClient.AccessType := wHttpAt_Direct;
  end;

end;

procedure TTileDownloaderBaseThread.AddEvent(AEvent: ITileDownloaderEvent);
begin
  FBusy := True;
  FEvent := AEvent;
  ReleaseSemaphore(FSemaphore, 1, nil);
end;

procedure TTileDownloaderBaseThread.DoRequest;
var
  VUrl: string;
  VRawRequestHeader: string;
begin
  try
    try
      if (FTileDownloaderConfig <> nil) and (FRequestBuilderScript <> nil) then
      begin
        FRequestBuilderScript.GenRequest(FEvent.TileXY, FEvent.TileZoom, FRawResponseHeader, VUrl, VRawRequestHeader);
        PrepareHttpClientConfig(FTileDownloaderConfig.DefaultMIMEType, VRawRequestHeader);
        try
          FResponseHeader.Clear;
          FHttpClient.Get(VUrl, FEvent.TileStream, FResponseHeader);
        except
          on E: EALHTTPClientException do
            FEvent.ErrorString := IntToStr(E.StatusCode) + ' ' + FResponseHeader.ReasonPhrase;
          on E: Exception do
            FEvent.ErrorString := E.Message;
        end;
        FRawResponseHeader := '';
        FEvent.RawResponseHeader := FResponseHeader.RawHeaderText;
        FEvent.TileMIME := FResponseHeader.ContentType;
        FEvent.DownloadResult := dtrOK;
      end;
    finally
      FEvent.ProcessEvent;
    end;
  finally
    FBusy := False;
    if FParentSemaphore <> 0 then    
      ReleaseSemaphore(FParentSemaphore, 1, nil);
  end;
end;

procedure TTileDownloaderBaseThread.Execute;
begin
  repeat
    if Terminated then
      Break;

    repeat
      if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0  then
        Break
      else
        if Terminated then
           Break;
    until False;

    if Assigned(FEvent) then
      DoRequest;

  until False;
end;

end.
