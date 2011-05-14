unit u_TileDownloaderBaseThread;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  i_RequestBuilderScript,
  i_TileDownloader;

type
  TTileDownloaderBaseThread = class(TThread)
  private
    FHttpClient: TALWinInetHTTPClient;
    FResponseHeader: TALHTTPResponseHeader;
    FRawResponseHeader: string;
    FRequestBuilderScript: IRequestBuilderScript;
    FTimeOut: Cardinal;
    FEvent: ITileDownloaderEvent;
    FSemaphore: THandle;
    FParentSemaphore: THandle;
    FBusy: Boolean;
    procedure SetTimeOutValue(AValue: Cardinal);
    procedure SetHttpTimeOut;
    procedure SetHttpOptions;
    procedure DoRequest;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEvent(AEvent: ITileDownloaderEvent);
    property TimeOut: Cardinal write SetTimeOutValue;
    property Busy: Boolean read FBusy default False;
    property RequestBuilderScript: IRequestBuilderScript read FRequestBuilderScript write FRequestBuilderScript default nil;
    property RawResponseHeader: string read FRawResponseHeader write FRawResponseHeader;
    property Semaphore: THandle read FParentSemaphore write FParentSemaphore;
  end;

implementation

{ TTileDownloaderBaseThread }

constructor TTileDownloaderBaseThread.Create;
begin
  FTimeOut := 30000; // ms
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FHttpClient := TALWinInetHTTPClient.Create(nil);
  FHttpClient.RequestHeader.UserAgent := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';
  FResponseHeader := TALHTTPResponseHeader.Create;
  FRawResponseHeader := '';
  SetHttpTimeOut;
  SetHttpOptions;
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

procedure TTileDownloaderBaseThread.SetTimeOutValue(AValue: Cardinal);
begin
  if FTimeOut <> AValue then
  begin
    FTimeOut := AValue;
    SetHttpTimeOut;
  end;
end;

procedure TTileDownloaderBaseThread.SetHttpTimeOut;
begin
  FHttpClient.ConnectTimeout := FTimeOut;
  FHttpClient.SendTimeout := FTimeOut;
  FHttpClient.ReceiveTimeout := FTimeOut;
end;

procedure TTileDownloaderBaseThread.SetHttpOptions;
begin
  with FHttpClient.ProxyParams do
  begin

  end;
  FHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                    wHttpIo_Pragma_nocache,
                                    wHttpIo_No_cookies,
                                    wHttpIo_Keep_connection
                                 ];
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
      FRequestBuilderScript.GenRequest(FEvent.TileXY, FEvent.TileZoom, FRawResponseHeader, VUrl, VRawRequestHeader);
      FHttpClient.RequestHeader.Accept := '*/*';
      if VRawRequestHeader <> '' then
        FHttpClient.RequestHeader.RawHeaderText := VRawRequestHeader;                                                           
      try
        FResponseHeader.Clear;
        FHttpClient.Get(VUrl, FEvent.TileStream, FResponseHeader);
      except
        on E: EALHTTPClientException do begin
          FEvent.ErrorString := IntToStr(E.StatusCode) + ' ' + FResponseHeader.ReasonPhrase;
        end;
      end;
      FRawResponseHeader := '';
      FEvent.RawResponseHeader := FResponseHeader.RawHeaderText;
      FEvent.TileMIME := FResponseHeader.ContentType;
      FEvent.DownloadResult := dtrOK;
    finally
      FEvent.ProcessEvent;
    end;
  finally
    FBusy := False;
    if FParentSemaphore <> 0 then    
      ReleaseSemaphore(FParentSemaphore, 1, nil);
    FEvent := nil;
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
