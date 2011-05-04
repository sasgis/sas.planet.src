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
  end;

implementation

{ TTileDownloaderBaseThread }

constructor TTileDownloaderBaseThread.Create;
begin
  FTimeOut := 30000; // ms
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FHttpClient := TALWinInetHTTPClient.Create(nil);
  FHttpClient.RequestHeader.UserAgent := '';
  FResponseHeader := TALHTTPResponseHeader.Create;
  FRawResponseHeader := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';
  SetHttpTimeOut;
  SetHttpOptions;
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
  FHttpClient.InternetOptions := [];
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
      if VRawRequestHeader <> '' then
        FHttpClient.RequestHeader.RawHeaderText := VRawRequestHeader;                                                           
      try
        FResponseHeader.Clear;
        FHttpClient.Get(VUrl, FEvent.TileStream, FResponseHeader);
      except
        on E: EALHTTPClientException do begin
          // E.StatusCode
        end;
      end;
      FRawResponseHeader := '';
      FEvent.RawResponseHeader := FResponseHeader.RawHeaderText;
      FEvent.TileMIME := FResponseHeader.ContentType;
    finally
      FEvent.ExecCallBackList;
      FEvent.ProcessEvent;
    end;
  finally
    FBusy := False;
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
