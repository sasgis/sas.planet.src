unit u_TileDownloaderBaseCore;

interface

uses
  SysUtils,
  SyncObjs,
  i_ConfigDataProvider,
  i_TileDownloader,
  u_TileDownloaderBaseThread;

type
  TTileDownloaderBaseCore = class(TInterfacedObject, ITileDownloader)
  private
    FWaitInterval: Cardinal;
    FCS: TCriticalSection;
    FDownloadThread: TTileDownloaderBaseThread;
    FRawResponseHeader: string;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(AConfig: IConfigDataProvider; AZMPFileName: string);
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent);
    procedure UpdateResponseHeaders(AEvent: ITileDownloaderEvent);
    function  GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);
    property  WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;
  end;

implementation

{ TTileDownloaderBaseCore }

constructor TTileDownloaderBaseCore.Create(AConfig: IConfigDataProvider; AZMPFileName: string);
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FDownloadThread := TTileDownloaderBaseThread.Create;
end;

destructor TTileDownloaderBaseCore.Destroy;
begin
  FreeAndNil(FCS);
  FDownloadThread.Terminate;
  inherited Destroy;
end;

procedure TTileDownloaderBaseCore.Download(AEvent: ITileDownloaderEvent);
begin
  Lock;
  try
    repeat
      if not FDownloadThread.Busy then
      begin
        AEvent.AddToCallBackList(UpdateResponseHeaders);
        FDownloadThread.TimeOut := FWaitInterval;
        FDownloadThread.RawResponseHeader := FRawResponseHeader;
        FDownloadThread.AddEvent(AEvent);
        Break;
      end;
      Sleep(30);
    until False;
  finally
    Unlock;
  end;
end;

procedure TTileDownloaderBaseCore.UpdateResponseHeaders(AEvent: ITileDownloaderEvent);
begin
  Lock;
  try
    FRawResponseHeader := AEvent.RawResponseHeader;
  finally
    Unlock;
  end;
end;

procedure TTileDownloaderBaseCore.Lock;
begin
  FCS.Acquire;
end;

procedure TTileDownloaderBaseCore.UnLock;
begin
  FCS.Release;
end;

procedure TTileDownloaderBaseCore.SetWaitInterval(AValue: Cardinal);
begin
  Lock;
  try
    FWaitInterval := AValue;
  finally
    Unlock;
  end;
end;

function TTileDownloaderBaseCore.GetWaitInterval: Cardinal;
begin
  Lock;
  try
    Result := FWaitInterval;
  finally
    Unlock;
  end;
end;

end.
