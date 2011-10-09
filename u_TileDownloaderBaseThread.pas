unit u_TileDownloaderBaseThread;

interface

uses
  Windows,
  SysUtils,
  Classes,
  SyncObjs,
  i_JclNotify,
  i_OperationNotifier,
  i_InetConfig,
  i_TileRequestBuilder,
  i_TileDownloader,
  i_TileDownloaderConfig,
  u_TileDownloaderHttp;

type
  TTileDownloaderBaseThread = class(TThread)
  private
    FCancelListener: IJclListener;
    FCancelEvent: TEvent;
    FTileRequestBuilder: ITileRequestBuilder;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FHttpDownloader: TTileDownloaderHttp;
    FEvent: ITileDownloaderEvent;
    FSemaphore: THandle;
    FParentSemaphore: THandle;
    FBusy: Boolean;
    FWasConnectError: Boolean;
    FLastDownloadTime: Cardinal;
    FIsCanceled: Boolean;
    FSessionCS: TCriticalSection;
    procedure DoRequest;
    function IsCanceled: Boolean;
    procedure SetIsCanceled;
    procedure SetNotCanceled;
    procedure SleepCancelable(ATime: Cardinal);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEvent(AEvent: ITileDownloaderEvent);
    procedure OnCancelEvent(Sender: TObject);
    property Busy: Boolean read FBusy default False;
    property TileRequestBuilder: ITileRequestBuilder write FTileRequestBuilder default nil;
    property TileDownloaderConfig: ITileDownloaderConfig write FTileDownloaderConfig default nil;
    property Semaphore: THandle read FParentSemaphore write FParentSemaphore;
  end;

implementation

uses
  i_DownloadResult,
  u_NotifyEventListener;

{ TTileDownloaderBaseThread }

constructor TTileDownloaderBaseThread.Create;
begin
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyEventListener.Create(Self.OnCancelEvent);
  FIsCanceled := False;
  FSessionCS := TCriticalSection.Create;
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FHttpDownloader := TTileDownloaderHttp.Create;
  FWasConnectError := False;
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TTileDownloaderBaseThread.Destroy;
begin
  try
    SetIsCanceled;
    FreeAndNil(FHttpDownloader);
    CloseHandle(FSemaphore);
    FreeAndNil(FSessionCS);
    FreeAndNil(FCancelEvent);
  finally
    inherited Destroy;
  end;
end;

procedure TTileDownloaderBaseThread.AddEvent(AEvent: ITileDownloaderEvent);
begin
  FBusy := True;
  FEvent := AEvent;
  ReleaseSemaphore(FSemaphore, 1, nil);
end;

procedure TTileDownloaderBaseThread.DoRequest;

  procedure SleepIfConnectErrorOrWaitInterval(
    ATileDownloaderConfigStatic: ITileDownloaderConfigStatic
  );
  var
    VNow: Cardinal;
    VTimeFromLastDownload: Cardinal;
    VSleepTime: Cardinal;
    VInetConfig: IInetConfigStatic;
  begin
    VInetConfig := ATileDownloaderConfigStatic.InetConfigStatic;
    VNow := GetTickCount;
    if VNow >= FLastDownloadTime then begin
      VTimeFromLastDownload := VNow - FLastDownloadTime;
    end else begin
      VTimeFromLastDownload := MaxInt;
    end;
    if FWasConnectError then begin
      if VTimeFromLastDownload < VInetConfig.SleepOnResetConnection then begin
        VSleepTime := VInetConfig.SleepOnResetConnection - VTimeFromLastDownload;
        SleepCancelable(VSleepTime);
      end;
    end else begin
      if VTimeFromLastDownload < ATileDownloaderConfigStatic.WaitInterval then begin
        VSleepTime := ATileDownloaderConfigStatic.WaitInterval - VTimeFromLastDownload;
        SleepCancelable(VSleepTime);
      end;
    end;
  end;

  function OperationCanceled: Boolean;
  begin
    Result := IsCanceled;
    if Result then begin
      FEvent.DownloadResult := FHttpDownloader.Cancel(FEvent.Request);
    end;
  end;

var
  VCount: Integer;
  VTryCount: Integer;
  VTileDownloaderConfigStatic: ITileDownloaderConfigStatic;
begin
  VTileDownloaderConfigStatic := FTileDownloaderConfig.GetStatic;
  SetNotCanceled;
  try
    try
      if (VTileDownloaderConfigStatic <> nil) and (FTileRequestBuilder <> nil) then begin
        try
          if FEvent.CancelNotifier <> nil then begin
            FEvent.CancelNotifier.AddListener(FCancelListener);
          end;
          if FEvent.DownloadResult = nil then begin
            VCount := 0;
            VTryCount := VTileDownloaderConfigStatic.InetConfigStatic.DownloadTryCount;
            FWasConnectError := False;
            FLastDownloadTime := MaxInt;
            repeat
              if OperationCanceled then begin
                Break;
              end;
              SleepIfConnectErrorOrWaitInterval(VTileDownloaderConfigStatic);
              FEvent.Request := FTileRequestBuilder.BuildRequest(
                FEvent.TileXY,
                FEvent.TileZoom,
                FEvent.VersionInfo,
                FEvent.LastResponseInfo
              );
              FEvent.DownloadResult := FHttpDownloader.Get(
                FEvent.Request,
                VTileDownloaderConfigStatic,
                FEvent.CheckTileSize,
                FEvent.OldTileSize
              );
              if OperationCanceled then begin
                Break;
              end;
              Inc(VCount);
              FLastDownloadTime := GetTickCount;
              if FEvent.DownloadResult <> nil then begin
                FWasConnectError := not FEvent.DownloadResult.IsServerExists;
              end;
            until (not FWasConnectError) or (VCount >= VTryCount);
          end;
        finally
          if FEvent.CancelNotifier <> nil then begin
            FEvent.CancelNotifier.RemoveListener(FCancelListener);
          end;
        end;
      end;
    finally
      FEvent.ProcessEvent;
    end;
  finally
    FBusy := False;
    if FParentSemaphore <> 0 then begin
      ReleaseSemaphore(FParentSemaphore, 1, nil);
    end;
  end;
end;

procedure TTileDownloaderBaseThread.Execute;
begin
  repeat
    if Terminated then begin
      Break;
    end;
    repeat
      if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0  then begin
        Break
      end else if Terminated then begin
        Break;
      end;
    until False;
    if Assigned(FEvent) then begin
      DoRequest;
    end;
  until False;
end;

procedure TTileDownloaderBaseThread.OnCancelEvent(Sender: TObject);
begin
  SetIsCanceled;
  if Assigned(FHttpDownloader) then begin
    FHttpDownloader.Disconnect;
  end;
end;

function TTileDownloaderBaseThread.IsCanceled: Boolean;
begin
  FSessionCS.Acquire;
  try
    Result := FIsCanceled;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBaseThread.SetIsCanceled;
begin
  FSessionCS.Acquire;
  try
    FCancelEvent.SetEvent;
    FIsCanceled := True;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBaseThread.SetNotCanceled;
begin
  FSessionCS.Acquire;
  try
    FCancelEvent.ResetEvent;
    FIsCanceled := False;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBaseThread.SleepCancelable(ATime: Cardinal);
begin
  if ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

end.
