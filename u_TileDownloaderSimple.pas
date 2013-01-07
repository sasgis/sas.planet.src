unit u_TileDownloaderSimple;

interface

uses
  Windows,
  SysUtils,
  SyncObjs,
  i_Notifier,
  i_Listener,
  i_NotifierOperation,
  i_LastResponseInfo,
  i_Downloader,
  i_TileRequest,
  i_TileRequestResult,
  i_TileDownloadResultSaver,
  i_TileDownloaderConfig,
  i_TileDownloader,
  i_TileDownloadRequestBuilder,
  u_BaseInterfacedObject;

type
  TTileDownloaderSimple = class(TBaseInterfacedObject, ITileDownloader)
  private
    FTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FHttpDownloader: IDownloader;
    FResultSaver: ITileDownloadResultSaver;
    FAppClosingNotifier: INotifierOneOperation;
    FLastResponseInfo: ILastResponseInfo;

    FDestroyNotifierInternal: INotifierOperationInternal;
    FDestroyNotifier: INotifierOperation;
    FDestroyOperationID: Integer;

    FAppClosingListener: IListener;
    FCS: IReadWriteSync;
    FCancelListener: IListener;
    FConfigChangeListener: IListener;
    FCancelEvent: TEvent;
    FWasConnectError: Boolean;
    FLastDownloadTime: Cardinal;

    FDownloadTryCount: Integer;
    FSleepOnResetConnection: Cardinal;
    FSleepAfterDownload: Cardinal;
    procedure OnConfigChange;
    procedure OnCancelEvent;
    procedure OnAppClosing;
    procedure SleepCancelable(ATime: Cardinal);
    procedure SleepIfConnectErrorOrWaitInterval;
  private
    function Download(
      const ACancelNotifier: INotifierOneOperation;
      const ATileRequest: ITileRequest
    ): ITileRequestResult;
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const ATileDownloadRequestBuilder: ITileDownloadRequestBuilder;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const AHttpDownloader: IDownloader;
      const AResultSaver: ITileDownloadResultSaver;
      const ALastResponseInfo: ILastResponseInfo
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  i_InetConfig,
  i_DownloadResult,
  i_TileDownloadRequest,
  u_Notifier,
  u_NotifierOperation,
  u_ListenerByEvent,
  u_TileRequestResult;

{ TITileDownloaderSimple }

constructor TTileDownloaderSimple.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const ATileDownloadRequestBuilder: ITileDownloadRequestBuilder;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const AHttpDownloader: IDownloader;
  const AResultSaver: ITileDownloadResultSaver;
  const ALastResponseInfo: ILastResponseInfo
);
var
  VOperationNotifier: TNotifierOperation;
begin
  inherited Create;
  FAppClosingNotifier := AAppClosingNotifier;
  FTileDownloadRequestBuilder := ATileDownloadRequestBuilder;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FHttpDownloader := AHttpDownloader;
  FResultSaver := AResultSaver;
  FLastResponseInfo := ALastResponseInfo;
  Assert(FResultSaver <> nil);

  VOperationNotifier := TNotifierOperation.Create(TNotifierBase.Create);
  FDestroyNotifierInternal := VOperationNotifier;
  FDestroyNotifier := VOperationNotifier;
  FDestroyOperationID := FDestroyNotifier.CurrentOperation;

  FCS := MakeSyncRW_Std(Self, FALSE);
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancelEvent);
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FTileDownloaderConfig.ChangeNotifier.Add(FConfigChangeListener);
  FWasConnectError := False;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

destructor TTileDownloaderSimple.Destroy;
begin
  FDestroyNotifierInternal.NextOperation;
  FAppClosingNotifier.Remove(FAppClosingListener);
  FAppClosingListener := nil;
  FAppClosingNotifier := nil;

  FTileDownloaderConfig.ChangeNotifier.Remove(FConfigChangeListener);
  FConfigChangeListener := nil;
  FTileDownloaderConfig := nil;

  FCS := nil;
  FreeAndNil(FCancelEvent);
  inherited;
end;

function TTileDownloaderSimple.Download(
  const ACancelNotifier: INotifierOneOperation;
  const ATileRequest: ITileRequest
): ITileRequestResult;
var
  VDownloadRequest: ITileDownloadRequest;
  VDownloadResult: IDownloadResult;
  VDownloadResultError: IDownloadResultError;
  VCount: Integer;
  VTryCount: Integer;
  VResultWithRespond: IDownloadResultWithServerRespond;
begin
  Result := nil;
  if not ACancelNotifier.IsExecuted then begin
    FCS.BeginWrite;
    try
      if not ACancelNotifier.IsExecuted then begin
        FCancelEvent.ResetEvent;
        ACancelNotifier.Add(FCancelListener);
        try
          if not ACancelNotifier.IsExecuted then begin
            VCount := 0;
            VTryCount := FDownloadTryCount;
            repeat
              SleepIfConnectErrorOrWaitInterval;
              if ACancelNotifier.IsExecuted then begin
                Result := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
                Break;
              end;
              try
                VDownloadRequest :=
                  FTileDownloadRequestBuilder.BuildRequest(
                    ATileRequest,
                    FLastResponseInfo,
                    FDestroyNotifier,
                    FDestroyOperationID
                  );
              except
                on E: Exception do begin
                  Result :=
                    TTileRequestResultErrorBeforBuildDownloadRequest.Create(
                      ATileRequest,
                      E.Message
                    );
                  Break;
                end;
              end;
              if VDownloadRequest = nil then begin
                Result :=
                  TTileRequestResultErrorBeforBuildDownloadRequest.Create(
                    ATileRequest,
                    'Tile does not exist'
                  );
                Break;
              end;
              if ACancelNotifier.IsExecuted then begin
                Result := TTileRequestResultCanceledAfterBuildDownloadRequest.Create(VDownloadRequest);
                Break;
              end;
              VDownloadResult :=
                FHttpDownloader.DoRequest(
                  VDownloadRequest,
                  FDestroyNotifier,
                  FDestroyOperationID
                );
              Inc(VCount);
              FLastDownloadTime := GetTickCount;
              if VDownloadResult <> nil then begin
                if VDownloadResult.IsServerExists then begin
                  FWasConnectError := False;
                  if Supports(VDownloadResult, IDownloadResultWithServerRespond, VResultWithRespond) then begin
                    FLastResponseInfo.ResponseHead := VResultWithRespond.RawResponseHeader;
                  end;
                end else begin
                  FWasConnectError := True;
                end;
                try
                  Result := FResultSaver.SaveDownloadResult(VDownloadResult);
                except
                  on E: Exception do begin
                    Result := TTileRequestResultErrorAfterDownloadRequest.Create(
                      VDownloadResult,
                      E.Message
                    );
                  end;
                end;
                if Result = nil then begin
                  if Supports(VDownloadResult, IDownloadResultError, VDownloadResultError) then begin
                    Result := TTileRequestResultDownloadError.Create(VDownloadResultError)
                  end else begin
                    Result :=
                      TTileRequestResultErrorAfterDownloadRequest.Create(
                        VDownloadResult,
                        'Unknown error'
                      );
                  end;
                end;
              end else begin
                Result := TTileRequestResultCanceledAfterBuildDownloadRequest.Create(VDownloadRequest);
              end;
            until (not FWasConnectError) or (VCount >= VTryCount);
          end else begin
            Result := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
          end;
        finally
          ACancelNotifier.Remove(FCancelListener);
        end;
      end else begin
        Result := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
      end;
    finally
      FCS.EndWrite;
    end;
  end else begin
    Result := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
  end;
end;

procedure TTileDownloaderSimple.OnAppClosing;
begin
  FDestroyNotifierInternal.NextOperation;
  FCancelEvent.SetEvent;
end;

procedure TTileDownloaderSimple.OnCancelEvent;
begin
  FCancelEvent.SetEvent;
end;

procedure TTileDownloaderSimple.OnConfigChange;
var
  VTileDownloaderConfigStatic: ITileDownloaderConfigStatic;
begin
  VTileDownloaderConfigStatic := FTileDownloaderConfig.GetStatic;
  FDownloadTryCount := VTileDownloaderConfigStatic.InetConfigStatic.DownloadTryCount;
  FSleepOnResetConnection := VTileDownloaderConfigStatic.InetConfigStatic.SleepOnResetConnection;
  FSleepAfterDownload := VTileDownloaderConfigStatic.WaitInterval;
end;

procedure TTileDownloaderSimple.SleepCancelable(ATime: Cardinal);
begin
  if ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderSimple.SleepIfConnectErrorOrWaitInterval;
var
  VNow: Cardinal;
  VTimeFromLastDownload: Cardinal;
  VSleepTime: Cardinal;
  VInterval: Cardinal;
begin
  VNow := GetTickCount;

  if VNow >= FLastDownloadTime then begin
    VTimeFromLastDownload := VNow - FLastDownloadTime;
  end else begin
    VTimeFromLastDownload := MaxInt;
  end;

  if FWasConnectError then begin
    VInterval := FSleepOnResetConnection;
  end else begin
    VInterval := FSleepAfterDownload;
  end;

  if VTimeFromLastDownload < VInterval then begin
    VSleepTime := VInterval - VTimeFromLastDownload;
    SleepCancelable(VSleepTime);
  end;
end;

end.
