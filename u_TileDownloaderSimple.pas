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
  i_TileDownloadResultSaver,
  i_TileDownloaderConfig,
  i_TileDownloader,
  i_TileDownloadRequestBuilder,
  u_NotifierOperation;

type
  TTileDownloaderSimple = class(TInterfacedObject, ITileDownloader)
  private
    FTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FHttpDownloader: IDownloader;
    FResultSaver: ITileDownloadResultSaver;
    FAppClosingNotifier: INotifier;
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
    procedure Download(
      const ATileRequest: ITileRequest
    );
  public
    constructor Create(
      const AAppClosingNotifier: INotifier;
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
  i_TileRequestResult,
  u_ListenerByEvent,
  u_TileRequestResult;

{ TITileDownloaderSimple }

constructor TTileDownloaderSimple.Create(
  const AAppClosingNotifier: INotifier;
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

  VOperationNotifier := TNotifierOperation.Create;
  FDestroyNotifierInternal := VOperationNotifier;
  FDestroyNotifier := VOperationNotifier;
  FDestroyOperationID := FDestroyNotifier.CurrentOperation;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);

  FCS := MakeSyncRW_Std(Self, FALSE);
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancelEvent);
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FTileDownloaderConfig.ChangeNotifier.Add(FConfigChangeListener);
  FWasConnectError := False;
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

procedure TTileDownloaderSimple.Download(
  const ATileRequest: ITileRequest
);
var
  VDownloadRequest: ITileDownloadRequest;
  VTileRequestResult: ITileRequestResult;
  VDownloadResult: IDownloadResult;
  VCount: Integer;
  VTryCount: Integer;
  VResultWithRespond: IDownloadResultWithServerRespond;
begin
  ATileRequest.StartNotifier.Notify(ATileRequest);
  try
    if not ATileRequest.CancelNotifier.IsOperationCanceled(ATileRequest.OperationID) then begin
      FCS.BeginWrite;
      try
        if not ATileRequest.CancelNotifier.IsOperationCanceled(ATileRequest.OperationID) then begin
          FCancelEvent.ResetEvent;
          ATileRequest.CancelNotifier.AddListener(FCancelListener);
          try
            if not ATileRequest.CancelNotifier.IsOperationCanceled(ATileRequest.OperationID) then begin
              VTileRequestResult := nil;
              VCount := 0;
              VTryCount := FDownloadTryCount;
              repeat
                SleepIfConnectErrorOrWaitInterval;
                if ATileRequest.CancelNotifier.IsOperationCanceled(ATileRequest.OperationID) then begin
                  VTileRequestResult := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
                  Break;
                end;
                try
                  VDownloadRequest := FTileDownloadRequestBuilder.BuildRequest(
                    ATileRequest,
                    FLastResponseInfo,
                    FDestroyNotifier,
                    FDestroyOperationID
                  );
                except
                  on E: Exception do begin
                    VTileRequestResult :=
                      TTileRequestResultErrorBeforBuildDownloadRequest.Create(
                        ATileRequest,
                        E.Message
                      );
                    Break;
                  end;
                end;
                if VDownloadRequest = nil then begin
                  VTileRequestResult :=
                    TTileRequestResultErrorBeforBuildDownloadRequest.Create(
                      ATileRequest,
                      'Tile not exists'
                    );
                  Break;
                end;
                if ATileRequest.CancelNotifier.IsOperationCanceled(ATileRequest.OperationID) then begin
                  VTileRequestResult := TTileRequestResultCanceledAfterBuildDownloadRequest.Create(VDownloadRequest);
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
                end;
                try
                  FResultSaver.SaveDownloadResult(VDownloadResult);
                  VTileRequestResult := TTileRequestResultOk.Create(VDownloadResult);
                except
                  on E: Exception do begin
                    VTileRequestResult := TTileRequestResultErrorAfterDownloadRequest.Create(
                      VDownloadResult,
                      E.Message
                    );
                  end;
                end;
              until (not FWasConnectError) or (VCount >= VTryCount);
            end else begin
              VTileRequestResult := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
            end;
          finally
            ATileRequest.CancelNotifier.RemoveListener(FCancelListener);
          end;
        end else begin
          VTileRequestResult := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
        end;
      finally
        FCS.EndWrite;
      end;
    end else begin
      VTileRequestResult := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
    end;
  finally
    ATileRequest.FinishNotifier.Notify(VTileRequestResult);
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
