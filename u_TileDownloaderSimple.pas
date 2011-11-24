unit u_TileDownloaderSimple;

interface
uses
  Windows,
  SyncObjs,
  Classes,
  i_JclNotify,
  i_OperationNotifier,
  i_LastResponseInfo,
  i_SimpleDownloader,
  i_TileRequest,
  i_TileDownloaderConfig,
  i_TileDownloaderAsync,
  i_TileDownloadRequestBuilder;

type
  TTileDownloaderSimple = class(TInterfacedObject, ITileDownloader)
  private
    FTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FHttpDownloader: ISimpleDownloader;
    FLastResponseInfo: ILastResponseInfo;

    FCS: TCriticalSection;
    FCancelListener: IJclListener;
    FConfigChangeListener: IJclListener;
    FCancelEvent: TEvent;
    FWasConnectError: Boolean;
    FLastDownloadTime: Cardinal;

    FDownloadTryCount: Integer;
    FSleepOnResetConnection: Cardinal;
    FSleepAfterDownload: Cardinal;
    procedure OnConfigChange(Sender: TObject);
    procedure OnCancelEvent(Sender: TObject);
    procedure SleepCancelable(ATime: Cardinal);
    procedure SleepIfConnectErrorOrWaitInterval;
  protected
    procedure Download(
      ATileRequest: ITileRequest
    );
  public
    constructor Create(
      ATileDownloadRequestBuilder: ITileDownloadRequestBuilder;
      ATileDownloaderConfig: ITileDownloaderConfig;
      AHttpDownloader: ISimpleDownloader;
      ALastResponseInfo: ILastResponseInfo
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_InetConfig,
  i_DownloadResult,
  i_TileDownloadChecker,
  i_TileDownloadRequest,
  i_TileRequestResult,
  u_NotifyEventListener,
  u_TileRequestResult;

{ TITileDownloaderSimple }

constructor TTileDownloaderSimple.Create(
  ATileDownloadRequestBuilder: ITileDownloadRequestBuilder;
  ATileDownloaderConfig: ITileDownloaderConfig;
  AHttpDownloader: ISimpleDownloader;
  ALastResponseInfo: ILastResponseInfo
);
begin
  FTileDownloadRequestBuilder := ATileDownloadRequestBuilder;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FHttpDownloader := AHttpDownloader;
  FLastResponseInfo := ALastResponseInfo;

  FCS := TCriticalSection.Create;
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyEventListener.Create(Self.OnCancelEvent);
  FConfigChangeListener := TNotifyEventListener.Create(Self.OnConfigChange);
  FTileDownloaderConfig.ChangeNotifier.Add(FConfigChangeListener);
  FWasConnectError := False;
end;

destructor TTileDownloaderSimple.Destroy;
begin
  FTileDownloaderConfig.ChangeNotifier.Remove(FConfigChangeListener);
  FConfigChangeListener := nil;
  FTileDownloaderConfig := nil;

  FreeAndNil(FCS);
  FreeAndNil(FCancelEvent);
  inherited;
end;

procedure TTileDownloaderSimple.Download(
  ATileRequest: ITileRequest
);
var
  VDownloadRequest: ITileDownloadRequest;
  VTileRequestResult: ITileRequestResult;
  VDownloadResult: IDownloadResult;
  VCount: Integer;
  VTryCount: Integer;
  VResultWithRespond: IDownloadResultWithServerRespond;
  VTileRequestWithChecker: ITileRequestWithChecker;
begin
  ATileRequest.StartNotifier.Notify(ATileRequest);
  try
    if not ATileRequest.CancelNotifier.IsOperationCanceled(ATileRequest.OperationID) then begin
      FCS.Acquire;
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
                VDownloadRequest := FTileDownloadRequestBuilder.BuildRequest(
                  ATileRequest,
                  FLastResponseInfo
                );
                if ATileRequest.CancelNotifier.IsOperationCanceled(ATileRequest.OperationID) then begin
                  VTileRequestResult := TTileRequestResultCanceledAfterBuildDownloadRequest.Create(VDownloadRequest);
                  Break;
                end;
                VDownloadResult :=
                  FHttpDownloader.DoRequest(
                    VDownloadRequest,
                    ATileRequest.CancelNotifier,
                    ATileRequest.OperationID
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
                if Supports(ATileRequest, ITileRequestWithChecker, VTileRequestWithChecker) then begin
                  VTileRequestWithChecker.Checker.AfterDownload(VDownloadResult);
                end;
                VTileRequestResult := TTileRequestResultOk.Create(VDownloadResult);
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
        FCS.Release;
      end;
    end else begin
      VTileRequestResult := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
    end;
  finally
    ATileRequest.FinishNotifier.Notify(VTileRequestResult);
  end;
end;

procedure TTileDownloaderSimple.OnCancelEvent(Sender: TObject);
begin
  FCancelEvent.SetEvent;
end;

procedure TTileDownloaderSimple.OnConfigChange(Sender: TObject);
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
