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
    FCancelEvent: TEvent;
    FWasConnectError: Boolean;
    FLastDownloadTime: Cardinal;

    procedure OnCancelEvent(Sender: TObject);
    procedure SleepCancelable(ATime: Cardinal);
    procedure SleepIfConnectErrorOrWaitInterval(
      ATileDownloaderConfigStatic: ITileDownloaderConfigStatic
    );
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
  FWasConnectError := False;
end;

destructor TTileDownloaderSimple.Destroy;
begin
  FreeAndNil(FCS);
  FreeAndNil(FCancelEvent);
  inherited;
end;

procedure TTileDownloaderSimple.Download(
  ATileRequest: ITileRequest
);
var
  VTileDownloaderConfigStatic: ITileDownloaderConfigStatic;
  VDownloadRequest: ITileDownloadRequest;
  VTileRequestResult: ITileRequestResult;
  VDownloadResult: IDownloadResult;
  VCount: Integer;
  VTryCount: Integer;
  VResultWithRespond: IDownloadResultWithServerRespond;
begin
  FCS.Acquire;
  try
    FCancelEvent.ResetEvent;
    ATileRequest.CancelNotifier.AddListener(FCancelListener);
    try
      if not ATileRequest.CancelNotifier.IsOperationCanceled(ATileRequest.OperationID) then begin
        VTileRequestResult := nil;
        ATileRequest.StartNotifier.Notify(ATileRequest);
        try
          VTileDownloaderConfigStatic := FTileDownloaderConfig.GetStatic;
          VCount := 0;
          VTryCount := VTileDownloaderConfigStatic.InetConfigStatic.DownloadTryCount;
          repeat
            SleepIfConnectErrorOrWaitInterval(VTileDownloaderConfigStatic);
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
                if Supports(VDownloadResult, IDownloadResultWithServerRespond, VResultWithRespond) then begin
                  FLastResponseInfo.ResponseHead := VResultWithRespond.RawResponseHeader;
                end;
              end else begin
                FWasConnectError := True;
              end;
            end;
            if ATileRequest.CancelNotifier.IsOperationCanceled(ATileRequest.OperationID) then begin
              VTileRequestResult := TTileRequestResultCanceledAfterDownloadRequest.Create(VDownloadResult);
              Break;
            end;
            VTileRequestResult := TTileRequestResultOk.Create(VDownloadResult);
          until (not FWasConnectError) or (VCount >= VTryCount);
        finally
          ATileRequest.FinishNotifier.Notify(VTileRequestResult);
        end;
      end;
    finally
      ATileRequest.CancelNotifier.RemoveListener(FCancelListener);
    end;
  finally
    FCS.Release;
  end;
end;

procedure TTileDownloaderSimple.OnCancelEvent(Sender: TObject);
begin
  FCancelEvent.SetEvent;
end;

procedure TTileDownloaderSimple.SleepCancelable(ATime: Cardinal);
begin
  if ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderSimple.SleepIfConnectErrorOrWaitInterval(
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

end.
