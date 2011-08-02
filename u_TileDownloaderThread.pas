unit u_TileDownloaderThread;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  i_OperationCancelNotifier,
  u_OperationCancelNotifier,
  i_TileError,
  i_TileDownloader,
  u_MapType,
  u_TileDownloaderEventElement;

type
  TTileDownloaderThread = class (TThread)
  private
    FCancelNotifierInternal: TOperationCancelNotifier;
  protected
    FCancelEvent: TEvent;
    FCancelNotifier: IOperationCancelNotifier;
    FMapType: TMapType;
    FErrorLogger: ITileErrorLogger;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FMaxRequestCount: Integer;
    FSemaphore: THandle;
    function  GetNewEventElement(
      ATile: TPoint;
      AZoom: Byte;
      ACallBack: TOnDownloadCallBack;
      ACheckExistsTileSize: Boolean;
      ACancelNotifier: IOperationCancelNotifier
    ): ITileDownloaderEvent;
    procedure Download(
      ATile: TPoint;
      AZoom: Byte;
      ACallBack: TOnDownloadCallBack;
      ACheckExistsTileSize: Boolean;
      ACancelNotifier: IOperationCancelNotifier
    );
    procedure SleepCancelable(ATime: Cardinal);
  public
    constructor Create(
      ACreateSuspended: Boolean;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger;
      AMaxRequestCount: Integer
    );
    destructor Destroy; override;
    procedure OnTileDownload(AEvent: ITileDownloaderEvent); virtual;
    procedure Terminate; reintroduce;
  end;

implementation

uses
  SysUtils;

{ TTileDownloaderThread }

constructor TTileDownloaderThread.Create(
  ACreateSuspended: Boolean;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger;
  AMaxRequestCount: Integer);
begin
  inherited Create(ACreateSuspended);
  FCancelEvent := TEvent.Create;
  FCancelNotifierInternal := TOperationCancelNotifier.Create;
  FCancelNotifier := FCancelNotifierInternal;
  FMapType := nil;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FMaxRequestCount := AMaxRequestCount;
  FSemaphore := CreateSemaphore(nil, FMaxRequestCount, FMaxRequestCount, nil);
end;

destructor TTileDownloaderThread.Destroy;
begin
  try
    Terminate;
    FreeAndNil(FCancelEvent);
    CloseHandle(FSemaphore);
  finally
    inherited Destroy;
  end;
end;

procedure TTileDownloaderThread.Terminate;
begin
  inherited;
  FCancelEvent.SetEvent;
  FCancelNotifierInternal.SetCanceled;
end;

procedure TTileDownloaderThread.SleepCancelable(ATime: Cardinal);
begin
  if ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderThread.OnTileDownload(AEvent: ITileDownloaderEvent);
begin
  ReleaseSemaphore(FSemaphore, 1, nil);
end;

function TTileDownloaderThread.GetNewEventElement(
  ATile: TPoint;
  AZoom: Byte;
  ACallBack: TOnDownloadCallBack;
  ACheckExistsTileSize: Boolean;
  ACancelNotifier: IOperationCancelNotifier
): ITileDownloaderEvent;
begin
  Result := TTileDownloaderEventElement.Create(
              FMapTileUpdateEvent,
              FErrorLogger,
              FMapType,
              ACancelNotifier
            );
  Result.AddToCallBackList(ACallBack);
  Result.TileXY := ATile;
  Result.TileZoom := AZoom;
  Result.CheckTileSize := ACheckExistsTileSize;
end;

procedure TTileDownloaderThread.Download(
  ATile: TPoint;
  AZoom: Byte;
  ACallBack: TOnDownloadCallBack;
  ACheckExistsTileSize: Boolean;
  ACancelNotifier: IOperationCancelNotifier
);
begin
  repeat // Стартуем закачку
    if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0 then begin
      FMapType.DownloadTile(
        GetNewEventElement(
          ATile,
          AZoom,
          ACallBack,
          ACheckExistsTileSize,
          ACancelNotifier
        )
      );
      Break;
    end else if Terminated then begin
      Break;
    end;
  until False;
  if not Terminated then begin
    repeat // Ждём освобождения потока(ов)
      if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0 then begin
        ReleaseSemaphore(FSemaphore, 1, nil);
        Break;
      end else if Terminated then begin
        Break;
      end;
    until False;
  end;
end;

end.
