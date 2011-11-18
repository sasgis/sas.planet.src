unit u_TileDownloaderAsyncByThread;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  i_OperationNotifier,
  i_TileRequest,
  i_TileDownloaderAsync,
  u_InterfacedThread;

type
  TTileDownloaderAsyncByThread = class(TInterfacedThread, ITileDownloader)
  private
    FTileDownloaderSync: ITileDownloader;
    FOnFinishEvent: TNotifyEvent;

    FTaskReadySemaphore: THandle;
    FStopThreadEvent: TEvent;

    FTileRequest: ITileRequest;
    FCancelNotifier: IOperationNotifier;
    FOperationID: Integer;
  protected
    procedure Execute; override;
    procedure Terminate; override;
  protected
    procedure Download(
      ATileRequest: ITileRequest;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    );
  public
    constructor Create(
      AOnFinishEvent: TNotifyEvent;
      ATileDownloaderSync: ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TTileDownloaderAsyncByThread }

constructor TTileDownloaderAsyncByThread.Create(
  AOnFinishEvent: TNotifyEvent;
  ATileDownloaderSync: ITileDownloader
);
begin
  inherited Create;
  FTileDownloaderSync := ATileDownloaderSync;
  FOnFinishEvent := AOnFinishEvent;

  FTaskReadySemaphore := CreateSemaphore(nil, 0, 1, nil);
  FStopThreadEvent := TEvent.Create;
end;

destructor TTileDownloaderAsyncByThread.Destroy;
begin
  if FTaskReadySemaphore <> 0 then begin
    CloseHandle(FTaskReadySemaphore);
  end;
  FreeAndNil(FStopThreadEvent);
  FTileDownloaderSync := nil;
  inherited;
end;

procedure TTileDownloaderAsyncByThread.Download(
  ATileRequest: ITileRequest;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
);
begin
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FTileRequest := ATileRequest;
  ReleaseSemaphore(FTaskReadySemaphore, 1, nil);
end;

procedure TTileDownloaderAsyncByThread.Execute;
var
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  inherited;
  VHandles[0] := FTaskReadySemaphore;
  VHandles[1] := FStopThreadEvent.Handle;
  while not Terminated do begin
    VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
    case VWaitResult of
      WAIT_OBJECT_0:
      begin
        FTileDownloaderSync.Download(FTileRequest, FCancelNotifier, FOperationID);
        FOnFinishEvent(nil);
      end;
    end;
  end;

end;

procedure TTileDownloaderAsyncByThread.Terminate;
begin
  inherited;
  FStopThreadEvent.SetEvent;
end;

end.
