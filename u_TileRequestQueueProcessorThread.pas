unit u_TileRequestQueueProcessorThread;

interface

uses
  i_NotifierOperation,
  i_Listener,
  i_ThreadConfig,
  i_TileRequestTask,
  i_InterfaceQueue,
  i_TileDownloader,
  u_InterfacedThread;

type
  TTileRequestQueueProcessorThread = class(TInterfacedThread)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FTileRequestQueue: IInterfaceQueue;
    FTileDownloaderSync: ITileDownloader;

    FAppClosingListener: IListener;
    procedure OnAppClosing;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATileRequestQueue: IInterfaceQueue;
      const ATileDownloaderSync: ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_TileRequestResult,
  u_ListenerByEvent;

{ TTileRequestQueueProcessorThread }

constructor TTileRequestQueueProcessorThread.Create(
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATileRequestQueue: IInterfaceQueue;
  const ATileDownloaderSync: ITileDownloader
);
begin
  inherited Create(AThreadConfig, AnsiString(Self.ClassName));
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRequestQueue := ATileRequestQueue;
  FTileDownloaderSync := ATileDownloaderSync;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

destructor TTileRequestQueueProcessorThread.Destroy;
begin
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingListener := nil;
    FAppClosingNotifier := nil;
  end;

  FTileDownloaderSync := nil;
  FTileRequestQueue := nil;
  inherited;
end;

procedure TTileRequestQueueProcessorThread.Execute;
var
  VTileRequestTask: ITileRequestTaskInternal;
  VResult: ITileRequestResult;
begin
  inherited;
  while not Terminated do begin
    if Supports(FTileRequestQueue.Pull, ITileRequestTaskInternal, VTileRequestTask) then begin
      VResult := nil;
      try
        VResult := FTileDownloaderSync.Download(VTileRequestTask.CancelNotifier, VTileRequestTask.TileRequest);
      finally
        VTileRequestTask.SetFinished(VResult);
      end;
    end;
  end;
end;

procedure TTileRequestQueueProcessorThread.OnAppClosing;
begin
  Terminate;
end;

end.
