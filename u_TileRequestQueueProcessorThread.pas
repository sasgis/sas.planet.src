unit u_TileRequestQueueProcessorThread;

interface

uses
  i_NotifierOperation,
  i_Listener,
  i_ThreadConfig,
  i_TileRequest,
  i_TileRequestQueue,
  i_TileDownloader,
  u_InterfacedThread;

type
  TTileRequestQueueProcessorThread = class(TInterfacedThread)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FTileRequestQueue: ITileRequestQueue;
    FTileDownloaderSync: ITileDownloader;

    FAppClosingListener: IListener;
    procedure OnAppClosing;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATileRequestQueue: ITileRequestQueue;
      const ATileDownloaderSync: ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

{ TTileRequestQueueProcessorThread }

constructor TTileRequestQueueProcessorThread.Create(
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATileRequestQueue: ITileRequestQueue;
  const ATileDownloaderSync: ITileDownloader
);
begin
  inherited Create(AThreadConfig, Self.ClassName);
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
  FAppClosingNotifier.Remove(FAppClosingListener);
  FAppClosingListener := nil;
  FAppClosingNotifier := nil;

  FTileDownloaderSync := nil;
  FTileRequestQueue := nil;
  inherited;
end;

procedure TTileRequestQueueProcessorThread.Execute;
var
  VTileRequest: ITileRequest;
begin
  inherited;
  while not Terminated do begin
    VTileRequest := FTileRequestQueue.Pull;
    if VTileRequest <> nil then begin
      FTileDownloaderSync.Download(VTileRequest);
    end;
  end;
end;

procedure TTileRequestQueueProcessorThread.OnAppClosing;
begin
  Terminate;
end;

end.
