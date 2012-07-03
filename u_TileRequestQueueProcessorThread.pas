unit u_TileRequestQueueProcessorThread;

interface

uses
  i_Notifier,
  i_Listener,
  i_ThreadConfig,
  i_TileRequest,
  i_TileRequestQueue,
  i_TileDownloader,
  u_InterfacedThread;

type
  TTileRequestQueueProcessorThread = class(TInterfacedThread)
  private
    FAppClosingNotifier: INotifier;
    FTileRequestQueue: ITileRequestQueue;
    FTileDownloaderSync: ITileDownloader;

    FAppClosingListener: IListener;
    procedure OnAppClosing;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifier;
      const ATileRequestQueue: ITileRequestQueue;
      const ATileDownloaderSync: ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_NotifyEventListener;

{ TTileRequestQueueProcessorThread }

constructor TTileRequestQueueProcessorThread.Create(
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifier;
  const ATileRequestQueue: ITileRequestQueue;
  const ATileDownloaderSync: ITileDownloader
);
begin
  inherited Create(AThreadConfig);
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRequestQueue := ATileRequestQueue;
  FTileDownloaderSync := ATileDownloaderSync;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
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
