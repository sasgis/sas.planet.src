unit u_TileRequestQueueProcessorThread;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  i_JclNotify,
  i_OperationNotifier,
  i_TileRequest,
  i_TileRequestQueue,
  i_TileDownloaderAsync,
  u_InterfacedThread;

type
  TTileRequestQueueProcessorThread = class(TInterfacedThread)
  private
    FAppClosingNotifier: IJclNotifier;
    FTileRequestQueue: ITileRequestQueue;
    FTileDownloaderSync: ITileDownloader;

    FAppClosingListener: IJclListener;
    procedure OnAppClosing;
  protected
    procedure Execute; override;
  public
    constructor Create(
      APriority: TThreadPriority;
      AAppClosingNotifier: IJclNotifier;
      ATileRequestQueue: ITileRequestQueue;
      ATileDownloaderSync: ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener;

{ TTileRequestQueueProcessorThread }

constructor TTileRequestQueueProcessorThread.Create(
  APriority: TThreadPriority;
  AAppClosingNotifier: IJclNotifier;
  ATileRequestQueue: ITileRequestQueue;
  ATileDownloaderSync: ITileDownloader
);
begin
  inherited Create(APriority);
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
