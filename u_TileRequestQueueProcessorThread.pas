unit u_TileRequestQueueProcessorThread;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  i_OperationNotifier,
  i_TileRequest,
  i_TileRequestQueue,
  i_TileDownloaderAsync,
  u_InterfacedThread;

type
  TTileRequestQueueProcessorThread = class(TInterfacedThread)
  private
    FTileRequestQueue: ITileRequestQueue;
    FTileDownloaderSync: ITileDownloader;
  protected
    procedure Execute; override;
  public
    constructor Create(
      ATileRequestQueue: ITileRequestQueue;
      ATileDownloaderSync: ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TTileRequestQueueProcessorThread }

constructor TTileRequestQueueProcessorThread.Create(
  ATileRequestQueue: ITileRequestQueue;
  ATileDownloaderSync: ITileDownloader
);
begin
  inherited Create;
  FTileRequestQueue := ATileRequestQueue;
  FTileDownloaderSync := ATileDownloaderSync;
end;

destructor TTileRequestQueueProcessorThread.Destroy;
begin
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

end.
