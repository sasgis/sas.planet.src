unit u_TileDownloaderWithQueue;

interface

uses
  Windows,
  i_TileRequest,
  i_TileRequestQueue,
  i_TileDownloaderAsync,
  i_ITileRequestProcessorPool,
  i_TTLCheckNotifier;

type
  TTileDownloaderWithQueue = class(TInterfacedObject, ITileDownloader)
  private
    FQueue: ITileRequestQueue;
    FSyncTileRequestProcessorPull: ITileRequestProcessorPool;
  protected
    procedure Download(
      ATileRequest: ITileRequest
    );
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      AQueueCapacity: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TTileDownloaderWithQueue }

constructor TTileDownloaderWithQueue.Create(
  AGCList: ITTLCheckNotifier;
  AQueueCapacity: Integer
);
begin
end;

destructor TTileDownloaderWithQueue.Destroy;
begin
  inherited;
end;

procedure TTileDownloaderWithQueue.Download(ATileRequest: ITileRequest);
begin
  FQueue.Push(ATileRequest);
  FSyncTileRequestProcessorPull.InitThreadsIfNeed;
end;

end.
