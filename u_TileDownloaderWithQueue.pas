unit u_TileDownloaderWithQueue;

interface

uses
  Windows,
  i_JclNotify,
  i_TileRequest,
  i_TileRequestQueue,
  i_TileDownloaderList,
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
      ATileDownloaderList: ITileDownloaderList;
      AGCList: ITTLCheckNotifier;
      AAppClosingNotifier: IJclNotifier;
      AQueueCapacity: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_TileRequestQueue,
  u_TileRequestProcessorPool;

{ TTileDownloaderWithQueue }

constructor TTileDownloaderWithQueue.Create(
  ATileDownloaderList: ITileDownloaderList;
  AGCList: ITTLCheckNotifier;
  AAppClosingNotifier: IJclNotifier;
  AQueueCapacity: Integer
);
begin
  FQueue :=
    TTileRequestQueue.Create(
      AGCList,
      AAppClosingNotifier,
      AQueueCapacity
    );
  FSyncTileRequestProcessorPull :=
    TTileRequestProcessorPool.Create(
      AGCList,
      AAppClosingNotifier,
      FQueue,
      ATileDownloaderList
    );
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
