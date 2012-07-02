unit u_TileDownloaderWithQueue;

interface

uses
  i_JclNotify,
  i_ThreadConfig,
  i_TileRequest,
  i_TileRequestQueue,
  i_TileDownloaderList,
  i_TileDownloader,
  i_ITileRequestProcessorPool,
  i_TTLCheckNotifier;

type
  TTileDownloaderWithQueue = class(TInterfacedObject, ITileDownloader)
  private
    FQueue: ITileRequestQueue;
    FSyncTileRequestProcessorPull: ITileRequestProcessorPool;
  protected
    procedure Download(
      const ATileRequest: ITileRequest
    );
  public
    constructor Create(
      const ATileDownloaderList: ITileDownloaderList;
      const AGCList: ITTLCheckNotifier;
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifier;
      AQueueCapacity: Integer
    );
  end;

implementation

uses
  u_TileRequestQueue,
  u_TileRequestProcessorPool;

{ TTileDownloaderWithQueue }

constructor TTileDownloaderWithQueue.Create(
  const ATileDownloaderList: ITileDownloaderList;
  const AGCList: ITTLCheckNotifier;
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifier;
  AQueueCapacity: Integer
);
begin
  inherited Create;
  FQueue :=
    TTileRequestQueue.Create(
      AGCList,
      AAppClosingNotifier,
      AQueueCapacity
    );
  FSyncTileRequestProcessorPull :=
    TTileRequestProcessorPool.Create(
      AGCList,
      AThreadConfig,
      AAppClosingNotifier,
      FQueue,
      ATileDownloaderList
    );
end;

procedure TTileDownloaderWithQueue.Download(const ATileRequest: ITileRequest);
begin
  FQueue.Push(ATileRequest);
  FSyncTileRequestProcessorPull.InitThreadsIfNeed;
end;

end.

