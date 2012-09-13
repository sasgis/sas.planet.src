unit u_TileDownloaderWithQueue;

interface

uses
  i_NotifierOperation,
  i_ThreadConfig,
  i_TileRequestTask,
  i_TileRequestQueue,
  i_TileDownloaderList,
  i_TileDownloader,
  i_ITileRequestProcessorPool,
  i_NotifierTTLCheck;

type
  TTileDownloaderWithQueue = class(TInterfacedObject, ITileDownloaderAsync)
  private
    FQueue: ITileRequestQueue;
    FSyncTileRequestProcessorPull: ITileRequestProcessorPool;
  private
    procedure Download(
      const ATileRequestTask: ITileRequestTask
    );
  public
    constructor Create(
      const ATileDownloaderList: ITileDownloaderList;
      const AGCList: INotifierTTLCheck;
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifierOneOperation;
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
  const AGCList: INotifierTTLCheck;
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifierOneOperation;
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

procedure TTileDownloaderWithQueue.Download(
  const ATileRequestTask: ITileRequestTask
);
begin
  FQueue.Push(ATileRequestTask);
  FSyncTileRequestProcessorPull.InitThreadsIfNeed;
end;

end.
