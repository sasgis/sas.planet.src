unit u_TileDownloaderWithQueue;

interface

uses
  i_NotifierOperation,
  i_ThreadConfig,
  i_TileRequestTask,
  i_InterfaceQueue,
  i_TileDownloaderList,
  i_TileDownloader,
  i_TileRequestProcessorPool,
  i_NotifierTime,
  u_BaseInterfacedObject;

type
  TTileDownloaderWithQueue = class(TBaseInterfacedObject, ITileDownloaderAsync)
  private
    FQueue: IInterfaceQueue;
    FSyncTileRequestProcessorPull: ITileRequestProcessorPool;
  private
    procedure Download(
      const ATileRequestTask: ITileRequestTask
    );
  public
    constructor Create(
      const ATileDownloaderList: ITileDownloaderList;
      const AGCNotifier: INotifierTime;
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      AQueueCapacity: Integer
    );
  end;

implementation

uses
  u_InterfaceQueue,
  u_TileRequestProcessorPool;

{ TTileDownloaderWithQueue }

constructor TTileDownloaderWithQueue.Create(
  const ATileDownloaderList: ITileDownloaderList;
  const AGCNotifier: INotifierTime;
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  AQueueCapacity: Integer
);
begin
  inherited Create;
  FQueue :=
    TInterfaceQueue.Create(
      AAppClosingNotifier,
      AQueueCapacity
    );
  FSyncTileRequestProcessorPull :=
    TTileRequestProcessorPool.Create(
      AGCNotifier,
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
