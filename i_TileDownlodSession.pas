unit i_TileDownlodSession;

interface

uses
  i_OperationCancelNotifier,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_DownloadChecker;

type
  ITileDownlodSession = interface
    ['{2F41E328-BD28-4893-AAC5-8DC93FCC2BCF}']
    function DownloadTile(
      ACancelNotifier: IOperationCancelNotifier;
      AResultFactory: IDownloadResultFactory;
      AUrl, ARequestHead: string;
      ADownloadChecker: IDownloadChecker
    ): IDownloadResult;
  end;

  ITileDownlodSessionFactory = interface
    ['{62196012-45CC-45D1-BBEF-9959636DA479}']
    function CreateSession: ITileDownlodSession;
  end;

implementation

end.
