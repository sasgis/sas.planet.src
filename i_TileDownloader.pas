unit i_TileDownloader;

interface

uses
  i_NotifierOperation,
  i_TileRequest,
  i_TileRequestResult,
  i_TileRequestTask;

type
  ITileDownloader = interface
    ['{79AB7B90-1F22-4B2E-B14A-BBAD3F94E26C}']
    function Download(
      const ASoftCancelNotifier: INotifierOneOperation;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ATileRequest: ITileRequest
    ): ITileRequestResult;
  end;

  ITileDownloaderAsync = interface
    ['{93075321-B145-459A-9347-1F81EA73C177}']
    procedure Download(
      const ATileRequestTask: ITileRequestTask
    );
  end;

implementation

end.
