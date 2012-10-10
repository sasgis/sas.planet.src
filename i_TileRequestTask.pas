unit i_TileRequestTask;

interface

uses
  i_NotifierOperation,
  i_TileRequest,
  i_TileRequestResult;

type
  ITileRequestTask = interface
    ['{1B2C3DD6-4527-4366-B498-D1B825D05B23}']
    function GetTileRequest: ITileRequest;
    property TileRequest: ITileRequest read GetTileRequest;

    function GetCancelNotifier: INotifierOneOperation;
    property CancelNotifier: INotifierOneOperation read GetCancelNotifier;

    function GetResult: ITileRequestResult;
    property Result: ITileRequestResult read GetResult;

    function GetFinishNotifier: INotifierOneOperation;
    property FinishNotifier: INotifierOneOperation read GetFinishNotifier;
  end;

  ITileRequestTaskInternal = interface(ITileRequestTask)
    ['{1F2A8AAD-A290-4019-8E81-7A33227EF877}']
    procedure SetFinished(const AResult: ITileRequestResult);
  end;

implementation

end.
