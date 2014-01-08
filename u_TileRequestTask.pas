unit u_TileRequestTask;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_TileRequest,
  i_TileRequestResult,
  i_TileRequestTask,
  u_BaseInterfacedObject;

type
  TTileRequestTask = class(TBaseInterfacedObject, ITileRequestTask, ITileRequestTaskInternal)
  private
    FTileRequest: ITileRequest;
    FCancelNotifier: INotifierOneOperation;
    FFinishNotifier: ITileRequestTaskFinishNotifier;
  private
    { ITileRequestTask }
    function GetTileRequest: ITileRequest;
    function GetCancelNotifier: INotifierOneOperation;
    { ITileRequestTaskInternal }
    procedure SetFinished(const AResult: ITileRequestResult);
  public
    constructor Create(
      const ATileRequest: ITileRequest;
      const ACancelNotifier: INotifierOneOperation;
      const AFinishNotifier: ITileRequestTaskFinishNotifier
    );
  end;

  TTileRequestTaskFinishNotifierCallBack = procedure(
    const ATask: ITileRequestTask;
    const AResult: ITileRequestResult
  ) of object;

  TTileRequestTaskFinishNotifier = class(TBaseInterfacedObject, ITileRequestTaskFinishNotifier)
  private
    FCallBack: TTileRequestTaskFinishNotifierCallBack;
  private
    procedure Notify(
      const ATask: ITileRequestTask;
      const AResult: ITileRequestResult
    );
  public
    constructor Create(const ACallBack: TTileRequestTaskFinishNotifierCallBack);
  end;


implementation

{ TTileRequestTask }

constructor TTileRequestTask.Create(
  const ATileRequest: ITileRequest;
  const ACancelNotifier: INotifierOneOperation;
  const AFinishNotifier: ITileRequestTaskFinishNotifier
);
begin
  Assert(AFinishNotifier <> nil);
  inherited Create;
  FTileRequest := ATileRequest;
  FCancelNotifier := ACancelNotifier;
  FFinishNotifier := AFinishNotifier;
end;

function TTileRequestTask.GetCancelNotifier: INotifierOneOperation;
begin
  Result := FCancelNotifier;
end;

function TTileRequestTask.GetTileRequest: ITileRequest;
begin
  Result := FTileRequest;
end;

procedure TTileRequestTask.SetFinished(const AResult: ITileRequestResult);
begin
  FFinishNotifier.Notify(Self, AResult);
end;

{ TTileRequestTaskFinishNotifier }

constructor TTileRequestTaskFinishNotifier.Create(
 const ACallBack: TTileRequestTaskFinishNotifierCallBack
);
begin
  Assert(Assigned(ACallBack));
  inherited Create;
  FCallBack := ACallBack;
end;

procedure TTileRequestTaskFinishNotifier.Notify(
  const ATask: ITileRequestTask;
  const AResult: ITileRequestResult
);
begin
  FCallBack(ATask, AResult);
end;

end.
