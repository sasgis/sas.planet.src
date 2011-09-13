unit i_IPoolOfObjectsSimple;

interface

uses
  Types,
  i_OperationNotifier,
  i_PoolElement;

type
  IPoolOfObjectsSimple = interface
    ['{897D0F1B-A25C-4CEB-8CD0-5E96DDD5D543}']
    function TryGetPoolElement(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    ): IPoolElement;
    function GetPoolSize: Cardinal;
  end;

implementation

end.
