unit i_IPoolOfObjectsSimple;

interface

uses
  Types,
  i_IPoolElement;

type
  IPoolOfObjectsSimple = interface
    ['{897D0F1B-A25C-4CEB-8CD0-5E96DDD5D543}']
    function TryGetPoolElement(ATimeOut: Cardinal): IPoolElement;
    function GetPoolSize: Cardinal;
  end;

implementation

end.
