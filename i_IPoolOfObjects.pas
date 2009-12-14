unit i_IPoolOfObjects;

interface

uses
  Types,
  i_IPoolElement;

type
  IPoolOfObjects = interface
  ['{897D0F1B-A25C-4CEB-8CD0-5E96DDD5D543}']
    function TryGetPoolElement(ATimeOut: Cardinal): IPoolElement;
    function GetPoolSize: Cardinal;
    function GetPoolMaxSize: Cardinal;
    procedure SetPoolMaxSize(ASize: Cardinal);
  end;
implementation

end.
