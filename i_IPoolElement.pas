unit i_IPoolElement;

interface

uses
  Types;

type
  IPoolElement = interface
  ['']
    function GetLastUseTime: TDateTime;
    function TryLockIfFree: Boolean;
    procedu UnLock;
    function GetObject: IUnknown;
  end;
implementation

end.
