unit i_StorageStateProxy;

interface

uses
  i_StorageState,
  i_Changeable;

type
  IStorageStateProxy = interface(IChangeable)
    ['{0C95A8D3-B486-4A9A-A84B-EC209B24BA0C}']
    function GetTarget: IStorageStateChangeble;
    procedure SetTarget(const AValue: IStorageStateChangeble);
    property Target: IStorageStateChangeble read GetTarget write SetTarget;

    function GetStatic: IStorageStateStatic;
  end;

implementation

end.
