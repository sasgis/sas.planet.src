unit i_MarkSystemImplChangeable;

interface

uses
  i_Changeable,
  i_ReadWriteState,
  i_MarkSystemImpl;

type
  IMarkSystemImplChangeable = interface(IChangeable)
    ['{10C330D7-959D-415F-A0EF-E2372761212C}']
    function GetState: IReadWriteStateChangeble;
    property State: IReadWriteStateChangeble read GetState;

    function GetStatic: IMarkSystemImpl;
  end;

implementation

end.
