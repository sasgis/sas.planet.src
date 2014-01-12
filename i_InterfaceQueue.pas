unit i_InterfaceQueue;

interface

type
  IInterfaceQueue = interface
    ['{49C05F75-E54F-4C5A-926E-67C60AF2F9EA}']
    procedure Push(const AObj: IInterface);
    function Pull: IInterface;

    function GetIsEmpty: Boolean;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

implementation

end.
