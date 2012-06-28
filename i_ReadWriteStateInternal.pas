unit i_ReadWriteStateInternal;

interface

uses
  t_CommonTypes,
  i_ReadWriteState,
  i_ConfigDataElement;

type
  IReadWriteStateInternal = interface(IConfigDataElement)
    ['{1ABE492B-E945-4921-AA99-709090CF62F5}']
    function GetReadAccess: TAccesState;
    procedure SetReadAccess(AValue: TAccesState);
    property ReadAccess: TAccesState read GetReadAccess write SetReadAccess;

    function GetWriteAccess: TAccesState;
    procedure SetWriteAccess(AValue: TAccesState);
    property WriteAccess: TAccesState read GetWriteAccess write SetWriteAccess;

    function GetStatic: IReadWriteStateStatic;
  end;

implementation

end.
