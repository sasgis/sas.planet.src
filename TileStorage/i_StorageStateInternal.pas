unit i_StorageStateInternal;

interface

uses
  t_CommonTypes,
  i_StorageState,
  i_ConfigDataElement;

type
  IStorageStateInternal = interface(IConfigDataElement)
    ['{1ABE492B-E945-4921-AA99-709090CF62F5}']
    function GetReadAccess: TAccesState;
    procedure SetReadAccess(AValue: TAccesState);
    property ReadAccess: TAccesState read GetReadAccess write SetReadAccess;

    function GetWriteAccess: TAccesState;
    procedure SetWriteAccess(AValue: TAccesState);
    property WriteAccess: TAccesState read GetWriteAccess write SetWriteAccess;

    function GetDeleteAccess: TAccesState;
    procedure SetDeleteAccess(AValue: TAccesState);
    property DeleteAccess: TAccesState read GetDeleteAccess write SetDeleteAccess;

    function GetAddAccess: TAccesState;
    procedure SetAddAccess(AValue: TAccesState);
    property AddAccess: TAccesState read GetAddAccess write SetAddAccess;

    function GetReplaceAccess: TAccesState;
    procedure SetReplaceAccess(AValue: TAccesState);
    property ReplaceAccess: TAccesState read GetReplaceAccess write SetReplaceAccess;

    function GetStatic: IStorageStateStatic;
  end;

implementation

end.
