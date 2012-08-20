unit i_StorageState;

interface

uses
  t_CommonTypes,
  i_Changeable;

type
  IStorageStateStatic = interface
    ['{C3CDBB82-28B7-4470-9DC6-C17A5B69F07A}']
    function GetReadAccess: TAccesState;
    property ReadAccess: TAccesState read GetReadAccess;

    function GetWriteAccess: TAccesState;
    property WriteAccess: TAccesState read GetWriteAccess;

    function GetDeleteAccess: TAccesState;
    property DeleteAccess: TAccesState read GetDeleteAccess;

    function GetAddAccess: TAccesState;
    property AddAccess: TAccesState read GetAddAccess;

    function GetReplaceAccess: TAccesState;
    property ReplaceAccess: TAccesState read GetReplaceAccess;

    function IsSame(const AValue: IStorageStateStatic): Boolean;
  end;

  IStorageStateChangeble = interface(IChangeable)
    ['{6202AB73-00A2-4711-87F4-D195CEFD9C3F}']
    function GetStatic: IStorageStateStatic;
  end;

implementation

end.
