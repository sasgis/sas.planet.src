unit i_InterfaceListSimple;

interface

uses
  Classes,
  i_InterfaceListStatic;

type
  IInterfaceListSimple = interface
    ['{3B5B128E-0BCB-4DBC-8826-6DEFAC4F3152}']
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function First: IInterface;
    function IndexOf(const AItem: IInterface): Integer;
    function Add(const AItem: IInterface): Integer;
    procedure AddList(const AList: IInterfaceList);
    procedure AddListStatic(const AList: IInterfaceListStatic);
    procedure AddListSimple(const AList: IInterfaceListSimple);
    procedure Insert(AIndex: Integer; const AItem: IInterface);
    function Last: IInterface;
    function Remove(const AItem: IInterface): Integer;

    function GetItem(AIndex: Integer): IInterface;
    procedure SetItem(AIndex: Integer; const AItem: IInterface);
    property Items[Index: Integer]: IInterface read GetItem write SetItem; default;

    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;

    function GetCount: Integer;
    procedure SetCount(ANewCount: Integer);
    property Count: Integer read GetCount write SetCount;

    function MakeStaticAndClear: IInterfaceListStatic;
    function MakeStaticCopy: IInterfaceListStatic;
  end;

implementation

end.
