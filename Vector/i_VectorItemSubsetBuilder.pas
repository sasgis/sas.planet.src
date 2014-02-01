unit i_VectorItemSubsetBuilder;

interface

uses
  i_VectorDataItemSimple,
  i_VectorItemSubset;

type
  IVectorItemSubsetBuilder = interface
    ['{2D06DA41-7DEB-4D01-9478-FEE9E9EF19AC}']
    procedure Clear;
    function Add(const AItem: IVectorDataItemSimple): Integer;

    function GetItem(AIndex: Integer): IVectorDataItemSimple;
    property Items[Index: Integer]: IVectorDataItemSimple read GetItem; default;

    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    procedure RemoveDuplicates;

    function MakeStaticAndClear: IVectorItemSubset;
    function MakeStaticCopy: IVectorItemSubset;
  end;

  IVectorItemSubsetBuilderFactory = interface
    ['{6322B1BA-E3E4-4D31-A20A-B27BC6174BCC}']
    function Build: IVectorItemSubsetBuilder;
  end;

implementation

end.
