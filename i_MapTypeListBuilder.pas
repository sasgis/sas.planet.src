unit i_MapTypeListBuilder;

interface

uses
  i_MapTypes,
  i_MapTypeListStatic;

type
  IMapTypeListBuilder = interface
    ['{9349108A-1B0F-4920-B273-32710FE6659B}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;

    function GetItem(AIndex: Integer): IMapType;
    procedure SetItem(AIndex: Integer; const AItem: IMapType);
    property Items[Index: Integer]: IMapType read GetItem write SetItem; default;

    procedure Add(const AItem: IMapType);
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function MakeCopy: IMapTypeListStatic;
    function MakeAndClear: IMapTypeListStatic;
  end;

  IMapTypeListBuilderFactory = interface
    ['{C5573186-2284-470D-B617-30F4C22898FF}']
    function Build: IMapTypeListBuilder;
  end;

implementation

end.
