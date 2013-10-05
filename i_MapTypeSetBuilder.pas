unit i_MapTypeSetBuilder;

interface

uses
  i_MapTypes,
  i_MapTypeSet;

type
  IMapTypeSetBuilder = interface
    ['{B41BE8B9-B70A-4E7D-B462-DA31513DB13A}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;

    procedure Add(const AItem: IMapType);
    procedure Clear;
    function MakeCopy: IMapTypeSet;
    function MakeAndClear: IMapTypeSet;
  end;

  IMapTypeSetBuilderFactory = interface
    ['{C5573186-2284-470D-B617-30F4C22898FF}']
    function Build(const AAllowNil: Boolean): IMapTypeSetBuilder;
  end;

implementation

end.
