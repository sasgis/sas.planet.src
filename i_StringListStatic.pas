unit i_StringListStatic;

interface

type
  IStringListStatic = interface
    ['{48634CCB-0DAF-4EF9-B97E-3D4311119341}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): string;
    property Items[AIndex: Integer]: string read GetItem;

    function IndexOf(const S: string): Integer;
  end;

implementation

end.
