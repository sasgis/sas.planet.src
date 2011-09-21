unit i_GUIDListStatic;

interface

type
  IGUIDListStatic = interface
    ['{0197E18E-381E-426B-8575-4C50C6B7E11F}']
    function GetItem(AIndex: Integer): TGUID;
    property Items[AIndex: Integer]: TGUID read GetItem;

    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

implementation

end.
