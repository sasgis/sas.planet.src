unit i_BinaryDataListStatic;

interface

uses
  i_BinaryData;

type
  IBinaryDataListStatic = interface
    ['{FF5EB67D-D188-41A7-A051-1F0625EDBE7D}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IBinaryData;
    property Item[AIndex: Integer]: IBinaryData read GetItem;
  end;

implementation

end.
