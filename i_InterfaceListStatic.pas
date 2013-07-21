unit i_InterfaceListStatic;

interface

type
  IInterfaceListStatic = interface
    ['{7D7F60FD-18B5-4C1B-95D9-40E8CC80DD6D}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(const AIndex: Integer): IInterface;
    property Items[const AIndex: Integer]: IInterface read GetItem; default;
  end;

implementation

end.
