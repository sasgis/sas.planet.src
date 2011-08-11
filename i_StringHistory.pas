unit i_StringHistory;

interface

uses
  i_ConfigDataElement;

type
  IStringHistory = interface(IConfigDataElement)
    ['{945058AC-426D-4498-80E7-77474D86C694}']
    function GetCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): string;
    procedure RemoveItem(AIndex: Integer);
    procedure AddItem(AValue: string);
  end;

implementation

end.
