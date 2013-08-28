unit i_MarkId;

interface

uses
  i_Category,
  i_Mark;

type
  IMarkId = interface
    ['{A3FE0170-8D32-4777-A3EA-53D678875B7B}']
    function GetCategory: ICategory;
    property Category: ICategory read GetCategory;

    function GetName: string;
    property Name: string read GetName;

    function GetMarkType: TGUID;
    property MarkType: TGUID read GetMarkType;

    function IsSameId(const AMarkId: IMarkId): Boolean;
    function IsSameMark(const AMark: IMark): Boolean;
  end;

implementation

end.
