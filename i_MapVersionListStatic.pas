unit i_MapVersionListStatic;

interface

uses
  i_MapVersionInfo;

type
  IMapVersionListStatic = interface
    ['{6F7FA14E-6DC8-43B8-86FA-10541B204D77}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IMapVersionInfo;
    property Item[AIndex: Integer]: IMapVersionInfo read GetItem;

    function GetSorted: Boolean;
    property Sorted: Boolean read GetSorted;
  end;

implementation

end.
