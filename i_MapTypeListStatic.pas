unit i_MapTypeListStatic;

interface

uses
  t_Hash,
  i_MapTypes;

type
  IMapTypeListStatic = interface
    ['{0A48D2E0-5C39-4E1A-A438-B50535E6D69B}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IMapType;
    property Items[AIndex: Integer]: IMapType read GetItem;

    function IsEqual(const AValue: IMapTypeListStatic): Boolean;
  end;

implementation

end.
