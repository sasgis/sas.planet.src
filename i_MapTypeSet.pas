unit i_MapTypeSet;

interface

uses
  ActiveX,
  t_Hash,
  i_MapTypes;

type
  IMapTypeSet = interface
    ['{45EF5080-01DC-4FE1-92E1-E93574439718}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IMapType;
    property Items[AIndex: Integer]: IMapType read GetItem;

    function IsEqual(const AValue: IMapTypeSet): Boolean;
    function IsExists(const AGUID: TGUID): Boolean;
    function GetMapTypeByGUID(const AGUID: TGUID): IMapType;
    function GetIterator: IEnumGUID;
    function GetMapTypeIterator: IEnumUnknown;
  end;

implementation

end.
