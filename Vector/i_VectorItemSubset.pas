unit i_VectorItemSubset;

interface

uses
  ActiveX,
  t_Hash,
  t_GeoTypes,
  i_VectorDataItemSimple;

type
  IVectorItemSubset = interface
    ['{D2DBC018-AAF5-44CB-A2B1-B5AC1C3341C5}']
    function GetSubsetByLonLatRect(const ARect: TDoubleRect): IVectorItemSubset;
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;
    function IsEqual(const ASubset: IVectorItemSubset): Boolean;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IVectorDataItemSimple;
    property Items[AIndex: Integer]: IVectorDataItemSimple read GetItem; default;

    function GetHash: THashValue;
    property Hash: THashValue read GetHash;
  end;

implementation

end.
