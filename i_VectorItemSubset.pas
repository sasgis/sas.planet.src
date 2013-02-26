unit i_VectorItemSubset;

interface

uses
  ActiveX,
  t_GeoTypes,
  i_Category,
  i_VectorDataItemSimple;

type
  IVectorItemSubset = interface
    ['{D2DBC018-AAF5-44CB-A2B1-B5AC1C3341C5}']
    function GetSubsetByLonLatRect(const ARect: TDoubleRect): IVectorItemSubset;
    function GetSubsetByCategory(const ACategory: ICategory): IVectorItemSubset;
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IVectorDataItemSimple;
  end;

implementation

end.
