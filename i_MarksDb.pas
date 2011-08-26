unit i_MarksDb;

interface

uses
  Classes,
  t_GeoTypes,
  i_MarkCategory,
  i_MarkFactory,
  i_MarksSimple;

type
  IMarksDb = interface
    ['{0B5DFEC6-E519-4D06-8DBA-2D24E2F9A372}']
    function GetMarkByID(AMarkId: IMarkId): IMarkFull;
    function DeleteMark(AMarkId: IMarkId): Boolean;
    procedure DeleteMarksByCategoryID(ACategory: IMarkCategory);
    procedure WriteMark(AMark: IMarkFull);
    procedure WriteMarksList(AMarkList: IInterfaceList);
    procedure SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
    function GetMarkVisible(AMark: IMarkId): Boolean; overload;
    function GetMarkVisible(AMark: IMarkFull): Boolean; overload;
    function GetAllMarskIdList: IInterfaceList;
    function GetMarskIdListByCategory(ACategory: IMarkCategory): IInterfaceList;

    procedure SetAllMarksInCategoryVisible(ACategory: IMarkCategory; ANewVisible: Boolean);

    function GetMarksSubset(ARect: TDoubleRect; ACategoryList: IInterfaceList; AIgnoreVisible: Boolean): IMarksSubset;

    function GetFactory: IMarkFactory;
    property Factory: IMarkFactory read GetFactory;
  end;

implementation

end.
