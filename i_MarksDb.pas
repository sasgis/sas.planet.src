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
    function GetMarkByID(AMarkId: IMarkId): IMark;
    function DeleteMark(AMarkId: IMarkId): Boolean;
    procedure DeleteMarksByCategoryID(ACategory: ICategory);
    procedure WriteMark(AMark: IMark);
    procedure WriteMarksList(AMarkList: IInterfaceList);
    procedure SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
    function GetMarkVisible(AMark: IMarkId): Boolean; overload;
    function GetMarkVisible(AMark: IMark): Boolean; overload;
    function GetAllMarskIdList: IInterfaceList;
    function GetMarskIdListByCategory(ACategory: ICategory): IInterfaceList;

    procedure SetAllMarksInCategoryVisible(ACategory: ICategory; ANewVisible: Boolean);

    function GetMarksSubset(ARect: TDoubleRect; ACategoryList: IInterfaceList; AIgnoreVisible: Boolean): IMarksSubset;

    function GetFactory: IMarkFactory;
    property Factory: IMarkFactory read GetFactory;
  end;

implementation

end.
