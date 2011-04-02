unit i_MarkCategoryDB;

interface

uses
  Classes,
  i_MarkCategory,
  i_MarkCategoryFactory;

type
  IMarkCategoryDB = interface
    ['{F418B319-3B89-4B09-BC9E-0E4FC684BADF}']
    function GetCategoryByName(AName: string): IMarkCategory;
    function WriteCategory(ACategory: IMarkCategory): IMarkCategory;
    procedure DeleteCategory(ACategory: IMarkCategory);

    function GetCategoriesList: IInterfaceList;
    procedure SetAllCategoriesVisible(ANewVisible: Boolean);

    function GetFactory: IMarkCategoryFactory;
    property Factory: IMarkCategoryFactory read GetFactory;
  end;

implementation

end.
