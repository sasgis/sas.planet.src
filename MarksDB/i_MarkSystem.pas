unit i_MarkSystem;

interface

uses
  i_ReadWriteState,
  i_ImportConfig,
  i_InterfaceListStatic,
  i_VectorItemSubset,
  i_VectorItemTree,
  i_Mark,
  i_MarkCategory,
  i_MarkDb,
  i_MarkCategoryDB,
  i_StaticTreeItem;

type
  IMarkSystem = interface
    ['{E974C3C0-499C-4BB0-B82E-34D39AFCBA9F}']
    function GetState: IReadWriteStateChangeble;
    property State: IReadWriteStateChangeble read GetState;

    function GetMarkDb: IMarkDb;
    property MarkDb: IMarkDb read GetMarkDb;

    function GetCategoryDB: IMarkCategoryDB;
    property CategoryDB: IMarkCategoryDB read GetCategoryDB;

    function GetStringIdByMark(const AMark: IMark): string;
    function GetMarkByStringId(const AId: string): IMark;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;

    function ImportItemsTree(
      const ADataItemTree: IVectorItemTree;
      const AImportConfig: IImportConfig
    ): IInterfaceListStatic;

    function GetVisibleCategories(AZoom: Byte): IInterfaceListStatic;
    function GetVisibleCategoriesIgnoreZoom: IInterfaceListStatic;
    procedure DeleteCategoryWithMarks(const ACategory: IMarkCategory);

    function CategoryTreeToMarkTree(const ACategoryTree: IStaticTreeItem): IVectorItemTree;
    function CategoryListToStaticTree(const AList: IInterfaceListStatic): IStaticTreeItem;
  end;

implementation

end.
