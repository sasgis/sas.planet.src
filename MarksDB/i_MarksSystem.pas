unit i_MarksSystem;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ReadWriteState,
  i_VectorDataItemSimple,
  i_MarksFactoryConfig,
  i_ImportConfig,
  i_MarksSimple,
  i_MarkCategory,
  i_MarksDb,
  i_MarkCategoryDB,
  i_StaticTreeItem;

type
  IMarksSystem = interface
    ['{E974C3C0-499C-4BB0-B82E-34D39AFCBA9F}']
    function GetState: IReadWriteStateChangeble;
    property State: IReadWriteStateChangeble read GetState;

    function GetMarksDb: IMarksDb;
    property MarksDb: IMarksDb read GetMarksDb;

    function GetCategoryDB: IMarkCategoryDB;
    property CategoryDB: IMarkCategoryDB read GetCategoryDB;

    function GetMarksFactoryConfig: IMarksFactoryConfig;
    property MarksFactoryConfig: IMarksFactoryConfig read GetMarksFactoryConfig;

    function GetMarkByStringId(const AId: string): IMark;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;

    function ImportItemsList(
      const ADataItemList: IVectorDataItemList;
      const AImportConfig: IImportConfig;
      const ANamePrefix: string
    ): IInterfaceList;

    function GetVisibleCategories(AZoom: Byte): IInterfaceList;
    function GetVisibleCategoriesIgnoreZoom: IInterfaceList;
    procedure DeleteCategoryWithMarks(const ACategory: IMarkCategory);

    function MarksSubsetToStaticTree(const ASubset: IMarksSubset): IStaticTreeItem;
    function CategoryListToStaticTree(const AList: IInterfaceList): IStaticTreeItem;

    procedure ReadConfig(const AConfigData: IConfigDataProvider);
    procedure WriteConfig(const AConfigData: IConfigDataWriteProvider);
  end;

implementation

end.
