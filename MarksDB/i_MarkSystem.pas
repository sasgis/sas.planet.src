unit i_MarkSystem;

interface

uses
  i_ReadWriteState,
  i_ImportConfig,
  i_InterfaceListStatic,
  i_VectorItemTree,
  i_VectorDataItemSimple,
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

    function GetStringIdByMark(const AMark: IVectorDataItemSimple): string;
    function GetMarkByStringId(const AId: string): IVectorDataItemSimple;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;

    function ImportItemsTree(
      const ADataItemTree: IVectorItemTree;
      const AImportConfig: IImportConfig
    ): IInterfaceListStatic;

    procedure DeleteCategoryWithMarks(const ACategory: IMarkCategory);

    function CategoryTreeToMarkTree(
      const ACategoryTree: IStaticTreeItem;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemTree;
  end;

implementation

end.
