unit i_MenuGeneratorByTree;

interface

uses
  TB2Item,
  i_StaticTreeItem;

type
  IMenuGeneratorByTree = interface
    ['{8482166C-1719-4817-A9AC-2DA7F1FE63DB}']
    procedure BuildMenu(
      ARootMenu: TTBCustomItem;
      ATree: IStaticTreeItem
    );
  end;

implementation

end.
