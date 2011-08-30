unit i_StaticTreeBuilder;

interface

uses
  i_StaticTreeItem;

type
  IStaticTreeBuilder = interface
    ['{AAEB2DD4-E902-4C8C-9454-F289831CBDB6}']
    function BuildStatic(ASource: IInterface): IStaticTreeItem;
  end;

implementation

end.
