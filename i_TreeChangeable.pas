unit i_TreeChangeable;

interface

uses
  i_JclNotify,
  i_StaticTreeItem;

type
  ITreeChangeable = interface
    ['{5AB4FBFC-826D-412C-B40A-4C1FA7366F18}']
    function GetStatic: IStaticTreeItem;

    function GetChangeNotifier: IJclNotifier;
    property ChangeNotifier: IJclNotifier read GetChangeNotifier;
  end;

implementation

end.
