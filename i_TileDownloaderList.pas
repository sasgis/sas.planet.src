unit i_TileDownloaderList;

interface

uses
  i_Notifier,
  i_TileDownloader;

type
  ITileDownloaderListStatic = interface
    ['{09A9BF9E-8714-484F-9247-F9820138A8D7}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): ITileDownloader;
    property Item[AIndex: Integer]: ITileDownloader read GetItem;
  end;

  ITileDownloaderList = interface
    ['{6271C038-A418-4FD8-B958-D1BA9B57A51F}']
    function GetStatic: ITileDownloaderListStatic;

    function GetChangeNotifier: INotifier;
    property ChangeNotifier: INotifier read GetChangeNotifier;
  end;

implementation

end.
