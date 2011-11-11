unit i_TileDownloadChecker;

interface

uses
  i_DownloadResult,
  i_DownloadChecker;

type
  ITileDownloadChecker = interface(IDownloadChecker)
    ['{FEE65BAA-3562-4B04-9807-BC590A93A285}']
    procedure AfterDownload(AResult: IDownloadResult);
  end;

  ITileRequestWithChecker = interface
    ['{76BA399A-D581-424E-8D8E-6809C590CA30}']
    function GetChecker: ITileDownloadChecker;
    property Checker: ITileDownloadChecker read GetChecker;
  end;

implementation

end.
