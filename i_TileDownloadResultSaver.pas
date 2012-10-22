unit i_TileDownloadResultSaver;

interface

uses
  i_TileDownloaderState,
  i_TileRequestResult,
  i_DownloadResult;

type
  ITileDownloadResultSaver = interface
    ['{AD5499C5-4ED1-42A1-8BF0-A33D7C925B34}']
    function GetState: ITileDownloaderStateChangeble;
    property State: ITileDownloaderStateChangeble read GetState;

    function SaveDownloadResult(const AResult: IDownloadResult): ITileRequestResult;
  end;

implementation

end.
