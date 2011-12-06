unit i_TileDownloadResultSaver;

interface

uses
  i_DownloadResult;

type
  ITileDownloadResultSaver = interface
    ['{AD5499C5-4ED1-42A1-8BF0-A33D7C925B34}']
    procedure SaveDownloadResult(AResult: IDownloadResult);
  end;

implementation

end.
