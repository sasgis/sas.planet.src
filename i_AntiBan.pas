unit i_AntiBan;

interface

uses
  Types,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_TileDownlodSession;

type
  IAntiBan = interface
    ['{19B5BF44-50AA-43C9-BC2C-94A92A85A209}']
    procedure PreDownload(
      ADownloader: ITileDownlodSession;
      const ATile: TPoint;
      const AZoom: Byte;
      const AUrl: string
    );
    function PostCheckDownload(
      AResultFactory: IDownloadResultFactory;
      ADownloader: ITileDownlodSession;
      ADownloadResult: IDownloadResult
    ): IDownloadResult;
  end;

implementation

end.
