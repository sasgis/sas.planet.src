unit i_IAntiBan;

interface

uses
  Types,
  i_ITileDownlodSession;

type
  IAntiBan = interface
    ['{19B5BF44-50AA-43C9-BC2C-94A92A85A209}']
    procedure PreDownload(
      ADownloader: ITileDownlodSession;
      ATile: TPoint;
      AZoom: Byte;
      AUrl: string
    );
    function PostCheckDownload(
      ADownloader: ITileDownlodSession;
      ATile: TPoint;
      AZoom: Byte;
      AUrl: string;
      ADownloadResult: TDownloadTileResult;
      AStatusCode: Cardinal;
      AContentType: string;
      ADownloadBuffer: Pointer;
      ADownloadSize: Cardinal
    ): TDownloadTileResult;
  end;
implementation

end.
