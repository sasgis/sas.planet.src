unit i_SimpleDownloader;

interface

uses
  Classes;

type
  TSimpleDownloaderEvent = procedure (
    Sender: TObject;
    AResponseCode: Cardinal;
    const AContentType: string;
    const AResponseHead: string;
    AResponseBuf: TMemoryStream
  ) of object;

  ISimpleDownloader = interface
    ['{B45879E0-C88E-4C4F-954B-72991CF39FF4}']
    function GetFromInternet(
      AUrl: string;
      AAcceptEncoding: string;
      ARequestHead: string;
      ARequestBuf: TMemoryStream;
      AResponseBuf: TMemoryStream;
      out AContentType: string;
      out AResponseHead: string
    ): Cardinal;
    procedure GetFromInternetAsync(
      AUrl: string;
      AAcceptEncoding: string;
      ARequestHead: string;
      ARequestBuf: TMemoryStream;
      AOnDownload: TSimpleDownloaderEvent
    );
  end;

implementation

end.
