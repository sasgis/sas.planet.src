unit i_SimpleDownloader;

interface

uses
  Classes;

type
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
  end;

implementation

end.
