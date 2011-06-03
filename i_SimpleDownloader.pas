unit i_SimpleDownloader;

interface

uses
  Classes;

type
  ISimpleDownloader = interface
    ['{B45879E0-C88E-4C4F-954B-72991CF39FF4}']
    function GetFromIntet(
      AUrl, ARequestHead: string;
      AResultBuf: TMemoryStream;
      out AContentType, AResponseHead: string
    ): Cardinal;
  end;

implementation

end.
