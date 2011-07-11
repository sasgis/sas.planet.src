unit i_DownloadChecker;

interface

uses
  Classes,
  i_DownloadResult;

type
  IDownloadChecker = interface
    ['{70846BCE-6732-4FEB-8304-23BEFD4646D6}']
    function BeforeRequest(
      const AUrl:  string;
      const ARequestHead: string
    ): IDownloadResult;
    function AfterResponse(
      var AStatusCode: Cardinal;
      var AContentType: string;
      var AResponseHead: string
    ): IDownloadResult;
    function AfterReciveData(
      const ARecivedSize: Integer;
      const ARecivedBuffer: Pointer;
      var AStatusCode: Cardinal;
      var AResponseHead: string
    ): IDownloadResult;
  end;

implementation

end.
