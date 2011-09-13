unit i_DownloadChecker;

interface

uses
  Classes,
  i_DownloadRequest,
  i_DownloadResult;

type
  IDownloadChecker = interface
    ['{70846BCE-6732-4FEB-8304-23BEFD4646D6}']
    function BeforeRequest(
      ARequest: IDownloadRequest
    ): IDownloadResult;
    function AfterResponse(
      ARequest: IDownloadRequest;
      var AStatusCode: Cardinal;
      var AContentType: string;
      var AResponseHead: string
    ): IDownloadResult;
    function AfterReciveData(
      ARequest: IDownloadRequest;
      const ARecivedSize: Integer;
      const ARecivedBuffer: Pointer;
      var AStatusCode: Cardinal;
      var AResponseHead: string
    ): IDownloadResult;
  end;

implementation

end.
