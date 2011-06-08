unit i_DownloadChecker;

interface

uses
  Classes;

type
  IDownloadChecker = interface
    ['{70846BCE-6732-4FEB-8304-23BEFD4646D6}']
    procedure BeforeRequest(var AUrl:  string; var ARequestHead: string);
    procedure AfterResponce(
      var AStatusCode: Cardinal;
      var AContentType: string;
      var AResponseHead: string
    );
    procedure AfterReciveData(
      ARecivedData: TMemoryStream;
      var AStatusCode: Cardinal;
      var AResponseHead: string
    );
  end;

implementation

end.
