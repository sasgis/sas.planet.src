unit i_DownloadRequest;

interface

type
  IDownloadRequest = interface
    ['{CE40F570-AB2A-465C-843D-0217CB2CFC47}']
    function GetUrl: string;
    property Url: string read GetUrl;

    function GetRequestHeader: string;
    property RequestHeader: string read GetRequestHeader;
  end;

implementation

end.
