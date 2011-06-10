unit i_DownloadResult;

interface

type
  IDownloadResult = interface
    ['{E93918EB-D64B-479E-B4D6-E49B30425824}']
    function GetUrl: string;
    property Url: string read GetUrl;

    function GetRequestHead: string;
    property RequestHead: string read GetRequestHead;

    function GetIsOk: Boolean;
    property IsOk: Boolean read GetIsOk;

    function GetIsServerExists: Boolean;
    property IsServerExists: Boolean read GetIsServerExists;
  end;

  IDownloadResultCanceled = interface(IDownloadResult)
    ['{2A22DD2C-6D70-4F27-AC7F-FB5ADB66B5A6}']
  end;

  IDownloadResultOk = interface(IDownloadResult)
    ['{EBBAA70B-60D4-421C-829D-F75CFFB43068}']
    function GetStatusCode: Cardinal;
    property StatusCode: Cardinal read GetStatusCode;

    function GetRawResponseHeader: string;
    property RawResponseHeader: string read GetRawResponseHeader;

    function GetContentType: string;
    property ContentType: string read GetContentType;

    function GetSize: Integer;
    property Size: Integer read GetSize;

    function GetBuffer: Pointer;
    property Buffer: Pointer read GetBuffer;
  end;

  IDownloadResultError = interface(IDownloadResult)
    ['{E1C06FFC-605C-4E0D-977F-DCB6FE77041D}']
    function GetErrorText: string;
    property ErrorText: string read GetErrorText;
  end;

  IDownloadResultProxyError = interface(IDownloadResultError)
    ['{E41CC6C1-5B0B-4D6F-875D-4B800DAB5A51}']
  end;

  IDownloadResultNoConnetctToServer = interface(IDownloadResultError)
    ['{0C6013F7-CD38-44EC-808D-1CA3D1B0712B}']
  end;

  IDownloadResultBanned = interface(IDownloadResultError)
    ['{C51C4998-89C4-440C-9605-A57F85BB7491}']
    function GetRawResponseHeader: string;
    property RawResponseHeader: string read GetRawResponseHeader;
  end;

  IDownloadResultBadContentType = interface(IDownloadResultError)
    ['{A8C2F27E-D1DA-43CA-8F34-4156F906D50B}']
    function GetRawResponseHeader: string;
    property RawResponseHeader: string read GetRawResponseHeader;
  end;

  IDownloadResultDataNotExists = interface(IDownloadResult)
    ['{BA3CF11A-2BD7-4541-B8F7-415E85047C20}']
    function GetReasonText: string;
    property ReasonText: string read GetReasonText;

    function GetRawResponseHeader: string;
    property RawResponseHeader: string read GetRawResponseHeader;
  end;

  IDownloadResultNotNecessary = interface(IDownloadResult)
    ['{C5E02C4F-733F-4E37-A565-700D3848E9DB}']
    function GetReasonText: string;
    property ReasonText: string read GetReasonText;

    function GetRawResponseHeader: string;
    property RawResponseHeader: string read GetRawResponseHeader;
  end;

implementation

end.
