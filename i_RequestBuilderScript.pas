unit i_RequestBuilderScript;

interface

uses
  Windows;

type
  IRequestBuilderScript = interface
    ['{E7A76B96-B48D-4268-8885-C5A2A9585B59}']
    function  GenRequestUrl(ATileXY: TPoint; AZoom: Byte): string;
    procedure GenRequest(ATileXY: TPoint; AZoom: Byte; const ARawResponseHeader: string; out AUrl, ARawRequestHeader: string);

    function  GetUrlBase: string;
    procedure SetUrlBase(AValue: string);
    function  GetDefUrlBase: string;
    function  GetRawRequestHeader: string;
    procedure SetRawRequestHeader(AValue: string);
    function  GetDefRawRequestHeader: string;

    property UrlBase: string read GetUrlBase write SetUrlBase;
    property DefUrlBase: string read GetDefUrlBase;
    property RawRequestHeader: string read GetRawRequestHeader write SetRawRequestHeader;
    property DefRawRequestHeader: string read GetDefRawRequestHeader;
  end;

implementation

end.
