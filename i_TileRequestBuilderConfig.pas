unit i_TileRequestBuilderConfig;

interface

uses
  i_ConfigDataElement;

type
  ITileRequestBuilderConfigStatic = interface
    ['{84B1A72C-951D-4591-80E4-3DA0CDC30ED7}']
    function  GetUrlBase: string;
    property UrlBase: string read GetUrlBase;

    function  GetRequestHeader: string;
    property RequestHeader: string read GetRequestHeader;
  end;


  ITileRequestBuilderConfig = interface(IConfigDataElement)
    ['{FA554C29-EDAF-4E3C-9B59-BC881502F33A}']
    function  GetUrlBase: string;
    procedure SetUrlBase(AValue: string);
    property UrlBase: string read GetUrlBase write SetUrlBase;

    function  GetRequestHeader: string;
    procedure SetRequestHeader(AValue: string);
    property RequestHeader: string read GetRequestHeader write SetRequestHeader;
  end;

implementation

end.
