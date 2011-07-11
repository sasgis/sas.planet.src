unit i_DownloadUIConfig;

interface

uses
  t_CommonTypes,
  i_ConfigDataElement;

type
  IDownloadUIConfig = interface(IConfigDataElement)
    ['{CED08DA4-F287-49A5-9FF2-C7959F1712F5}']
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(const AValue: TTileSource);
    property UseDownload: TTileSource read GetUseDownload write SetUseDownload;

    function GetTileMaxAgeInInternet: TDateTime;
    procedure SetTileMaxAgeInInternet(const AValue: TDateTime);
    property TileMaxAgeInInternet: TDateTime read GetTileMaxAgeInInternet write SetTileMaxAgeInInternet;

    function GetTilesOut: Integer;
    procedure SetTilesOut(const AValue: Integer);
    property TilesOut: Integer read GetTilesOut write SetTilesOut;
  end;


implementation

end.
