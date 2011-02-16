unit i_IDownloadUIConfig;

interface


uses
  t_CommonTypes,
  i_IConfigDataElement;

type
  IDownloadUIConfig = interface(IConfigDataElement)
    ['{CED08DA4-F287-49A5-9FF2-C7959F1712F5}']
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(AValue: TTileSource);
    property UseDownload: TTileSource read GetUseDownload write SetUseDownload;

    function GetTileMaxAgeInInternet: TDateTime;
    procedure SetTileMaxAgeInInternet(AValue: TDateTime);
    property TileMaxAgeInInternet: TDateTime read GetTileMaxAgeInInternet write SetTileMaxAgeInInternet;

    function GetTilesOut: Integer;
    procedure SetTilesOut(AValue: Integer);
    property TilesOut: Integer read GetTilesOut write SetTilesOut;
  end;


implementation

end.
