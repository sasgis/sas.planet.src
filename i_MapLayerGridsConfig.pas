unit i_MapLayerGridsConfig;

interface

uses
  GR32,
  t_GeoTypes,
  i_ILocalCoordConverter,
  i_ConfigDataElement;

type
  IBaseGridConfig = interface(IConfigDataElement)
    ['{A1E36D4D-2237-474E-A554-E47434449AA3}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetGridColor: TColor32;
    procedure SetGridColor(AValue: TColor32);
    property GridColor: TColor32 read GetGridColor write SetGridColor;

    function GetShowText: Boolean;
    procedure SetShowText(AValue: Boolean);
    property ShowText: Boolean read GetShowText write SetShowText;
  end;

  ITileGridConfig = interface(IBaseGridConfig)
    ['{55B99C82-8734-450D-A4C7-8150A23FF39C}']
    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(AValue: Boolean);
    property UseRelativeZoom: Boolean read GetUseRelativeZoom write SetUseRelativeZoom;

    function GetZoom: Integer;
    procedure SetZoom(AValue: Integer);
    property Zoom: Integer read GetZoom write SetZoom;

    function GetActualZoom(ALocalConverter: ILocalCoordConverter): Byte;
    function GetRectStickToGrid(ALocalConverter: ILocalCoordConverter; ASourceRect: TDoubleRect): TDoubleRect;
  end;

  IGenShtabGridConfig = interface(IBaseGridConfig)
    ['{5E125B9F-5D31-421B-B6BF-B7535123B18F}']
    function GetScale: Integer;
    procedure SetScale(AValue: Integer);
    property Scale: Integer read GetScale write SetScale;

    function GetRectStickToGrid(ALocalConverter: ILocalCoordConverter; ASourceRect: TDoubleRect): TDoubleRect;
  end;

  IMapLayerGridsConfig = interface(IConfigDataElement)
    ['{55B99C82-8734-450D-A4C7-8150A23FF39C}']
    function GetTileGrid: ITileGridConfig;
    property TileGrid: ITileGridConfig read GetTileGrid;

    function GetGenShtabGrid: IGenShtabGridConfig;
    property GenShtabGrid: IGenShtabGridConfig read GetGenShtabGrid;
  end;

implementation

end.
