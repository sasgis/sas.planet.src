unit i_FillingMapLayerConfig;

interface

uses
  GR32,
  i_LocalCoordConverter,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_ConfigDataElement;

type
  IFillingMapLayerConfigStatic = interface
  ['{6D257213-D59E-45D8-A632-6499B2549C64}']
    function GetVisible: Boolean;
    property Visible: Boolean read GetVisible;

    function GetSourceMap: IMapType;
    property SourceMap: IMapType read GetSourceMap;

    function GetUseRelativeZoom: Boolean;
    property UseRelativeZoom: Boolean read GetUseRelativeZoom;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetNoTileColor: TColor32;
    property NoTileColor: TColor32 read GetNoTileColor;

    function GetShowTNE: Boolean;
    property ShowTNE: Boolean read GetShowTNE;

    function GetTNEColor: TColor32;
    property TNEColor: TColor32 read GetTNEColor;

    function GetActualZoom(ALocalConverter: ILocalCoordConverter): Byte;
  end;

  IFillingMapMapsConfig = interface(IMainActiveMap)
    ['{5000C2DF-8CE0-4BC6-9238-28C0367C0C83}']
    function GetActualMap: IMapType;
  end;

  IFillingMapLayerConfig = interface(IConfigDataElement)
    ['{5A89A65C-7145-4063-8B8E-357DEEF9DC66}']
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(const AValue: Boolean);
    property UseRelativeZoom: Boolean read GetUseRelativeZoom write SetUseRelativeZoom;

    function GetZoom: Byte;
    procedure SetZoom(const AValue: Byte);
    property Zoom: Byte read GetZoom write SetZoom;

    function GetNoTileColor: TColor32;
    procedure SetNoTileColor(const AValue: TColor32);
    property NoTileColor: TColor32 read GetNoTileColor write SetNoTileColor;

    function GetShowTNE: Boolean;
    procedure SetShowTNE(const AValue: Boolean);
    property ShowTNE: Boolean read GetShowTNE write SetShowTNE;

    function GetTNEColor: TColor32;
    procedure SetTNEColor(const AValue: TColor32);
    property TNEColor: TColor32 read GetTNEColor write SetTNEColor;

    function GetSourceMap: IFillingMapMapsConfig;
    function GetStatic: IFillingMapLayerConfigStatic;
    function GetActualZoom(ALocalConverter: ILocalCoordConverter): Byte;
  end;

implementation

end.
