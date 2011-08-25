unit i_MiniMapLayerConfig;

interface

uses
  GR32,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_ConfigDataElement;

type
  IMiniMapMapsConfig = interface(IActivMapWithLayers)
    ['{13A59AC2-947D-452F-A816-06E78602DFFA}']
    function GetActiveMiniMap: IMapType;
  end;

  IMiniMapLayerConfig = interface(IConfigDataElement)
    ['{52CF4419-A937-47E1-9A07-966736ACAA86}']
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    property Width: Integer read GetWidth write SetWidth;

    function GetZoomDelta: Integer;
    procedure SetZoomDelta(AValue: Integer);
    property ZoomDelta: Integer read GetZoomDelta write SetZoomDelta;

    function GetMasterAlpha: Integer;
    procedure SetMasterAlpha(AValue: Integer);
    property MasterAlpha: Integer read GetMasterAlpha write SetMasterAlpha;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetBottomMargin: Integer;
    procedure SetBottomMargin(AValue: Integer);
    property BottomMargin: Integer read GetBottomMargin write SetBottomMargin;

    function GetPlusButton: TCustomBitmap32;
    property PlusButton: TCustomBitmap32 read GetPlusButton;

    function GetMinusButton: TCustomBitmap32;
    property MinusButton: TCustomBitmap32 read GetMinusButton;

    function GetMapsConfig: IMiniMapMapsConfig;
    property MapsConfig: IMiniMapMapsConfig read GetMapsConfig;
  end;

implementation

end.
