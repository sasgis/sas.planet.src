unit i_ISelectionRectLayerConfig;

interface

uses
  GR32,
  i_IConfigDataElement;

type
  ISelectionRectLayerConfig = interface(IConfigDataElement)
    ['{B8253870-8613-444C-B45C-47FD420B7EFB}']
    function GetFillColor: TColor32;
    procedure SetFillColor(AValue: TColor32);
    property FillColor: TColor32 read GetFillColor write SetFillColor;

    function GetBorderColor: TColor32;
    procedure SetBorderColor(AValue: TColor32);
    property BorderColor: TColor32 read GetBorderColor write SetBorderColor;

    function GetZoomDeltaCount: Integer;
    procedure SetZoomDeltaCount(AValue: Integer);
    property ZoomDeltaCount: Integer read GetZoomDeltaCount write SetZoomDeltaCount;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;

    function GetZoomDeltaColor(AIndex: Integer): TColor32;
    procedure SetZoomDeltaColor(AIndex: Integer; AValue: TColor32);
    property ZoomDeltaColor[AIndex: Integer]: TColor32 read GetZoomDeltaColor write SetZoomDeltaColor;

    function GetZoomDeltaColors: TArrayOfColor32;
    property ZoomDeltaColors: TArrayOfColor32 read GetZoomDeltaColors;
  end;

implementation

end.
