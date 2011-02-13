unit i_ILastSelectionLayerConfig;

interface

uses
  GR32,
  i_IConfigDataElement;

type
  ILastSelectionLayerConfig = interface(IConfigDataElement)
    ['{D3B5B8D5-B389-4406-9881-9704030CDD1E}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetLineWidth: Integer;
    procedure SetLineWidth(AValue: Integer);
    property LineWidth: Integer read GetLineWidth write SetLineWidth;

    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);
    property LineColor: TColor32 read GetLineColor write SetLineColor;
 end;

implementation

end.
