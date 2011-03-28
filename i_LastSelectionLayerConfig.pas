unit i_LastSelectionLayerConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  ILastSelectionLayerConfig = interface(IConfigDataElement)
    ['{C4E88481-E628-473D-88A5-F85577E76416}']
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
