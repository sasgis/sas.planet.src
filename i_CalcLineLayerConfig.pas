unit i_CalcLineLayerConfig;

interface

uses
  GR32,
  i_IPolyLineLayerConfig;

type
  ICalcLineLayerConfig = interface(IPolyLineLayerConfig)
    ['{6D7DFAAC-D654-4CB2-8073-5C33A0DBEE12}']
    function GetLenShow: Boolean;
    procedure SetLenShow(AValue: Boolean);
    property LenShow: Boolean read GetLenShow write SetLenShow;

    function GetTextColor: TColor32;
    procedure SetTextColor(AValue: TColor32);
    property TextColor: TColor32 read GetTextColor write SetTextColor;

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(AValue: TColor32);
    property TextBGColor: TColor32 read GetTextBGColor write SetTextBGColor;
 end;

implementation

end.
