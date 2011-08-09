unit i_BitmapMarkerProviderSimpleConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  IBitmapMarkerProviderSimpleConfigStatic = interface
    ['{EBE59B49-48A8-4657-AF1D-9C0951D5AEA9}']
    function GetMarkerSize: Integer;
    property MarkerSize: Integer read GetMarkerSize;

    function GetMarkerColor: TColor32;
    property MarkerColor: TColor32 read GetMarkerColor;

    function GetBorderColor: TColor32;
    property BorderColor: TColor32 read GetBorderColor;
  end;

  IBitmapMarkerProviderSimpleConfig = interface(IConfigDataElement)
    ['{77A05655-3105-400E-90A2-CF24DE062F0A}']
    function GetMarkerSize: Integer;
    procedure SetMarkerSize(AValue: Integer);
    property MarkerSize: Integer read GetMarkerSize write SetMarkerSize;

    function GetMarkerColor: TColor32;
    procedure SetMarkerColor(AValue: TColor32);
    property MarkerColor: TColor32 read GetMarkerColor write SetMarkerColor;

    function GetBorderColor: TColor32;
    procedure SetBorderColor(AValue: TColor32);
    property BorderColor: TColor32 read GetBorderColor write SetBorderColor;

    function GetStatic: IBitmapMarkerProviderSimpleConfigStatic;
  end;

implementation

end.
