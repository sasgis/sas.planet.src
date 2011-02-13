unit i_ICenterScaleConfig;

interface

uses
  GR32,
  i_IConfigDataElement;

type
  ICenterScaleConfig = interface(IConfigDataElement)
    ['{D3B5B8D5-B389-4406-9881-9704030CDD1E}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetBitmap: TCustomBitmap32;
    procedure SetBitmap(AValue: TCustomBitmap32);
    property Bitmap: TCustomBitmap32 read GetBitmap write SetBitmap;
 end;

implementation

end.
