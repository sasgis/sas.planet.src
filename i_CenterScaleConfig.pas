unit i_CenterScaleConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  ICenterScaleConfig = interface(IConfigDataElement)
    ['{8C83DD24-D0D4-4DAD-ACEF-9359587DDE0B}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetBitmap: TCustomBitmap32;
    procedure SetBitmap(AValue: TCustomBitmap32);
    property Bitmap: TCustomBitmap32 read GetBitmap write SetBitmap;
 end;

implementation

end.
