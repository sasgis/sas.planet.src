unit i_AppearanceHelper;

interface

uses
  t_Bitmap32,
  i_ImportConfig,
  i_MarkPicture,
  i_Appearance,
  i_AppearanceOfMarkFactory;

type
  IColorSetHelper = interface
    ['{7D35BEEA-3C43-4116-9BB2-6D7C3E64DDA7}']
    procedure Reset;

    function GetColor: TColor32;
    procedure SetColor(AValue: TColor32);
    property Color: TColor32 read GetColor write SetColor;

    function GetFound: Boolean;
    property Found: Boolean read GetFound;

    function SetGPXColorName(const AName: WideString): Boolean;
    procedure SetKMLColorValue(const AValue: LongWord);
  end;

  IIntegerSetHelper = interface
    ['{9888B573-FF22-4D4E-A85A-D6A19487B5ED}']
    procedure Reset;

    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
    property Value: Integer read GetValue write SetValue;

    function GetFound: Boolean;
    property Found: Boolean read GetFound;
  end;

  IIconSetHelper = interface
    ['{6DCC5E22-BD64-4EC0-AD11-C05E88F63A79}']
    procedure Reset;

    function GetIcon: IMarkPicture;
    procedure SetIcon(const AIcon: IMarkPicture);
    property Icon: IMarkPicture read GetIcon write SetIcon;

    function GetFound: Boolean;
    property Found: Boolean read GetFound;

    function SetByName(const AName: String): Boolean;
  end;

  IAppearanceHelper = interface
    ['{75208191-C955-44A0-B9A0-A9215BC6712C}']
    procedure Reset;

    function GetTextColor: IColorSetHelper;
    property TextColor: IColorSetHelper read GetTextColor;

    function GetLineColor: IColorSetHelper;
    property LineColor: IColorSetHelper read GetLineColor;

    function GetFillColor: IColorSetHelper;
    property FillColor: IColorSetHelper read GetFillColor;

    function GetLineWidth: IIntegerSetHelper;
    property LineWidth: IIntegerSetHelper read GetLineWidth;

    function GetTextSize: IIntegerSetHelper;
    property TextSize: IIntegerSetHelper read GetTextSize;

    function GetIconSize: IIntegerSetHelper;
    property IconSize: IIntegerSetHelper read GetIconSize;

    function GetIcon: IIconSetHelper;
    property Icon: IIconSetHelper read GetIcon;


    function GetHasPointAppearance: Boolean;
    property HasPointAppearance: Boolean read GetHasPointAppearance;

    function GetHasLineAppearance: Boolean;
    property HasLineAppearance: Boolean read GetHasLineAppearance;

    function GetHasPolygonAppearance: Boolean;
    property HasPolygonAppearance: Boolean read GetHasPolygonAppearance;


    function RedefinePointAppearance: IAppearance;
    function RedefineLineAppearance: IAppearance;
    function RedefinePolygonAppearance: IAppearance;
  end;
  
implementation

end.
