unit i_MarksDrawConfig;

interface

uses
  Types,
  i_ConfigDataElement;

type
  IMarksDrawConfigStatic = interface
    ['{2BC70BD2-74E8-4063-BB70-03445CBCFD00}']
    function GetShowPointCaption: Boolean;
    property ShowPointCaption: Boolean read GetShowPointCaption;

    function GetUseSimpleDrawOrder: Boolean;
    property UseSimpleDrawOrder: Boolean read GetUseSimpleDrawOrder;

    function GetOverSizeRect: TRect;
    property OverSizeRect: TRect read GetOverSizeRect;
  end;

  IMarksDrawConfig = interface(IConfigDataElement)
    ['{992DD23C-E0AA-4731-99A9-9049F55DFF6E}']
    function GetShowPointCaption: Boolean;
    procedure SetShowPointCaption(AValue: Boolean);
    property ShowPointCaption: Boolean read GetShowPointCaption write SetShowPointCaption;

    function GetUseSimpleDrawOrder: Boolean;
    procedure SetUseSimpleDrawOrder(AValue: Boolean);
    property UseSimpleDrawOrder: Boolean read GetUseSimpleDrawOrder write SetUseSimpleDrawOrder;

    function GetOverSizeRect: TRect;
    procedure SetOverSizeRect(AValue: TRect);
    property OverSizeRect: TRect read GetOverSizeRect write SetOverSizeRect;

    function GetMagnetDraw: Boolean;
    procedure SetMagnetDraw(AValue: Boolean);
    property MagnetDraw: Boolean read GetMagnetDraw write SetMagnetDraw;

    function GetStatic: IMarksDrawConfigStatic;
  end;

implementation

end.
