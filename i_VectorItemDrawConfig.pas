unit i_VectorItemDrawConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataElement;

type
  IVectorItemDrawConfigStatic = interface
    ['{8896BCE3-39C2-4481-B61C-197EAB16E3E9}']
    function GetMainColor: TColor32;
    property MainColor: TColor32 read GetMainColor;

    function GetShadowColor: TColor32;
    property ShadowColor: TColor32 read GetShadowColor;
  end;

  IVectorItemDrawConfig = interface(IConfigDataElement)
    ['{41BE92C7-D58B-4CBD-B14E-245C5C7AA9D6}']
    function GetMainColor: TColor32;
    procedure SetMainColor(AValue: TColor32);
    property MainColor: TColor32 read GetMainColor write SetMainColor;

    function GetShadowColor: TColor32;
    procedure SetShadowColor(AValue: TColor32);
    property ShadowColor: TColor32 read GetShadowColor write SetShadowColor;

    function GetStatic: IVectorItemDrawConfigStatic;
  end;

implementation

end.
