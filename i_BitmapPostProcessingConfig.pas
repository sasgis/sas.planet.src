unit i_BitmapPostProcessingConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  IBitmapPostProcessingConfigStatic = interface
    ['{3DBBF6CA-6AA3-4578-8D23-3E04D1D42C34}']
    function GetInvertColor: boolean;
    property InvertColor: boolean read GetInvertColor;

    // Число для гамма преобразования тайлов перед отображением
    function GetGammaN: Integer;
    property GammaN: Integer read GetGammaN;

    // Число для изменения контрастности тайлов перед отображением
    function GetContrastN: Integer;
    property ContrastN: Integer read GetContrastN;

    procedure ProcessBitmap(Bitmap: TCustomBitmap32);
  end;

  IBitmapPostProcessingConfig = interface(IConfigDataElement)
    ['{3CF3CE21-3488-495C-9A17-A2164763342E}']
    function GetInvertColor: boolean;
    procedure SetInvertColor(AValue: boolean);
    property InvertColor: boolean read GetInvertColor write SetInvertColor;

    function GetGammaN: Integer;
    procedure SetGammaN(AValue: Integer);
    property GammaN: Integer read GetGammaN write SetGammaN;

    function GetContrastN: Integer;
    procedure SetContrastN(AValue: Integer);
    property ContrastN: Integer read GetContrastN write SetContrastN;

    function GetStatic: IBitmapPostProcessingConfigStatic;
  end;

implementation

end.
