unit i_StartUpLogoConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  IStartUpLogoConfig = interface(IConfigDataElement)
    ['{475F5EEB-3129-4365-B28A-C092D5885E3F}']
    // Отображать окошко с логотипом при запуске
    function GetIsShowLogo: Boolean;
    procedure SetIsShowLogo(AValue: Boolean);
    property IsShowLogo: Boolean read GetIsShowLogo write SetIsShowLogo;

    function GetLogo: TCustomBitmap32;
    property Logo: TCustomBitmap32 read GetLogo;
  end;

implementation

end.
