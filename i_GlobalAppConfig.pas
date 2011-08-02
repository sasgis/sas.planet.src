unit i_GlobalAppConfig;

interface

uses
  i_ConfigDataElement;

type
  IGlobalAppConfig = interface(IConfigDataElement)
    ['{3DBA929F-BD4C-46A3-A64B-F61786D41FED}']
    // Показывать иконку в трее
    function GetIsShowIconInTray: Boolean;
    procedure SetIsShowIconInTray(AValue: Boolean);
    property IsShowIconInTray: Boolean read GetIsShowIconInTray write SetIsShowIconInTray;

    // Заходить на сайт автора при старте программы
    function GetIsSendStatistic: Boolean;
    procedure SetIsSendStatistic(AValue: Boolean);
    property IsSendStatistic: Boolean read GetIsSendStatistic write SetIsSendStatistic;

    // Выводить отладочную инфромацию о производительности
    function GetIsShowDebugInfo: Boolean;
    procedure SetIsShowDebugInfo(AValue: Boolean);
    property IsShowDebugInfo: Boolean read GetIsShowDebugInfo write SetIsShowDebugInfo;
  end;

implementation

end.
