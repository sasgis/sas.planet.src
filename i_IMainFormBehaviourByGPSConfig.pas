unit i_IMainFormBehaviourByGPSConfig;

interface

uses
  i_IConfigDataElement;

type
  IMainFormBehaviourByGPSConfig = interface(IConfigDataElement)
    ['{2236C930-B6F6-4A6D-A8EB-FC83999973EA}']
    // Двигать карту при изменении gps-координат
    function GetMapMove: Boolean;
    procedure SetMapMove(AValue: Boolean);
    property MapMove: Boolean read GetMapMove write SetMapMove;

    // Центрировать карту на GPS позиции
    function GetMapMoveCentered: Boolean;
    procedure SetMapMoveCentered(AValue: Boolean);
    property MapMoveCentered: Boolean read GetMapMoveCentered write SetMapMoveCentered;

    // Расстояние, на которое должено измениться положение GPS что бы вызвать сдиг карты
    function GetMinMoveDelta: Double;
    procedure SetMinMoveDelta(AValue: Double);
    property MinMoveDelta: Double read GetMinMoveDelta write SetMinMoveDelta;

    // Скрывать/показывать панель датчиков при подключении/отключении GPS
    function GetSensorsAutoShow: Boolean;
    procedure SetSensorsAutoShow(AValue: Boolean);
    property SensorsAutoShow: Boolean read GetSensorsAutoShow write SetSensorsAutoShow;

    // Двигать карту и перерисовывать трек только если главное окно активно
    function GetProcessGPSIfActive: Boolean;
    procedure SetProcessGPSIfActive(AValue: Boolean);
    property ProcessGPSIfActive: Boolean read GetProcessGPSIfActive write SetProcessGPSIfActive;
  end;

implementation

end.
