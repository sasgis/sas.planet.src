unit i_KeyMovingConfig;

interface

uses
  i_ConfigDataElement;

type
  IKeyMovingConfig = interface(IConfigDataElement)
    ['{87769678-9D11-4E47-AAE5-88F4809B7406}']
    function GetFirstKeyPressDelta: Double;
    procedure SetFirstKeyPressDelta(AValue: Double);
    property FirstKeyPressDelta: Double read GetFirstKeyPressDelta write SetFirstKeyPressDelta;

    function GetMinPixelPerSecond: Double;
    procedure SetMinPixelPerSecond(AValue: Double);
    property MinPixelPerSecond: Double read GetMinPixelPerSecond write SetMinPixelPerSecond;

    function GetMaxPixelPerSecond: Double;
    procedure SetMaxPixelPerSecond(AValue: Double);
    property MaxPixelPerSecond: Double read GetMaxPixelPerSecond write SetMaxPixelPerSecond;

    function GetSpeedChangeTime: Double;
    procedure SetSpeedChangeTime(AValue: Double);
    property SpeedChangeTime: Double read GetSpeedChangeTime write SetSpeedChangeTime;
  end;

implementation

end.
