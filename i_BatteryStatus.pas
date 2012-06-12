unit i_BatteryStatus;

interface

uses
  i_Changeable;

type
  IBatteryStatusStatic = interface
    ['{5A0E6C17-5834-40FA-8B16-6B00BCDAC175}']
    function GetACLineStatus: Byte;
    property ACLineStatus: Byte read GetACLineStatus;

    function GetBatteryFlag: Byte;
    property BatteryFlag: Byte read GetBatteryFlag;

    function GetBatteryLifePercent: Byte;
    property BatteryLifePercent: Byte read GetBatteryLifePercent;

    function GetBatteryLifeTime: LongWord;
    property BatteryLifeTime: LongWord read GetBatteryLifeTime;
  end;

  IBatteryStatus = interface(IChangeable)
    ['{ABA8A37A-AAA2-4E93-ADC1-6BC0E75D3238}']
    function GetStatic: IBatteryStatusStatic;
  end;


implementation

end.
