unit i_TileDownloaderState;

interface

uses
  i_Changeable;

type
  ITileDownloaderStateStatic = interface
    ['{A0A51F5A-7F35-48A2-870D-203739666B64}']
    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;

    function GetDisableReason: string;
    property DisableReason: string read GetDisableReason;
  end;

  ITileDownloaderStateChangeble = interface(IChangeable)
    ['{48AAA474-723C-443E-940E-2D4332ED41F7}']
    function GetStatic: ITileDownloaderStateStatic;
  end;

implementation

end.
