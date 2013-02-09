unit i_SystemTimeProvider;

interface

uses
  i_Notifier;

type
  ISystemTimeProvider = interface
    ['{75FBE2A6-9385-4EBE-AF6E-5E4F44C340D6}']
    function GetLocalTime: TDateTime;
    function GetUTCTime: TDateTime;
    function UTCToLocalTime(const ASysTime: TDateTime): TDateTime;

    function GetSystemTimeChangedNotifier: INotifier;
    property SystemTimeChangedNotifier: INotifier read GetSystemTimeChangedNotifier;
  end;

  ISystemTimeProviderInternal = interface(ISystemTimeProvider)
    procedure SystemTimeChanged;
  end;

implementation

end.
