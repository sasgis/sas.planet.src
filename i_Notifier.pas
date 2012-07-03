unit i_Notifier;

interface

uses
  i_Listener;

type
  INotifier = interface
    ['{CAAD7814-DD04-497C-91AC-558C2D5BFF81}']
    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  end;

  INotifierInternal = interface(INotifier)
    procedure Notify(const AMsg: IInterface);
  end;

implementation

end.



