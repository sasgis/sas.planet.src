unit i_Notify;

interface

type
  // forward declarations
  IListener = interface;
  INotificationMessage = interface;
  INotifier = interface;

  IListener = interface
    ['{26A52ECC-4C22-4B71-BC88-D0EB98AF4ED5}']
    procedure Notification(msg: INotificationMessage); stdcall;
  end;

  INotificationMessage = interface
    ['{2618CCC6-0C7D-47EE-9A91-7A7F5264385D}']
  end;

  INotifier = interface
    ['{CAAD7814-DD04-497C-91AC-558C2D5BFF81}']
    procedure Add(listener: IListener); stdcall;
    procedure Remove(listener: IListener); stdcall;
    procedure Notify(msg: INotificationMessage); stdcall;
  end;

implementation

end.

