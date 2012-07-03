unit i_Notify;

interface

type
  IListener = interface
    ['{26A52ECC-4C22-4B71-BC88-D0EB98AF4ED5}']
    procedure Notification(const AMsg: IInterface);
  end;

  IListenerDisconnectable = interface(IListener)
    ['{7E47F99B-3D00-4743-B6EC-EBEB3257CA08}']
    procedure Disconnect;
  end;

  INotifier = interface
    ['{CAAD7814-DD04-497C-91AC-558C2D5BFF81}']
    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  end;

  INotifierInternal = interface(INotifier)
    ['{ADED3CD4-8131-422A-8009-A8CC49C07834}']
    procedure Notify(const AMsg: IInterface);
  end;

implementation

end.


