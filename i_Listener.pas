unit i_Listener;

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

implementation

end.
