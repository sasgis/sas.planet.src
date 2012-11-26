unit i_NotifierFactory;

interface

uses
  i_Notifier;

type
  INotifierFactory = interface
    ['{7BEC48C3-3281-4FD7-99E6-77C20D5C5FEA}']
    function Make(const AName: string): INotifierInternal;
  end;

implementation

end.

