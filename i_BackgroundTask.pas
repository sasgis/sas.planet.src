unit i_BackgroundTask;

interface

uses
  i_IThread;

type
  IBackgroundTask = interface(IThread)
    ['{89B2175F-D75C-4C2D-8202-33F0891A7CAA}']
    procedure StartExecute;
    procedure StopExecute;
  end;

implementation

end.
