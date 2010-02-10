unit u_Notify;

interface

uses
  Classes,
  SysUtils,
  i_Notify;

type
  TBaseListener = class (TInterfacedObject, IListener)
  protected
    procedure Notification(msg: INotificationMessage); virtual; stdcall;
  end;

  TBaseNotificationMessage = class (TInterfacedObject, INotificationMessage)
  end;

  TBaseNotifier = class (TInterfacedObject, INotifier)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FListeners: TInterfaceList;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
  protected
    procedure Add(listener: IListener); stdcall;
    procedure Notify(msg: INotificationMessage); stdcall;
    procedure Remove(listener: IListener); stdcall;
  end;

implementation

{ TBaseNotifier }

constructor TBaseNotifier.Create;
begin
  inherited Create;
  FListeners := TInterfaceList.Create;
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TBaseNotifier.Destroy;
begin
  FSynchronizer.BeginWrite;
  try
  FreeAndNil(FListeners);
  finally
    FSynchronizer.EndWrite;
    FreeAndNil(FSynchronizer);
  end;
  inherited Destroy;
end;

procedure TBaseNotifier.Add(listener: IListener);
begin
  FSynchronizer.BeginWrite;
  try
    if FListeners.IndexOf(listener) < 0 then
      FListeners.Add(listener);
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TBaseNotifier.Notify(msg: INotificationMessage);
var
  idx: Integer;
begin
  FSynchronizer.BeginRead;
  try
    for idx := 0 to FListeners.Count - 1 do
      IListener(FListeners[idx]).Notification(msg);
  finally
    FSynchronizer.EndRead;
  end;
end;

procedure TBaseNotifier.Remove(listener: IListener);
var
  idx: Integer;
begin
  FSynchronizer.BeginWrite;
  try
    idx := FListeners.IndexOf(listener);
    if idx >= 0 then
      FListeners.Delete(idx);
  finally
    FSynchronizer.EndWrite;
  end;
end;

{ TBaseListener }

procedure TBaseListener.Notification(msg: INotificationMessage);
begin
  // do nothing; descendants should override this method to process incoming notifications
end;

end.

