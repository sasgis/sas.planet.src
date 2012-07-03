unit u_Notifier;

interface

uses
  Classes,
  SysUtils,
  i_Notify;

type
  TBaseNotifier = class (TInterfacedObject, INotifier)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FListeners: TInterfaceList;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
  protected
    procedure Add(const listener: IListener);
    procedure Notify(const msg: IInterface);
    procedure Remove(const listener: IListener);
  end;

  TBaseNotifierFaked = class (TInterfacedObject, INotifier)
  protected
    procedure Add(const listener: IListener);
    procedure Notify(const msg: IInterface);
    procedure Remove(const listener: IListener);
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

procedure TBaseNotifier.Add(const listener: IListener);
begin
  FSynchronizer.BeginWrite;
  try
    if FListeners.IndexOf(listener) < 0 then
      FListeners.Add(listener);
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TBaseNotifier.Notify(const msg: IInterface);
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

procedure TBaseNotifier.Remove(const listener: IListener);
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

{ TBaseNotifierFaked }

procedure TBaseNotifierFaked.Add(const listener: IListener);
begin
  // do nothing;
end;

procedure TBaseNotifierFaked.Notify(const msg: IInterface);
begin
  // do nothing;
end;

procedure TBaseNotifierFaked.Remove(const listener: IListener);
begin
  // do nothing;
end;

end.




