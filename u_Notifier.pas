unit u_Notifier;

interface

uses
  Classes,
  SysUtils,
  i_Notify;

type
  TNotifierBase = class (TInterfacedObject, INotifier, INotifierInternal)
  private
    FListeners: TInterfaceList;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
  private
    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  private
    procedure Notify(const AMsg: IInterface);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TNotifierFaked = class (TInterfacedObject, INotifier, INotifierInternal)
  private
    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  private
    procedure Notify(const AMsg: IInterface);
  end;

implementation

{ TNotifierBase }

constructor TNotifierBase.Create;
begin
  inherited Create;
  FListeners := TInterfaceList.Create;
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TNotifierBase.Destroy;
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

procedure TNotifierBase.Add(const AListener: IListener);
begin
  FSynchronizer.BeginWrite;
  try
    if FListeners.IndexOf(AListener) < 0 then
      FListeners.Add(AListener);
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TNotifierBase.Notify(const AMsg: IInterface);
var
  idx: Integer;
begin
  FSynchronizer.BeginRead;
  try
    for idx := 0 to FListeners.Count - 1 do
      IListener(FListeners[idx]).Notification(AMsg);
  finally
    FSynchronizer.EndRead;
  end;
end;

procedure TNotifierBase.Remove(const AListener: IListener);
var
  idx: Integer;
begin
  FSynchronizer.BeginWrite;
  try
    idx := FListeners.IndexOf(AListener);
    if idx >= 0 then
      FListeners.Delete(idx);
  finally
    FSynchronizer.EndWrite;
  end;
end;

{ TNotifierFaked }

procedure TNotifierFaked.Add(const AListener: IListener);
begin
  // do nothing;
end;

procedure TNotifierFaked.Notify(const AMsg: IInterface);
begin
  // do nothing;
end;

procedure TNotifierFaked.Remove(const AListener: IListener);
begin
  // do nothing;
end;

end.






