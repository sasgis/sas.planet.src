unit u_Notifier;

interface

uses
  Classes,
  SysUtils,
  i_Notifier,
  i_Listener;

type
  TNotifierBase = class (TInterfacedObject, INotifier, INotifierInternal)
  private
    FListeners: TList;
    FSynchronizer: IReadWriteSync;
  protected
    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  protected
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

uses
  u_Synchronizer;

{ TNotifierBase }

constructor TNotifierBase.Create;
begin
  inherited Create;
  FListeners := TList.Create;
  FSynchronizer := MakeSyncRW_Std(Self, False);
end;

destructor TNotifierBase.Destroy;
var
  i: integer;
begin
  for i := 0 to FListeners.Count - 1 do begin
    IInterface(FListeners.Items[i])._Release;
  end;
  FreeAndNil(FListeners);
  inherited;
end;

procedure TNotifierBase.Add(const AListener: IListener);
var
  idx: Integer;
begin
  FSynchronizer.BeginWrite;
  try
    idx := FListeners.IndexOf(Pointer(AListener));
    if idx < 0 then begin
      FListeners.Add(Pointer(AListener));
      AListener._AddRef;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TNotifierBase.Notify(const AMsg: IInterface);
var
  idx: Integer;
  VList: array of IListener;
begin
  FSynchronizer.BeginRead;
  try
    SetLength(VList, FListeners.Count);
    for idx := 0 to FListeners.Count - 1 do
      VList[idx] := IListener(Pointer(FListeners[idx]));
  finally
    FSynchronizer.EndRead;
  end;
  for idx := 0 to Length(VList) - 1 do begin
    VList[idx].Notification(AMsg);
    VList[idx] := nil;
  end;
  VList := nil;
end;

procedure TNotifierBase.Remove(const AListener: IListener);
var
  idx: Integer;
begin
  FSynchronizer.BeginWrite;
  try
    idx := FListeners.IndexOf(Pointer(AListener));
    if idx >= 0 then begin
      FListeners.Delete(idx);
      AListener._Release;
    end;
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







