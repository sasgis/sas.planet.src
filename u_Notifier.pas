unit u_Notifier;

interface

uses
  Classes,
  SysUtils,
  i_Notifier,
  i_Listener,
  u_BaseInterfacedObject;

type
  TNotifierBase = class (TBaseInterfacedObject, INotifier, INotifierInternal)
  private
    FListeners: TList;
    FSync: IReadWriteSync;
  private
    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  private
    procedure Notify(const AMsg: IInterface);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TNotifierFaked = class (TBaseInterfacedObject, INotifier, INotifierInternal)
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
  FSync := MakeSyncRW_Std(Self, False);
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
begin
  FSync.BeginWrite;
  try
    AListener._AddRef;
    FListeners.Add(Pointer(AListener));
  finally
    FSync.EndWrite;
  end;
end;

procedure TNotifierBase.Notify(const AMsg: IInterface);
var
  idx: Integer;
  VList: array of IListener;
begin
  FSync.BeginRead;
  try
    SetLength(VList, FListeners.Count);
    for idx := 0 to FListeners.Count - 1 do
      VList[idx] := IListener(Pointer(FListeners[idx]));
  finally
    FSync.EndRead;
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
  VLastIndex: Integer;
begin
  FSync.BeginWrite;
  try
    idx := FListeners.IndexOf(Pointer(AListener));
    if idx >= 0 then begin
      VLastIndex := FListeners.Count - 1;
      if idx < VLastIndex then begin
        FListeners[idx] :=  FListeners[VLastIndex];
      end;
      FListeners.Delete(VLastIndex);
      AListener._Release;
    end;
  finally
    FSync.EndWrite;
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







