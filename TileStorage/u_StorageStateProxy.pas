unit u_StorageStateProxy;

interface

uses
  t_CommonTypes,
  i_Listener,
  i_StorageState,
  i_StorageStateProxy,
  u_ConfigDataElementBase;

type
  TStorageStateProxy = class(TConfigDataElementBaseEmptySaveLoad, IStorageStateProxy, IStorageStateChangeble)
  private
    FTarget: IStorageStateChangeble;
    FTargetChangeListener: IListener;
    FDisableStatic: IStorageStateStatic;

    FLastStatic: IStorageStateStatic;
    procedure OnTargetChange;
  private
    function GetTarget: IStorageStateChangeble;
    procedure SetTarget(const AValue: IStorageStateChangeble);

    function GetStatic: IStorageStateStatic;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_StorageStateStatic;

{ TStorageStateProxy }

constructor TStorageStateProxy.Create;
begin
  inherited Create;
  FTarget := nil;
  FDisableStatic :=
    TStorageStateStatic.Create(
      asDisabled,
      asDisabled,
      asDisabled,
      asDisabled,
      asDisabled
    );
  FLastStatic := FDisableStatic;
  FTargetChangeListener := TNotifyNoMmgEventListener.Create(Self.OnTargetChange);
end;

destructor TStorageStateProxy.Destroy;
begin
  LockWrite;
  try
    if Assigned(FTarget) and Assigned(FTargetChangeListener) then begin
      FTarget.ChangeNotifier.Remove(FTargetChangeListener);
      FTarget := nil;
      FTargetChangeListener := nil;
    end;
  finally
    UnlockWrite;
  end;

  inherited;
end;

function TStorageStateProxy.GetStatic: IStorageStateStatic;
begin
  LockRead;
  try
    Result := FLastStatic;
  finally
    UnlockRead;
  end;
end;

function TStorageStateProxy.GetTarget: IStorageStateChangeble;
begin
  LockRead;
  try
    Result := FTarget;
  finally
    UnlockRead;
  end;
end;

procedure TStorageStateProxy.OnTargetChange;
var
  VState: IStorageStateStatic;
begin
  LockWrite;
  try
    VState := nil;
    if FTarget <> nil then begin
      VState := FTarget.GetStatic;
    end;
    if VState = nil then begin
      VState := FDisableStatic;
    end;
    if not FLastStatic.IsSame(VState) then begin
      FLastStatic := VState;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TStorageStateProxy.SetTarget(const AValue: IStorageStateChangeble);
begin
  LockWrite;
  try
    if FTarget <> AValue then begin
      if FTarget <> nil then begin
        FTarget.ChangeNotifier.Remove(FTargetChangeListener);
      end;
      FTarget := AValue;
      if FTarget <> nil then begin
        FTarget.ChangeNotifier.Add(FTargetChangeListener);
      end;
      OnTargetChange;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
