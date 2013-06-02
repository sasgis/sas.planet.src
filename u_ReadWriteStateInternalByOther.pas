unit u_ReadWriteStateInternalByOther;

interface

uses
  i_ReadWriteState,
  i_Listener,
  u_ConfigDataElementBase;

type
  IReadWriteStateInternalByOther = interface(IReadWriteStateChangeble)
    procedure SetOther(const AState: IReadWriteStateChangeble);
  end;

  TReadWriteStateInternalByOther = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IReadWriteStateChangeble, IReadWriteStateInternalByOther)
  private
    FOtherState: IReadWriteStateChangeble;
    FDefault: IReadWriteStateStatic;
    FOtherStateListener: IListener;
    procedure OnOtherStateChange;
  protected
    function CreateStatic: IInterface; override;
  private
    procedure SetOther(const AState: IReadWriteStateChangeble);
  private
    function GetStatic: IReadWriteStateStatic;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  t_CommonTypes,
  u_ListenerByEvent,
  u_ReadWriteStateStatic;

{ TReadWriteStateInternalByOther }

constructor TReadWriteStateInternalByOther.Create;
begin
  inherited Create;
  FDefault := TReadWriteStateStatic.Create(asDisabled, asDisabled);
  FOtherStateListener := TNotifyNoMmgEventListener.Create(Self.OnOtherStateChange);
  FOtherState := nil;
end;

destructor TReadWriteStateInternalByOther.Destroy;
begin
  if Assigned(FOtherState) and Assigned(FOtherStateListener) then begin
    FOtherState.ChangeNotifier.Remove(FOtherStateListener);
    FOtherState := nil;
  end;
  inherited;
end;

function TReadWriteStateInternalByOther.CreateStatic: IInterface;
begin
  LockRead;
  try
    if FOtherState <> nil then begin
      Result := FOtherState.GetStatic;
    end else begin
      Result := FDefault;
    end;
  finally
    UnlockRead;
  end;
end;

function TReadWriteStateInternalByOther.GetStatic: IReadWriteStateStatic;
begin
  Result := IReadWriteStateStatic(GetStaticInternal);
end;

procedure TReadWriteStateInternalByOther.OnOtherStateChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TReadWriteStateInternalByOther.SetOther(
  const AState: IReadWriteStateChangeble);
begin
  LockWrite;
  try
    if FOtherState <> AState then begin
      if FOtherState <> nil then begin
        FOtherState.ChangeNotifier.Remove(FOtherStateListener);
      end;
      FOtherState := AState;
      if FOtherState <> nil then begin
        FOtherState.ChangeNotifier.Add(FOtherStateListener);
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
