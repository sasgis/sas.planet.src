unit u_ReadWriteStateInternal;

interface

uses
  t_CommonTypes,
  i_ReadWriteState,
  i_ReadWriteStateInternal,
  u_ConfigDataElementBase;

type
  TReadWriteStateInternal = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IReadWriteStateInternal, IReadWriteStateChangeble)
  private
    FReadAccess: TAccesState;
    FWriteAccess: TAccesState;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetReadAccess: TAccesState;
    procedure SetReadAccess(AValue: TAccesState);

    function GetWriteAccess: TAccesState;
    procedure SetWriteAccess(AValue: TAccesState);

    function GetStatic: IReadWriteStateStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_ReadWriteStateStatic;

{ TReadWriteStateInternal }

constructor TReadWriteStateInternal.Create;
begin
  inherited Create;
  FReadAccess := asUnknown;
  FWriteAccess := asUnknown;
end;

function TReadWriteStateInternal.CreateStatic: IInterface;
var
  VStatic: IReadWriteStateStatic;
begin
  VStatic :=
    TReadWriteStateStatic.Create(
      FReadAccess,
      FWriteAccess
    );
  Result := VStatic;
end;

function TReadWriteStateInternal.GetReadAccess: TAccesState;
begin
  LockRead;
  try
    Result := FReadAccess;
  finally
    UnlockRead;
  end;
end;

function TReadWriteStateInternal.GetWriteAccess: TAccesState;
begin
  LockRead;
  try
    Result := FWriteAccess;
  finally
    UnlockRead;
  end;
end;

function TReadWriteStateInternal.GetStatic: IReadWriteStateStatic;
begin
  Result := IReadWriteStateStatic(GetStaticInternal);
end;

procedure TReadWriteStateInternal.SetReadAccess(AValue: TAccesState);
begin
  LockWrite;
  try
    if FReadAccess <> AValue then begin
      FReadAccess := AValue;
      if FReadAccess = asDisabled then begin
        FWriteAccess := asDisabled;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TReadWriteStateInternal.SetWriteAccess(AValue: TAccesState);
begin
  LockWrite;
  try
    if FWriteAccess <> AValue then begin
      FWriteAccess := AValue;
      if (FWriteAccess = asEnabled) then begin
        if (FReadAccess <> asEnabled) then begin
          FReadAccess := asEnabled;
        end;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
