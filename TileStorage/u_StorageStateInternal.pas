unit u_StorageStateInternal;

interface

uses
  t_CommonTypes,
  i_TileStorageAbilities,
  i_StorageState,
  i_StorageStateInternal,
  u_ConfigDataElementBase;

type
  TStorageStateInternal = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IStorageStateInternal, IStorageStateChangeble)
  private
    FStorageForceAbilities: ITileStorageAbilities;

    FReadAccess: TAccesState;
    FWriteAccess: TAccesState;
    FDeleteAccess: TAccesState;
    FAddAccess: TAccesState;
    FReplaceAccess: TAccesState;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetReadAccess: TAccesState;
    procedure SetReadAccess(AValue: TAccesState);

    function GetWriteAccess: TAccesState;
    procedure SetWriteAccess(AValue: TAccesState);

    function GetDeleteAccess: TAccesState;
    procedure SetDeleteAccess(AValue: TAccesState);

    function GetAddAccess: TAccesState;
    procedure SetAddAccess(AValue: TAccesState);

    function GetReplaceAccess: TAccesState;
    procedure SetReplaceAccess(AValue: TAccesState);

    function GetStatic: IStorageStateStatic;
  public
    constructor Create(
      const AStorageForceAbilities: ITileStorageAbilities
    );
  end;

implementation

uses
  u_StorageStateStatic;

{ TStorageStateInternal }

constructor TStorageStateInternal.Create(
  const AStorageForceAbilities: ITileStorageAbilities
);
begin
  Assert(AStorageForceAbilities <> nil);
  inherited Create;
  FStorageForceAbilities := AStorageForceAbilities;

  FReadAccess := asUnknown;
  FWriteAccess := asUnknown;
  FDeleteAccess := asUnknown;
  FAddAccess := asUnknown;
  FReplaceAccess := asUnknown;
  if not FStorageForceAbilities.AllowRead then begin
    FReadAccess := asDisabled;
  end;

  if FStorageForceAbilities.IsReadOnly then begin
    FWriteAccess := asDisabled;
    FDeleteAccess := asDisabled;
    FAddAccess := asDisabled;
    FReplaceAccess := asDisabled;
  end else begin
    FWriteAccess := asUnknown;
    if FStorageForceAbilities.AllowAdd then begin
      FAddAccess := asUnknown;
    end else begin
      FAddAccess := asDisabled;
    end;
    if FStorageForceAbilities.AllowDelete then begin
      FDeleteAccess := asUnknown;
    end else begin
      FDeleteAccess := asDisabled;
    end;
    if FStorageForceAbilities.AllowReplace then begin
      FReplaceAccess := asUnknown;
    end else begin
      FReplaceAccess := asDisabled;
    end;
    if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
      FWriteAccess := asDisabled;
    end;
  end;
end;

function TStorageStateInternal.CreateStatic: IInterface;
var
  VStatic: IStorageStateStatic;
begin
  VStatic :=
    TStorageStateStatic.Create(
      FReadAccess,
      FWriteAccess,
      FDeleteAccess,
      FAddAccess,
      FReplaceAccess
    );
  Result := VStatic;
end;

function TStorageStateInternal.GetAddAccess: TAccesState;
begin
  LockRead;
  try
    Result := FAddAccess;
  finally
    UnlockRead;
  end;
end;

function TStorageStateInternal.GetDeleteAccess: TAccesState;
begin
  LockRead;
  try
    Result := FDeleteAccess;
  finally
    UnlockRead;
  end;
end;

function TStorageStateInternal.GetReadAccess: TAccesState;
begin
  LockRead;
  try
    Result := FReadAccess;
  finally
    UnlockRead;
  end;
end;

function TStorageStateInternal.GetReplaceAccess: TAccesState;
begin
  LockRead;
  try
    Result := FReplaceAccess;
  finally
    UnlockRead;
  end;
end;

function TStorageStateInternal.GetWriteAccess: TAccesState;
begin
  LockRead;
  try
    Result := FWriteAccess;
  finally
    UnlockRead;
  end;
end;

function TStorageStateInternal.GetStatic: IStorageStateStatic;
begin
  Result := IStorageStateStatic(GetStaticInternal);
end;

procedure TStorageStateInternal.SetAddAccess(AValue: TAccesState);
var
  VValue: TAccesState;
begin
  VValue := AValue;
  if FStorageForceAbilities.IsReadOnly or not FStorageForceAbilities.AllowAdd then begin
    VValue := asDisabled;
  end;

  LockWrite;
  try
    if FAddAccess <> VValue then begin
      FAddAccess := VValue;
      if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
        FWriteAccess := asDisabled;
      end;
      if (FAddAccess = asEnabled) then begin
        if (FWriteAccess <> asEnabled) then begin
          FWriteAccess := asEnabled;
        end;
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

procedure TStorageStateInternal.SetDeleteAccess(AValue: TAccesState);
var
  VValue: TAccesState;
begin
  VValue := AValue;
  if FStorageForceAbilities.IsReadOnly or not FStorageForceAbilities.AllowDelete then begin
    VValue := asDisabled;
  end;

  LockWrite;
  try
    if FDeleteAccess <> VValue then begin
      FDeleteAccess := VValue;
      if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
        FWriteAccess := asDisabled;
      end;
      if (FDeleteAccess = asEnabled) then begin
        if (FWriteAccess <> asEnabled) then begin
          FWriteAccess := asEnabled;
        end;
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

procedure TStorageStateInternal.SetReadAccess(AValue: TAccesState);
begin
  LockWrite;
  try
    if FReadAccess <> AValue then begin
      FReadAccess := AValue;
      if FAddAccess = asDisabled then begin
        FWriteAccess := asDisabled;
        FDeleteAccess := asDisabled;
        FAddAccess := asDisabled;
        FReplaceAccess := asDisabled;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TStorageStateInternal.SetReplaceAccess(AValue: TAccesState);
var
  VValue: TAccesState;
begin
  VValue := AValue;
  if FStorageForceAbilities.IsReadOnly or not FStorageForceAbilities.AllowReplace then begin
    VValue := asDisabled;
  end;

  LockWrite;
  try
    if FReplaceAccess <> VValue then begin
      FReplaceAccess := VValue;
      if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
        FWriteAccess := asDisabled;
      end;
      if (FReplaceAccess = asEnabled) then begin
        if (FWriteAccess <> asEnabled) then begin
          FWriteAccess := asEnabled;
        end;
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

procedure TStorageStateInternal.SetWriteAccess(AValue: TAccesState);
var
  VValue: TAccesState;
begin
  VValue := AValue;
  if FStorageForceAbilities.IsReadOnly or not (
    FStorageForceAbilities.AllowReplace or
    FStorageForceAbilities.AllowAdd or
    FStorageForceAbilities.AllowDelete
    ) then begin
    VValue := asDisabled;
  end;

  LockWrite;
  try
    if (FDeleteAccess = asDisabled) and (FAddAccess = asDisabled) and (FReplaceAccess = asDisabled) then begin
      VValue := asDisabled;
    end;
    if FWriteAccess <> VValue then begin
      FWriteAccess := VValue;
      if FWriteAccess = asDisabled then begin
        FDeleteAccess := asDisabled;
        FAddAccess := asDisabled;
        FReplaceAccess := asDisabled;
      end;
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
