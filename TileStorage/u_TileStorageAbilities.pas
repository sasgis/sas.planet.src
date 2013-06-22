unit u_TileStorageAbilities;

interface

uses
  i_TileStorageAbilities,
  u_BaseInterfacedObject;

type
  TTileStorageAbilities = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    FIsReadOnly: Boolean;
    FAllowAdd: Boolean;
    FAllowDelete: Boolean;
    FAllowReplace: Boolean;
  private
    function GetIsReadOnly: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
  public
    constructor Create(
      const AIsReadOnly: Boolean;
      const AAllowAdd: Boolean;
      const AAllowDelete: Boolean;
      const AAllowReplace: Boolean
    );
  end;


implementation

{ TTileStorageAbilities }

constructor TTileStorageAbilities.Create(
  const AIsReadOnly, AAllowAdd,  AAllowDelete, AAllowReplace: Boolean
);
begin
  inherited Create;
  FIsReadOnly := AIsReadOnly or not (AAllowAdd or AAllowDelete or AAllowReplace);
  FAllowAdd := AAllowAdd and not FIsReadOnly;
  FAllowDelete := AAllowDelete and not FIsReadOnly;
  FAllowReplace := AAllowReplace and not FIsReadOnly;
end;

function TTileStorageAbilities.GetAllowAdd: Boolean;
begin
  Result := FAllowAdd;
end;

function TTileStorageAbilities.GetAllowDelete: Boolean;
begin
  Result := FAllowDelete;
end;

function TTileStorageAbilities.GetAllowReplace: Boolean;
begin
  Result := FAllowReplace;
end;

function TTileStorageAbilities.GetIsReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

end.
