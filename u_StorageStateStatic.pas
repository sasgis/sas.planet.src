unit u_StorageStateStatic;

interface

uses
  t_CommonTypes,
  i_StorageState;

type
  TStorageStateStatic = class(TInterfacedObject, IStorageStateStatic)
  private
    FReadAccess: TAccesState;
    FWriteAccess: TAccesState;
    FDeleteAccess: TAccesState;
    FAddAccess: TAccesState;
    FReplaceAccess: TAccesState;
  private
    function GetReadAccess: TAccesState;
    function GetWriteAccess: TAccesState;
    function GetDeleteAccess: TAccesState;
    function GetAddAccess: TAccesState;
    function GetReplaceAccess: TAccesState;
  public
    constructor Create(
      AReadAccess: TAccesState;
      AWriteAccess: TAccesState;
      ADeleteAccess: TAccesState;
      AAddAccess: TAccesState;
      AReplaceAccess: TAccesState
    );
  end;

implementation

{ TStorageStateStatic }

constructor TStorageStateStatic.Create(
  AReadAccess: TAccesState;
  AWriteAccess: TAccesState;
  ADeleteAccess: TAccesState;
  AAddAccess: TAccesState;
  AReplaceAccess: TAccesState
);
begin
  inherited Create;
  FReadAccess := AReadAccess;
  FWriteAccess := AWriteAccess;
  FDeleteAccess := ADeleteAccess;
  FAddAccess := AAddAccess;
  FReplaceAccess := AReplaceAccess;
end;


function TStorageStateStatic.GetAddAccess: TAccesState;
begin
  Result := FAddAccess;
end;

function TStorageStateStatic.GetDeleteAccess: TAccesState;
begin
  Result := FDeleteAccess;
end;

function TStorageStateStatic.GetReadAccess: TAccesState;
begin
  Result := FReadAccess;
end;

function TStorageStateStatic.GetReplaceAccess: TAccesState;
begin
  Result := FReplaceAccess;
end;

function TStorageStateStatic.GetWriteAccess: TAccesState;
begin
  Result := FWriteAccess;
end;

end.
