unit u_BerkeleyDBFactory;

interface

uses
  i_BinaryData,
  i_BerkeleyDB,
  i_BerkeleyDBEnv,
  i_BerkeleyDBFactory,
  i_GlobalBerkeleyDBHelper,
  u_BaseInterfacedObject;

type
  TBerkeleyDBFactory = class(TBaseInterfacedObject, IBerkeleyDBFactory)
  private
    FHelper: IGlobalBerkeleyDBHelper;
    FPageSize: Cardinal;
    FOnDeadLockRetryCount: Integer;
    FIsReadOnly: Boolean;
    FMetaKey: IBinaryData;
    FMetaValue: IBinaryData;
  private
    { IBerkeleyDBFactory }
    function CreateDatabase(
      const ADatabaseFileName: string;
      const AEnvironment: IBerkeleyDBEnvironment
    ): IBerkeleyDB;
  public
    constructor Create(
      const AHelper: IGlobalBerkeleyDBHelper;
      const APageSize: Cardinal;
      const AOnDeadLockRetryCount: Integer;
      const AIsReadOnly: Boolean;
      const AMetaKey: IBinaryData;
      const AMetaValue: IBinaryData
    );
  end;

implementation

uses
  u_BerkeleyDB;

{ TBerkeleyDBFactory }

constructor TBerkeleyDBFactory.Create(
  const AHelper: IGlobalBerkeleyDBHelper;
  const APageSize: Cardinal;
  const AOnDeadLockRetryCount: Integer;
  const AIsReadOnly: Boolean;
  const AMetaKey: IBinaryData;
  const AMetaValue: IBinaryData
);
begin
  Assert(Assigned(AHelper));
  inherited Create;
  FHelper := AHelper;
  FPageSize := APageSize;
  FOnDeadLockRetryCount := AOnDeadLockRetryCount;
  FIsReadOnly := AIsReadOnly;
  FMetaKey := AMetaKey;
  FMetaValue := AMetaValue;
end;

function TBerkeleyDBFactory.CreateDatabase(
  const ADatabaseFileName: string;
  const AEnvironment: IBerkeleyDBEnvironment
): IBerkeleyDB;
var
  VDatabase: IBerkeleyDB;
begin
  VDatabase := TBerkeleyDB.Create(
    FHelper,
    AEnvironment,
    FIsReadOnly,
    FOnDeadLockRetryCount,
    FPageSize
  );
  VDatabase.Open(ADatabaseFileName);
  if not VDatabase.Exists(FMetaKey) then begin
    VDatabase.Write(FMetaKey, FMetaValue);
  end;
  Result := VDatabase;
end;

end.
