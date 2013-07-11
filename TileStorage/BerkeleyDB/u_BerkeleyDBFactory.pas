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
  SysUtils,
  libdb51,
  t_BerkeleyDB,
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
  Assert(AHelper <> nil);
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
  I: Integer;
  VDatabase: IBerkeleyDB;
  VIsNew: Boolean;
  VIsDeadLock: Boolean;
  VTransaction: PBerkeleyTxn;
begin
  Result := nil;
  
  VDatabase := TBerkeleyDB.Create(
    FHelper,
    AEnvironment,
    FIsReadOnly,
    FOnDeadLockRetryCount,
    FPageSize
  );

  VDatabase.LockWrite;
  try
    VIsNew := not FileExists(ADatabaseFileName);

    VDatabase.Open(ADatabaseFileName);
    Result := VDatabase;

    if VIsNew and not FIsReadOnly then begin
      I := 0;
      VIsDeadLock := False;
      repeat
        Inc(I);
        AEnvironment.TransactionBegin(VTransaction);
        try
          if not VDatabase.Exists(FMetaKey, VTransaction, VIsDeadLock, DB_RMW) then begin
            if VIsDeadLock then begin
              AEnvironment.TransactionAbort(VTransaction);
              Sleep(50);
              Continue;
            end else begin
              if VDatabase.Write(FMetaKey, FMetaValue, VTransaction, VIsDeadLock) then begin
                AEnvironment.TransactionCommit(VTransaction);
                Break;
              end else if VIsDeadLock then begin
                AEnvironment.TransactionAbort(VTransaction);
                Sleep(50);
                Continue;
              end else begin
                AEnvironment.TransactionAbort(VTransaction);
                Assert(False, 'Can''t write MetaKey to ' + ADatabaseFileName);
                Break;
              end;
            end;
          end else begin
            AEnvironment.TransactionAbort(VTransaction);
            Break;
          end;
        except
          on E: Exception do begin
            AEnvironment.TransactionAbort(VTransaction);
            raise E;
          end;
        end;
      until I > FOnDeadLockRetryCount;
      if VIsDeadLock then begin
        CheckBDB(DB_LOCK_DEADLOCK);
      end;
    end;
  finally
    VDatabase.UnLockWrite;
  end;
end;

end.
