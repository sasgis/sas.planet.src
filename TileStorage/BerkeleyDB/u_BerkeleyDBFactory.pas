unit u_BerkeleyDBFactory;

interface

uses
  Types,
  SysUtils,
  i_Listener,
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
    FEnvironment: IBerkeleyDBEnvironment;
    FSyncCallListener: IListener;
    FIsReadOnly: Boolean;
    FMetaKey: IBinaryData;
    FMetaValue: IBinaryData;
  private
    { IBerkeleyDBFactory }
    function CreateDatabase(const ADatabaseFileName: string): IBerkeleyDB;
  public
    constructor Create(
      const AHelper: IGlobalBerkeleyDBHelper;
      const AEnvironment: IBerkeleyDBEnvironment;
      const ASyncCallListener: IListener;
      const AIsReadOnly: Boolean;
      const AMetaKey: IBinaryData;
      const AMetaValue: IBinaryData
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_BerkeleyDB;

const
  cBerkeleyDBPageSize = 1024; // 1k

{ TBerkeleyDBFactory }

constructor TBerkeleyDBFactory.Create(
  const AHelper: IGlobalBerkeleyDBHelper;
  const AEnvironment: IBerkeleyDBEnvironment;
  const ASyncCallListener: IListener;
  const AIsReadOnly: Boolean;
  const AMetaKey: IBinaryData;
  const AMetaValue: IBinaryData
);
begin
  Assert(Assigned(AHelper));
  Assert(Assigned(AEnvironment));
  inherited Create;
  FHelper := AHelper;
  FEnvironment := AEnvironment;
  FSyncCallListener := ASyncCallListener;
  FMetaKey := AMetaKey;
  FMetaValue := AMetaValue;
end;

destructor TBerkeleyDBFactory.Destroy;
begin
  FEnvironment := nil;
  FHelper := nil;
  inherited Destroy;
end;

function TBerkeleyDBFactory.CreateDatabase(
  const ADatabaseFileName: string
): IBerkeleyDB;
var
  VDatabase: IBerkeleyDB;
begin
  VDatabase := TBerkeleyDB.Create(
    FHelper,
    FEnvironment,
    FSyncCallListener,
    FIsReadOnly,
    cBerkeleyDBPageSize
  );
  VDatabase.Open(ADatabaseFileName);
  if not VDatabase.Exists(FMetaKey) then begin
    VDatabase.Write(FMetaKey, FMetaValue);
  end;
  Result := VDatabase;
end;

end.
