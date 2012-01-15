unit u_IdCacheSimpleThreadSafe;

interface

uses
  SyncObjs,
  i_IDList,
  i_IdCacheSimple;

type
  TIdCacheSimpleThreadSafe = class(TInterfacedObject, IIdCacheSimple)
  private
    FCS: TCriticalSection;
    FIdList: IIDInterfaceList;
  private
    function GetByID(AID: Integer): IInterface;
    procedure Clear;
    procedure Add(AID: Integer; AInterface: IInterface);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_IDInterfaceList;

{ TIdCacheSimpleThreadSafe }

constructor TIdCacheSimpleThreadSafe.Create;
begin
  FCS := TCriticalSection.Create;
  FIdList := TIDInterfaceList.Create(False);
end;

destructor TIdCacheSimpleThreadSafe.Destroy;
begin
  FreeAndNil(FCS);
  inherited;
end;

procedure TIdCacheSimpleThreadSafe.Add(AID: Integer; AInterface: IInterface);
begin
  FCS.Acquire;
  try
    FIdList.Add(AID, AInterface);
  finally
    FCS.Release;
  end;
end;

procedure TIdCacheSimpleThreadSafe.Clear;
begin
  FCS.Acquire;
  try
    FIdList.Clear;
  finally
    FCS.Release;
  end;
end;

function TIdCacheSimpleThreadSafe.GetByID(AID: Integer): IInterface;
begin
  FCS.Acquire;
  try
    Result := FIdList.GetByID(AID);
  finally
    FCS.Release;
  end;
end;

end.
