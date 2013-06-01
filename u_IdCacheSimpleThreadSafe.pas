unit u_IdCacheSimpleThreadSafe;

interface

uses
  SysUtils,
  i_IDList,
  i_IdCacheSimple,
  u_BaseInterfacedObject;

type
  TIdCacheSimpleThreadSafe = class(TBaseInterfacedObject, IIdCacheSimple)
  private
    FCS: IReadWriteSync;
    FIdList: IIDInterfaceList;
  private
    function GetByID(AID: Integer): IInterface;
    procedure Clear;
    procedure Add(
      AID: Integer;
      const AInterface: IInterface
    );
  public
    constructor Create;
  end;

implementation

uses
  u_Synchronizer,
  u_IDInterfaceList;

{ TIdCacheSimpleThreadSafe }

constructor TIdCacheSimpleThreadSafe.Create;
begin
  inherited Create;
  FCS := MakeSyncRW_Std(Self, TRUE);
  FIdList := TIDInterfaceList.Create(False);
end;

procedure TIdCacheSimpleThreadSafe.Add(
  AID: Integer;
  const AInterface: IInterface
);
begin
  FCS.BeginWrite;
  try
    FIdList.Add(AID, AInterface);
  finally
    FCS.EndWrite;
  end;
end;

procedure TIdCacheSimpleThreadSafe.Clear;
begin
  FCS.BeginWrite;
  try
    FIdList.Clear;
  finally
    FCS.EndWrite;
  end;
end;

function TIdCacheSimpleThreadSafe.GetByID(AID: Integer): IInterface;
begin
  FCS.BeginRead;
  try
    Result := FIdList.GetByID(AID);
  finally
    FCS.EndRead;
  end;
end;

end.
