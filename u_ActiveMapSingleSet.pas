unit u_ActiveMapSingleSet;

interface

uses
  i_GUIDSet,
  i_ActiveMapsConfig,
  u_BaseInterfacedObject;

type
  TActiveMapSingleSet = class(TBaseInterfacedObject, IActiveMapSingleSet)
  private
    FSet: IGUIDInterfaceSet;
  private
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
  public
    constructor Create(ASet: IGUIDInterfaceSet);
  end;

implementation

{ TActiveMapSingleSet }

constructor TActiveMapSingleSet.Create(ASet: IGUIDInterfaceSet);
begin
  Assert(ASet <> nil);
  inherited Create;
  FSet := ASet;
end;

function TActiveMapSingleSet.GetMapSingle(
  const AMapGUID: TGUID): IActiveMapSingle;
begin
  Result := IActiveMapSingle(FSet.GetByGUID(AMapGUID));
end;


end.
