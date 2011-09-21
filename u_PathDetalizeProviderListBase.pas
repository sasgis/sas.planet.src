unit u_PathDetalizeProviderListBase;

interface

uses
  ActiveX,
  u_ConfigDataElementBase,
  i_GUIDSet,
  i_PathDetalizeProviderList;

type
  TPathDetalizeProviderListBase = class(TConfigDataElementBaseEmptySaveLoad, IPathDetalizeProviderList)
  private
    FList: IGUIDInterfaceSet;
  protected { IPathDetalizeProviderList }
    function GetGUIDEnum: IEnumGUID;
    function Get(AGUID: TGUID): IPathDetalizeProviderListEntity;
  protected
    procedure Add(AItem: IPathDetalizeProviderListEntity);
  public
    constructor Create;
  end;

implementation

uses
  u_GUIDInterfaceSet;

{ TPathDetalizeProviderListBase }

constructor TPathDetalizeProviderListBase.Create;
begin
  inherited;
  FList := TGUIDInterfaceSet.Create(False);
end;

procedure TPathDetalizeProviderListBase.Add(
  AItem: IPathDetalizeProviderListEntity);
begin
  LockWrite;
  try
    if not FList.IsExists(AItem.GUID) then begin
      FList.Add(AItem.GUID, AItem);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TPathDetalizeProviderListBase.Get(
  AGUID: TGUID): IPathDetalizeProviderListEntity;
begin
  LockRead;
  try
    Result := IPathDetalizeProviderListEntity(FList.GetByGUID(AGUID));
  finally
    UnlockRead;
  end;
end;

function TPathDetalizeProviderListBase.GetGUIDEnum: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

end.
