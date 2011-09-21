unit u_SensorListBase;

interface

uses
  ActiveX,
  u_ConfigDataElementBase,
  i_GUIDSet,
  i_SensorList;

type
  TSensorListBase = class(TConfigDataElementBaseEmptySaveLoad, ISensorList)
  private
    FList: IGUIDInterfaceSet;
  protected { ISensorList }
    function GetGUIDEnum: IEnumGUID;
    function Get(AGUID: TGUID): ISensorListEntity;
  protected
    procedure Add(AItem: ISensorListEntity);
  public
    constructor Create;
  end;

implementation

uses
  u_GUIDInterfaceSet;

{ TSensorListBase }

constructor TSensorListBase.Create;
begin
  inherited;
  FList := TGUIDInterfaceSet.Create(False);
end;

procedure TSensorListBase.Add(AItem: ISensorListEntity);
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

function TSensorListBase.Get(AGUID: TGUID): ISensorListEntity;
begin
  LockRead;
  try
    Result := ISensorListEntity(FList.GetByGUID(AGUID));
  finally
    UnlockRead;
  end;
end;

function TSensorListBase.GetGUIDEnum: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

end.
