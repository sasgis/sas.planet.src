unit u_MapTypeList;

interface

uses
  ActiveX,
  i_IGUIDList,
  i_MapTypes,
  UMapType;

type
  TMapTypeList = class(TInterfacedObject, IMapTypeList)
  private
    FList: IGUIDInterfaceList;
    function GetMapTypeByGUID(AGUID: TGUID): IMapType;
    function GetIterator: IEnumGUID;
  public
    procedure Add(AMap: IMapType);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_GUIDInterfaceList;

{ TMapTypeList }

procedure TMapTypeList.Add(AMap: IMapType);
begin
  FList.Add(AMap.GetMapType.GUID, AMap);
end;

constructor TMapTypeList.Create;
begin
  FList := TGUIDInterfaceList.Create(False);
end;

destructor TMapTypeList.Destroy;
begin
  FList := nil;
  inherited;
end;

function TMapTypeList.GetIterator: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

function TMapTypeList.GetMapTypeByGUID(AGUID: TGUID): IMapType;
begin
  Result := Flist.GetByGUID(AGUID) as IMapType;
end;

end.
