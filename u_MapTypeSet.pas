unit u_MapTypeSet;

interface

uses
  ActiveX,
  i_GUIDSet,
  i_MapTypes;

type
  TMapTypeSet = class(TInterfacedObject, IMapTypeSet)
  private
    FList: IGUIDInterfaceSet;
    function GetMapTypeByGUID(AGUID: TGUID): IMapType;
    function GetIterator: IEnumGUID;
  public
    procedure Add(AMap: IMapType);
    constructor Create(AAllowNil: Boolean);
    destructor Destroy; override;
  end;

implementation

uses
  u_GUIDInterfaceSet,
  c_ZeroGUID;

{ TMapTypeList }

procedure TMapTypeSet.Add(AMap: IMapType);
var
  VGUID: TGUID;
begin
  if AMap <> nil then begin
    VGUID := AMap.GUID;
  end else begin
    VGUID := CGUID_Zero;
  end;
  FList.Add(VGUID, AMap);
end;

constructor TMapTypeSet.Create(AAllowNil: Boolean);
begin
  FList := TGUIDInterfaceSet.Create(AAllowNil);
end;

destructor TMapTypeSet.Destroy;
begin
  FList := nil;
  inherited;
end;

function TMapTypeSet.GetIterator: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

function TMapTypeSet.GetMapTypeByGUID(AGUID: TGUID): IMapType;
begin
  Result := Flist.GetByGUID(AGUID) as IMapType;
end;

end.
