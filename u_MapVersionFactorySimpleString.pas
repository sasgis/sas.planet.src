unit u_MapVersionFactorySimpleString;

interface

uses
  i_MapVersionInfo,
  i_MapVersionConfig,
  u_BaseInterfacedObject;

type
  TMapVersionFactorySimpleString = class(TBaseInterfacedObject, IMapVersionFactory)
  private
    FShowPrevVersion: Boolean;
  private
    function CreateByStoreString(const AValue: string): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo): IMapVersionInfo;
  public
    constructor Create(const AShowPrevVersion: Boolean = False);
  end;

implementation

uses
  u_MapVersionInfo;

{ TMapVersionFactorySimpleString }

constructor TMapVersionFactorySimpleString.Create(const AShowPrevVersion: Boolean);
begin
  inherited Create;
  FShowPrevVersion := AShowPrevVersion;
end;

function TMapVersionFactorySimpleString.CreateByMapVersion(
  const AValue: IMapVersionInfo
): IMapVersionInfo;
begin
  if AValue <> nil then begin
    Result := AValue;
  end else begin
    Result := CreateByStoreString('');
  end;
end;

function TMapVersionFactorySimpleString.CreateByStoreString(
  const AValue: string
): IMapVersionInfo;
begin
  Result := TMapVersionInfo.Create(AValue, FShowPrevVersion);
end;

end.
