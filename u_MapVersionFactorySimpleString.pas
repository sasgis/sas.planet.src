unit u_MapVersionFactorySimpleString;

interface

uses
  i_MapVersionInfo,
  i_MapVersionConfig;

type
  TMapVersionFactorySimpleString = class(TInterfacedObject, IMapVersionFactory)
  private
    function CreateByStoreString(const AValue: string): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo): IMapVersionInfo;
  end;

implementation

uses
  u_MapVersionInfo;

{ TMapVersionFactorySimpleString }

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
  Result := TMapVersionInfo.Create(AValue);
end;

end.
