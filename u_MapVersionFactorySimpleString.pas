unit u_MapVersionFactorySimpleString;

interface

uses
  i_HashFunction,
  i_MapVersionInfo,
  i_MapVersionFactory,
  u_BaseInterfacedObject;

type
  IMapVersionFactorySimpleInternal = interface
    ['{AF11A02B-BB29-47FE-A2DC-FC72568827C5}']
  end;

  TMapVersionFactorySimpleString = class(TBaseInterfacedObject, IMapVersionFactory, IMapVersionFactorySimpleInternal)
  private
    FHashFunction: IHashFunction;
  private
    { IMapVersionFactory }
    function CreateByStoreString(const AValue: string): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo): IMapVersionInfo;
    function IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  SysUtils,
  t_Hash,
  u_MapVersionInfo;

{ TMapVersionFactorySimpleString }

constructor TMapVersionFactorySimpleString.Create(
  const AHashFunction: IHashFunction);
begin
  Assert(Assigned(AHashFunction));
  inherited Create;
  FHashFunction := AHashFunction;
end;

function TMapVersionFactorySimpleString.CreateByMapVersion(
  const AValue: IMapVersionInfo
): IMapVersionInfo;
begin
  if AValue <> nil then begin
    Result := CreateByStoreString(AValue.StoreString);
  end else begin
    Result := CreateByStoreString('');
  end;
end;

function TMapVersionFactorySimpleString.CreateByStoreString(
  const AValue: string
): IMapVersionInfo;
var
  VHash: THashValue;
begin
  VHash := FHashFunction.CalcHashByString(AValue);
  Result := TMapVersionInfo.Create(VHash, AValue);
end;

function TMapVersionFactorySimpleString.IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
var V: IMapVersionFactorySimpleInternal;
begin
  if (nil=AMapVersionFactory) then begin
    Result := False;
  end else begin
    Result := Supports(AMapVersionFactory, IMapVersionFactorySimpleInternal, V);
  end;
end;

end.
