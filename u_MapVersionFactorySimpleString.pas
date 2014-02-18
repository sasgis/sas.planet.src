unit u_MapVersionFactorySimpleString;

interface

uses
  t_Hash,
  i_HashFunction,
  i_MapVersionInfo,
  i_MapVersionFactory,
  u_HashCacheWithQueuesAbstract;

type
  IMapVersionFactorySimpleInternal = interface
    ['{AF11A02B-BB29-47FE-A2DC-FC72568827C5}']
  end;

  TMapVersionFactorySimpleString = class(THashCacheWithQueuesAbstract, IMapVersionFactory, IMapVersionFactorySimpleInternal)
  private
    FHashFunction: IHashFunction;
  private
    { IMapVersionFactory }
    function CreateByStoreString(const AValue: string): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo): IMapVersionInfo;
    function IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
  protected
    function CreateByKey(
      const AKey: THashValue;
      AData: Pointer
    ): IInterface; override;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  SysUtils,
  u_MapVersionInfo;

{ TMapVersionFactorySimpleString }

constructor TMapVersionFactorySimpleString.Create(
  const AHashFunction: IHashFunction);
begin
  Assert(Assigned(AHashFunction));
  inherited Create(10, 256, 512, 256);
  FHashFunction := AHashFunction;
end;

function TMapVersionFactorySimpleString.CreateByKey(
  const AKey: THashValue;
  AData: Pointer
): IInterface;
var
  VResult: IMapVersionInfo;
begin
  VResult := TMapVersionInfo.Create(AKey, string(AData));
  Result := VResult;
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
  Result := IMapVersionInfo(GetOrCreateItem(VHash, Pointer(AValue)));
end;

function TMapVersionFactorySimpleString.IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
begin
  if (nil=AMapVersionFactory) then begin
    Result := False;
  end else begin
    Result := Supports(AMapVersionFactory, IMapVersionFactorySimpleInternal);
  end;
end;

end.
