unit u_MapVersionFactorySimpleString;

interface

uses
  i_MapVersionInfo,
  i_MapVersionConfig,
  u_BaseInterfacedObject;

type
  IMapVersionFactorySimpleInternal = interface
    ['{AF11A02B-BB29-47FE-A2DC-FC72568827C5}']
  end;

  TMapVersionFactorySimpleString = class(TBaseInterfacedObject, IMapVersionFactory, IMapVersionFactorySimpleInternal)
  private
    { IMapVersionFactory }
    function CreateByStoreString(const AValue: string; const AShowPrevVersion: Boolean = False): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo; const AShowPrevVersion: Boolean = False): IMapVersionInfo;
    function IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
  end;

implementation

uses
  SysUtils,
  u_MapVersionInfo;

{ TMapVersionFactorySimpleString }

function TMapVersionFactorySimpleString.CreateByMapVersion(
  const AValue: IMapVersionInfo;
  const AShowPrevVersion: Boolean
): IMapVersionInfo;
begin
  if AValue <> nil then begin
    if (AValue.ShowPrevVersion = AShowPrevVersion) then
      Result := AValue
    else
      Result := CreateByStoreString(AValue.StoreString, AShowPrevVersion);
  end else begin
    Result := CreateByStoreString('', AShowPrevVersion);
  end;
end;

function TMapVersionFactorySimpleString.CreateByStoreString(
  const AValue: string;
  const AShowPrevVersion: Boolean
): IMapVersionInfo;
begin
  Result := TMapVersionInfo.Create(AValue, AShowPrevVersion);
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
