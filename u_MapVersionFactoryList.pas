unit u_MapVersionFactoryList;

interface

uses
  i_HashFunction,
  i_MapVersionFactory,
  i_MapVersionFactoryList,
  u_BaseInterfacedObject;

type
  TMapVersionFactoryList = class(TBaseInterfacedObject, IMapVersionFactoryList)
  private
    // существующие фабрики
    FSimpleVersionFactory: IMapVersionFactory;
    FGEVersionFactory: IMapVersionFactory;
  private
    function GetSimpleVersionFactory: IMapVersionFactory;
    function GetGEVersionFactory: IMapVersionFactory; deprecated;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  c_CacheTypeCodes,
  u_MapVersionFactoryGE,
  u_MapVersionFactorySimpleString;

{ TMapVersionFactoryList }

constructor TMapVersionFactoryList.Create(
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FSimpleVersionFactory := TMapVersionFactorySimpleString.Create(AHashFunction);
  FGEVersionFactory := TMapVersionFactoryGE.Create;
end;

function TMapVersionFactoryList.GetGEVersionFactory: IMapVersionFactory;
begin
  Result := FGEVersionFactory;
end;

function TMapVersionFactoryList.GetSimpleVersionFactory: IMapVersionFactory;
begin
  Result := FSimpleVersionFactory;
end;

end.
