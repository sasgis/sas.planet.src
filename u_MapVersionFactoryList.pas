unit u_MapVersionFactoryList;

interface

uses
  i_MapVersionConfig,
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
    function GetGEVersionFactory: IMapVersionFactory;
    function GetVersionFactoryByCode(const ACacheTypeCode: Integer): IMapVersionFactory;
  public
    constructor Create;
  end;

implementation

uses
  c_CacheTypeCodes,
  u_MapVersionFactoryGE,
  u_MapVersionFactorySimpleString;

{ TMapVersionFactoryList }

constructor TMapVersionFactoryList.Create;
begin
  inherited Create;
  FSimpleVersionFactory := TMapVersionFactorySimpleString.Create;
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

function TMapVersionFactoryList.GetVersionFactoryByCode(const ACacheTypeCode: Integer): IMapVersionFactory;
begin
  case ACacheTypeCode of
    c_File_Cache_Id_GE, c_File_Cache_Id_GC: begin
      Result := FGEVersionFactory;
    end;
    else begin
      Result := FSimpleVersionFactory;
    end;
  end;
end;

end.