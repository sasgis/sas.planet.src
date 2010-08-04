unit u_MapTypeBasic;

interface

uses
  UMapType,
  i_MapTypes;

type
  TMapTypeBasic = class(TInterfacedObject, IMapType)
  private
    FMapType: TMapType;
    function GetMapType: TMapType;
  public
    constructor Create(AMapType: TMapType);
  end;

  TMapTypeBasicFactory = class(TInterfacedObject, IMapTypeFactory)
  private
    function CreateItem(AMap: TMapType): IMapType;
  end;


implementation

{ TMapTypeBasic }

constructor TMapTypeBasic.Create(AMapType: TMapType);
begin
  FMapType := AMapType;
end;

function TMapTypeBasic.GetMapType: TMapType;
begin
  Result := FMapType;
end;



{ TMapTypeBasicFactory }

function TMapTypeBasicFactory.CreateItem(AMap: TMapType): IMapType;
begin
  Result := TMapTypeBasic.Create(AMap);
end;

end.
 