unit u_MapTypeListGeneratorFromFullListBasic;

interface

uses
  UMapType,
  i_MapTypes;

type
  TMapTypeListGeneratorFromFullListBasic = class(TInterfacedObject, IMapTypeListFactory)
  protected
    FMainMap: Boolean;
    FItemFactory: IMapTypeFactory;
    function CheckIsAddMap(AMapType: TMapType): Boolean; virtual;
    function CreateList: IMapTypeList; virtual;
  public
    constructor Create(AMainMapsList: Boolean; AFactory: IMapTypeFactory);
  end;

implementation

uses
  SysUtils,
  u_GlobalState,
  u_MapTypeList;

{ TMapTypeListGeneratorFromFullListBasic }

function TMapTypeListGeneratorFromFullListBasic.CheckIsAddMap(
  AMapType: TMapType): Boolean;
begin
  if FMainMap then begin
    Result := not AMapType.asLayer;
  end else begin
    Result := AMapType.asLayer;
  end;
end;

constructor TMapTypeListGeneratorFromFullListBasic.Create(
  AMainMapsList: Boolean; AFactory: IMapTypeFactory);
begin
  FMainMap := AMainMapsList;
  FItemFactory := AFactory;
end;

function TMapTypeListGeneratorFromFullListBasic.CreateList: IMapTypeList;
var
  i: Integer;
  VMapType: TMapType;
  VList: TMapTypeList;
begin
  VList := TMapTypeList.Create(False);
  Result := VList;
  for i := 0 to GState.MapType.Count - 1 do begin
    VMapType := GState.MapType[i];
    if CheckIsAddMap(VMapType) then begin
      VList.Add(FItemFactory.CreateItem(VMapType));
    end;
  end;
end;

end.
