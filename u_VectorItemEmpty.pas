unit u_VectorItemEmpty;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_VectorItemLonLat,
  i_Datum;

type
  TLineSetEmpty = class(TInterfacedObject, ILonLatPath, ILonLatPolygon)
  private
    FBounds: TDoubleRect;
    FEnumLonLat: IEnumLonLatPoint;
  private
    function GetItemLonLatPathLine(AIndex: Integer): ILonLatPathLine;
    function GetItemLonLatPolygonLine(AIndex: Integer): ILonLatPolygonLine;

    function GetEnumLonLat: IEnumLonLatPoint;
    function CalcAreaLonLat(const ADatum: IDatum): Double;
  private
    function GetBounds: TDoubleRect;
    function GetCount: Integer;
    function CalcLength(const ADatum: IDatum): Double;
    function CalcPerimeter(const ADatum: IDatum): Double;

    function ILonLatPolygon.CalcArea = CalcAreaLonLat;

    function ILonLatPath.GetEnum = GetEnumLonLat;
    function ILonLatPolygon.GetEnum = GetEnumLonLat;

    function ILonLatPath.GetItem = GetItemLonLatPathLine;
    function ILonLatPolygon.GetItem = GetItemLonLatPolygonLine;
  public
    constructor Create();
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointEmpty }

type
  TEnumDoublePointEmpty = class(TInterfacedObject, IEnumDoublePoint, IEnumLonLatPoint)
  private
    function Next(out APoint: TDoublePoint): Boolean;
  end;

function TEnumDoublePointEmpty.Next(out APoint: TDoublePoint): Boolean;
begin
  APoint := CEmptyDoublePoint;
  Result := False;
end;

{ TLineSetEmpty }

constructor TLineSetEmpty.Create;
var
  VEnum: TEnumDoublePointEmpty;
begin
  VEnum := TEnumDoublePointEmpty.Create;
  FEnumLonLat := VEnum;
  FBounds.TopLeft := CEmptyDoublePoint;
  FBounds.BottomRight := CEmptyDoublePoint;
end;

function TLineSetEmpty.CalcAreaLonLat(const ADatum: IDatum): Double;
begin
  Result := 0;
end;

function TLineSetEmpty.CalcLength(const ADatum: IDatum): Double;
begin
  Result := 0;
end;

function TLineSetEmpty.CalcPerimeter(const ADatum: IDatum): Double;
begin
  Result := 0;
end;

function TLineSetEmpty.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TLineSetEmpty.GetCount: Integer;
begin
  Result := 0;
end;

function TLineSetEmpty.GetEnumLonLat: IEnumLonLatPoint;
begin
  Result := FEnumLonLat;
end;

function TLineSetEmpty.GetItemLonLatPathLine(AIndex: Integer): ILonLatPathLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetItemLonLatPolygonLine(
  AIndex: Integer): ILonLatPolygonLine;
begin
  Result := nil;
end;

end.
