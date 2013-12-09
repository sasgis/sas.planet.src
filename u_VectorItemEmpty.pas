unit u_VectorItemEmpty;

interface

uses
  t_GeoTypes,
  t_Hash,
  i_LonLatRect,
  i_EnumDoublePoint,
  i_VectorItemLonLat,
  i_Datum,
  i_NotifierOperation,
  u_BaseInterfacedObject;

type
  TLineSetEmpty = class(TBaseInterfacedObject, ILonLatPath, ILonLatPolygon)
  private
    FEnumLonLat: IEnumLonLatPoint;
  private
    function GetItemLonLatPathLine(AIndex: Integer): ILonLatPathLine;
    function GetItemLonLatPolygonLine(AIndex: Integer): ILonLatPolygonLine;

    function GetEnumLonLat: IEnumLonLatPoint;
    function CalcAreaLonLat(
      const ADatum: IDatum;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
  private
    function GetBounds: ILonLatRect;
    function GetHash: THashValue;
    function GetCount: Integer;
    function CalcLength(const ADatum: IDatum): Double;
    function CalcPerimeter(const ADatum: IDatum): Double;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSamePath(const APath: ILonLatPath): Boolean;
    function IsSamePolygon(const APolygon: ILonLatPolygon): Boolean;

    function ILonLatPolygon.CalcArea = CalcAreaLonLat;

    function ILonLatPath.IsSame = IsSamePath;
    function ILonLatPolygon.IsSame = IsSamePolygon;

    function ILonLatPath.GetEnum = GetEnumLonLat;
    function ILonLatPolygon.GetEnum = GetEnumLonLat;

    function ILonLatPath.GetItem = GetItemLonLatPathLine;
    function ILonLatPolygon.GetItem = GetItemLonLatPolygonLine;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  u_GeoFun;

{ TEnumDoublePointEmpty }

type
  TEnumDoublePointEmpty = class(TBaseInterfacedObject, IEnumDoublePoint, IEnumLonLatPoint)
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
  inherited Create;
  VEnum := TEnumDoublePointEmpty.Create;
  FEnumLonLat := VEnum;
end;

function TLineSetEmpty.CalcAreaLonLat(
  const ADatum: IDatum;
  const ANotifier: INotifierOperation = nil;
  const AOperationID: Integer = 0
): Double;
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

function TLineSetEmpty.GetBounds: ILonLatRect;
begin
  Result := nil;
end;

function TLineSetEmpty.GetCount: Integer;
begin
  Result := 0;
end;

function TLineSetEmpty.GetEnumLonLat: IEnumLonLatPoint;
begin
  Result := FEnumLonLat;
end;

function TLineSetEmpty.GetHash: THashValue;
begin
  Result := 0;
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

function TLineSetEmpty.IsSameGeometry(
  const AGeometry: IGeometryLonLat
): Boolean;
var
  VLine: ILonLatPath;
  VPolygon: ILonLatPolygon;
begin
  Result := False;
  if Supports(AGeometry, ILonLatPolygon, VPolygon) then begin
    Result := IsSamePolygon(VPolygon);
  end else if Supports(AGeometry, ILonLatPath, VLine) then begin
    Result := IsSamePath(VLine);
  end;
end;

function TLineSetEmpty.IsSamePath(const APath: ILonLatPath): Boolean;
begin
  Result := (APath.Count = 0);
end;

function TLineSetEmpty.IsSamePolygon(const APolygon: ILonLatPolygon): Boolean;
begin
  Result := (APolygon.Count = 0);
end;

end.
