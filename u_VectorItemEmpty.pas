unit u_VectorItemEmpty;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_VectorItemLocal,
  i_Datum,
  i_ProjectionInfo,
  i_LocalCoordConverter;

type
  TLineSetEmpty = class(TInterfacedObject, ILonLatPath, ILonLatPolygon, IProjectedPath, IProjectedPolygon, ILocalPath, ILocalPolygon)
  private
    FBounds: TDoubleRect;
    FEnumLonLat: IEnumLonLatPoint;
    FEnumProjected: IEnumProjectedPoint;
    FEnumLocal: IEnumLocalPoint;
  private
    function GetItemLonLatPathLine(AIndex: Integer): ILonLatPathLine;
    function GetItemLonLatPolygonLine(AIndex: Integer): ILonLatPolygonLine;
    function GetItemProjectedPathLine(AIndex: Integer): IProjectedPathLine;
    function GetItemProjectedPolygonLine(AIndex: Integer): IProjectedPolygonLine;
    function GetItemLocalPathLine(AIndex: Integer): ILocalPathLine;
    function GetItemLocalPolygonLine(AIndex: Integer): ILocalPolygonLine;

    function GetEnumLonLat: IEnumLonLatPoint;
    function GetEnumProjected: IEnumProjectedPoint;
    function GetEnumLocal: IEnumLocalPoint;
    function CalcAreaLonLat(ADatum: IDatum): Double;
    function CalcAreaProjected: Double;
  private
    function GetProjection: IProjectionInfo;
    function GetLocalConverter: ILocalCoordConverter;
    function GetBounds: TDoubleRect;
    function GetCount: Integer;
    function IsPointOnPath(APoint:TDoublePoint; ADist: Double): Boolean;
    function IsPointOnBorder(APoint:TDoublePoint; ADist: Double): Boolean;
    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function CalcLength(ADatum: IDatum): Double;
    function CalcPerimeter(ADatum: IDatum): Double;

    function ILonLatPolygon.CalcArea = CalcAreaLonLat;
    function IProjectedPolygon.CalcArea = CalcAreaProjected;

    function ILonLatPath.GetEnum = GetEnumLonLat;
    function ILonLatPolygon.GetEnum = GetEnumLonLat;
    function IProjectedPath.GetEnum = GetEnumProjected;
    function IProjectedPolygon.GetEnum = GetEnumProjected;
    function ILocalPath.GetEnum = GetEnumLocal;
    function ILocalPolygon.GetEnum = GetEnumLocal;

    function ILonLatPath.GetItem = GetItemLonLatPathLine;
    function ILonLatPolygon.GetItem = GetItemLonLatPolygonLine;
    function IProjectedPath.GetItem = GetItemProjectedPathLine;
    function IProjectedPolygon.GetItem = GetItemProjectedPolygonLine;
    function ILocalPath.GetItem = GetItemLocalPathLine;
    function ILocalPolygon.GetItem = GetItemLocalPolygonLine;
  public
    constructor Create();
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointEmpty }

type
  TEnumDoublePointEmpty = class(TInterfacedObject, IEnumDoublePoint, IEnumLonLatPoint, IEnumProjectedPoint, IEnumLocalPoint)
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
  FEnumProjected := VEnum;
  FEnumLocal := VEnum;
  FBounds.TopLeft := CEmptyDoublePoint;
  FBounds.BottomRight := CEmptyDoublePoint;
end;

function TLineSetEmpty.CalcAreaLonLat(ADatum: IDatum): Double;
begin
  Result := 0;
end;

function TLineSetEmpty.CalcAreaProjected: Double;
begin
  Result := 0;
end;

function TLineSetEmpty.CalcLength(ADatum: IDatum): Double;
begin
  Result := 0;
end;

function TLineSetEmpty.CalcPerimeter(ADatum: IDatum): Double;
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

function TLineSetEmpty.GetEnumLocal: IEnumLocalPoint;
begin
  Result := FEnumLocal;
end;

function TLineSetEmpty.GetEnumLonLat: IEnumLonLatPoint;
begin
  Result := FEnumLonLat;
end;

function TLineSetEmpty.GetEnumProjected: IEnumProjectedPoint;
begin
  Result := FEnumProjected;
end;

function TLineSetEmpty.GetItemLocalPathLine(AIndex: Integer): ILocalPathLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetItemLocalPolygonLine(
  AIndex: Integer): ILocalPolygonLine;
begin
  Result := nil;
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

function TLineSetEmpty.GetItemProjectedPathLine(
  AIndex: Integer): IProjectedPathLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetItemProjectedPolygonLine(
  AIndex: Integer): IProjectedPolygonLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetLocalConverter: ILocalCoordConverter;
begin
  Result := nil;
end;

function TLineSetEmpty.GetProjection: IProjectionInfo;
begin
  Result := nil;
end;

function TLineSetEmpty.IsPointInPolygon(const APoint: TDoublePoint): Boolean;
begin
  Result := False;
end;

function TLineSetEmpty.IsPointOnBorder(APoint: TDoublePoint;
  ADist: Double): Boolean;
begin
  Result := False;
end;

function TLineSetEmpty.IsPointOnPath(APoint: TDoublePoint;
  ADist: Double): Boolean;
begin
  Result := False;
end;

end.
