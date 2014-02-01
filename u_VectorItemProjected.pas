unit u_VectorItemProjected;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_ProjectionInfo,
  i_InterfaceListStatic,
  i_GeometryProjected,
  u_BaseInterfacedObject;

type
  TGeometryProjectedMultiBase = class(TBaseInterfacedObject, IGeometryProjected)
  private
    FList: IInterfaceListStatic;
    FProjection: IProjectionInfo;
    FBounds: TDoubleRect;
  private
    function GetCount: Integer;
    function GetProjection: IProjectionInfo;
    function GetBounds: TDoubleRect;
  public
    constructor Create(
      const AProjection: IProjectionInfo;
      const ABounds: TDoubleRect;
      const AList: IInterfaceListStatic
    );
  end;

  TGeometryProjectedMultiLine = class(TGeometryProjectedMultiBase, IGeometryProjectedMultiLine)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
    function GetItem(AIndex: Integer): IGeometryProjectedLine;
  end;

  TGeometryProjectedMultiPolygon = class(TGeometryProjectedMultiBase, IGeometryProjectedMultiPolygon)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;
    function GetItem(AIndex: Integer): IGeometryProjectedPolygon;
  end;

  TGeometryProjectedMultiLineOneLine = class(TBaseInterfacedObject, IGeometryProjectedMultiLine)
  private
    FLine: IGeometryProjectedLine;
  private
    function GetProjection: IProjectionInfo;
    function GetCount: Integer;
    function GetEnum: IEnumProjectedPoint;
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
    function GetBounds: TDoubleRect;
    function GetItem(AIndex: Integer): IGeometryProjectedLine;
  public
    constructor Create(
      const ALine: IGeometryProjectedLine
    );
  end;

  TGeometryProjectedMultiPolygonOneLine = class(TBaseInterfacedObject, IGeometryProjectedMultiPolygon)
  private
    FLine: IGeometryProjectedPolygon;
  private
    function GetProjection: IProjectionInfo;
    function GetCount: Integer;
    function GetEnum: IEnumProjectedPoint;
    function GetBounds: TDoubleRect;
    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;
    function GetItem(AIndex: Integer): IGeometryProjectedPolygon;
  public
    constructor Create(
      const ALine: IGeometryProjectedPolygon
    );
  end;

  TProjectedLineSetEmpty = class(TBaseInterfacedObject, IEnumDoublePoint, IEnumProjectedPoint)
  private
    FProjection: IProjectionInfo;
  private
    function GetProjection: IProjectionInfo;
    function GetCount: Integer;
    function GetEnum: IEnumProjectedPoint;
    function GetBounds: TDoubleRect;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const AProjection: IProjectionInfo
    );
  end;

  TProjectedPathEmpty = class(TProjectedLineSetEmpty, IGeometryProjectedMultiLine)
  private
    function GetItem(AIndex: Integer): IGeometryProjectedLine;
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
  end;

  TProjectedPolygonEmpty = class(TProjectedLineSetEmpty, IGeometryProjectedMultiPolygon)
  private
    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;
    function GetItem(AIndex: Integer): IGeometryProjectedPolygon;
  end;

implementation

uses
  SysUtils,
  u_GeoFunc,
  u_EnumDoublePointByLineSet;

{ TProjectedLineSet }

constructor TGeometryProjectedMultiBase.Create(
  const AProjection: IProjectionInfo;
  const ABounds: TDoubleRect;
  const AList: IInterfaceListStatic
);
begin
  Assert(AList <> nil);
  Assert(AProjection <> nil);
  inherited Create;
  FList := AList;
  FBounds := ABounds;
  FProjection := AProjection;
end;

function TGeometryProjectedMultiBase.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TGeometryProjectedMultiBase.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGeometryProjectedMultiBase.GetProjection: IProjectionInfo;
begin
  Result := FProjection;
end;

{ TProjectedPath }

function TGeometryProjectedMultiLine.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumProjectedPointByPath.Create(Self);
end;

function TGeometryProjectedMultiLine.GetItem(AIndex: Integer): IGeometryProjectedLine;
begin
  if not Supports(FList[AIndex], IGeometryProjectedLine, Result) then begin
    Result := nil;
  end;
end;

function TGeometryProjectedMultiLine.IsPointOnPath(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedLine;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do begin
    VLine := GetItem(i);
    if VLine.IsPointOnPath(APoint, ADist) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TGeometryProjectedMultiLine.IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedLine;
begin
  Result := False;
  if IsIntersecProjectedRect(ARect, FBounds) then begin
    for i := 0 to FList.Count - 1 do begin
      VLine := GetItem(i);
      if VLine.IsRectIntersectPath(ARect) then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TProjectedPolygon }

function TGeometryProjectedMultiPolygon.CalcArea: Double;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do begin
    VLine := GetItem(i);
    Result := Result + VLine.CalcArea;
  end;
end;

function TGeometryProjectedMultiPolygon.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumProjectedPointByPolygon.Create(Self);
end;

function TGeometryProjectedMultiPolygon.GetItem(AIndex: Integer): IGeometryProjectedPolygon;
begin
  if not Supports(FList[AIndex], IGeometryProjectedPolygon, Result) then begin
    Result := nil;
  end;
end;

function TGeometryProjectedMultiPolygon.IsPointInPolygon(
  const APoint: TDoublePoint): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do begin
    VLine := GetItem(i);
    if VLine.IsPointInPolygon(APoint) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TGeometryProjectedMultiPolygon.IsPointOnBorder(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do begin
    VLine := GetItem(i);
    if VLine.IsPointOnBorder(APoint, ADist) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TGeometryProjectedMultiPolygon.IsRectIntersectBorder(
  const ARect: TDoubleRect): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := False;
  if IsIntersecProjectedRect(ARect, FBounds) then begin
    for i := 0 to FList.Count - 1 do begin
      VLine := GetItem(i);
      if VLine.IsRectIntersectBorder(ARect) then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TGeometryProjectedMultiPolygon.IsRectIntersectPolygon(
  const ARect: TDoubleRect
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := False;
  if IsIntersecProjectedRect(ARect, FBounds) then begin
    for i := 0 to FList.Count - 1 do begin
      VLine := GetItem(i);
      if VLine.IsRectIntersectPolygon(ARect) then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TProjectedPathOneLine }

constructor TGeometryProjectedMultiLineOneLine.Create(const ALine: IGeometryProjectedLine);
begin
  inherited Create;
  FLine := ALine;
end;

function TGeometryProjectedMultiLineOneLine.GetBounds: TDoubleRect;
begin
  Result := FLine.Bounds;
end;

function TGeometryProjectedMultiLineOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TGeometryProjectedMultiLineOneLine.GetEnum: IEnumProjectedPoint;
begin
  Result := FLine.GetEnum;
end;

function TGeometryProjectedMultiLineOneLine.GetItem(AIndex: Integer): IGeometryProjectedLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TGeometryProjectedMultiLineOneLine.GetProjection: IProjectionInfo;
begin
  Result := FLine.Projection;
end;

function TGeometryProjectedMultiLineOneLine.IsPointOnPath(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
begin
  Result := FLine.IsPointOnPath(APoint, ADist);
end;

function TGeometryProjectedMultiLineOneLine.IsRectIntersectPath(
  const ARect: TDoubleRect
): Boolean;
begin
  Result := FLine.IsRectIntersectPath(ARect);
end;

{ TProjectedPolygonOneLine }

constructor TGeometryProjectedMultiPolygonOneLine.Create(const ALine: IGeometryProjectedPolygon);
begin
  inherited Create;
  FLine := ALine;
end;

function TGeometryProjectedMultiPolygonOneLine.CalcArea: Double;
begin
  Result := FLine.CalcArea;
end;

function TGeometryProjectedMultiPolygonOneLine.GetBounds: TDoubleRect;
begin
  Result := FLine.Bounds;
end;

function TGeometryProjectedMultiPolygonOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TGeometryProjectedMultiPolygonOneLine.GetEnum: IEnumProjectedPoint;
begin
  Result := FLine.GetEnum;
end;

function TGeometryProjectedMultiPolygonOneLine.GetItem(
  AIndex: Integer): IGeometryProjectedPolygon;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TGeometryProjectedMultiPolygonOneLine.GetProjection: IProjectionInfo;
begin
  Result := FLine.Projection;
end;

function TGeometryProjectedMultiPolygonOneLine.IsPointInPolygon(
  const APoint: TDoublePoint): Boolean;
begin
  Result := FLine.IsPointInPolygon(APoint);
end;

function TGeometryProjectedMultiPolygonOneLine.IsPointOnBorder(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
begin
  Result := FLine.IsPointOnBorder(APoint, ADist);
end;

function TGeometryProjectedMultiPolygonOneLine.IsRectIntersectBorder(
  const ARect: TDoubleRect): Boolean;
begin
  Result := FLine.IsRectIntersectBorder(ARect);
end;

function TGeometryProjectedMultiPolygonOneLine.IsRectIntersectPolygon(
  const ARect: TDoubleRect
): Boolean;
begin
  Result := FLine.IsRectIntersectPolygon(ARect);
end;

{ TProjectedLineSetEmpty }

constructor TProjectedLineSetEmpty.Create(const AProjection: IProjectionInfo);
begin
  inherited Create;
  FProjection := AProjection;
end;

function TProjectedLineSetEmpty.GetBounds: TDoubleRect;
begin
  Result := DoubleRect(CEmptyDoublePoint, CEmptyDoublePoint);
end;

function TProjectedLineSetEmpty.GetCount: Integer;
begin
  Result := 0;
end;

function TProjectedLineSetEmpty.GetEnum: IEnumProjectedPoint;
begin
  Result := Self;
end;

function TProjectedLineSetEmpty.GetProjection: IProjectionInfo;
begin
  Result := FProjection;
end;

function TProjectedLineSetEmpty.Next(out APoint: TDoublePoint): Boolean;
begin
  APoint := CEmptyDoublePoint;
  Result := False;
end;

{ TLocalPathEmpty }

function TProjectedPathEmpty.GetItem(AIndex: Integer): IGeometryProjectedLine;
begin
  Result := nil;
end;

function TProjectedPathEmpty.IsPointOnPath(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
begin
  Result := False;
end;

function TProjectedPathEmpty.IsRectIntersectPath(
  const ARect: TDoubleRect
): Boolean;
begin
  Result := False;
end;

{ TLocalPolygonEmpty }

function TProjectedPolygonEmpty.CalcArea: Double;
begin
  Result := 0;
end;

function TProjectedPolygonEmpty.GetItem(AIndex: Integer): IGeometryProjectedPolygon;
begin
  Result := nil;
end;

function TProjectedPolygonEmpty.IsPointInPolygon(
  const APoint: TDoublePoint): Boolean;
begin
  Result := False;
end;

function TProjectedPolygonEmpty.IsPointOnBorder(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
begin
  Result := False;
end;

function TProjectedPolygonEmpty.IsRectIntersectBorder(
  const ARect: TDoubleRect): Boolean;
begin
  Result := False;
end;

function TProjectedPolygonEmpty.IsRectIntersectPolygon(
  const ARect: TDoubleRect
): Boolean;
begin
  Result := False;
end;

end.
