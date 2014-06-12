unit u_GeometryProjectedMulti;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_InterfaceListStatic,
  i_GeometryProjected,
  u_BaseInterfacedObject;

type
  TGeometryProjectedMultiBase = class(TBaseInterfacedObject, IGeometryProjected)
  private
    FList: IInterfaceListStatic;
    FBounds: TDoubleRect;
  private
    function GetCount: Integer;
    function GetBounds: TDoubleRect;
  public
    constructor Create(
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
    function GetItem(AIndex: Integer): IGeometryProjectedSingleLine;
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
    function GetItem(AIndex: Integer): IGeometryProjectedSinglePolygon;
  end;

  TGeometryProjectedMultiLineOneLine = class(TBaseInterfacedObject, IGeometryProjectedMultiLine)
  private
    FLine: IGeometryProjectedSingleLine;
  private
    function GetCount: Integer;
    function GetEnum: IEnumProjectedPoint;
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
    function GetBounds: TDoubleRect;
    function GetItem(AIndex: Integer): IGeometryProjectedSingleLine;
  public
    constructor Create(
      const ALine: IGeometryProjectedSingleLine
    );
  end;

  TGeometryProjectedMultiPolygonOneLine = class(TBaseInterfacedObject, IGeometryProjectedMultiPolygon)
  private
    FLine: IGeometryProjectedSinglePolygon;
  private
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
    function GetItem(AIndex: Integer): IGeometryProjectedSinglePolygon;
  public
    constructor Create(
      const ALine: IGeometryProjectedSinglePolygon
    );
  end;

  TProjectedLineSetEmpty = class(TBaseInterfacedObject, IEnumDoublePoint, IEnumProjectedPoint)
  private
    function GetCount: Integer;
    function GetEnum: IEnumProjectedPoint;
    function GetBounds: TDoubleRect;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create;
  end;

  TProjectedPathEmpty = class(TProjectedLineSetEmpty, IGeometryProjectedMultiLine)
  private
    function GetItem(AIndex: Integer): IGeometryProjectedSingleLine;
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
    function GetItem(AIndex: Integer): IGeometryProjectedSinglePolygon;
  end;

implementation

uses
  SysUtils,
  u_GeoFunc,
  u_EnumDoublePointByLineSet;

{ TProjectedLineSet }

constructor TGeometryProjectedMultiBase.Create(
  const ABounds: TDoubleRect;
  const AList: IInterfaceListStatic
);
begin
  Assert(AList <> nil);
  inherited Create;
  FList := AList;
  FBounds := ABounds;
end;

function TGeometryProjectedMultiBase.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TGeometryProjectedMultiBase.GetCount: Integer;
begin
  Result := FList.Count;
end;

{ TProjectedPath }

function TGeometryProjectedMultiLine.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumProjectedPointByPath.Create(Self);
end;

function TGeometryProjectedMultiLine.GetItem(AIndex: Integer): IGeometryProjectedSingleLine;
begin
  if not Supports(FList[AIndex], IGeometryProjectedSingleLine, Result) then begin
    Result := nil;
  end;
end;

function TGeometryProjectedMultiLine.IsPointOnPath(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedSingleLine;
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
  VLine: IGeometryProjectedSingleLine;
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
  VLine: IGeometryProjectedSinglePolygon;
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

function TGeometryProjectedMultiPolygon.GetItem(AIndex: Integer): IGeometryProjectedSinglePolygon;
begin
  if not Supports(FList[AIndex], IGeometryProjectedSinglePolygon, Result) then begin
    Result := nil;
  end;
end;

function TGeometryProjectedMultiPolygon.IsPointInPolygon(
  const APoint: TDoublePoint): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedSinglePolygon;
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
  VLine: IGeometryProjectedSinglePolygon;
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
  VLine: IGeometryProjectedSinglePolygon;
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
  VLine: IGeometryProjectedSinglePolygon;
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

constructor TGeometryProjectedMultiLineOneLine.Create(const ALine: IGeometryProjectedSingleLine);
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

function TGeometryProjectedMultiLineOneLine.GetItem(AIndex: Integer): IGeometryProjectedSingleLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
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

constructor TGeometryProjectedMultiPolygonOneLine.Create(const ALine: IGeometryProjectedSinglePolygon);
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
  AIndex: Integer): IGeometryProjectedSinglePolygon;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
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

constructor TProjectedLineSetEmpty.Create;
begin
  inherited Create;
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

function TProjectedLineSetEmpty.Next(out APoint: TDoublePoint): Boolean;
begin
  APoint := CEmptyDoublePoint;
  Result := False;
end;

{ TLocalPathEmpty }

function TProjectedPathEmpty.GetItem(AIndex: Integer): IGeometryProjectedSingleLine;
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

function TProjectedPolygonEmpty.GetItem(AIndex: Integer): IGeometryProjectedSinglePolygon;
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
