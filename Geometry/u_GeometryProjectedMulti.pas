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
    function IsEmpty: Boolean;
    function GetCount: Integer;
    function GetBounds: TDoubleRect;
  public
    constructor Create(
      const ABounds: TDoubleRect;
      const AList: IInterfaceListStatic
    );
  end;

  TGeometryProjectedMultiLine = class(TGeometryProjectedMultiBase, IGeometryProjectedLine, IGeometryProjectedMultiLine)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
    function GetItem(AIndex: Integer): IGeometryProjectedSingleLine;
  end;

  TGeometryProjectedMultiPolygon = class(TGeometryProjectedMultiBase, IGeometryProjectedPolygon, IGeometryProjectedMultiPolygon)
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

  TGeometryProjectedMultiLineOneLine = class(TBaseInterfacedObject, IGeometryProjectedLine, IGeometryProjectedMultiLine)
  private
    FLine: IGeometryProjectedSingleLine;
  private
    function IsEmpty: Boolean;
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

  TGeometryProjectedMultiPolygonOneLine = class(TBaseInterfacedObject, IGeometryProjectedPolygon, IGeometryProjectedMultiPolygon)
  private
    FLine: IGeometryProjectedSinglePolygon;
  private
    function IsEmpty: Boolean;
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

  TProjectedLineSetEmpty = class(TBaseInterfacedObject, IGeometryProjectedLine, IGeometryProjectedMultiLine, IGeometryProjectedPolygon, IGeometryProjectedMultiPolygon, IEnumDoublePoint, IEnumProjectedPoint)
  private
    function IsEmpty: Boolean;
    function GetCount: Integer;
    function GetEnum: IEnumProjectedPoint;
    function GetBounds: TDoubleRect;
  private
    function GetItemLine(AIndex: Integer): IGeometryProjectedSingleLine;
    function IGeometryProjectedMultiLine.GetItem = GetItemLine;
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
  private
    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;
    function GetItemPolygon(AIndex: Integer): IGeometryProjectedSinglePolygon;
    function IGeometryProjectedMultiPolygon.GetItem = GetItemPolygon;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create;
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

function TGeometryProjectedMultiBase.IsEmpty: Boolean;
begin
  Result := False;
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

function TGeometryProjectedMultiLineOneLine.IsEmpty: Boolean;
begin
  Result := False;
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

function TGeometryProjectedMultiPolygonOneLine.IsEmpty: Boolean;
begin
  Result := False;
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

function TProjectedLineSetEmpty.GetItemLine(AIndex: Integer): IGeometryProjectedSingleLine;
begin
  Result := nil;
end;

function TProjectedLineSetEmpty.IsPointOnPath(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
begin
  Result := False;
end;

function TProjectedLineSetEmpty.IsRectIntersectPath(
  const ARect: TDoubleRect
): Boolean;
begin
  Result := False;
end;

function TProjectedLineSetEmpty.CalcArea: Double;
begin
  Result := 0;
end;

function TProjectedLineSetEmpty.GetItemPolygon(AIndex: Integer): IGeometryProjectedSinglePolygon;
begin
  Result := nil;
end;

function TProjectedLineSetEmpty.IsEmpty: Boolean;
begin
  Result := True;
end;

function TProjectedLineSetEmpty.IsPointInPolygon(
  const APoint: TDoublePoint): Boolean;
begin
  Result := False;
end;

function TProjectedLineSetEmpty.IsPointOnBorder(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
begin
  Result := False;
end;

function TProjectedLineSetEmpty.IsRectIntersectBorder(
  const ARect: TDoubleRect): Boolean;
begin
  Result := False;
end;

function TProjectedLineSetEmpty.IsRectIntersectPolygon(
  const ARect: TDoubleRect
): Boolean;
begin
  Result := False;
end;

end.
