unit u_GeometryProjectedMulti;

interface

uses
  t_GeoTypes,
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

  TGeometryProjectedMultiLine = class(TGeometryProjectedMultiBase, IGeometryProjectedLine, IGeometryProjectedMultiLine)
  private
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
    function GetItem(AIndex: Integer): IGeometryProjectedSingleLine;
  end;

  TGeometryProjectedMultiPolygon = class(TGeometryProjectedMultiBase, IGeometryProjectedPolygon, IGeometryProjectedMultiPolygon)
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
  u_GeoFunc;

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

end.
