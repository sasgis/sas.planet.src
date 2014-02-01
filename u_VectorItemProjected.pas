unit u_VectorItemProjected;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_ProjectionInfo,
  i_InterfaceListStatic,
  i_VectorItemProjected,
  u_BaseInterfacedObject;

type
  TProjectedLineSet = class(TBaseInterfacedObject)
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

  TProjectedPath = class(TProjectedLineSet, IGeometryProjectedMultiLine)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
    function GetItem(AIndex: Integer): IProjectedPathLine;
  end;

  TProjectedPolygon = class(TProjectedLineSet, IGeometryProjectedMultiPolygon)
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
    function GetItem(AIndex: Integer): IProjectedPolygonLine;
  end;

  TProjectedPathOneLine = class(TBaseInterfacedObject, IGeometryProjectedMultiLine)
  private
    FLine: IProjectedPathLine;
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
    function GetItem(AIndex: Integer): IProjectedPathLine;
  public
    constructor Create(
      const ALine: IProjectedPathLine
    );
  end;

  TProjectedPolygonOneLine = class(TBaseInterfacedObject, IGeometryProjectedMultiPolygon)
  private
    FLine: IProjectedPolygonLine;
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
    function GetItem(AIndex: Integer): IProjectedPolygonLine;
  public
    constructor Create(
      const ALine: IProjectedPolygonLine
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
    function GetItem(AIndex: Integer): IProjectedPathLine;
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
    function GetItem(AIndex: Integer): IProjectedPolygonLine;
  end;

implementation

uses
  SysUtils,
  u_GeoFunc,
  u_EnumDoublePointByLineSet;

{ TProjectedLineSet }

constructor TProjectedLineSet.Create(
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

function TProjectedLineSet.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TProjectedLineSet.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TProjectedLineSet.GetProjection: IProjectionInfo;
begin
  Result := FProjection;
end;

{ TProjectedPath }

function TProjectedPath.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumProjectedPointByPath.Create(Self);
end;

function TProjectedPath.GetItem(AIndex: Integer): IProjectedPathLine;
begin
  if not Supports(FList[AIndex], IProjectedPathLine, Result) then begin
    Result := nil;
  end;
end;

function TProjectedPath.IsPointOnPath(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  i: Integer;
  VLine: IProjectedPathLine;
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

function TProjectedPath.IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
var
  i: Integer;
  VLine: IProjectedPathLine;
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

function TProjectedPolygon.CalcArea: Double;
var
  i: Integer;
  VLine: IProjectedPolygonLine;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do begin
    VLine := GetItem(i);
    Result := Result + VLine.CalcArea;
  end;
end;

function TProjectedPolygon.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumProjectedPointByPolygon.Create(Self);
end;

function TProjectedPolygon.GetItem(AIndex: Integer): IProjectedPolygonLine;
begin
  if not Supports(FList[AIndex], IProjectedPolygonLine, Result) then begin
    Result := nil;
  end;
end;

function TProjectedPolygon.IsPointInPolygon(
  const APoint: TDoublePoint): Boolean;
var
  i: Integer;
  VLine: IProjectedPolygonLine;
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

function TProjectedPolygon.IsPointOnBorder(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  i: Integer;
  VLine: IProjectedPolygonLine;
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

function TProjectedPolygon.IsRectIntersectBorder(
  const ARect: TDoubleRect): Boolean;
var
  i: Integer;
  VLine: IProjectedPolygonLine;
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

function TProjectedPolygon.IsRectIntersectPolygon(
  const ARect: TDoubleRect
): Boolean;
var
  i: Integer;
  VLine: IProjectedPolygonLine;
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

constructor TProjectedPathOneLine.Create(const ALine: IProjectedPathLine);
begin
  inherited Create;
  FLine := ALine;
end;

function TProjectedPathOneLine.GetBounds: TDoubleRect;
begin
  Result := FLine.Bounds;
end;

function TProjectedPathOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TProjectedPathOneLine.GetEnum: IEnumProjectedPoint;
begin
  Result := FLine.GetEnum;
end;

function TProjectedPathOneLine.GetItem(AIndex: Integer): IProjectedPathLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TProjectedPathOneLine.GetProjection: IProjectionInfo;
begin
  Result := FLine.Projection;
end;

function TProjectedPathOneLine.IsPointOnPath(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
begin
  Result := FLine.IsPointOnPath(APoint, ADist);
end;

function TProjectedPathOneLine.IsRectIntersectPath(
  const ARect: TDoubleRect
): Boolean;
begin
  Result := FLine.IsRectIntersectPath(ARect);
end;

{ TProjectedPolygonOneLine }

constructor TProjectedPolygonOneLine.Create(const ALine: IProjectedPolygonLine);
begin
  inherited Create;
  FLine := ALine;
end;

function TProjectedPolygonOneLine.CalcArea: Double;
begin
  Result := FLine.CalcArea;
end;

function TProjectedPolygonOneLine.GetBounds: TDoubleRect;
begin
  Result := FLine.Bounds;
end;

function TProjectedPolygonOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TProjectedPolygonOneLine.GetEnum: IEnumProjectedPoint;
begin
  Result := FLine.GetEnum;
end;

function TProjectedPolygonOneLine.GetItem(
  AIndex: Integer): IProjectedPolygonLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TProjectedPolygonOneLine.GetProjection: IProjectionInfo;
begin
  Result := FLine.Projection;
end;

function TProjectedPolygonOneLine.IsPointInPolygon(
  const APoint: TDoublePoint): Boolean;
begin
  Result := FLine.IsPointInPolygon(APoint);
end;

function TProjectedPolygonOneLine.IsPointOnBorder(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
begin
  Result := FLine.IsPointOnBorder(APoint, ADist);
end;

function TProjectedPolygonOneLine.IsRectIntersectBorder(
  const ARect: TDoubleRect): Boolean;
begin
  Result := FLine.IsRectIntersectBorder(ARect);
end;

function TProjectedPolygonOneLine.IsRectIntersectPolygon(
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

function TProjectedPathEmpty.GetItem(AIndex: Integer): IProjectedPathLine;
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

function TProjectedPolygonEmpty.GetItem(AIndex: Integer): IProjectedPolygonLine;
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
