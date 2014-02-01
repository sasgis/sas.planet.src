unit u_VectorItemLocal;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_LocalCoordConverter,
  i_InterfaceListStatic,
  i_GeometryLocal,
  u_BaseInterfacedObject;

type
  TGeometryLocalMultiBase = class(TBaseInterfacedObject, IGeometryLocal)
  private
    FList: IInterfaceListStatic;
    FLocalConverter: ILocalCoordConverter;
  private
    function GetCount: Integer;
    function GetLocalConverter: ILocalCoordConverter;
  public
    constructor Create(
      const ALocalConverter: ILocalCoordConverter;
      const AList: IInterfaceListStatic
    );
  end;

  TGeometryLocalMultiLine = class(TGeometryLocalMultiBase, IGeometryLocalMultiLine)
  private
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): IGeometryLocalLine;
  end;

  TGeometryLocalMultiPolygon = class(TGeometryLocalMultiBase, IGeometryLocalMultiPolygon)
  private
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): IGeometryLocalPolygon;
  end;

  TGeometryLocalMultiLineOneLine = class(TBaseInterfacedObject, IGeometryLocalMultiLine)
  private
    FLine: IGeometryLocalLine;
  private
    function GetLocalConverter: ILocalCoordConverter;
    function GetCount: Integer;
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): IGeometryLocalLine;
  public
    constructor Create(
      const ALine: IGeometryLocalLine
    );
  end;

  TGeometryLocalMultiPolygonOneLine = class(TBaseInterfacedObject, IGeometryLocalMultiPolygon)
  private
    FLine: IGeometryLocalPolygon;
  private
    function GetLocalConverter: ILocalCoordConverter;
    function GetCount: Integer;
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): IGeometryLocalPolygon;
  public
    constructor Create(
      const ALine: IGeometryLocalPolygon
    );
  end;

  TGeometryLocalEmpty = class(TBaseInterfacedObject, IGeometryLocal, IEnumDoublePoint, IEnumLocalPoint)
  private
    FLocalConverter: ILocalCoordConverter;
  private
    function GetLocalConverter: ILocalCoordConverter;
    function GetCount: Integer;
    function GetEnum: IEnumLocalPoint;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ALocalConverter: ILocalCoordConverter
    );
  end;

  TGeometryLocalMultiLineEmpty = class(TGeometryLocalEmpty, IGeometryLocalMultiLine)
  private
    function GetItem(AIndex: Integer): IGeometryLocalLine;
  end;

  TGeometryLocalMultiPolygonEmpty = class(TGeometryLocalEmpty, IGeometryLocalMultiPolygon)
  private
    function GetItem(AIndex: Integer): IGeometryLocalPolygon;
  end;

implementation

uses
  SysUtils,
  u_GeoFunc,
  u_EnumDoublePointByLineSet;

{ TLocalLineSet }

constructor TGeometryLocalMultiBase.Create(
  const ALocalConverter: ILocalCoordConverter;
  const AList: IInterfaceListStatic
);
begin
  Assert(AList <> nil);
  Assert(ALocalConverter <> nil);
  inherited Create;
  FList := AList;
  FLocalConverter := ALocalConverter;
end;

function TGeometryLocalMultiBase.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGeometryLocalMultiBase.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLocalConverter;
end;

{ TLocalPath }

function TGeometryLocalMultiLine.GetEnum: IEnumLocalPoint;
begin
  Result := TEnumLocalPointByPath.Create(Self);
end;

function TGeometryLocalMultiLine.GetItem(AIndex: Integer): IGeometryLocalLine;
begin
  if not Supports(FList[AIndex], IGeometryLocalLine, Result) then begin
    Result := nil;
  end;
end;

{ TLocalPolygon }

function TGeometryLocalMultiPolygon.GetEnum: IEnumLocalPoint;
begin
  Result := TEnumLocalPointByPolygon.Create(Self);
end;

function TGeometryLocalMultiPolygon.GetItem(AIndex: Integer): IGeometryLocalPolygon;
begin
  if not Supports(FList[AIndex], IGeometryLocalPolygon, Result) then begin
    Result := nil;
  end;
end;

{ TLocalPathOneLine }

constructor TGeometryLocalMultiLineOneLine.Create(const ALine: IGeometryLocalLine);
begin
  inherited Create;
  FLine := ALine;
end;

function TGeometryLocalMultiLineOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TGeometryLocalMultiLineOneLine.GetEnum: IEnumLocalPoint;
begin
  Result := FLine.GetEnum;
end;

function TGeometryLocalMultiLineOneLine.GetItem(AIndex: Integer): IGeometryLocalLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TGeometryLocalMultiLineOneLine.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLine.LocalConverter;
end;

{ TLocalPolygonOneLine }

constructor TGeometryLocalMultiPolygonOneLine.Create(const ALine: IGeometryLocalPolygon);
begin
  inherited Create;
  FLine := ALine;
end;

function TGeometryLocalMultiPolygonOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TGeometryLocalMultiPolygonOneLine.GetEnum: IEnumLocalPoint;
begin
  Result := FLine.GetEnum;
end;

function TGeometryLocalMultiPolygonOneLine.GetItem(
  AIndex: Integer): IGeometryLocalPolygon;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TGeometryLocalMultiPolygonOneLine.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLine.LocalConverter;
end;

{ TLocalLineSetEmpty }

constructor TGeometryLocalEmpty.Create(const ALocalConverter: ILocalCoordConverter);
begin
  inherited Create;
  FLocalConverter := ALocalConverter;
end;

function TGeometryLocalEmpty.GetCount: Integer;
begin
  Result := 0;
end;

function TGeometryLocalEmpty.GetEnum: IEnumLocalPoint;
begin
  Result := Self;
end;

function TGeometryLocalEmpty.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLocalConverter;
end;

function TGeometryLocalEmpty.Next(out APoint: TDoublePoint): Boolean;
begin
  APoint := CEmptyDoublePoint;
  Result := False;
end;

{ TLocalPathEmpty }

function TGeometryLocalMultiLineEmpty.GetItem(AIndex: Integer): IGeometryLocalLine;
begin
  Result := nil;
end;

{ TLocalPolygonEmpty }

function TGeometryLocalMultiPolygonEmpty.GetItem(AIndex: Integer): IGeometryLocalPolygon;
begin
  Result := nil;
end;

end.
