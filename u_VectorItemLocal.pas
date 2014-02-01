unit u_VectorItemLocal;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_LocalCoordConverter,
  i_InterfaceListStatic,
  i_VectorItemLocal,
  u_BaseInterfacedObject;

type
  TLocalLineSet = class(TBaseInterfacedObject)
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

  TLocalPath = class(TLocalLineSet, IGeometryLocalMultiLine)
  private
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): IGeometryLocalLine;
  end;

  TLocalPolygon = class(TLocalLineSet, IGeometryLocalMultiPolygon)
  private
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): IGeometryLocalPolygon;
  end;

  TLocalPathOneLine = class(TBaseInterfacedObject, IGeometryLocalMultiLine)
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

  TLocalPolygonOneLine = class(TBaseInterfacedObject, IGeometryLocalMultiPolygon)
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

  TLocalLineSetEmpty = class(TBaseInterfacedObject, IEnumDoublePoint, IEnumLocalPoint)
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

  TLocalPathEmpty = class(TLocalLineSetEmpty, IGeometryLocalMultiLine)
  private
    function GetItem(AIndex: Integer): IGeometryLocalLine;
  end;

  TLocalPolygonEmpty = class(TLocalLineSetEmpty, IGeometryLocalMultiPolygon)
  private
    function GetItem(AIndex: Integer): IGeometryLocalPolygon;
  end;

implementation

uses
  SysUtils,
  u_GeoFunc,
  u_EnumDoublePointByLineSet;

{ TLocalLineSet }

constructor TLocalLineSet.Create(
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

function TLocalLineSet.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TLocalLineSet.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLocalConverter;
end;

{ TLocalPath }

function TLocalPath.GetEnum: IEnumLocalPoint;
begin
  Result := TEnumLocalPointByPath.Create(Self);
end;

function TLocalPath.GetItem(AIndex: Integer): IGeometryLocalLine;
begin
  if not Supports(FList[AIndex], IGeometryLocalLine, Result) then begin
    Result := nil;
  end;
end;

{ TLocalPolygon }

function TLocalPolygon.GetEnum: IEnumLocalPoint;
begin
  Result := TEnumLocalPointByPolygon.Create(Self);
end;

function TLocalPolygon.GetItem(AIndex: Integer): IGeometryLocalPolygon;
begin
  if not Supports(FList[AIndex], IGeometryLocalPolygon, Result) then begin
    Result := nil;
  end;
end;

{ TLocalPathOneLine }

constructor TLocalPathOneLine.Create(const ALine: IGeometryLocalLine);
begin
  inherited Create;
  FLine := ALine;
end;

function TLocalPathOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TLocalPathOneLine.GetEnum: IEnumLocalPoint;
begin
  Result := FLine.GetEnum;
end;

function TLocalPathOneLine.GetItem(AIndex: Integer): IGeometryLocalLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TLocalPathOneLine.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLine.LocalConverter;
end;

{ TLocalPolygonOneLine }

constructor TLocalPolygonOneLine.Create(const ALine: IGeometryLocalPolygon);
begin
  inherited Create;
  FLine := ALine;
end;

function TLocalPolygonOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TLocalPolygonOneLine.GetEnum: IEnumLocalPoint;
begin
  Result := FLine.GetEnum;
end;

function TLocalPolygonOneLine.GetItem(
  AIndex: Integer): IGeometryLocalPolygon;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TLocalPolygonOneLine.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLine.LocalConverter;
end;

{ TLocalLineSetEmpty }

constructor TLocalLineSetEmpty.Create(const ALocalConverter: ILocalCoordConverter);
begin
  inherited Create;
  FLocalConverter := ALocalConverter;
end;

function TLocalLineSetEmpty.GetCount: Integer;
begin
  Result := 0;
end;

function TLocalLineSetEmpty.GetEnum: IEnumLocalPoint;
begin
  Result := Self;
end;

function TLocalLineSetEmpty.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLocalConverter;
end;

function TLocalLineSetEmpty.Next(out APoint: TDoublePoint): Boolean;
begin
  APoint := CEmptyDoublePoint;
  Result := False;
end;

{ TLocalPathEmpty }

function TLocalPathEmpty.GetItem(AIndex: Integer): IGeometryLocalLine;
begin
  Result := nil;
end;

{ TLocalPolygonEmpty }

function TLocalPolygonEmpty.GetItem(AIndex: Integer): IGeometryLocalPolygon;
begin
  Result := nil;
end;

end.
