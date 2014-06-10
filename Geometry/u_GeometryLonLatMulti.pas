unit u_GeometryLonLatMulti;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_LonLatRect,
  i_EnumDoublePoint,
  i_InterfaceListStatic,
  i_GeometryLonLat,
  u_BaseInterfacedObject;

type
  TGeometryLonLatMultiBase = class(TBaseInterfacedObject)
  private
    FList: IInterfaceListStatic;
    FHash: THashValue;
    FBounds: ILonLatRect;
  private
    function GetCount: Integer;
    function GetHash: THashValue;
    function GetBounds: ILonLatRect;
  public
    constructor Create(
      const ABounds: ILonLatRect;
      const AHash: THashValue;
      const AList: IInterfaceListStatic
    );
  end;

  TGeometryLonLatMultiLine = class(TGeometryLonLatMultiBase, IGeometryLonLat, IGeometryLonLatMultiLine)
  private
    function GetEnum: IEnumLonLatPoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const APath: IGeometryLonLatMultiLine): Boolean;
    function GetGoToPoint: TDoublePoint;
    function GetItem(AIndex: Integer): IGeometryLonLatSingleLine;
  end;

  TGeometryLonLatMultiPolygon = class(TGeometryLonLatMultiBase, IGeometryLonLat, IGeometryLonLatMultiPolygon)
  private
    function GetEnum: IEnumLonLatPoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const APolygon: IGeometryLonLatMultiPolygon): Boolean;
    function GetGoToPoint: TDoublePoint;
    function GetItem(AIndex: Integer): IGeometryLonLatSinglePolygon;
  end;

  TLonLatPathOneLine = class(TBaseInterfacedObject, IGeometryLonLat, IGeometryLonLatMultiLine)
  private
    FLine: IGeometryLonLatSingleLine;
  private
    function GetCount: Integer;
    function GetEnum: IEnumLonLatPoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const APath: IGeometryLonLatMultiLine): Boolean;
    function GetGoToPoint: TDoublePoint;
    function GetBounds: ILonLatRect;
    function GetHash: THashValue;
    function GetItem(AIndex: Integer): IGeometryLonLatSingleLine;
  public
    constructor Create(
      const ALine: IGeometryLonLatSingleLine
    );
  end;

  TLonLatPolygonOneLine = class(TBaseInterfacedObject, IGeometryLonLat, IGeometryLonLatMultiPolygon)
  private
    FLine: IGeometryLonLatSinglePolygon;
  private
    function GetCount: Integer;
    function GetEnum: IEnumLonLatPoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const APolygon: IGeometryLonLatMultiPolygon): Boolean;
    function GetGoToPoint: TDoublePoint;
    function GetBounds: ILonLatRect;
    function GetHash: THashValue;
    function GetItem(AIndex: Integer): IGeometryLonLatSinglePolygon;
  public
    constructor Create(
      const ALine: IGeometryLonLatSinglePolygon
    );
  end;

implementation

uses
  SysUtils,
  u_EnumDoublePointByLineSet;

{ TLonLatLineSet }

constructor TGeometryLonLatMultiBase.Create(
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const AList: IInterfaceListStatic
);
begin
  Assert(AList <> nil);
  Assert(ABounds <> nil);
  Assert(AList.Count > 1);
  inherited Create;
  FBounds := ABounds;
  FHash := AHash;
  FList := AList;
end;

function TGeometryLonLatMultiBase.GetBounds: ILonLatRect;
begin
  Result := FBounds;
end;

function TGeometryLonLatMultiBase.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGeometryLonLatMultiBase.GetHash: THashValue;
begin
  Result := FHash;
end;

{ TLonLatPath }

function TGeometryLonLatMultiLine.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumLonLatPointByPath.Create(Self);
end;

function TGeometryLonLatMultiLine.GetGoToPoint: TDoublePoint;
begin
  Result := GetItem(0).GetGoToPoint;
end;

function TGeometryLonLatMultiLine.GetItem(AIndex: Integer): IGeometryLonLatSingleLine;
begin
  if not Supports(FList[AIndex], IGeometryLonLatSingleLine, Result) then begin
    Result := nil;
  end;
end;

function TGeometryLonLatMultiLine.IsSame(const APath: IGeometryLonLatMultiLine): Boolean;
var
  i: Integer;
  VLine: IGeometryLonLatSingleLine;
begin
  if APath = IGeometryLonLatMultiLine(Self) then begin
    Result := True;
    Exit;
  end;

  if FList.Count <> APath.Count then begin
    Result := False;
    Exit;
  end;

  if (FHash <> 0) and (APath.Hash <> 0) then begin
    Result := FHash = APath.Hash;
  end else begin
    for i := 0 to FList.Count - 1 do begin
      VLine := GetItem(i);
      if VLine = nil then begin
        Result := False;
        Exit;
      end;
      if not VLine.IsSame(APath.Item[i]) then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function TGeometryLonLatMultiLine.IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
var
  VLine: IGeometryLonLatMultiLine;
begin
  if AGeometry = nil then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AGeometry.Hash <> 0) and (FHash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;

  Result := False;
  if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

{ TLonLatPolygon }

function TGeometryLonLatMultiPolygon.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumLonLatPointByPolygon.Create(Self);
end;

function TGeometryLonLatMultiPolygon.GetGoToPoint: TDoublePoint;
begin
  Result := FBounds.CalcRectCenter;
end;

function TGeometryLonLatMultiPolygon.GetItem(AIndex: Integer): IGeometryLonLatSinglePolygon;
begin
  if not Supports(FList[AIndex], IGeometryLonLatSinglePolygon, Result) then begin
    Result := nil;
  end;
end;

function TGeometryLonLatMultiPolygon.IsSame(const APolygon: IGeometryLonLatMultiPolygon): Boolean;
var
  i: Integer;
  VLine: IGeometryLonLatSinglePolygon;
begin
  if APolygon = IGeometryLonLatMultiPolygon(Self) then begin
    Result := True;
    Exit;
  end;

  if FList.Count <> APolygon.Count then begin
    Result := False;
    Exit;
  end;

  if (FHash <> 0) and (APolygon.Hash <> 0) then begin
    Result := FHash = APolygon.Hash;
  end else begin
    for i := 0 to FList.Count - 1 do begin
      VLine := GetItem(i);
      if VLine = nil then begin
        Result := False;
        Exit;
      end;
      if not VLine.IsSame(APolygon.Item[i]) then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function TGeometryLonLatMultiPolygon.IsSameGeometry(
  const AGeometry: IGeometryLonLat
): Boolean;
var
  VLine: IGeometryLonLatMultiPolygon;
begin
  if AGeometry = nil then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AGeometry.Hash <> 0) and (FHash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;

  Result := False;
  if Supports(AGeometry, IGeometryLonLatMultiPolygon, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

{ TLonLatPathOneLine }

constructor TLonLatPathOneLine.Create(const ALine: IGeometryLonLatSingleLine);
begin
  inherited Create;
  FLine := ALine;
end;

function TLonLatPathOneLine.GetBounds: ILonLatRect;
begin
  Result := FLine.Bounds;
end;

function TLonLatPathOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TLonLatPathOneLine.GetEnum: IEnumLonLatPoint;
begin
  Result := FLine.GetEnum;
end;

function TLonLatPathOneLine.GetGoToPoint: TDoublePoint;
begin
  Result := FLine.GetGoToPoint;
end;

function TLonLatPathOneLine.GetHash: THashValue;
begin
  Result := FLine.Hash;
end;

function TLonLatPathOneLine.GetItem(AIndex: Integer): IGeometryLonLatSingleLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TLonLatPathOneLine.IsSame(const APath: IGeometryLonLatMultiLine): Boolean;
begin
  if APath.Count <> 1 then begin
    Result := False;
    Exit;
  end;
  Result := FLine.IsSame(APath.Item[0]);
end;

function TLonLatPathOneLine.IsSameGeometry(
  const AGeometry: IGeometryLonLat
): Boolean;
var
  VLine: IGeometryLonLatMultiLine;
begin
  if AGeometry = nil then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FLine.Hash <> 0) and (AGeometry.Hash <> 0) and (FLine.Hash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;

  Result := False;
  if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

{ TLonLatPolygonOneLine }

constructor TLonLatPolygonOneLine.Create(const ALine: IGeometryLonLatSinglePolygon);
begin
  inherited Create;
  FLine := ALine;
end;

function TLonLatPolygonOneLine.GetBounds: ILonLatRect;
begin
  Result := FLine.Bounds;
end;

function TLonLatPolygonOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TLonLatPolygonOneLine.GetEnum: IEnumLonLatPoint;
begin
  Result := FLine.GetEnum;
end;

function TLonLatPolygonOneLine.GetGoToPoint: TDoublePoint;
begin
  Result := FLine.GetGoToPoint;
end;

function TLonLatPolygonOneLine.GetHash: THashValue;
begin
  Result := FLine.Hash;
end;

function TLonLatPolygonOneLine.GetItem(AIndex: Integer): IGeometryLonLatSinglePolygon;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TLonLatPolygonOneLine.IsSame(const APolygon: IGeometryLonLatMultiPolygon): Boolean;
begin
  if APolygon.Count <> 1 then begin
    Result := False;
    Exit;
  end;
  Result := FLine.IsSame(APolygon.Item[0]);
end;

function TLonLatPolygonOneLine.IsSameGeometry(
  const AGeometry: IGeometryLonLat
): Boolean;
var
  VLine: IGeometryLonLatMultiPolygon;
begin
  if AGeometry = nil then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FLine.Hash <> 0) and (AGeometry.Hash <> 0) and (FLine.Hash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;

  Result := False;
  if Supports(AGeometry, IGeometryLonLatMultiPolygon, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

end.
