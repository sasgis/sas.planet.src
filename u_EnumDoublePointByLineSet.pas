unit u_EnumDoublePointByLineSet;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_VectorItemLocal,
  u_BaseInterfacedObject;

type
  TEnumDoublePointByLineSetBase = class(TBaseInterfacedObject, IEnumDoublePoint)
  private
    FSourceLineSet: IInterface;
    FClosed: Boolean;
    FCurrentEnum: IEnumDoublePoint;
    FCount: Integer;
    FIndex: Integer;
    FNeedEmptyPoint: Boolean;
    FFinished: Boolean;
    FPreparedPointExists: Boolean;
    FPreparedPoint: TDoublePoint;
    function GetNextEnum: IEnumDoublePoint; virtual; abstract;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  private
    constructor CreateInternal(
      const ALineSet: IInterface;
      ALineCount: Integer;
      AClosed: Boolean
    );
  end;

  TEnumLonLatPointByPath = class(TEnumDoublePointByLineSetBase, IEnumLonLatPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: IGeometryLonLatMultiLine);
  end;

  TEnumLonLatPointByPolygon = class(TEnumDoublePointByLineSetBase, IEnumLonLatPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: IGeometryLonLatMultiPolygon);
  end;

  TEnumProjectedPointByPath = class(TEnumDoublePointByLineSetBase, IEnumProjectedPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: IGeometryProjectedMultiLine);
  end;

  TEnumProjectedPointByPolygon = class(TEnumDoublePointByLineSetBase, IEnumProjectedPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: IGeometryProjectedMultiPolygon);
  end;

  TEnumLocalPointByPath = class(TEnumDoublePointByLineSetBase, IEnumLocalPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: IGeometryLocalMultiLine);
  end;

  TEnumLocalPointByPolygon = class(TEnumDoublePointByLineSetBase, IEnumLocalPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: IGeometryLocalMultiPolygon);
  end;

implementation

uses
  u_GeoFunc;

{ TEnumDoublePointByLineSetBase }

constructor TEnumDoublePointByLineSetBase.CreateInternal(
  const ALineSet: IInterface;
  ALineCount: Integer;
  AClosed: Boolean
);
begin
  inherited Create;
  FSourceLineSet := ALineSet;
  FClosed := AClosed;
  FCurrentEnum := nil;
  FCount := ALineCount;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

function TEnumDoublePointByLineSetBase.Next(out APoint: TDoublePoint): Boolean;
begin
  while not FFinished do begin
    if FCurrentEnum <> nil then begin
      if FPreparedPointExists then begin
        APoint := FPreparedPoint;
        FPreparedPointExists := False;
        FNeedEmptyPoint := True;
        Break;
      end;
      if FCurrentEnum.Next(APoint) then begin
        FNeedEmptyPoint := True;
        Break;
      end else begin
        FCurrentEnum := nil;
      end;
    end else begin
      Inc(FIndex);
      if FIndex < FCount then begin
        FCurrentEnum := GetNextEnum;
        if FCurrentEnum <> nil then begin
          if FNeedEmptyPoint then begin
            if FCurrentEnum.Next(FPreparedPoint) then begin
              FPreparedPointExists := True;
              FNeedEmptyPoint := False;
              APoint := CEmptyDoublePoint;
              Break;
            end;
          end;
        end;
      end else begin
        FFinished := True;
        FSourceLineSet := nil;
      end;
    end;
  end;
  Result := not FFinished;
end;

{ TEnumLonLatPointByPath }

constructor TEnumLonLatPointByPath.Create(const ALineSet: IGeometryLonLatMultiLine);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, False);
end;

function TEnumLonLatPointByPath.GetNextEnum: IEnumDoublePoint;
begin
  Result := IGeometryLonLatMultiLine(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumLonLatPointByPolygon }

constructor TEnumLonLatPointByPolygon.Create(const ALineSet: IGeometryLonLatMultiPolygon);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, True);
end;

function TEnumLonLatPointByPolygon.GetNextEnum: IEnumDoublePoint;
begin
  Result := IGeometryLonLatMultiPolygon(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumProjectedPointByPath }

constructor TEnumProjectedPointByPath.Create(const ALineSet: IGeometryProjectedMultiLine);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, False);
end;

function TEnumProjectedPointByPath.GetNextEnum: IEnumDoublePoint;
begin
  Result := IGeometryProjectedMultiLine(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumProjectedPointByPolygon }

constructor TEnumProjectedPointByPolygon.Create(const ALineSet: IGeometryProjectedMultiPolygon);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, True);
end;

function TEnumProjectedPointByPolygon.GetNextEnum: IEnumDoublePoint;
begin
  Result := IGeometryProjectedMultiPolygon(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumLocalPointByPath }

constructor TEnumLocalPointByPath.Create(const ALineSet: IGeometryLocalMultiLine);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, False);
end;

function TEnumLocalPointByPath.GetNextEnum: IEnumDoublePoint;
begin
  Result := IGeometryLocalMultiLine(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumLocalPointByPolygon }

constructor TEnumLocalPointByPolygon.Create(const ALineSet: IGeometryLocalMultiPolygon);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, True);
end;

function TEnumLocalPointByPolygon.GetNextEnum: IEnumDoublePoint;
begin
  Result := IGeometryLocalMultiPolygon(FSourceLineSet).Item[FIndex].GetEnum;
end;

end.
