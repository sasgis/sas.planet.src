unit u_EnumDoublePointClosePoly;

interface

uses
  t_GeoTypes,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointClosePoly = class(TBaseInterfacedObject, IEnumDoublePoint)
  private
    FSourceEnum: IEnumDoublePoint;
    FFirstPoint: TDoublePoint;
    FLastPointEqualToFirst: Boolean;
    FPointsInPolyCount: Integer;
    FFinished: Boolean;
    FNeedAddBreak: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumLonLatPointClosePoly = class(TEnumDoublePointClosePoly, IEnumLonLatPoint)
  public
    constructor Create(
      const ASourceEnum: IEnumLonLatPoint
    );
  end;

  TEnumProjectedPointClosePoly = class(TEnumDoublePointClosePoly, IEnumProjectedPoint)
  public
    constructor Create(
      const ASourceEnum: IEnumProjectedPoint
    );
  end;

  TEnumLocalPointClosePoly = class(TEnumDoublePointClosePoly, IEnumLocalPoint)
  public
    constructor Create(
      const ASourceEnum: IEnumLocalPoint
    );
  end;

  TDoublePointFilterPolygonClose = class(TBaseInterfacedObject, IDoublePointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumDoublePoint): IEnumDoublePoint;
  end;

  TLonLatPointFilterPolygonClose = class(TBaseInterfacedObject, ILonLatPointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumLonLatPoint): IEnumLonLatPoint;
  end;

  TProjectedPointFilterPolygonClose = class(TBaseInterfacedObject, IProjectedPointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumProjectedPoint): IEnumProjectedPoint;
  end;

  TLocalPointFilterPolygonClose = class(TBaseInterfacedObject, ILocalPointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumLocalPoint): IEnumLocalPoint;
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointClosePoly }

constructor TEnumDoublePointClosePoly.Create(const ASourceEnum: IEnumDoublePoint);
begin
  inherited Create;
  FSourceEnum := ASourceEnum;
  FFinished := False;
  FPointsInPolyCount := 0;
  FNeedAddBreak := False;
end;

function TEnumDoublePointClosePoly.Next(out APoint: TDoublePoint): Boolean;
var
  VPoint: TDoublePoint;
begin
  if not FFinished then begin
    if FNeedAddBreak then begin
      FNeedAddBreak := False;
      APoint := CEmptyDoublePoint;
      Result := True;
    end else begin
      if FSourceEnum.Next(VPoint) then begin
        if PointIsEmpty(VPoint) then begin
          if (FPointsInPolyCount > 1) and (not FLastPointEqualToFirst) then begin
            APoint := FFirstPoint;
            FNeedAddBreak := True;
          end else begin
            APoint := VPoint;
          end;
          Result := True;
          FPointsInPolyCount := 0;
        end else begin
          if FPointsInPolyCount = 0 then begin
            FFirstPoint := VPoint;
            FPointsInPolyCount := 1;
            FLastPointEqualToFirst := True;
          end else begin
            FLastPointEqualToFirst := DoublePointsEqual(VPoint, FFirstPoint);
            Inc(FPointsInPolyCount);
          end;
          APoint := VPoint;
          Result := True;
        end;
      end else begin
        FFinished := True;
        if (FPointsInPolyCount > 1) and (not FLastPointEqualToFirst) then begin
          APoint := FFirstPoint;
          Result := True;
        end else begin
          APoint := CEmptyDoublePoint;
          Result := False;
        end;
      end;
    end;
  end else begin
    Result := False;
    APoint := CEmptyDoublePoint;
  end;
end;

{ TEnumLonLatPointClosePoly }

constructor TEnumLonLatPointClosePoly.Create(const ASourceEnum: IEnumLonLatPoint);
begin
  inherited Create(ASourceEnum);
end;

{ TEnumProjectedPointClosePoly }

constructor TEnumProjectedPointClosePoly.Create(
  const ASourceEnum: IEnumProjectedPoint
);
begin
  inherited Create(ASourceEnum);
end;

{ TEnumLocalPointClosePoly }

constructor TEnumLocalPointClosePoly.Create(const ASourceEnum: IEnumLocalPoint);
begin
  inherited Create(ASourceEnum);
end;

{ TDoublePointFilterPolygonClose }

function TDoublePointFilterPolygonClose.CreateFilteredEnum(
  const ASource: IEnumDoublePoint
): IEnumDoublePoint;
begin
  Result := TEnumDoublePointClosePoly.Create(ASource);
end;

{ TLonLatPointFilterPolygonClose }

function TLonLatPointFilterPolygonClose.CreateFilteredEnum(
  const ASource: IEnumLonLatPoint
): IEnumLonLatPoint;
begin
  Result := TEnumLonLatPointClosePoly.Create(ASource);
end;

{ TProjectedPointFilterPolygonClose }

function TProjectedPointFilterPolygonClose.CreateFilteredEnum(
  const ASource: IEnumProjectedPoint
): IEnumProjectedPoint;
begin
  Result := TEnumProjectedPointClosePoly.Create(ASource);
end;

{ TLocalPointFilterPolygonClose }

function TLocalPointFilterPolygonClose.CreateFilteredEnum(
  const ASource: IEnumLocalPoint
): IEnumLocalPoint;
begin
  Result := TEnumLocalPointClosePoly.Create(ASource);
end;

end.
