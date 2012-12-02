unit u_EnumDoublePointFilterFirstSegment;

interface

uses
  t_GeoTypes,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointFilterFirstSegment = class(TBaseInterfacedObject, IEnumDoublePoint)
  private
    FSourceEnum: IEnumDoublePoint;
    FStarted: Boolean;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumLonLatPointFilterFirstSegment = class(TEnumDoublePointFilterFirstSegment, IEnumLonLatPoint)
  public
    constructor Create(
      const ASourceEnum: IEnumLonLatPoint
    );
  end;

  TEnumProjectedPointFilterFirstSegment = class(TEnumDoublePointFilterFirstSegment, IEnumProjectedPoint)
  public
    constructor Create(
      const ASourceEnum: IEnumProjectedPoint
    );
  end;

  TDoublePointFilterFirstSegment = class(TBaseInterfacedObject, IDoublePointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumDoublePoint): IEnumDoublePoint;
  end;

  TLonLatPointFilterFirstSegment = class(TBaseInterfacedObject, ILonLatPointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumLonLatPoint): IEnumLonLatPoint;
  end;

  TProjectedPointFilterFirstSegment = class(TBaseInterfacedObject, IProjectedPointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumProjectedPoint): IEnumProjectedPoint;
  end;


implementation

uses
  u_GeoFun;

{ TEnumDoublePointFilterFirstSegment }

constructor TEnumDoublePointFilterFirstSegment.Create(
  const ASourceEnum: IEnumDoublePoint);
begin
  inherited Create;
  FSourceEnum := ASourceEnum;
  FStarted := False;
  FFinished := False;
end;

function TEnumDoublePointFilterFirstSegment.Next(
  out APoint: TDoublePoint): Boolean;
var
  VPoint: TDoublePoint;
begin
  while not FFinished do begin
    if FSourceEnum.Next(VPoint) then begin
      if FStarted then begin
        if PointIsEmpty(VPoint) then begin
          FFinished := True;
        end else begin
          APoint := VPoint;
          Break;
        end;
      end else begin
        if not PointIsEmpty(VPoint) then begin
          FStarted := True;
          APoint := VPoint;
          Break;
        end;
      end;
    end else begin
      FFinished := True;
    end;
  end;
  Result := not FFinished;
end;

{ TEnumLonLatPointFilterFirstSegment }

constructor TEnumLonLatPointFilterFirstSegment.Create(
  const ASourceEnum: IEnumLonLatPoint
);
begin
  inherited Create(ASourceEnum);
end;

{ TEnumProjectedPointFilterFirstSegment }

constructor TEnumProjectedPointFilterFirstSegment.Create(
  const ASourceEnum: IEnumProjectedPoint
);
begin
  inherited Create(ASourceEnum);
end;

{ TDoublePointFilterFirstSegment }

function TDoublePointFilterFirstSegment.CreateFilteredEnum(
  const ASource: IEnumDoublePoint
): IEnumDoublePoint;
begin
  Result := TEnumDoublePointFilterFirstSegment.Create(ASource);
end;

{ TLonLatPointFilterFirstSegment }

function TLonLatPointFilterFirstSegment.CreateFilteredEnum(
  const ASource: IEnumLonLatPoint
): IEnumLonLatPoint;
begin
  Result := TEnumLonLatPointFilterFirstSegment.Create(ASource);
end;

{ TProjectedPointFilterFirstSegment }

function TProjectedPointFilterFirstSegment.CreateFilteredEnum(
  const ASource: IEnumProjectedPoint
): IEnumProjectedPoint;
begin
  Result := TEnumProjectedPointFilterFirstSegment.Create(ASource);
end;

end.
