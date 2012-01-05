unit u_EnumDoublePointFilterEqual;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint;

type
  TEnumDoublePointFilterEqual = class(TInterfacedObject, IEnumDoublePoint)
  private
    FSourceEnum: IEnumDoublePoint;
    FPrevEmpty: Boolean;
    FPrevPoint: TDoublePoint;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumLonLatPointFilterEqual = class(TEnumDoublePointFilterEqual, IEnumLonLatPoint)
  public
    constructor Create(
      ASourceEnum: IEnumLonLatPoint
    );
  end;

  TEnumProjectedPointFilterEqual = class(TEnumDoublePointFilterEqual, IEnumProjectedPoint)
  public
    constructor Create(
      ASourceEnum: IEnumProjectedPoint
    );
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointFilterEqual }

constructor TEnumDoublePointFilterEqual.Create(ASourceEnum: IEnumDoublePoint);
begin
  FSourceEnum := ASourceEnum;
  FPrevEmpty := True;
  FFinished := False;
end;

function TEnumDoublePointFilterEqual.Next(out APoint: TDoublePoint): Boolean;
var
  VPoint: TDoublePoint;
  VPointIsEmpty: Boolean;
begin
  while not FFinished do begin
    if FSourceEnum.Next(VPoint) then begin
      VPointIsEmpty := PointIsEmpty(VPoint);
      if VPointIsEmpty then begin
        if not FPrevEmpty then begin
          FPrevEmpty := True;
          FPrevPoint := VPoint;
          APoint := VPoint;
          Break;
        end;
      end else begin
        if FPrevEmpty then begin
          FPrevEmpty := False;
          FPrevPoint := VPoint;
          APoint := VPoint;
          Break;
        end else begin
          if (abs(VPoint.X - FPrevPoint.X) > 1) or (abs(VPoint.Y - FPrevPoint.Y) > 1) then begin
            FPrevEmpty := False;
            FPrevPoint := VPoint;
            APoint := VPoint;
            Break;
          end;
        end;
      end;
    end else begin
      FFinished := True;
    end;
  end;
  Result := not FFinished;
end;

{ TEnumLonLatPointFilterEqual }

constructor TEnumLonLatPointFilterEqual.Create(ASourceEnum: IEnumLonLatPoint);
begin
  inherited Create(ASourceEnum);
end;

{ TEnumProjectedPointFilterEqual }

constructor TEnumProjectedPointFilterEqual.Create(
  ASourceEnum: IEnumProjectedPoint);
begin
  inherited Create(ASourceEnum);
end;

end.
