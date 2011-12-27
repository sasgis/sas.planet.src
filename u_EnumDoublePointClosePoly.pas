unit u_EnumDoublePointClosePoly;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint;

type
  TEnumDoublePointClosePoly = class(TInterfacedObject, IEnumDoublePoint)
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
      ASourceEnum: IEnumDoublePoint
    );
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointClosePoly }

constructor TEnumDoublePointClosePoly.Create(ASourceEnum: IEnumDoublePoint);
begin
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
          Result :=  False;
        end;
      end;
    end;
  end else begin
    Result := False;
    APoint := CEmptyDoublePoint;
  end;
end;

end.
