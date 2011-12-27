unit u_EnumDoublePointFilterFirstPoly;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint;

type
  TEnumDoublePointFilterFirstPoly = class(TInterfacedObject, IEnumDoublePoint)
  private
    FSourceEnum: IEnumDoublePoint;
    FStarted: Boolean;
    FFinished: Boolean;
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

{ TEnumDoublePointFilterFirstPoly }

constructor TEnumDoublePointFilterFirstPoly.Create(
  ASourceEnum: IEnumDoublePoint);
begin
  FSourceEnum := ASourceEnum;
  FStarted := False;
  FFinished := False;
end;

function TEnumDoublePointFilterFirstPoly.Next(
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

end.
