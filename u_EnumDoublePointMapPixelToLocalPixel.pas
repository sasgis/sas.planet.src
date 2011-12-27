unit u_EnumDoublePointMapPixelToLocalPixel;

interface

uses
  t_GeoTypes,
  i_LocalCoordConverter,
  i_EnumDoublePoint;

type
  TEnumDoublePointMapPixelToLocalPixel = class(TInterfacedObject, IEnumDoublePoint)
  private
    FSourceEnum: IEnumDoublePoint;
    FLocalConverter: ILocalCoordConverter;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      ALocalConverter: ILocalCoordConverter;
      ASourceEnum: IEnumDoublePoint
    );
  end;


implementation

uses
  u_GeoFun;

{ TEnumDoublePointMapPixelToLocalPixel }

constructor TEnumDoublePointMapPixelToLocalPixel.Create(
  ALocalConverter: ILocalCoordConverter;
  ASourceEnum: IEnumDoublePoint
);
begin
  FSourceEnum := ASourceEnum;
  FLocalConverter := ALocalConverter;
  FFinished := False;
end;

function TEnumDoublePointMapPixelToLocalPixel.Next(
  out APoint: TDoublePoint): Boolean;
var
  VPoint: TDoublePoint;
begin
  if FFinished then begin
    Result := False;
    APoint := CEmptyDoublePoint;
  end else begin
    if FSourceEnum.Next(VPoint) then begin
      if PointIsEmpty(VPoint) then begin
        APoint := VPoint;
      end else begin
        APoint := FLocalConverter.MapPixelFloat2LocalPixelFloat(VPoint);
      end;
      Result := True;
    end else begin
      FFinished := True;
      Result := False;
      APoint := CEmptyDoublePoint;
    end;
  end;
end;

end.

