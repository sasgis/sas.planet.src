unit u_EnumDoublePointLonLatToMapPixel;

interface

uses
  t_GeoTypes,
  i_CoordConverter,
  i_EnumDoublePoint;

type
  TEnumDoublePointLonLatToMapPixel = class(TInterfacedObject, IEnumDoublePoint)
  private
    FSourceEnum: IEnumDoublePoint;
    FZoom: Byte;
    FConverter: ICoordConverter;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      AZoom: Byte;
      AConverter: ICoordConverter;
      ASourceEnum: IEnumDoublePoint
    );
  end;


implementation

uses
  u_GeoFun;

{ TEnumDoublePointLonLatToMapPixels }

constructor TEnumDoublePointLonLatToMapPixel.Create(
  AZoom: Byte;
  AConverter: ICoordConverter;
  ASourceEnum: IEnumDoublePoint
);
begin
  FSourceEnum := ASourceEnum;
  FZoom := AZoom;
  FConverter := AConverter;
  FFinished := False;
end;

function TEnumDoublePointLonLatToMapPixel.Next(
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
        FConverter.CheckLonLatPos(VPoint);
        APoint := FConverter.LonLat2PixelPosFloat(VPoint, FZoom);
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
