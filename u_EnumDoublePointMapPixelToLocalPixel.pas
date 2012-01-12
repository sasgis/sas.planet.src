unit u_EnumDoublePointMapPixelToLocalPixel;

interface

uses
  t_GeoTypes,
  i_LocalCoordConverter,
  i_DoublePointFilter,
  i_EnumDoublePoint;

type
  TEnumDoublePointMapPixelToLocalPixel = class(TInterfacedObject, IEnumLocalPoint)
  private
    FSourceEnum: IEnumProjectedPoint;
    FLocalConverter: ILocalCoordConverter;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      ALocalConverter: ILocalCoordConverter;
      ASourceEnum: IEnumProjectedPoint
    );
  end;

  TProjectedPointConverter = class(TInterfacedObject, IProjectedPointConverter)
  private
    FLocalConverter: ILocalCoordConverter;
  private
    function CreateFilteredEnum(ASource: IEnumProjectedPoint): IEnumLocalPoint;
  public
    constructor Create(
      ALocalConverter: ILocalCoordConverter
    );
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointMapPixelToLocalPixel }

constructor TEnumDoublePointMapPixelToLocalPixel.Create(
  ALocalConverter: ILocalCoordConverter;
  ASourceEnum: IEnumProjectedPoint
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

{ TProjectedPointConverter }

constructor TProjectedPointConverter.Create(
  ALocalConverter: ILocalCoordConverter);
begin
  FLocalConverter := ALocalConverter;
end;

function TProjectedPointConverter.CreateFilteredEnum(
  ASource: IEnumProjectedPoint): IEnumLocalPoint;
begin
  Result := TEnumDoublePointMapPixelToLocalPixel.Create(FLocalConverter, ASource);
end;

end.

