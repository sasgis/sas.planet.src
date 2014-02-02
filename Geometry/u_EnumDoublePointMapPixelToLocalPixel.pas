unit u_EnumDoublePointMapPixelToLocalPixel;

interface

uses
  t_GeoTypes,
  i_LocalCoordConverter,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointMapPixelToLocalPixel = class(TBaseInterfacedObject, IEnumLocalPoint)
  private
    FSourceEnum: IEnumProjectedPoint;
    FLocalConverter: ILocalCoordConverter;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceEnum: IEnumProjectedPoint
    );
  end;

  TProjectedPointConverter = class(TBaseInterfacedObject, IProjectedPointConverter)
  private
    FLocalConverter: ILocalCoordConverter;
  private
    function CreateFilteredEnum(const ASource: IEnumProjectedPoint): IEnumLocalPoint;
  public
    constructor Create(
      const ALocalConverter: ILocalCoordConverter
    );
  end;

implementation

uses
  u_GeoFunc;

{ TEnumDoublePointMapPixelToLocalPixel }

constructor TEnumDoublePointMapPixelToLocalPixel.Create(
  const ALocalConverter: ILocalCoordConverter;
  const ASourceEnum: IEnumProjectedPoint
);
begin
  inherited Create;
  FSourceEnum := ASourceEnum;
  FLocalConverter := ALocalConverter;
  FFinished := False;
end;

function TEnumDoublePointMapPixelToLocalPixel.Next(
  out APoint: TDoublePoint
): Boolean;
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
  const ALocalConverter: ILocalCoordConverter
);
begin
  inherited Create;
  FLocalConverter := ALocalConverter;
end;

function TProjectedPointConverter.CreateFilteredEnum(
  const ASource: IEnumProjectedPoint
): IEnumLocalPoint;
begin
  Result := TEnumDoublePointMapPixelToLocalPixel.Create(FLocalConverter, ASource);
end;

end.
