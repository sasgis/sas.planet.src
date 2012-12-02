unit u_EnumDoublePointLonLatToMapPixel;

interface

uses
  t_GeoTypes,
  i_CoordConverter,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointLonLatToMapPixel = class(TBaseInterfacedObject, IEnumProjectedPoint)
  private
    FSourceEnum: IEnumLonLatPoint;
    FZoom: Byte;
    FConverter: ICoordConverter;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      AZoom: Byte;
      const AConverter: ICoordConverter;
      const ASourceEnum: IEnumLonLatPoint
    );
  end;

type
  TLonLatPointConverter = class(TBaseInterfacedObject, ILonLatPointConverter)
  private
    FZoom: Byte;
    FConverter: ICoordConverter;
  private
    function CreateFilteredEnum(const ASource: IEnumLonLatPoint): IEnumProjectedPoint;
  public
    constructor Create(
      AZoom: Byte;
      const AConverter: ICoordConverter
    );
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointLonLatToMapPixels }

constructor TEnumDoublePointLonLatToMapPixel.Create(
  AZoom: Byte;
  const AConverter: ICoordConverter;
  const ASourceEnum: IEnumLonLatPoint
);
begin
  inherited Create;
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

{ TLonLatPointConverter }

constructor TLonLatPointConverter.Create(
  AZoom: Byte;
  const AConverter: ICoordConverter
);
begin
  inherited Create;
  FZoom := AZoom;
  FConverter := AConverter;
end;

function TLonLatPointConverter.CreateFilteredEnum(
  const ASource: IEnumLonLatPoint
): IEnumProjectedPoint;
begin
  Result := TEnumDoublePointLonLatToMapPixel.Create(FZoom, FConverter, ASource);
end;

end.
