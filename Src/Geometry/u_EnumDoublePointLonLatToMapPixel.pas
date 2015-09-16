unit u_EnumDoublePointLonLatToMapPixel;

interface

uses
  t_GeoTypes,
  i_ProjectionType,
  i_ProjectionInfo,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointLonLatToMapPixel = class(TBaseInterfacedObject, IEnumProjectedPoint)
  private
    FSourceEnum: IEnumLonLatPoint;
    FProjection: IProjection;
    FProjectionType: IProjectionType;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const AProjection: IProjection;
      const ASourceEnum: IEnumLonLatPoint
    );
  end;

type
  TLonLatPointConverter = class(TBaseInterfacedObject, ILonLatPointConverter)
  private
    FProjection: IProjection;
  private
    function CreateFilteredEnum(const ASource: IEnumLonLatPoint): IEnumProjectedPoint;
  public
    constructor Create(
      const AProjection: IProjection
    );
  end;

implementation

uses
  Math,
  u_GeoFunc;

{ TEnumDoublePointLonLatToMapPixels }

constructor TEnumDoublePointLonLatToMapPixel.Create(
  const AProjection: IProjection;
  const ASourceEnum: IEnumLonLatPoint
);
begin
  Assert(Assigned(AProjection));
  Assert(Assigned(ASourceEnum));
  inherited Create;
  FSourceEnum := ASourceEnum;
  FProjection := AProjection;
  FProjectionType := FProjection.ProjectionType;
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
        FProjectionType.ValidateLonLatPos(VPoint);
        APoint := FProjection.LonLat2PixelPosFloat(VPoint);
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
  const AProjection: IProjection
);
begin
  Assert(Assigned(AProjection));
  inherited Create;
  FProjection := AProjection;
end;

function TLonLatPointConverter.CreateFilteredEnum(
  const ASource: IEnumLonLatPoint
): IEnumProjectedPoint;
begin
  Result := TEnumDoublePointLonLatToMapPixel.Create(FProjection, ASource);
end;

end.
