unit u_EnumDoublePointLine2Poly;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_DoublePointFilter,
  i_ProjectionInfo,
  i_DoublePointsAggregator,
  u_BaseInterfacedObject;

type
  TEnumDoublePointLine2Poly = class(TBaseInterfacedObject, IEnumLonLatPoint)
  private
    FSourceEnum: IEnumLonLatPoint;
    FRadius: Double;
    FProjection: IProjection;
    FTemp: IDoublePointsAggregator;

    FLineStarted: Boolean;
    FFinished: Boolean;
    FReturnTemp: Boolean;
    FTempIndex: Integer;
    FPrevLonLat: TDoublePoint;
    FPrevPoint: TDoublePoint;
    FPrevVectorAngle: Double;
    FPrevRadius: Double;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ASourceEnum: IEnumLonLatPoint;
      const ARadius: Double;
      const AProjection: IProjection;
      const ATemp: IDoublePointsAggregator = nil
    );
  end;

  TLonLatPointFilterLine2Poly = class(TBaseInterfacedObject, ILonLatPointFilter)
  private
    FRadius: Double;
    FProjection: IProjection;
    FTemp: IDoublePointsAggregator;
  private
    function CreateFilteredEnum(const ASource: IEnumLonLatPoint): IEnumLonLatPoint;
  public
    constructor Create(
      const ARadius: Double;
      const AProjection: IProjection;
      const ATemp: IDoublePointsAggregator = nil
    );
  end;


implementation

uses
  Math,
  u_DoublePointsAggregator,
  u_GeoFunc;

{ TEnumDoublePointLine2Poly }

constructor TEnumDoublePointLine2Poly.Create(
  const ASourceEnum: IEnumLonLatPoint;
  const ARadius: Double;
  const AProjection: IProjection;
  const ATemp: IDoublePointsAggregator
);
begin
  inherited Create;
  FSourceEnum := ASourceEnum;
  FRadius := ARadius;
  FProjection := AProjection;
  FTemp := ATemp;
  if FTemp = nil then begin
    FTemp := TDoublePointsAggregator.Create;
  end;
  FReturnTemp := False;
  FFinished := False;
  FLineStarted := False;
end;

function TEnumDoublePointLine2Poly.Next(out APoint: TDoublePoint): Boolean;
var
  VCurrLonLat: TDoublePoint;
  VCurrPoint: TDoublePoint;
  VCurrVectorAngle: Double;
  VVector: TDoublePoint;
  VLonLatMul: Double;
  Angle: Double;
  VRadius: Double;
  s, c: Extended;
  a3: Double;
  VSinA3: Double;
  VResultPixelPos: TDoublePoint;
  VDist: Double;
begin
  if FReturnTemp then begin
    APoint := FTemp.Points[FTempIndex];
    Result := True;
    Dec(FTempIndex);
    if FTempIndex < 0 then begin
      FReturnTemp := False;
    end;
  end else begin
    if not FFinished then begin
      Result := False;
      while not FFinished do begin
        FFinished := not FSourceEnum.Next(VCurrLonLat);
        if not FFinished then begin
          if PointIsEmpty(VCurrLonLat) then begin
            FFinished := True;
          end else begin
            VCurrPoint := FProjection.LonLat2PixelPosFloat(VCurrLonLat);
            if FLineStarted then begin
              VVector.X := VCurrPoint.X - FPrevPoint.X;
              VVector.Y := VCurrPoint.Y - FPrevPoint.Y;
              VDist := FProjection.ProjectionType.Datum.CalcDist(FPrevLonLat, VCurrLonLat);
              if VDist > 0.000001 then begin
                VLonLatMul := FRadius / VDist;
                VLonLatMul := VLonLatMul * sqrt(sqr(VVector.X) + sqr(VVector.Y));

                VCurrVectorAngle := Math.Arctan2(VVector.Y, VVector.X);
                if VCurrVectorAngle < 0 then begin
                  VCurrVectorAngle := 2 * pi + VCurrVectorAngle;
                end;
                Angle := (VCurrVectorAngle + FPrevVectorAngle) / 2;
                if abs(FPrevVectorAngle - VCurrVectorAngle) > Pi then begin
                  Angle := Angle - Pi;
                end;
                a3 := abs((pi / 2 + Angle) - VCurrVectorAngle);
                if a3 > Pi then begin
                  a3 := a3 - Pi;
                end;
                VSinA3 := sin(a3);
                if VSinA3 < 1.0 / 7 then begin
                  VRadius := VLonLatMul * 7;
                end else begin
                  VRadius := VLonLatMul / VSinA3;
                end;

                SinCos(pi / 2 + Angle, s, c);
                VResultPixelPos := DoublePoint(FPrevPoint.x + VRadius * c, FPrevPoint.y + VRadius * s);
                FProjection.ValidatePixelPosFloat(VResultPixelPos, false);
                APoint := FProjection.PixelPosFloat2LonLat(VResultPixelPos);
                SinCos(pi / 2 + Angle + pi, s, c);
                VResultPixelPos := DoublePoint(FPrevPoint.x + VRadius * c, FPrevPoint.y + VRadius * s);
                FProjection.ValidatePixelPosFloat(VResultPixelPos, false);
                FTemp.Add(
                  FProjection.PixelPosFloat2LonLat(VResultPixelPos)
                );
                FPrevLonLat := VCurrLonLat;
                FPrevPoint := VCurrPoint;
                FPrevVectorAngle := VCurrVectorAngle;
                FPrevRadius := VLonLatMul;
                Result := True;
                Break;
              end;
            end else begin
              FTemp.Clear;
              FPrevLonLat := VCurrLonLat;
              FPrevPoint := VCurrPoint;

              FFinished := not FSourceEnum.Next(VCurrLonLat);
              if not FFinished then begin
                if PointIsEmpty(VCurrLonLat) then begin
                  FFinished := True;
                end;
              end;
              if FFinished then begin
                APoint := FPrevLonLat;
                Result := True;
                Break;
              end else begin
                VCurrPoint := FProjection.LonLat2PixelPosFloat(VCurrLonLat);
                VVector.X := VCurrPoint.X - FPrevPoint.X;
                VVector.Y := VCurrPoint.Y - FPrevPoint.Y;
                VDist := FProjection.ProjectionType.Datum.CalcDist(FPrevLonLat, VCurrLonLat);
                if VDist > 0.000001 then begin
                  VLonLatMul := FRadius / VDist;
                  VLonLatMul := VLonLatMul * sqrt(sqr(VVector.X) + sqr(VVector.Y));

                  VCurrVectorAngle := Math.Arctan2(VVector.Y, VVector.X);
                  if VCurrVectorAngle < 0 then begin
                    VCurrVectorAngle := 2 * pi + VCurrVectorAngle;
                  end;
                  Angle := VCurrVectorAngle;
                  VRadius := VLonLatMul / sin(pi / 4);
                  SinCos(pi / 2 + pi / 4 + Angle, s, c);
                  VResultPixelPos := DoublePoint(FPrevPoint.x + VRadius * c, FPrevPoint.y + VRadius * s);
                  FProjection.ValidatePixelPosFloat(VResultPixelPos, false);
                  APoint := FProjection.PixelPosFloat2LonLat(VResultPixelPos);
                  FTemp.Add(APoint);
                  SinCos(pi / 2 - pi / 4 + Angle + pi, s, c);
                  VResultPixelPos := DoublePoint(FPrevPoint.x + VRadius * c, FPrevPoint.y + VRadius * s);
                  FProjection.ValidatePixelPosFloat(VResultPixelPos, false);
                  FTemp.Add(FProjection.PixelPosFloat2LonLat(VResultPixelPos));
                  FLineStarted := True;

                  FPrevLonLat := VCurrLonLat;
                  FPrevPoint := VCurrPoint;
                  FPrevVectorAngle := VCurrVectorAngle;
                  FPrevRadius := VLonLatMul;

                  Result := True;
                  Break;
                end;
              end;
            end;
          end;
        end;
        if FFinished then begin
          if FLineStarted then begin
            Angle := FPrevVectorAngle;
            VRadius := FPrevRadius / sin(pi / 4);
            SinCos(pi / 4 + Angle, s, c);
            VResultPixelPos := DoublePoint(FPrevPoint.x + VRadius * c, FPrevPoint.y + VRadius * s);
            FProjection.ValidatePixelPosFloat(VResultPixelPos, false);
            APoint := FProjection.PixelPosFloat2LonLat(VResultPixelPos);
            Result := True;

            SinCos(pi / 2 + pi / 4 + Angle + pi, s, c);
            VResultPixelPos := DoublePoint(FPrevPoint.x + VRadius * c, FPrevPoint.y + VRadius * s);
            FProjection.ValidatePixelPosFloat(VResultPixelPos, false);
            FTemp.Add(
              FProjection.PixelPosFloat2LonLat(VResultPixelPos)
            );
            FTempIndex := FTemp.Count - 1;
            FReturnTemp := True;
          end;
        end;
      end;
    end else begin
      APoint := CEmptyDoublePoint;
      Result := False;
    end;
  end;
end;

{ TLonLatPointFilterLine2Poly }

constructor TLonLatPointFilterLine2Poly.Create(
  const ARadius: Double;
  const AProjection: IProjection;
  const ATemp: IDoublePointsAggregator
);
begin
  inherited Create;
  FRadius := ARadius;
  FProjection := AProjection;
  FTemp := ATemp;
end;

function TLonLatPointFilterLine2Poly.CreateFilteredEnum(
  const ASource: IEnumLonLatPoint
): IEnumLonLatPoint;
begin
  Result := TEnumDoublePointLine2Poly.Create(ASource, FRadius, FProjection, FTemp);
end;

end.
