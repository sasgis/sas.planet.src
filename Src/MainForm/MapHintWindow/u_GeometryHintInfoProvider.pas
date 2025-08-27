{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_GeometryHintInfoProvider;

interface

uses
  Types,
  t_GeoTypes,
  i_Datum,
  i_GeoCalc,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_GeometryProjectedProvider,
  i_GeometryHintInfoProvider,
  i_LocalCoordConverter,
  i_VectorDataItemSimple,
  u_BaseInterfacedObject;

type
  TGeometryHintInfoProvider = class(TBaseInterfacedObject, IGeometryHintInfoProvider)
  private
    FDatum: IDatum;

    FGeoCalc: IGeoCalcChangeable;
    FProjectedProvider: IGeometryProjectedProvider;

    // line info cache
    FLineItem: IGeometryLonLatLine;
    FLines: TArrayOfGeometryLonLatSingleLine;
    FLineInfo: array of record
      Distance: array of Double;
    end;

    // polygon info cache
    FPolyItem: IGeometryLonLatPolygon;
    FPolyInfo: array of record
      Area: Double;
      Perimeter: Double;
      PointsCount: Integer;
    end;
    FLocalConverter: ILocalCoordConverter;
    FProjectedPolygon: IGeometryProjectedPolygon;
  private
    { IGeometryHintInfoProvider }
    function GetLineHintInfo(
      const ALocalConverter: ILocalCoordConverter;
      const ALine: IGeometryLonLatLine;
      const AMousePos: TPoint;
      out AInfo: TLineHintInfo
    ): Boolean;

    function GetPolyHintInfo(
      const ALocalConverter: ILocalCoordConverter;
      const APoly: IGeometryLonLatPolygon;
      const AMousePos: TPoint;
      out AInfo: TPolyHintInfo
    ): Boolean;
  public
    constructor Create(
      const AGeoCalc: IGeoCalcChangeable;
      const AProjectedProvider: IGeometryProjectedProvider
    );
  end;

implementation

uses
  Math,
  SysUtils,
  DateUtils,
  u_GeometryFunc,
  i_Projection,
  i_ProjectionType;

{ TGeometryHintInfoProvider }

constructor TGeometryHintInfoProvider.Create(
  const AGeoCalc: IGeoCalcChangeable;
  const AProjectedProvider: IGeometryProjectedProvider
);
begin
  inherited Create;
  FGeoCalc := AGeoCalc;
  FProjectedProvider := AProjectedProvider;
end;

function TGeometryHintInfoProvider.GetLineHintInfo(
  const ALocalConverter: ILocalCoordConverter;
  const ALine: IGeometryLonLatLine;
  const AMousePos: TPoint;
  out AInfo: TLineHintInfo
): Boolean;

  function GetNearestPointIndex(
    const ASingle: IGeometryLonLatSingleLine;
    const AProjection: IProjection;
    const APixelPos: TDoublePoint;
    out ADist: Double
  ): Integer;
  var
    I: Integer;
    VPoints: PDoublePointArray;
    VProjectionType: IProjectionType;
    VMapPoint: TDoublePoint;
    VCurrDist: Double;
  begin
    Result := -1;
    ADist := 0;

    VPoints := ASingle.Points;
    VProjectionType := AProjection.ProjectionType;

    for I := 0 to ASingle.Count - 1 do begin
      VProjectionType.ValidateLonLatPos(VPoints[I]);
      VMapPoint := AProjection.LonLat2PixelPosFloat(VPoints[I]);
      VCurrDist := Sqr(VMapPoint.X - APixelPos.X) + Sqr(VMapPoint.Y - APixelPos.Y);
      if (Result < 0) or (VCurrDist < ADist) then begin
        ADist := VCurrDist;
        Result := I;
      end;
    end;
  end;

  function CalcSpeed(ALineIndex, APointIndex: Integer): Double;
  var
    VDistA, VDistB: Double;
    VTimeA, VTimeB: TDateTime;
    VSeconds: Int64;
  begin
    Result := 0;

    if (ALineIndex = 0) and (APointIndex = 0) then begin
      Exit;
    end;

    if FLines[ALineIndex].Meta = nil then begin
      Exit;
    end;

    VDistB := FLineInfo[ALineIndex].Distance[APointIndex];
    VTimeB := FLines[ALineIndex].Meta.TimeStamp[APointIndex];

    repeat
      Dec(APointIndex);
      if APointIndex < 0 then begin
        Dec(ALineIndex);
        if ALineIndex < 0 then begin
          Exit;
        end;
        APointIndex := Length(FLineInfo[ALineIndex].Distance) - 1;
      end;

      if FLines[ALineIndex].Meta = nil then begin
        Exit;
      end;

      VDistA := FLineInfo[ALineIndex].Distance[APointIndex];
      VTimeA := FLines[ALineIndex].Meta.TimeStamp[APointIndex];

    until (VTimeA <> VTimeB) and (VDistB <> VDistA);

    VSeconds := SecondsBetween(VTimeA, VTimeB);
    if VSeconds = 0 then begin
      Exit;
    end;

    Result := Abs(VDistB - VDistA) / VSeconds; // m/s

    Result := Result * 3.6; // km/h
  end;

var
  I, J: Integer;
  VDatum: IDatum;
  VSingle: IGeometryLonLatSingleLine;
  VPixelPos: TDoublePoint;
  VDist, VCurrDist: Double;
  VLineIndex: Integer;
  VPointIndex: Integer;
  VPoints: PDoublePointArray;
  VMeta: PDoublePointsMeta;
begin
  VDatum := FGeoCalc.Datum;

  if not VDatum.IsSameDatum(FDatum) or not ALine.IsSameGeometry(FLineItem) then begin
    FDatum := VDatum;
    FLineItem := ALine;
    FLines := GeometryLonLatLineToArray(ALine);

    VDist := 0;
    SetLength(FLineInfo, Length(FLines));

    for I := 0 to Length(FLines) - 1 do begin
      VSingle := FLines[I];
      VPoints := VSingle.Points;

      SetLength(FLineInfo[I].Distance, VSingle.Count);

      FLineInfo[I].Distance[0] := VDist;
      for J := 1 to VSingle.Count - 1 do begin
        FLineInfo[I].Distance[J] := VDist + FDatum.CalcDist(VPoints[J-1], VPoints[J]);
        VDist := FLineInfo[I].Distance[J];
      end;
    end;
  end;

  VLineIndex := -1;
  VPointIndex := -1;

  VPixelPos := ALocalConverter.LocalPixel2MapPixelFloat(AMousePos);

  VDist := 0;
  for I := 0 to Length(FLines) - 1 do begin
    J := GetNearestPointIndex(FLines[I], ALocalConverter.Projection, VPixelPos, VCurrDist);
    if (VLineIndex < 0) or (VPointIndex < 0) or (VCurrDist < VDist) then begin
      VLineIndex := I;
      VPointIndex := J;
      VDist := VCurrDist;
    end;
  end;

  Result :=
    (VLineIndex >= 0) and (VLineIndex < Length(FLineInfo)) and
    (VPointIndex >= 0) and (VPointIndex < Length(FLineInfo[VLineIndex].Distance));

  if not Result then begin
    Exit;
  end;

  AInfo.LonLatPos := FLines[VLineIndex].Points[VPointIndex];
  AInfo.Distance := FLineInfo[VLineIndex].Distance[VPointIndex];

  AInfo.Elevation := NaN;
  AInfo.TimeStamp := 0;
  AInfo.Speed := NaN;

  VMeta := FLines[VLineIndex].Meta;
  if VMeta <> nil then begin
    if VMeta.Elevation <> nil then begin
      AInfo.Elevation := VMeta.Elevation[VPointIndex];
    end;
    if VMeta.TimeStamp <> nil then begin
      AInfo.TimeStamp := VMeta.TimeStamp[VPointIndex];
      if AInfo.TimeStamp <> 0 then begin
        AInfo.TimeStamp := TTimeZone.Local.ToLocalTime(AInfo.TimeStamp);
      end;
    end;
    if AInfo.TimeStamp <> 0 then begin
      AInfo.Speed := CalcSpeed(VLineIndex, VPointIndex);
    end;
  end;
end;

function TGeometryHintInfoProvider.GetPolyHintInfo(
  const ALocalConverter: ILocalCoordConverter;
  const APoly: IGeometryLonLatPolygon;
  const AMousePos: TPoint;
  out AInfo: TPolyHintInfo
): Boolean;

  function FindContourIndex(): Integer;
  var
    I: Integer;
    VPixelPos: TDoublePoint;
    VProjection: IProjection;
    VProjectdPolygon: IGeometryProjectedPolygon;
    VMulti: IGeometryProjectedMultiPolygon;
    VSingle: IGeometryProjectedSinglePolygon;
  begin
    Result := -1;

    VProjection := ALocalConverter.Projection;

    if ALocalConverter.GetIsSameConverter(FLocalConverter) then begin
      VProjectdPolygon := FProjectedPolygon;
    end else begin
      VProjectdPolygon := FProjectedProvider.GetProjectedPolygon(VProjection, APoly);
      FLocalConverter := ALocalConverter;
      FProjectedPolygon := VProjectdPolygon;
    end;

    if VProjectdPolygon = nil then begin
      Exit;
    end;

    if Supports(VProjectdPolygon, IGeometryProjectedMultiPolygon, VMulti) then begin
      VPixelPos := ALocalConverter.LocalPixel2MapPixelFloat(AMousePos);
      VProjection.ValidatePixelPosFloatStrict(VPixelPos, False);
      for I := 0 to VMulti.Count - 1 do begin
        VSingle := VMulti.Item[I];
        if VSingle.IsPointInPolygon(VPixelPos) or VSingle.IsPointOnBorder(VPixelPos, 3) then begin
          Result := I;
          Exit;
        end;
      end;
    end else
    if Supports(VProjectdPolygon, IGeometryProjectedSinglePolygon) then begin
      Assert(False);
      Result := 0;
    end else begin
      raise Exception.Create('Unknown projected polygon type!');
    end;
  end;

  function CalcArea(const ASingle: IGeometryLonLatSinglePolygon): Double;
  var
    I: Integer;
    VContour: IGeometryLonLatContour;
  begin
    Result := 0;
    VContour := ASingle.OuterBorder;
    if VContour.Count > 2 then begin
      Result := FDatum.CalcPolygonArea(VContour.Points, VContour.Count);
      for I := 0 to ASingle.HoleCount - 1 do begin
        VContour := ASingle.HoleBorder[I];
        if VContour.Count > 2 then begin
          Result := Result - FDatum.CalcPolygonArea(VContour.Points, VContour.Count);
        end;
      end;
    end;
  end;

  function CalcPerimeter(const ASingle: IGeometryLonLatSinglePolygon): Double;
  var
    I: Integer;
    VContour: IGeometryLonLatContour;
  begin
    VContour := ASingle.OuterBorder;
    Result := FDatum.CalcPolygonPerimeter(VContour.Points, VContour.Count);
    for I := 0 to ASingle.HoleCount - 1 do begin
      VContour := ASingle.HoleBorder[I];
      Result := Result + FDatum.CalcPolygonPerimeter(VContour.Points, VContour.Count);
    end;
  end;

  procedure CalcPolyInfo(out AContourIndex: Integer);
  var
    I: Integer;
    VMulti: IGeometryLonLatMultiPolygon;
    VSingle: IGeometryLonLatSinglePolygon;
  begin
    if Supports(APoly, IGeometryLonLatSinglePolygon, VSingle) then begin
      AContourIndex := 0;
      SetLength(FPolyInfo, 1);
      FPolyInfo[0].Area := CalcArea(VSingle);
      FPolyInfo[0].Perimeter := CalcPerimeter(VSingle);
      FPolyInfo[0].PointsCount := VSingle.OuterBorder.Count;
    end else
    if Supports(APoly, IGeometryLonLatMultiPolygon, VMulti) then begin
      AContourIndex := FindContourIndex;
      SetLength(FPolyInfo, VMulti.Count);
      for I := 0 to VMulti.Count - 1 do begin
        VSingle := VMulti.Item[I];
        FPolyInfo[I].Area := CalcArea(VSingle);
        FPolyInfo[I].Perimeter := CalcPerimeter(VSingle);
        FPolyInfo[I].PointsCount := VSingle.OuterBorder.Count;
      end;
    end else begin
      raise Exception.Create('Unknown lonlat polygon type!');
    end;
  end;

var
  VIndex: Integer;
  VDatum: IDatum;
begin
  VDatum := FGeoCalc.Datum;

  if not VDatum.IsSameDatum(FDatum) or not APoly.IsSameGeometry(FPolyItem) then begin
    FDatum := VDatum;
    FPolyItem := APoly;
    CalcPolyInfo(VIndex);
  end else begin
    if Supports(APoly, IGeometryLonLatSinglePolygon) then begin
      VIndex := 0;
    end else begin
      VIndex := FindContourIndex();
    end;
  end;

  Result := (VIndex >= 0) and (VIndex < Length(FPolyInfo));

  if not Result then begin
    Exit;
  end;

  AInfo.Area := FPolyInfo[VIndex].Area;
  AInfo.Perimeter := FPolyInfo[VIndex].Perimeter;
  AInfo.PointsCount := FPolyInfo[VIndex].PointsCount;
  AInfo.ContoursCount := Length(FPolyInfo);
  AInfo.CurrentContour := VIndex + 1;
end;

end.
