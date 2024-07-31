{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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
    FProjectedProvider: IGeometryProjectedProvider;

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
    function GetPolyHintInfo(
      const ALocalConverter: ILocalCoordConverter;
      const APoly: IGeometryLonLatPolygon;
      const AMousePos: TPoint;
      out AInfo: TPolyHintInfo
    ): Boolean;
  public
    constructor Create(
      const ADatum: IDatum;
      const AProjectedProvider: IGeometryProjectedProvider
    );
  end;

implementation

uses
  SysUtils,
  i_Projection;

{ TGeometryHintInfoProvider }

constructor TGeometryHintInfoProvider.Create(
  const ADatum: IDatum;
  const AProjectedProvider: IGeometryProjectedProvider
);
begin
  inherited Create;
  FDatum := ADatum;
  FProjectedProvider := AProjectedProvider;
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
begin
  if APoly.IsSameGeometry(FPolyItem) then begin
    if Supports(APoly, IGeometryLonLatSinglePolygon) then begin
      VIndex := 0;
    end else begin
      VIndex := FindContourIndex();
    end;
  end else begin
    FPolyItem := APoly;
    CalcPolyInfo(VIndex);
  end;

  Result := (VIndex >= 0) and (VIndex < Length(FPolyInfo));
  if not Result then begin
    Exit;
  end;

  with AInfo do begin
    Area := FPolyInfo[VIndex].Area;
    Perimeter := FPolyInfo[VIndex].Perimeter;
    PointsCount := FPolyInfo[VIndex].PointsCount;
    ContoursCount := Length(FPolyInfo);
    CurrentContour := VIndex + 1;
  end;
end;

end.
