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

unit u_TextByVectorItemMarkInfo;

interface

uses
  i_VectorDataItemSimple,
  i_CoordToStringConverter,
  i_ValueToStringConverter,
  i_GeometryLonLat,
  i_GeoCalc,
  i_TextByVectorItem,
  u_BaseInterfacedObject;

type
  TTextByVectorItemMarkInfo = class(TBaseInterfacedObject, ITextByVectorItem)
  private
    FCoordToStringConverter: ICoordToStringConverterChangeable;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FGeoCalcChangeable: IGeoCalcChangeable;

    FGeoCalc: IGeoCalc;

    function GetTextForGeometry(const AGeometry: IGeometryLonLat): string;

    function GetTextForGeometryPoint(const AGeometry: IGeometryLonLatPoint): string;
    function GetTextForGeometryLine(const AGeometry: IGeometryLonLatSingleLine): string;
    function GetTextForGeometryPolygon(const AGeometry: IGeometryLonLatSinglePolygon): string;
    function GetTextForGeometryMultiLine(const AGeometry: IGeometryLonLatMultiLine): string;
    function GetTextForGeometryMultiPolygon(const AGeometry: IGeometryLonLatMultiPolygon): string;
  private
    { ITextByVectorItem }
    function GetText(const AItem: IVectorDataItem): string;
  public
    constructor Create(
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AGeoCalc: IGeoCalcChangeable
    );
  end;


implementation

uses
  SysUtils,
  gnugettext,
  u_ResStrings;

{ TTextByVectorItemMarkInfo }

constructor TTextByVectorItemMarkInfo.Create(
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AGeoCalc: IGeoCalcChangeable
);
begin
  Assert(ACoordToStringConverter <> nil);
  Assert(AValueToStringConverter <> nil);
  Assert(Assigned(AGeoCalc));

  inherited Create;

  FCoordToStringConverter := ACoordToStringConverter;
  FValueToStringConverter := AValueToStringConverter;
  FGeoCalcChangeable := AGeoCalc;
end;

function TTextByVectorItemMarkInfo.GetTextForGeometry(
  const AGeometry: IGeometryLonLat
): string;
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatSingleLine;
  VPoly: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiLine;
  VMultiPoly: IGeometryLonLatMultiPolygon;
begin
  if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
    Result := GetTextForGeometryPoint(VPoint);
  end else
  if Supports(AGeometry, IGeometryLonLatSingleLine, VLine) then begin
    Result := GetTextForGeometryLine(VLine);
  end else
  if Supports(AGeometry, IGeometryLonLatSinglePolygon, VPoly) then begin
    Result := GetTextForGeometryPolygon(VPoly);
  end else
  if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
    Result := GetTextForGeometryMultiLine(VMultiLine);
  end else
  if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPoly) then begin
    Result := GetTextForGeometryMultiPolygon(VMultiPoly);
  end else begin
    Result := 'Unknown geometry type';
  end;
end;

function TTextByVectorItemMarkInfo.GetTextForGeometryLine(
  const AGeometry: IGeometryLonLatSingleLine
): string;
var
  VLength: Double;
  VPartsCount: Integer;
  VPointsCount: Integer;
  VConverter: IValueToStringConverter;
begin
  VPartsCount := 1;
  VPointsCount := AGeometry.Count;
  VLength := FGeoCalc.CalcLineLength(AGeometry);
  VConverter := FValueToStringConverter.GetStatic;

  Result :=
    Format(_('Parts count: %d'), [VPartsCount]) + '<br>' + #13#10 +
    Format(_('Points count: %d'), [VPointsCount]) + '<br>' + #13#10 +
    Format(_('Length: %s'), [VConverter.DistConvert(VLength)]) + '<br>' + #13#10;
end;

function TTextByVectorItemMarkInfo.GetTextForGeometryMultiLine(
  const AGeometry: IGeometryLonLatMultiLine
): string;
var
  I: Integer;
  VLength: Double;
  VPartsCount: Integer;
  VPointsCount: Integer;
  VConverter: IValueToStringConverter;
begin
  VPartsCount := AGeometry.Count;
  VPointsCount := 0;
  for I := 0 to VPartsCount - 1 do begin
    Inc(VPointsCount, AGeometry.Item[I].Count);
  end;
  VLength := FGeoCalc.CalcMultiLineLength(AGeometry);
  VConverter := FValueToStringConverter.GetStatic;

  Result :=
    Format(_('Parts count: %d'), [VPartsCount]) + '<br>' + #13#10 +
    Format(_('Points count: %d'), [VPointsCount]) + '<br>' + #13#10 +
    Format(_('Length: %s'), [VConverter.DistConvert(VLength)]) + '<br>' + #13#10;
end;

function CalcPolyPointsCount(const APoly: IGeometryLonLatSinglePolygon): Integer; inline;
var
  I: Integer;
begin
  Result := APoly.OuterBorder.Count;
  for I := 0 to APoly.HoleCount - 1 do begin
    Inc(Result, APoly.HoleBorder[I].Count);
  end;
end;

function TTextByVectorItemMarkInfo.GetTextForGeometryMultiPolygon(
  const AGeometry: IGeometryLonLatMultiPolygon
): string;
var
  I: Integer;
  VLength: Double;
  VArea: Double;
  VPartsCount: Integer;
  VPointsCount: Integer;
  VConverter: IValueToStringConverter;
begin
  VPartsCount := AGeometry.Count;
  VPointsCount := 0;
  for I := 0 to VPartsCount - 1 do begin
    Inc(VPointsCount, CalcPolyPointsCount(AGeometry.Item[I]));
  end;
  VLength := FGeoCalc.CalcMultiPolygonPerimeter(AGeometry);
  VArea := FGeoCalc.CalcMultiPolygonArea(AGeometry);
  VConverter := FValueToStringConverter.GetStatic;

  Result :=
    Format(_('Parts count: %d'), [VPartsCount]) + '<br>' + #13#10 +
    Format(_('Points count: %d'), [VPointsCount]) + '<br>' + #13#10 +
    Format(SAS_STR_Perimeter, [VConverter.DistConvert(VLength)]) + '<br>' + #13#10 +
    Format(SAS_STR_Area, [VConverter.AreaConvert(VArea)]) + '<br>' + #13#10;
end;

function TTextByVectorItemMarkInfo.GetTextForGeometryPoint(
  const AGeometry: IGeometryLonLatPoint
): string;
var
  VConverter: ICoordToStringConverter;
begin
  VConverter := FCoordToStringConverter.GetStatic;
  Result := Format(_('Coordinates: %s'), [VConverter.LonLatConvert(AGeometry.Point)]) + '<br>' + #13#10;
end;

function TTextByVectorItemMarkInfo.GetTextForGeometryPolygon(
  const AGeometry: IGeometryLonLatSinglePolygon
): string;
var
  VLength: Double;
  VArea: Double;
  VPartsCount: Integer;
  VPointsCount: Integer;
  VConverter: IValueToStringConverter;
begin
  VPartsCount := 1;
  VPointsCount := CalcPolyPointsCount(AGeometry);
  VLength := FGeoCalc.CalcPolygonPerimeter(AGeometry);
  VArea := FGeoCalc.CalcPolygonArea(AGeometry);
  VConverter := FValueToStringConverter.GetStatic;

  Result :=
    Format(_('Parts count: %d'), [VPartsCount]) + '<br>' + #13#10 +
    Format(_('Points count: %d'), [VPointsCount]) + '<br>' + #13#10 +
    Format(SAS_STR_Perimeter, [VConverter.DistConvert(VLength)]) + '<br>' + #13#10 +
    Format(SAS_STR_Area, [VConverter.AreaConvert(VArea)]) + '<br>' + #13#10;
end;

function TTextByVectorItemMarkInfo.GetText(const AItem: IVectorDataItem): string;
var
  VItemWithCategory: IVectorDataItemWithCategory;
  VCategoryName: string;
begin
  FGeoCalc := FGeoCalcChangeable.GetStatic;

  VCategoryName := '';
  if Supports(AItem.MainInfo, IVectorDataItemWithCategory, VItemWithCategory) then begin
    if VItemWithCategory.Category <> nil then begin
      VCategoryName := VItemWithCategory.Category.Name;
    end;
  end;

  Result :=
    Format(_('Category: %s'), [VCategoryName]) + '<br>' + #13#10 +
    Format(_('Name: %s'), [AItem.Name]) + '<br>' + #13#10 +
    GetTextForGeometry(AItem.Geometry) +
    Format(_('Description:<br>' + #13#10 + '%s'), [AItem.Desc]) + '<br>' + #13#10;

  if Result <> '' then begin
    Result :=
      '<html>' + #13#10 +
      '<head>' + #13#10 +
      '<title>' + AItem.GetInfoCaption + '</title>' + #13#10 +
      '</head>' + #13#10 +
      '<body>' + #13#10 +
      Result + #13#10 +
      '</body>' + #13#10 +
      '</html>';
  end;
end;

end.
