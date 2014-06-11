{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_GeoCalc;

interface

uses
  i_Datum,
  i_NotifierOperation,
  i_GeometryLonLat,
  i_GeoCalc,
  u_BaseInterfacedObject;

type
  TGeoCalc = class(TBaseInterfacedObject, IGeoCalc)
  private
    FDatum: IDatum;
  private
    function CalcSingleLineLength(const ALine: IGeometryLonLatSingleLine): Double;
    function CalcMultiLineLength(const ALine: IGeometryLonLatMultiLine): Double;

    function CalcSinglePolygonPerimeter(const ALine: IGeometryLonLatSinglePolygon): Double;
    function CalcMultiPolygonPerimeter(const ALine: IGeometryLonLatMultiPolygon): Double;
    function CalcSinglePolygonArea(
      const ALine: IGeometryLonLatSinglePolygon;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
    function CalcMultiPolygonArea(
      const ALine: IGeometryLonLatMultiPolygon;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
  private
    function CalcLineLength(const ALine: IGeometryLonLatLine): Double;

    function CalcPolygonPerimeter(const ALine: IGeometryLonLatPolygon): Double;
    function CalcPolygonArea(
      const ALine: IGeometryLonLatPolygon;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
  public
    constructor Create(
      const ADatum: IDatum
    );
  end;

implementation

uses
  SysUtils,
  Math,
  t_GeoTypes,
  i_EnumDoublePoint;

{ TGeoCalc }

constructor TGeoCalc.Create(const ADatum: IDatum);
begin
  Assert(Assigned(ADatum));
  inherited Create;
  FDatum := ADatum;
end;

function TGeoCalc.CalcSingleLineLength(const ALine: IGeometryLonLatSingleLine): Double;
var
  VEnum: IEnumLonLatPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := ALine.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + FDatum.CalcDist(VPrevPoint, VCurrPoint);
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TGeoCalc.CalcLineLength(const ALine: IGeometryLonLatLine): Double;
var
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  if Supports(ALine, IGeometryLonLatMultiLine, VMultiLine) then begin
    Result := CalcMultiLineLength(VMultiLine);
  end else if Supports(ALine, IGeometryLonLatSingleLine, VSingleLine) then begin
    Result := CalcSingleLineLength(VSingleLine);
  end else begin
    Result := NaN;
  end;
end;

function TGeoCalc.CalcMultiLineLength(
  const ALine: IGeometryLonLatMultiLine
): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ALine.Count - 1 do begin
    Result := Result + CalcSingleLineLength(ALine.Item[i]);
  end;
end;

function TGeoCalc.CalcMultiPolygonArea(
  const ALine: IGeometryLonLatMultiPolygon;
  const ANotifier: INotifierOperation;
  const AOperationID: Integer
): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ALine.Count - 1 do begin
    Result := Result + CalcSinglePolygonArea(ALine.Item[i], ANotifier, AOperationID);
  end;
end;

function TGeoCalc.CalcMultiPolygonPerimeter(
  const ALine: IGeometryLonLatMultiPolygon
): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ALine.Count - 1 do begin
    Result := Result + CalcSinglePolygonPerimeter(ALine.Item[i]);
  end;
end;

function TGeoCalc.CalcPolygonArea(
  const ALine: IGeometryLonLatPolygon;
  const ANotifier: INotifierOperation;
  const AOperationID: Integer
): Double;
var
  VSingleLine: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiPolygon;
begin
  if Supports(ALine, IGeometryLonLatMultiPolygon, VMultiLine) then begin
    Result := CalcMultiPolygonArea(VMultiLine);
  end else if Supports(ALine, IGeometryLonLatSinglePolygon, VSingleLine) then begin
    Result := CalcSinglePolygonArea(VSingleLine);
  end else begin
    Result := NaN;
  end;
end;

function TGeoCalc.CalcPolygonPerimeter(
  const ALine: IGeometryLonLatPolygon): Double;
var
  VSingleLine: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiPolygon;
begin
  if Supports(ALine, IGeometryLonLatMultiPolygon, VMultiLine) then begin
    Result := CalcMultiPolygonPerimeter(VMultiLine);
  end else if Supports(ALine, IGeometryLonLatSinglePolygon, VSingleLine) then begin
    Result := CalcSinglePolygonPerimeter(VSingleLine);
  end else begin
    Result := NaN;
  end;
end;

function TGeoCalc.CalcSinglePolygonArea(
  const ALine: IGeometryLonLatSinglePolygon;
  const ANotifier: INotifierOperation;
  const AOperationID: Integer
): Double;
var
  VCount: Integer;
begin
  VCount := ALine.Count;
  if VCount < 3 then begin
    Result := 0;
  end else begin
    Result := FDatum.CalcPolygonArea(ALine.Points, VCount, ANotifier, AOperationID);
  end;
end;

function TGeoCalc.CalcSinglePolygonPerimeter(
  const ALine: IGeometryLonLatSinglePolygon
): Double;
var
  VEnum: IEnumLonLatPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := ALine.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + FDatum.CalcDist(VPrevPoint, VCurrPoint);
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

end.
