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

unit u_GeoCalc;

interface

uses
  i_Datum,
  i_NotifierOperation,
  i_GeometryLonLat,
  i_GeoCalc,
  u_ChangeableBase,
  u_BaseInterfacedObject;

type
  TGeoCalc = class(TBaseInterfacedObject, IGeoCalc)
  private
    FDatum: IDatum;
  private
    { IGeoCalc }
    function GetDatum: IDatum;
    function IsSame(const AGeoCalc: IGeoCalc): Boolean;

    function CalcSingleLineLength(const ALine: IGeometryLonLatSingleLine): Double; inline;
    function CalcMultiLineLength(const ALine: IGeometryLonLatMultiLine): Double; inline;
    function CalcLineLength(const ALine: IGeometryLonLatLine): Double;

    function CalcContourPerimeter(const AContour: IGeometryLonLatContour): Double; inline;
    function CalcSinglePolygonPerimeter(const APolygon: IGeometryLonLatSinglePolygon): Double; inline;
    function CalcMultiPolygonPerimeter(const APolygon: IGeometryLonLatMultiPolygon): Double; inline;
    function CalcPolygonPerimeter(const APolygon: IGeometryLonLatPolygon): Double;

    function CalcContourArea(
      const AContour: IGeometryLonLatContour;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double; inline;

    function CalcSinglePolygonArea(
      const APolygon: IGeometryLonLatSinglePolygon;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double; inline;

    function CalcMultiPolygonArea(
      const APolygon: IGeometryLonLatMultiPolygon;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double; inline;

    function CalcPolygonArea(
      const APolygon: IGeometryLonLatPolygon;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
  public
    constructor Create(const ADatum: IDatum);
  end;

  TGeoCalcChangeable = class(TChangeableWithSimpleLockBase, IGeoCalcChangeable)
  private
    FStatic: IGeoCalc;
    FGpsCalc: IGeoCalc;
  private
    { IGeoCalcChangeable }
    function GetDatum: IDatum;
    procedure SetDatum(const AValue: IDatum);

    function GetGpsDatum: IDatum;
    function GetGpsCalc: IGeoCalc;

    function GetStatic: IGeoCalc;
  public
    constructor Create(const AGpsDatum: IDatum);
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
  Assert(ADatum <> nil);
  inherited Create;
  FDatum := ADatum;
end;

function TGeoCalc.GetDatum: IDatum;
begin
  Result := FDatum;
end;

function TGeoCalc.IsSame(const AGeoCalc: IGeoCalc): Boolean;
begin
  Result := FDatum.IsSameDatum(AGeoCalc.Datum);
end;

{$REGION 'Distance'}
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

function TGeoCalc.CalcMultiLineLength(const ALine: IGeometryLonLatMultiLine): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ALine.Count - 1 do begin
    Result := Result + CalcSingleLineLength(ALine.Item[I]);
  end;
end;

function TGeoCalc.CalcLineLength(const ALine: IGeometryLonLatLine): Double;
var
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  if Supports(ALine, IGeometryLonLatMultiLine, VMultiLine) then begin
    Result := CalcMultiLineLength(VMultiLine);
  end else
  if Supports(ALine, IGeometryLonLatSingleLine, VSingleLine) then begin
    Result := CalcSingleLineLength(VSingleLine);
  end else begin
    Result := NaN;
  end;
end;
{$ENDREGION}

{$REGION 'Perimeter'}
function TGeoCalc.CalcContourPerimeter(const AContour: IGeometryLonLatContour): Double;
var
  VEnum: IEnumLonLatPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := AContour.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + FDatum.CalcDist(VPrevPoint, VCurrPoint);
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TGeoCalc.CalcSinglePolygonPerimeter(const APolygon: IGeometryLonLatSinglePolygon): Double;
var
  I: Integer;
begin
  Result := CalcContourPerimeter(APolygon.OuterBorder);
  for I := 0 to APolygon.HoleCount - 1 do begin
    Result := Result + CalcContourPerimeter(APolygon.HoleBorder[I]);
  end;
end;

function TGeoCalc.CalcMultiPolygonPerimeter(const APolygon: IGeometryLonLatMultiPolygon): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to APolygon.Count - 1 do begin
    Result := Result + CalcSinglePolygonPerimeter(APolygon.Item[I]);
  end;
end;

function TGeoCalc.CalcPolygonPerimeter(const APolygon: IGeometryLonLatPolygon): Double;
var
  VSingleLine: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiPolygon;
begin
  if Supports(APolygon, IGeometryLonLatMultiPolygon, VMultiLine) then begin
    Result := CalcMultiPolygonPerimeter(VMultiLine);
  end else
  if Supports(APolygon, IGeometryLonLatSinglePolygon, VSingleLine) then begin
    Result := CalcSinglePolygonPerimeter(VSingleLine);
  end else begin
    Result := NaN;
  end;
end;
{$ENDREGION}

{$REGION 'Area'}
function TGeoCalc.CalcContourArea(
  const AContour: IGeometryLonLatContour;
  const ANotifier: INotifierOperation;
  const AOperationID: Integer
): Double;
var
  VCount: Integer;
begin
  VCount := AContour.Count;
  if VCount < 3 then begin
    Result := 0;
  end else begin
    Result := FDatum.CalcPolygonArea(AContour.Points, VCount, ANotifier, AOperationID);
  end;
end;

function TGeoCalc.CalcSinglePolygonArea(
  const APolygon: IGeometryLonLatSinglePolygon;
  const ANotifier: INotifierOperation;
  const AOperationID: Integer
): Double;
var
  I: Integer;
begin
  Result := CalcContourArea(APolygon.OuterBorder, ANotifier, AOperationID);
  for I := 0 to APolygon.HoleCount - 1 do begin
    Result := Result - CalcContourArea(APolygon.HoleBorder[I], ANotifier, AOperationID);
  end;
end;

function TGeoCalc.CalcMultiPolygonArea(
  const APolygon: IGeometryLonLatMultiPolygon;
  const ANotifier: INotifierOperation;
  const AOperationID: Integer
): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to APolygon.Count - 1 do begin
    Result := Result + CalcSinglePolygonArea(APolygon.Item[I], ANotifier, AOperationID);
  end;
end;

function TGeoCalc.CalcPolygonArea(
  const APolygon: IGeometryLonLatPolygon;
  const ANotifier: INotifierOperation;
  const AOperationID: Integer
): Double;
var
  VSingleLine: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiPolygon;
begin
  if Supports(APolygon, IGeometryLonLatMultiPolygon, VMultiLine) then begin
    Result := CalcMultiPolygonArea(VMultiLine);
  end else
  if Supports(APolygon, IGeometryLonLatSinglePolygon, VSingleLine) then begin
    Result := CalcSinglePolygonArea(VSingleLine);
  end else begin
    Result := NaN;
  end;
end;
{$ENDREGION}

{ TGeoCalcChangeable }

constructor TGeoCalcChangeable.Create(const AGpsDatum: IDatum);
begin
  Assert(AGpsDatum <> nil);
  inherited Create;
  FGpsCalc := TGeoCalc.Create(AGpsDatum);
  FStatic := FGpsCalc;
end;

procedure TGeoCalcChangeable.SetDatum(const AValue: IDatum);
var
  VIsSame: Boolean;
begin
  CS.BeginWrite;
  try
    VIsSame := FStatic.Datum.IsSameDatum(AValue);
    if not VIsSame then begin
      FStatic := TGeoCalc.Create(AValue);
    end;
  finally
    CS.EndWrite;
  end;
  if not VIsSame then begin
    DoChangeNotify;
  end;
end;

function TGeoCalcChangeable.GetDatum: IDatum;
begin
  CS.BeginRead;
  try
    Result := FStatic.Datum;
  finally
    CS.EndRead;
  end;
end;

function TGeoCalcChangeable.GetGpsDatum: IDatum;
begin
  Result := FGpsCalc.Datum;
end;

function TGeoCalcChangeable.GetGpsCalc: IGeoCalc;
begin
  Result := FGpsCalc;
end;

function TGeoCalcChangeable.GetStatic: IGeoCalc;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

end.
