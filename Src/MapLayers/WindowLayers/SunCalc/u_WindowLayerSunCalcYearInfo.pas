{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_WindowLayerSunCalcYearInfo;

interface

uses
  GR32,
  u_WindowLayerSunCalcInfoBase;

type
  TWindowLayerSunCalcYearInfo = class(TWindowLayerSunCalcInfoBase)
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  GR32_Polygons,
  i_SunCalcConfig;

{ TWindowLayerSunCalcYearInfo }

procedure TWindowLayerSunCalcYearInfo.AfterConstruction;
begin
  inherited;
  FRepaintOnDayChange := False;
  FRepaintOnTimeChange := False;
  FRepaintOnLocationChange := True;
end;

procedure TWindowLayerSunCalcYearInfo.PaintLayer(ABuffer: TBitmap32);
var
  VCircle, VMaxAlt, VMinAlt, VPoly: TArrayOfFixedPoint;
begin
  if not FShapesGenerator.IsIntersectScreenRect then begin
    Exit;
  end;

  ABuffer.BeginUpdate;
  try
    FShapesGenerator.ValidateCache;

    // Background circle
    FShapesGenerator.GetCirclePoints(VCircle);
    if Length(VCircle) > 0 then begin
      PolylineXS(ABuffer, VCircle, FShapesColors.YearCircleColor, True);
    end;

    // Min/Max altitude
    FShapesGenerator.GetMinMaxAltitudePoints(VMinAlt, VMaxAlt, VPoly);

    if Length(VMinAlt) > 0 then begin
      PolylineXS(ABuffer, VMinAlt, FShapesColors.YearPolyLinesColor, False);
    end;

    if Length(VMaxAlt) > 0 then begin
      PolylineXS(ABuffer, VMaxAlt, FShapesColors.YearPolyLinesColor, False);
    end;

    // Draw transparent polygon betwen min and max altitude curves
    if Length(VPoly) > 0 then begin
      PolygonTS(ABuffer, VPoly, FShapesColors.YearPolygonFillColor);
    end;

  finally
    ABuffer.EndUpdate;
    ABuffer.Changed;
  end;
end;

end.
