{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_WindowLayerSunCalcDayInfo;

interface

uses
  GR32,
  u_WindowLayerSunCalcInfoBase;

type
  TWindowLayerSunCalcDayInfo = class(TWindowLayerSunCalcInfoBase)
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  u_SunCalcDrawTools;

{ TWindowLayerSunCalcDayInfo }

procedure TWindowLayerSunCalcDayInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  FRepaintOnDayChange := True;
  FRepaintOnTimeChange := False;
  FRepaintOnLocationChange := True;
end;

procedure TWindowLayerSunCalcDayInfo.PaintLayer(ABuffer: TBitmap32);
var
  VDayPoints: TArrayOfFixedPoint;
  VSunrise: TFixedPoint;
  VSunset: TFixedPoint;
  VCenter: TFixedPoint;
begin
  if not FShapesGenerator.IsIntersectScreenRect then begin
    Exit;
  end;

  ABuffer.BeginUpdate;
  try
    FShapesGenerator.ValidateCache;

    // Day info
    FShapesGenerator.GetDayInfoPoints(VDayPoints, VSunrise, VSunset, VCenter);

    // Draw day curve
    if Length(VDayPoints) > 0 then begin
      ThickPolyLine(ABuffer, VDayPoints, FShapesColors.DayPolyLineColor);
    end;

    // Draw sunrise and sunset lines
    if (VSunrise.X <> VSunset.X) and (VSunrise.Y <> VSunset.Y) then begin
      ThickLine(ABuffer, VCenter, VSunrise, FShapesColors.DaySunriseLineColor, 6);
      ThickLine(ABuffer, VCenter, VSunset, FShapesColors.DaySunsetLineColor, 6);
    end;
  finally
    ABuffer.EndUpdate;
    ABuffer.Changed;
  end;
end;

end.
