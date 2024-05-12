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

unit u_WindowLayerSunCalcDayInfo;

interface

uses
  Types,
  GR32,
  u_WindowLayerSunCalcInfoBase;

type
  TWindowLayerSunCalcDayInfo = class(TWindowLayerSunCalcInfoBase)
  private
    FIsPointsValid: Boolean;
    FDayPoints: TArrayOfArrayOfFloatPoint;
    FRisePoint: TFloatPoint;
    FSetPoint: TFloatPoint;
    FCenterPoint: TFloatPoint;
  protected
    procedure InvalidateLayer; override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  u_GR32Func;

const
  CLineWidth = 4;

{ TWindowLayerSunCalcDayInfo }

procedure TWindowLayerSunCalcDayInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  FRepaintOnDayChange := True;
  FRepaintOnTimeChange := False;
  FRepaintOnLocationChange := True;
end;

procedure TWindowLayerSunCalcDayInfo.InvalidateLayer;
var
  VRect: TRect;
  VRectF: TFloatRect;
begin
  FIsPointsValid := Visible and FShapesGenerator.IsIntersectScreenRect;

  if FIsPointsValid then begin
    FShapesGenerator.ValidateCache;
    FShapesGenerator.GetDayInfoPoints(FDayPoints, FRisePoint, FSetPoint, FCenterPoint);

    VRectF := Rect(0, 0, 0, 0);

    UpdateRectByFloatPoint(VRectF, FRisePoint);
    UpdateRectByFloatPoint(VRectF, FSetPoint);
    UpdateRectByFloatPoint(VRectF, FCenterPoint);
    UpdateRectByArrayOfArrayOfFloatPoint(VRectF, FDayPoints);

    VRect := MakeRect(VRectF);
    GR32.InflateRect(VRect, CLineWidth, CLineWidth);

    DoInvalidateRect(VRect);
  end;
end;

procedure TWindowLayerSunCalcDayInfo.PaintLayer(ABuffer: TBitmap32);
var
  I: Integer;
begin
  if not FIsPointsValid then begin
    Exit;
  end;

  ABuffer.BeginUpdate;
  try
    // Draw day curve
    for I := 0 to Length(FDayPoints) - 1 do begin
      DrawThickPolyLine(ABuffer, FDayPoints[I], FShapesColors.DayPolyLineColor, CLineWidth);
    end;

    // Draw sun rise line
    if FRisePoint.X > 0 then begin
      DrawThickLine(ABuffer, FCenterPoint, FRisePoint, FShapesColors.DaySunriseLineColor, CLineWidth);
    end;

    // Draw sun set line
    if FSetPoint.X > 0 then begin
      DrawThickLine(ABuffer, FCenterPoint, FSetPoint, FShapesColors.DaySunsetLineColor, CLineWidth);
    end;
  finally
    ABuffer.EndUpdate;
  end;
end;

end.
