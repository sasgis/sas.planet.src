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

unit u_WindowLayerSunCalcTimeInfo;

interface

uses
  GR32,
  u_WindowLayerSunCalcInfoBase;

type
  TWindowLayerSunCalcTimeInfo = class(TWindowLayerSunCalcInfoBase)
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  u_SunCalcDrawTools;

{ TWindowLayerSunCalcTimeInfo }

procedure TWindowLayerSunCalcTimeInfo.AfterConstruction;
begin
  inherited;
  FRepaintOnDayChange := True;
  FRepaintOnTimeChange := True;
  FRepaintOnLocationChange := True;
end;

procedure TWindowLayerSunCalcTimeInfo.PaintLayer(ABuffer: TBitmap32);
var
  VSunPos: TFixedPoint;
  VCenter: TFixedPoint;
begin
  if not FShapesGenerator.IsIntersectScreenRect then begin
    Exit;
  end;

  ABuffer.BeginUpdate;
  try
    FShapesGenerator.ValidateCache;

    // Current time info
    FShapesGenerator.GetTimeInfoPoints(VSunPos, VCenter);

    // Draw sun line
    if (VSunPos.X > 0) and (VSunPos.Y > 0) then begin
      ThickLine(ABuffer, VCenter, VSunPos, FShapesColors.DayLineColor, 6);
    end;
  finally
    ABuffer.EndUpdate;
    ABuffer.Changed;
  end;
end;

end.
