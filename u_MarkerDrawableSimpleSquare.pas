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

unit u_MarkerDrawableSimpleSquare;

interface

uses
  GR32,
  t_GeoTypes,
  u_MarkerDrawableSimpleAbstract;

type
  TMarkerDrawableSimpleSquare = class(TMarkerDrawableSimpleAbstract)
  protected
    function GetBoundsForPosition(const APosition: TDoublePoint): TRect; override;
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean; override;
  end;

implementation

uses
  Types,
  u_GeoFunc;


{ TMarkerDrawableSimpleSquare }

function  TMarkerDrawableSimpleSquare.DrawToBitmap(ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint): Boolean;
var
  VHalfSize: Double;
  VDoubleRect: TDoubleRect;
  VRect: TRect;
  VTargetRect: TRect;
begin
  VHalfSize := Config.MarkerSize / 2;
  VDoubleRect.Left := APosition.X - VHalfSize;
  VDoubleRect.Top := APosition.Y - VHalfSize;
  VDoubleRect.Right := APosition.X + VHalfSize;
  VDoubleRect.Bottom := APosition.Y + VHalfSize;
  VRect := RectFromDoubleRect(VDoubleRect, rrClosest);

  VTargetRect := VRect;
  Types.IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if Types.IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;

  if not ABitmap.MeasuringMode then begin
    ABitmap.FillRectTS(VRect, Config.MarkerColor);
    if Config.MarkerColor <> Config.BorderColor then begin
      ABitmap.FrameRectTS(VRect, Config.BorderColor);
    end;
  end else begin
    ABitmap.Changed(VTargetRect);
  end;
  Result := True;
end;

function TMarkerDrawableSimpleSquare.GetBoundsForPosition(
  const APosition: TDoublePoint): TRect;
var
  VHalfSize: Double;
  VTargetDoubleRect: TDoubleRect;
begin
  VHalfSize := Config.MarkerSize / 2;
  VTargetDoubleRect.Left := APosition.X - VHalfSize;
  VTargetDoubleRect.Top := APosition.Y - VHalfSize;
  VTargetDoubleRect.Right := APosition.X + VHalfSize;
  VTargetDoubleRect.Bottom := APosition.Y + VHalfSize;

  Result := RectFromDoubleRect(VTargetDoubleRect, rrOutside);
end;

end.
