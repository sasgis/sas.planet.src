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

unit u_MarkerDrawableSimpleCross;

interface

uses
  GR32,
  t_GeoTypes,
  u_MarkerDrawableSimpleAbstract;

type
  TMarkerDrawableSimpleCross = class(TMarkerDrawableSimpleAbstract)
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
  Math,
  GR32_Polygons,
  u_GeoFunc;

{ TMarkerDrawableSimpleCross }

function TMarkerDrawableSimpleCross.DrawToBitmap(
  ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint
): Boolean;
var
  VCrossHalfWidth: Double;
  VHalfSize: Double;
  VPolygon: TArrayOfFloatPoint;
  VTargetRect: TRect;
  VTargetDoubleRect: TDoubleRect;
begin
  VCrossHalfWidth := Config.MarkerSize / 10;
  VHalfSize := Config.MarkerSize / 2;
  VTargetDoubleRect.Left := APosition.X - VHalfSize;
  VTargetDoubleRect.Top := APosition.Y - VHalfSize;
  VTargetDoubleRect.Right := APosition.X + VHalfSize;
  VTargetDoubleRect.Bottom := APosition.Y + VHalfSize;

  VTargetRect := RectFromDoubleRect(VTargetDoubleRect, rrOutside);
  Types.IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if Types.IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;
  if not ABitmap.MeasuringMode then begin
    ABitmap.BeginUpdate;
    try
      SetLength(VPolygon, 12);
      VPolygon[0] := FloatPoint(APosition.X - VCrossHalfWidth, APosition.Y - VHalfSize);
      VPolygon[1] := FloatPoint(APosition.X + VCrossHalfWidth, APosition.Y - VHalfSize);
      VPolygon[2] := FloatPoint(APosition.X + VCrossHalfWidth, APosition.Y - VCrossHalfWidth);
      VPolygon[3] := FloatPoint(APosition.X + VHalfSize, APosition.Y - VCrossHalfWidth);
      VPolygon[4] := FloatPoint(APosition.X + VHalfSize, APosition.Y + VCrossHalfWidth);
      VPolygon[5] := FloatPoint(APosition.X + VCrossHalfWidth, APosition.Y + VCrossHalfWidth);
      VPolygon[6] := FloatPoint(APosition.X + VCrossHalfWidth, APosition.Y + VHalfSize);
      VPolygon[7] := FloatPoint(APosition.X - VCrossHalfWidth, APosition.Y + VHalfSize);
      VPolygon[8] := FloatPoint(APosition.X - VCrossHalfWidth, APosition.Y + VCrossHalfWidth);
      VPolygon[9] := FloatPoint(APosition.X - VHalfSize, APosition.Y + VCrossHalfWidth);
      VPolygon[10] := FloatPoint(APosition.X - VHalfSize, APosition.Y - VCrossHalfWidth);
      VPolygon[11] := FloatPoint(APosition.X - VCrossHalfWidth, APosition.Y - VCrossHalfWidth);
      PolygonFS(ABitmap, VPolygon, Config.MarkerColor);
      PolylineFS(ABitmap, VPolygon, Config.BorderColor, True);
    finally
      ABitmap.EndUpdate;
    end;
  end;
  ABitmap.Changed(VTargetRect);
  Result := True;
end;

function TMarkerDrawableSimpleCross.GetBoundsForPosition(
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
