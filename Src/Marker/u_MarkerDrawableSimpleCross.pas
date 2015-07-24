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
  VPolygon: TPolygon32;
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
      VPolygon := TPolygon32.Create;
      try
        VPolygon.Closed := True;
        VPolygon.Antialiased := true;
        VPolygon.AntialiasMode := am2times;
        VPolygon.Add(FixedPoint(APosition.X - VCrossHalfWidth, APosition.Y - VHalfSize));
        VPolygon.Add(FixedPoint(APosition.X + VCrossHalfWidth, APosition.Y - VHalfSize));
        VPolygon.Add(FixedPoint(APosition.X + VCrossHalfWidth, APosition.Y - VCrossHalfWidth));
        VPolygon.Add(FixedPoint(APosition.X + VHalfSize, APosition.Y - VCrossHalfWidth));
        VPolygon.Add(FixedPoint(APosition.X + VHalfSize, APosition.Y + VCrossHalfWidth));
        VPolygon.Add(FixedPoint(APosition.X + VCrossHalfWidth, APosition.Y + VCrossHalfWidth));
        VPolygon.Add(FixedPoint(APosition.X + VCrossHalfWidth, APosition.Y + VHalfSize));
        VPolygon.Add(FixedPoint(APosition.X - VCrossHalfWidth, APosition.Y + VHalfSize));
        VPolygon.Add(FixedPoint(APosition.X - VCrossHalfWidth, APosition.Y + VCrossHalfWidth));
        VPolygon.Add(FixedPoint(APosition.X - VHalfSize, APosition.Y + VCrossHalfWidth));
        VPolygon.Add(FixedPoint(APosition.X - VHalfSize, APosition.Y - VCrossHalfWidth));
        VPolygon.Add(FixedPoint(APosition.X - VCrossHalfWidth, APosition.Y - VCrossHalfWidth));

        VPolygon.DrawFill(ABitmap, Config.MarkerColor);
        VPolygon.DrawEdge(ABitmap, Config.BorderColor);
      finally
        VPolygon.Free;
      end;
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
