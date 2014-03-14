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

unit u_MarkerDrawableSimpleArrow;

interface

uses
  GR32,
  t_GeoTypes,
  u_MarkerDrawableSimpleAbstract;

type
  TMarkerDrawableSimpleArrow = class(TMarkerDrawableWithDirectionSimpleAbstract)
  protected
    function DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint;
      const AAngle: Double
    ): Boolean; override;
  end;

implementation

uses
  Types,
  GR32_Polygons,
  GR32_Transforms,
  u_GeoFunc;

{ TMarkerDrawableSimpleArrow }

function TMarkerDrawableSimpleArrow.DrawToBitmapWithDirection(
  ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint;
  const AAngle: Double
): Boolean;
var
  VPolygon: TPolygon32;
  VTransform: TAffineTransformation;
  VHalfSize: Double;
  VWidth: Double;
  VTargetRect: TRect;
  VTargetDoubleRect: TDoubleRect;
begin
  VHalfSize := Config.MarkerSize / 2;
  VWidth := Config.MarkerSize / 3;

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
      VTransform := TAffineTransformation.Create;
      try
        VTransform.Rotate(APosition.X, APosition.Y, -AAngle);
        VPolygon := TPolygon32.Create;
        try
          VPolygon.Closed := True;
          VPolygon.Antialiased := true;
          VPolygon.AntialiasMode := am2times;
          VPolygon.Add(VTransform.Transform(FixedPoint(APosition.X, APosition.Y - VHalfSize)));
          VPolygon.Add(VTransform.Transform(FixedPoint(APosition.X - VWidth, APosition.Y + VHalfSize)));
          VPolygon.Add(VTransform.Transform(FixedPoint(APosition.X + VWidth, APosition.Y + VHalfSize)));

          VPolygon.DrawFill(ABitmap, Config.MarkerColor);
          VPolygon.DrawEdge(ABitmap, Config.BorderColor);
        finally
          VPolygon.Free;
        end;
      finally
        VTransform.Free;
      end;
    finally
      ABitmap.EndUpdate;
    end;
  end;
  ABitmap.Changed(VTargetRect);
  Result := True;
end;

end.
