{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BitmapMarkerProviderSimpleArrow;

interface

uses
  Types,
  GR32,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig,
  u_BitmapMarkerProviderSimpleBase;

type
  TBitmapMarkerProviderSimpleArrow = class(TBitmapMarkerWithDirectionProviderSimpleBase)
  protected
    function CreateMarker(
      ASize: Integer;
      ADirection: Double
    ): IBitmapMarkerWithDirection; override;
  end;

implementation

uses
  GR32_Polygons,
  GR32_Transforms,
  t_GeoTypes,
  i_Bitmap32Static,
  u_Bitmap32Static,
  u_BitmapMarker;

{ TBitmapMarkerProviderSimpleArrow }

function TBitmapMarkerProviderSimpleArrow.CreateMarker(
  ASize: Integer;
  ADirection: Double
): IBitmapMarkerWithDirection;
var
  VConfig: IBitmapMarkerProviderSimpleConfigStatic;
  VBitmap: TCustomBitmap32;
  VSize: TPoint;
  VPolygon: TPolygon32;
  VCenterPoint: TDoublePoint;
  VTransform: TAffineTransformation;
  VBitmapStatic: IBitmap32Static;
begin
  VBitmap := TCustomBitmap32.Create;
  try
    VConfig := Config;
    VSize := Point(ASize, ASize);

    VCenterPoint.X := VSize.X / 2;
    VCenterPoint.Y := VSize.Y / 2;

    VBitmap.SetSize(VSize.Y, VSize.Y);
    VBitmap.Clear(0);
    VTransform := TAffineTransformation.Create;
    try
      VTransform.Rotate(VCenterPoint.X, VCenterPoint.Y, -ADirection);
      VPolygon := TPolygon32.Create;
      try
        VPolygon.Antialiased := true;
        VPolygon.AntialiasMode := am32times;
        VPolygon.Add(VTransform.Transform(FixedPoint(VCenterPoint.X, 0)));
        VPolygon.Add(VTransform.Transform(FixedPoint(VCenterPoint.X - VSize.X / 3, VSize.Y - 3)));
        VPolygon.Add(VTransform.Transform(FixedPoint(VCenterPoint.X + VSize.X / 3, VSize.Y - 3)));

        VPolygon.DrawFill(VBitmap, VConfig.MarkerColor);
        VPolygon.DrawEdge(VBitmap, VConfig.BorderColor);
      finally
        VPolygon.Free;
      end;
    finally
      VTransform.Free;
    end;
    VBitmapStatic := TBitmap32Static.CreateWithOwn(VBitmap);
    VBitmap := nil;
  finally
    VBitmap.Free;
  end;
  Result :=
    TBitmapMarkerWithDirection.Create(
      VBitmapStatic,
      VCenterPoint,
      ADirection
    );
end;

end.
