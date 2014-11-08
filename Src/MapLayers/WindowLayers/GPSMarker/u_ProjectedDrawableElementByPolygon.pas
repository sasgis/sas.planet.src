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

unit u_ProjectedDrawableElementByPolygon;

interface

uses
  GR32,
  GR32_Polygons,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_GeometryProjected,
  i_ProjectedDrawableElement,
  u_BaseInterfacedObject;

type
  TProjectedDrawableElementByPolygonSimpleEdge = class(TBaseInterfacedObject, IProjectedDrawableElement)
  private
    FProjection: IProjectionInfo;
    FSource: IGeometryProjectedPolygon;
    FColor: TColor32;
    FAntialiasMode: TAntialiasMode;
  private
    function GetProjectionInfo: IProjectionInfo;
    procedure Draw(
      ABitmap: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    );
  public
    constructor Create(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryProjectedPolygon;
      const AAntialiasMode: TAntialiasMode;
      const AColor: TColor32
    );
  end;

implementation

uses
  t_GeoTypes,
  u_GeometryFunc,
  u_GeoFunc;

{ TProjectedDrawableElementByPolygonSimpleEdge }

constructor TProjectedDrawableElementByPolygonSimpleEdge.Create(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryProjectedPolygon;
  const AAntialiasMode: TAntialiasMode;
  const AColor: TColor32
);
begin
  Assert(ASource <> nil);
  inherited Create;
  FProjection := AProjection;
  FSource := ASource;
  FAntialiasMode := AAntialiasMode;
  FColor := AColor;
end;

procedure TProjectedDrawableElementByPolygonSimpleEdge.Draw(
  ABitmap: TCustomBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VDrawRect: TDoubleRect;
  VPolygon: TPolygon32;
  VPathFixedPoints: TArrayOfFixedPoint;
  VIntersectRect: TDoubleRect;
begin

  if not FSource.IsEmpty then begin
    VDrawRect := ALocalConverter.LocalRect2MapRectFloat(ABitmap.ClipRect);
    if IntersecProjectedRect(VIntersectRect, VDrawRect, FSource.Bounds) then begin
      if DoubleRectsEqual(VIntersectRect, FSource.Bounds) or FSource.IsRectIntersectBorder(VDrawRect) then begin
        VPolygon := TPolygon32.Create;
        try
          VPolygon.Closed := True;

          ProjectedPolygon2GR32Polygon(
            FSource,
            ALocalConverter,
            am4times,
            VPathFixedPoints,
            VPolygon
          );
          if Assigned(VPolygon) then begin
            VPolygon.DrawEdge(ABitmap, FColor);
          end;
          VPathFixedPoints := nil;
        finally
          VPolygon.Free;
        end;
      end;
    end;
  end;
end;

function TProjectedDrawableElementByPolygonSimpleEdge.GetProjectionInfo: IProjectionInfo;
begin
  Result := FProjection;
end;

end.
