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
    FSource: IGeometryProjectedMultiPolygon;
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
      const ASource: IGeometryProjectedMultiPolygon;
      const AAntialiasMode: TAntialiasMode;
      const AColor: TColor32
    );
  end;

implementation

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  u_GeoFunc;

{ TProjectedDrawableElementByPolygonSimpleEdge }

constructor TProjectedDrawableElementByPolygonSimpleEdge.Create(
  const ASource: IGeometryProjectedMultiPolygon;
  const AAntialiasMode: TAntialiasMode;
  const AColor: TColor32
);
begin
  Assert(ASource <> nil);
  inherited Create;
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
  i: Integer;
  VLine: IGeometryProjectedPolygon;
  VPathFixedPoints: TArrayOfFixedPoint;
  VIndex: Integer;
  VEnum: IEnumProjectedPoint;
  VPoint: TDoublePoint;
  VLocalPoint: TDoublePoint;
  VIntersectRect: TDoubleRect;
begin
  if FSource.Count > 0 then begin
    VDrawRect := ALocalConverter.LocalRect2MapRectFloat(ABitmap.ClipRect);
    if IntersecProjectedRect(VIntersectRect, VDrawRect, FSource.Bounds) then begin
      if DoubleRectsEqual(VIntersectRect, FSource.Bounds) or FSource.IsRectIntersectBorder(VDrawRect) then begin
        VPolygon := TPolygon32.Create;
        try
          VPolygon.Closed := True;
          VPolygon.Antialiased := FAntialiasMode <> amNone;
          VPolygon.AntialiasMode := FAntialiasMode;

          for i := 0 to FSource.Count - 1 do begin
            VLine := FSource.Item[i];
            SetLength(VPathFixedPoints, VLine.Count + 1);
            VIndex := 0;
            VEnum := VLine.GetEnum;
            while VEnum.Next(VPoint) do begin
              VLocalPoint := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPoint);
              VPathFixedPoints[VIndex] := FixedPoint(VLocalPoint.X, VLocalPoint.Y);
              Inc(VIndex);
            end;
            VPolygon.AddPoints(VPathFixedPoints[0], VIndex);
            VPolygon.NewLine;
          end;
          VPolygon.DrawEdge(ABitmap, FColor);
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
  Result := FSource.Projection;
end;

end.
