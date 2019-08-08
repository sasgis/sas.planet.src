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
  t_GeoTypes,
  i_InterfaceListStatic,
  i_Projection,
  i_LocalCoordConverter,
  i_GeometryProjected,
  i_ProjectedDrawableElement,
  u_BaseInterfacedObject;

type
  TProjectedDrawableElementByPolygonSimpleEdge = class(TBaseInterfacedObject, IProjectedDrawableElement)
  private
    FProjection: IProjection;
    FSource: IGeometryProjectedPolygon;
    FColor: TColor32;
    FAntialiasMode: TAntialiasMode;
  private
    function GetProjection: IProjection;
    procedure Draw(
      ABitmap: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    );
  public
    constructor Create(
      const AProjection: IProjection;
      const ASource: IGeometryProjectedPolygon;
      const AAntialiasMode: TAntialiasMode;
      const AColor: TColor32
    );
  end;

  TDrawableBaseByPoints = class(TBaseInterfacedObject)
  private
    FPoints: TArrayOfArrayOfFixedPoint;
    FProjection: IProjection;

    FBaseRelativeRect: TDoubleRect;
    FCachedForLocalCoordConverter: ILocalCoordConverter;
    FCachedPoints: TArrayOfArrayOfFixedPoint;
  protected
    function GetProjection: IProjection;
    function PreparePoints(const ALocalCoordConverter: ILocalCoordConverter): TArrayOfArrayOfFixedPoint;
  public
    constructor Create(
      const AProjection: IProjection;
      const AMapPixelAtLocalZero: TPoint;
      APoints: TArrayOfArrayOfFixedPoint
    );
  end;

  TDrawableSimpleLine = class(TDrawableBaseByPoints, IProjectedDrawableElement)
  private
    FClosed: Boolean;
    FColor: TColor32;
  private
    procedure Draw(
      ABitmap: TCustomBitmap32;
      const ALocalCoordConverter: ILocalCoordConverter
    );
  public
    constructor Create(
      const AProjection: IProjection;
      const AMapPixelAtLocalZero: TPoint;
      APoints: TArrayOfArrayOfFixedPoint;
      const AClosed: Boolean;
      const AColor: TColor32
    );
  end;

  TDrawablePolygonFill = class(TDrawableBaseByPoints, IProjectedDrawableElement)
  private
    FColor: TColor32;
  private
    procedure Draw(
      ABitmap: TCustomBitmap32;
      const ALocalCoordConverter: ILocalCoordConverter
    );
  public
    constructor Create(
      const AProjection: IProjection;
      const AMapPixelAtLocalZero: TPoint;
      APoints: TArrayOfArrayOfFixedPoint;
      const AColor: TColor32
    );
  end;

  TDrawableByList = class(TBaseInterfacedObject, IProjectedDrawableElement)
  private
    FProjection: IProjection;
    FList: IInterfaceListStatic;
  private
    function GetProjection: IProjection;
    procedure Draw(
      ABitmap: TCustomBitmap32;
      const ALocalCoordConverter: ILocalCoordConverter
    );
  public
    constructor Create(
      const AProjection: IProjection;
      const AList: IInterfaceListStatic
    );
  end;

implementation

uses
  SysUtils,
  GR32_Math,
  u_GeometryFunc,
  u_GeoFunc;

{ TProjectedDrawableElementByPolygonSimpleEdge }

constructor TProjectedDrawableElementByPolygonSimpleEdge.Create(
  const AProjection: IProjection;
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
  i: integer;
  VProjectedMultiLine: IGeometryProjectedMultiPolygon;
  VProjectedSingleLine: IGeometryProjectedSinglePolygon;
begin
  VDrawRect := ALocalConverter.LocalRect2MapRectFloat(ABitmap.ClipRect);
  if IntersecProjectedRect(VIntersectRect, VDrawRect, FSource.Bounds) then begin
    if DoubleRectsEqual(VIntersectRect, FSource.Bounds) or FSource.IsRectIntersectBorder(VDrawRect) then begin
      VPolygon := TPolygon32.Create;
      try
        VPolygon.Closed := True;
        if Supports(FSource, IGeometryProjectedSinglePolygon, VProjectedSingleLine) then begin
          ProjectedPolygon2GR32Polygon(
            VProjectedSingleLine,
            ALocalConverter,
            am4times,
            VPathFixedPoints,
            VPolygon
          );
          if Assigned(VPolygon) then begin
            VPolygon.DrawEdge(ABitmap, FColor);
          end;
        end else if Supports(FSource, IGeometryProjectedMultiPolygon, VProjectedMultiLine) then begin
          for i := 0 to VProjectedMultiLine.Count - 1 do begin
            VProjectedSingleLine := VProjectedMultiLine.Item[i];
            ProjectedPolygon2GR32Polygon(
              VProjectedSingleLine,
              ALocalConverter,
              am4times,
              VPathFixedPoints,
              VPolygon
            );
            if Assigned(VPolygon) then begin
              VPolygon.DrawEdge(ABitmap, FColor);
            end;
          end;
        end;

        VPathFixedPoints := nil;
      finally
        VPolygon.Free;
      end;
    end;
  end;
end;

function TProjectedDrawableElementByPolygonSimpleEdge.GetProjection: IProjection;
begin
  Result := FProjection;
end;

// Scales to a polygon (TArrayOfFixedPoint)
function ScalePolygon(const Points: TArrayOfFixedPoint; ScaleX, ScaleY: TFixed): TArrayOfFixedPoint;
var
  I, L: Integer;
begin
  L := Length(Points);
  SetLength(Result, L);
  for I := 0 to L - 1 do
  begin
    Result[I].X := FixedMul(Points[I].X, ScaleX);
    Result[I].Y := FixedMul(Points[I].Y, ScaleY);
  end;
end;

// Scales all sub polygons in a complex polygon (TArrayOfArrayOfFixedPoint)
function ScalePolyPolygon(const Points: TArrayOfArrayOfFixedPoint;
  ScaleX, ScaleY: TFixed): TArrayOfArrayOfFixedPoint;
var
  I, L: Integer;
begin
  L := Length(Points);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := ScalePolygon(Points[I], ScaleX, ScaleY);
end;
// Translates a polygon (TArrayOfFixedPoint)
function TranslatePolygon(const Points: TArrayOfFixedPoint;
  OffsetX, OffsetY: TFixed): TArrayOfFixedPoint;
var
  I, Len: Integer;
begin
  Len := Length(Points);
  SetLength(Result, Len);
  for I := 0 to Len - 1 do
  begin
    Result[I].X := Points[I].X + OffsetX;
    Result[I].Y := Points[I].Y + OffsetY;
  end;
end;
// Translates all sub polygons in a complex polygon (TArrayOfArrayOfFixedPoint)
function TranslatePolyPolygon(const Points: TArrayOfArrayOfFixedPoint;
  OffsetX, OffsetY: TFixed): TArrayOfArrayOfFixedPoint;
var
  I, L: Integer;
begin
  L := Length(Points);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := TranslatePolygon(Points[I], OffsetX, OffsetY);
end;

{ TDrawableBaseByPoints }

constructor TDrawableBaseByPoints.Create(
  const AProjection: IProjection;
  const AMapPixelAtLocalZero: TPoint;
  APoints: TArrayOfArrayOfFixedPoint
);
begin
  Assert(Assigned(AProjection));
  inherited Create;
  FProjection := AProjection;
  FPoints := APoints;

  FBaseRelativeRect := FProjection.PixelRectFloat2RelativeRect(DoubleRect(AMapPixelAtLocalZero.X, AMapPixelAtLocalZero.Y, AMapPixelAtLocalZero.X + 1, AMapPixelAtLocalZero.Y + 1));
end;

function TDrawableBaseByPoints.GetProjection: IProjection;
begin
  Result := FProjection;
end;

function TDrawableBaseByPoints.PreparePoints(
  const ALocalCoordConverter: ILocalCoordConverter
): TArrayOfArrayOfFixedPoint;
var
  VTranslateDelta: TDoublePoint;
  VTargetRect: TDoubleRect;
  VScale: TDoublePoint;
begin
  Assert(Assigned(ALocalCoordConverter));
  Assert(ALocalCoordConverter.Projection.ProjectionType.IsSame(FProjection.ProjectionType));
  if not Assigned(ALocalCoordConverter) then Exit;
  if ALocalCoordConverter.GetIsSameConverter(FCachedForLocalCoordConverter) then begin
    Result := FCachedPoints;
    Exit;
  end;
  FCachedPoints := FPoints;
  VTargetRect := ALocalCoordConverter.MapRectFloat2LocalRectFloat(ALocalCoordConverter.Projection.RelativeRect2PixelRectFloat(FBaseRelativeRect));
  VScale := RectSize(VTargetRect);
  if (VScale.X <> 1.0) or (VScale.Y <> 1.0) then begin
    FCachedPoints := ScalePolyPolygon(FCachedPoints, Fixed(VScale.X), Fixed(VScale.Y));
  end;
  VTranslateDelta := VTargetRect.TopLeft;

  if (Abs(VTranslateDelta.X) > 0.0001) or (Abs(VTranslateDelta.Y) > 0.0001) then begin
    FCachedPoints := TranslatePolyPolygon(FCachedPoints, Fixed(VTranslateDelta.X), Fixed(VTranslateDelta.Y));
  end;
  FCachedForLocalCoordConverter := ALocalCoordConverter;
  Result := FCachedPoints;
end;

{ TDrawableSimpleLine }

constructor TDrawableSimpleLine.Create(
  const AProjection: IProjection;
  const AMapPixelAtLocalZero: TPoint;
  APoints: TArrayOfArrayOfFixedPoint;
  const AClosed: Boolean;
  const AColor: TColor32
);
begin
  inherited Create(AProjection, AMapPixelAtLocalZero, APoints);
  FClosed := AClosed;
  FColor := AColor;
end;

procedure TDrawableSimpleLine.Draw(
  ABitmap: TCustomBitmap32;
  const ALocalCoordConverter: ILocalCoordConverter
);
var
  VPoints: TArrayOfArrayOfFixedPoint;
begin
  VPoints := PreparePoints(ALocalCoordConverter);
  PolyPolylineXS(ABitmap, VPoints, FColor, FClosed);
end;

{ TDrawablePolygonFill }

constructor TDrawablePolygonFill.Create(
  const AProjection: IProjection;
  const AMapPixelAtLocalZero: TPoint;
  APoints: TArrayOfArrayOfFixedPoint;
  const AColor: TColor32
);
begin
  inherited Create(AProjection, AMapPixelAtLocalZero, APoints);
  FColor := AColor;
end;

procedure TDrawablePolygonFill.Draw(
  ABitmap: TCustomBitmap32;
  const ALocalCoordConverter: ILocalCoordConverter
);
var
  VPoints: TArrayOfArrayOfFixedPoint;
begin
  VPoints := PreparePoints(ALocalCoordConverter);
  PolyPolygonXS(ABitmap, VPoints, FColor, pfWinding, am4times);
end;

{ TDrawableByList }

constructor TDrawableByList.Create(
  const AProjection: IProjection;
  const AList: IInterfaceListStatic
);
begin
  Assert(Assigned(AProjection));
  Assert(Assigned(AList));
  Assert(AList.Count > 1);
  inherited Create;
  FProjection := AProjection;
  FList := AList;
end;

procedure TDrawableByList.Draw(
  ABitmap: TCustomBitmap32;
  const ALocalCoordConverter: ILocalCoordConverter
);
var
  i: Integer;
  VItem: IProjectedDrawableElement;
begin
  for i := 0 to FList.Count - 1 do begin
    VItem := IProjectedDrawableElement(FList.Items[i]);
    VItem.Draw(ABitmap, ALocalCoordConverter);
  end;
end;

function TDrawableByList.GetProjection: IProjection;
begin
  Result := FProjection;
end;

end.
