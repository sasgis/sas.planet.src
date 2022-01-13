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
      const AColor: TColor32
    );
  end;

  TDrawableBaseByPoints = class(TBaseInterfacedObject)
  private
    FPoints: TArrayOfArrayOfFloatPoint;
    FProjection: IProjection;

    FBaseRelativeRect: TDoubleRect;
    FCachedForLocalCoordConverter: ILocalCoordConverter;
    FCachedPoints: TArrayOfArrayOfFloatPoint;
  protected
    function GetProjection: IProjection;
    function PreparePoints(const ALocalCoordConverter: ILocalCoordConverter): TArrayOfArrayOfFloatPoint;
  public
    constructor Create(
      const AProjection: IProjection;
      const AMapPixelAtLocalZero: TPoint;
      APoints: TArrayOfArrayOfFloatPoint
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
      APoints: TArrayOfArrayOfFloatPoint;
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
      APoints: TArrayOfArrayOfFloatPoint;
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
  GR32_VectorUtils,
  u_GeometryFunc,
  u_GeoFunc;

{ TProjectedDrawableElementByPolygonSimpleEdge }

constructor TProjectedDrawableElementByPolygonSimpleEdge.Create(
  const AProjection: IProjection;
  const ASource: IGeometryProjectedPolygon;
  const AColor: TColor32
);
begin
  Assert(ASource <> nil);
  inherited Create;
  FProjection := AProjection;
  FSource := ASource;
  FColor := AColor;
end;

procedure TProjectedDrawableElementByPolygonSimpleEdge.Draw(
  ABitmap: TCustomBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VDrawRect: TDoubleRect;
  VPolygon: TArrayOfArrayOfFloatPoint;
  VPathPoints: TArrayOfFloatPoint;
  VIntersectRect: TDoubleRect;
  i: integer;
  VProjectedMultiLine: IGeometryProjectedMultiPolygon;
  VProjectedSingleLine: IGeometryProjectedSinglePolygon;
begin
  VDrawRect := ALocalConverter.LocalRect2MapRectFloat(ABitmap.ClipRect);
  if IntersecProjectedRect(VIntersectRect, VDrawRect, FSource.Bounds) then begin
    if DoubleRectsEqual(VIntersectRect, FSource.Bounds) or FSource.IsRectIntersectBorder(VDrawRect) then begin
      if Supports(FSource, IGeometryProjectedSinglePolygon, VProjectedSingleLine) then begin
        VPolygon := ProjectedPolygon2ArrayOfArray(VProjectedSingleLine, ALocalConverter.GetRectInMapPixel, VPathPoints);
        if Assigned(VPolygon) then begin
          PolyPolylineFS(ABitmap, VPolygon, FColor, True);
        end;
      end else if Supports(FSource, IGeometryProjectedMultiPolygon, VProjectedMultiLine) then begin
        for i := 0 to VProjectedMultiLine.Count - 1 do begin
          VProjectedSingleLine := VProjectedMultiLine.Item[i];
          VPolygon := ProjectedPolygon2ArrayOfArray(VProjectedSingleLine, ALocalConverter.GetRectInMapPixel, VPathPoints);
          if Assigned(VPolygon) then begin
            PolyPolylineFS(ABitmap, VPolygon, FColor, True);
          end;
        end;
      end;
    end;
  end;
end;

function TProjectedDrawableElementByPolygonSimpleEdge.GetProjection: IProjection;
begin
  Result := FProjection;
end;

{ TDrawableBaseByPoints }

constructor TDrawableBaseByPoints.Create(
  const AProjection: IProjection;
  const AMapPixelAtLocalZero: TPoint;
  APoints: TArrayOfArrayOfFloatPoint
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
): TArrayOfArrayOfFloatPoint;
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
    FCachedPoints := ScalePolyPolygon(FCachedPoints, VScale.X, VScale.Y);
  end;
  VTranslateDelta := VTargetRect.TopLeft;

  if (Abs(VTranslateDelta.X) > 0.0001) or (Abs(VTranslateDelta.Y) > 0.0001) then begin
    FCachedPoints := TranslatePolyPolygon(FCachedPoints, VTranslateDelta.X, VTranslateDelta.Y);
  end;
  FCachedForLocalCoordConverter := ALocalCoordConverter;
  Result := FCachedPoints;
end;

{ TDrawableSimpleLine }

constructor TDrawableSimpleLine.Create(
  const AProjection: IProjection;
  const AMapPixelAtLocalZero: TPoint;
  APoints: TArrayOfArrayOfFloatPoint;
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
  VPoints: TArrayOfArrayOfFloatPoint;
begin
  VPoints := PreparePoints(ALocalCoordConverter);
  PolyPolylineFS(ABitmap, VPoints, FColor, FClosed);
end;

{ TDrawablePolygonFill }

constructor TDrawablePolygonFill.Create(
  const AProjection: IProjection;
  const AMapPixelAtLocalZero: TPoint;
  APoints: TArrayOfArrayOfFloatPoint;
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
  VPoints: TArrayOfArrayOfFloatPoint;
begin
  VPoints := PreparePoints(ALocalCoordConverter);
  PolyPolygonFS(ABitmap, VPoints, FColor, pfWinding);
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
