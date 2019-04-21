{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_VectorTileRenderer;

interface

uses
  Types,
  SysUtils,
  GR32,
  t_GeoTypes,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_Appearance,
  i_MarkerProviderByAppearancePointIcon,
  i_GeometryLonLat,
  i_GeometryProjectedProvider,
  i_NotifierOperation,
  i_BitmapMarker,
  i_GeometryProjected,
  i_Projection,
  i_VectorTileRenderer,
  u_BaseInterfacedObject;

type
  TVectorTileRenderer = class(TBaseInterfacedObject, IVectorTileRenderer)
  private
    FColorMain: TColor32;
    FColorBG: TColor32;
    FPointMarker: IBitmapMarker;
    FMarkerIconProvider: IMarkerProviderByAppearancePointIcon;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FProjectedCache: IGeometryProjectedProvider;

    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ASize: TPoint
    );
    function GetMarkerBoundsForPosition(
      const AMarker: IBitmapMarker;
      const APosition: TDoublePoint
    ): TRect;
    function DrawMarkerToBitmap(
      ABitmap: TCustomBitmap32;
      const AMarker: IBitmapMarker;
      const APosition: TDoublePoint
    ): Boolean;

    function DrawPoint(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const APoint: IGeometryLonLatPoint;
      const AProjection: IProjection;
      const AMapRect: TRect;
      const AAppearance: IAppearance
    ): Boolean;
    function DrawPath(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const ALine: IGeometryLonLatLine;
      const AProjection: IProjection;
      const AMapRect: TRect;
      const AAppearance: IAppearance;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
    function DrawSinglePolygon(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const APoly: IGeometryProjectedSinglePolygon;
      const AMapRect: TRect;
      const AAppearance: IAppearance;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
    function DrawPoly(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const APoly: IGeometryLonLatPolygon;
      const AProjection: IProjection;
      const AMapRect: TRect;
      const AAppearance: IAppearance;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
    function DrawWikiElement(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AData: IGeometryLonLat;
      const AProjection: IProjection;
      const AMapRect: TRect;
      const AAppearance: IAppearance;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
  private
    function RenderVectorTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjection: IProjection;
      const ATile: TPoint;
      const ASource: IVectorItemSubset
    ): IBitmap32Static;
  public
    constructor Create(
      AColorMain: TColor32;
      AColorBG: TColor32;
      const APointMarker: IBitmapMarker;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProjectedCache: IGeometryProjectedProvider;
      const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
    );
  end;

implementation

uses
  Math,
  GR32_Polygons,
  i_AppearanceOfVectorItem,
  u_Bitmap32ByStaticBitmap,
  u_GeoFunc,
  u_BitmapFunc,
  u_GeometryFunc;

{ TVectorTileRenderer }

constructor TVectorTileRenderer.Create(
  AColorMain: TColor32;
  AColorBG: TColor32;
  const APointMarker: IBitmapMarker;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProjectedCache: IGeometryProjectedProvider;
  const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
);
begin
  Assert(Assigned(APointMarker));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AProjectedCache));
  Assert(Assigned(AMarkerIconProvider));
  inherited Create;
  FColorMain := AColorMain;
  FColorBG := AColorBG;
  FPointMarker := APointMarker;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProjectedCache := AProjectedCache;
  FMarkerIconProvider := AMarkerIconProvider;
end;

function TVectorTileRenderer.DrawPath(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const ALine: IGeometryLonLatLine;
  const AProjection: IProjection;
  const AMapRect: TRect;
  const AAppearance: IAppearance;
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VPolygon: TPolygon32;
  VProjected: IGeometryProjectedLine;
  VAppearanceLine: IAppearanceLine;
begin
  Result := False;
  VPolygon := nil;
  VProjected := FProjectedCache.GetProjectedPath(AProjection, ALine);
  ProjectedLine2GR32Polygon(
    VProjected,
    AMapRect,
    am4times,
    AFixedPointArray,
    VPolygon
  );
  try
    if Assigned(VPolygon) then begin
      if not ABitmapInited then begin
        InitBitmap(ATargetBmp, Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top));
        ABitmapInited := True;
      end;

      if Supports(AAppearance, IAppearanceLine, VAppearanceLine) then begin
        with VPolygon.Outline do begin
          try
            with Grow(GR32.Fixed(VAppearanceLine.LineWidth / 2), 0.5) do begin
              try
                FillMode := pfWinding;
                DrawFill(ATargetBmp, VAppearanceLine.LineColor);
              finally
                free;
              end;
            end;
          finally
            free;
          end;
        end;
      end else begin
        with VPolygon.Outline do begin
          try
            with Grow(GR32.Fixed(0.5), 0.5) do begin
              try
                FillMode := pfWinding;
                DrawFill(ATargetBmp, FColorBG);
              finally
                free;
              end;
            end;
          finally
            free;
          end;
        end;
        VPolygon.DrawEdge(ATargetBmp, FColorMain);
      end;

      Result := True;
    end;
  finally
    VPolygon.Free;
  end;
end;

function TVectorTileRenderer.GetMarkerBoundsForPosition(
  const AMarker: IBitmapMarker;
  const APosition: TDoublePoint
): TRect;
var
  VTargetPoint: TPoint;
  VTargetPointFloat: TDoublePoint;
  VSourceSize: TPoint;
begin
  VTargetPointFloat :=
    DoublePoint(
      APosition.X - AMarker.AnchorPoint.X,
      APosition.Y - AMarker.AnchorPoint.Y
    );
  VSourceSize := AMarker.Size;
  VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);

  Result.TopLeft := VTargetPoint;
  Result.Right := Result.Left + VSourceSize.X;
  Result.Bottom := Result.Top + VSourceSize.Y;
end;

function TVectorTileRenderer.DrawMarkerToBitmap(
  ABitmap: TCustomBitmap32;
  const AMarker: IBitmapMarker;
  const APosition: TDoublePoint
): Boolean;
var
  VTargetPoint: TPoint;
  VTargetRect: TRect;
begin
  VTargetRect := GetMarkerBoundsForPosition(AMarker, APosition);
  VTargetPoint := VTargetRect.TopLeft;
  Types.IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if Types.IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;

  BlockTransferFull(
    ABitmap,
    VTargetPoint.X, VTargetPoint.Y,
    AMarker,
    dmBlend,
    ABitmap.CombineMode
  );
  Result := True;
end;

function TVectorTileRenderer.DrawPoint(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const APoint: IGeometryLonLatPoint;
  const AProjection: IProjection;
  const AMapRect: TRect;
  const AAppearance: IAppearance
): Boolean;
var
  VPointLL: TDoublePoint;
  VMapPixelPos: TDoublePoint;
  VLocalPos: TDoublePoint;
  VRect: TRect;
  VDrawMarker: IBitmapMarker;
  VAppearanceIcon: IAppearancePointIcon;
begin
  Result := False;
  VPointLL := APoint.Point;
  AProjection.ProjectionType.ValidateLonLatPos(VPointLL);
  VMapPixelPos := AProjection.LonLat2PixelPosFloat(VPointLL);
  VLocalPos.X := VMapPixelPos.X - AMapRect.Left;
  VLocalPos.Y := VMapPixelPos.Y - AMapRect.Top;
  if Supports(AAppearance, IAppearancePointIcon, VAppearanceIcon) then begin
    VDrawMarker := FMarkerIconProvider.GetMarker(VAppearanceIcon);
  end;
  if not Assigned(VDrawMarker) then begin
    VDrawMarker := FPointMarker;
  end;

  VRect := GetMarkerBoundsForPosition(VDrawMarker, VLocalPos);
  if Types.IntersectRect(VRect, Rect(0, 0, AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top), VRect) then begin
    if not ABitmapInited then begin
      InitBitmap(ATargetBmp, Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top));
      ABitmapInited := True;
    end;

    Result := DrawMarkerToBitmap(ATargetBmp, VDrawMarker, VLocalPos);
  end;
end;

function TVectorTileRenderer.DrawPoly(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const APoly: IGeometryLonLatPolygon;
  const AProjection: IProjection;
  const AMapRect: TRect;
  const AAppearance: IAppearance;
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VProjected: IGeometryProjectedPolygon;
  VProjectedSingle: IGeometryProjectedSinglePolygon;
  VProjectedMulti: IGeometryProjectedMultiPolygon;
  i: Integer;
begin
  Result := False;
  VProjected := FProjectedCache.GetProjectedPolygon(AProjection, APoly);
  if Assigned(VProjected) then begin
    if Supports(VProjected, IGeometryProjectedSinglePolygon, VProjectedSingle) then begin
      Result := DrawSinglePolygon(ABitmapInited, ATargetBmp, VProjectedSingle, AMapRect, AAppearance, AFixedPointArray);
    end else if Supports(VProjected, IGeometryProjectedMultiPolygon, VProjectedMulti) then begin
      for i := 0 to VProjectedMulti.Count - 1 do begin
        VProjectedSingle := VProjectedMulti.Item[i];
        if DrawSinglePolygon(ABitmapInited, ATargetBmp, VProjectedSingle, AMapRect, AAppearance, AFixedPointArray) then begin
          Result := True;
        end;
      end;
    end else begin
      Assert(False);
    end;
  end;
end;

function TVectorTileRenderer.DrawSinglePolygon(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const APoly: IGeometryProjectedSinglePolygon;
  const AMapRect: TRect;
  const AAppearance: IAppearance;
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VPolygon: TPolygon32;
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
begin
  Result := False;
  VPolygon := nil;
  try
    ProjectedPolygon2GR32Polygon(
      APoly,
      AMapRect,
      am4times,
      AFixedPointArray,
      VPolygon
    );
    if VPolygon <> nil then begin
      if not ABitmapInited then begin
        InitBitmap(ATargetBmp, Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top));
        ABitmapInited := True;
      end;
      if Assigned(AAppearance) then begin
        if Supports(AAppearance, IAppearancePolygonFill, VAppearanceFill) then begin
          VPolygon.DrawFill(ATargetBmp, VAppearanceFill.FillColor);
        end;
        if Supports(AAppearance, IAppearancePolygonBorder, VAppearanceBorder) then begin
          with VPolygon.Outline do begin
            try
              with Grow(GR32.Fixed(VAppearanceBorder.LineWidth / 2), 0.5) do begin
                try
                  FillMode := pfWinding;
                  DrawFill(ATargetBmp, VAppearanceBorder.LineColor);
                finally
                  free;
                end;
              end;
            finally
              free;
            end;
          end;
        end;
      end else begin
        with VPolygon.Outline do begin
          try
            with Grow(GR32.Fixed(0.5), 0.5) do begin
              try
                FillMode := pfWinding;
                DrawFill(ATargetBmp, FColorBG);
              finally
                free;
              end;
            end;
          finally
            free;
          end;
        end;
        VPolygon.DrawEdge(ATargetBmp, FColorMain);
      end;
      Result := True;
    end;
  finally
    VPolygon.Free;
  end;
end;

function TVectorTileRenderer.DrawWikiElement(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const AData: IGeometryLonLat;
  const AProjection: IProjection;
  const AMapRect: TRect;
  const AAppearance: IAppearance;
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VItemPoint: IGeometryLonLatPoint;
  VItemLine: IGeometryLonLatLine;
  VItemPoly: IGeometryLonLatPolygon;
begin
  if Supports(AData, IGeometryLonLatPoint, VItemPoint) then begin
    Result := DrawPoint(ABitmapInited, ATargetBmp, VItemPoint, AProjection, AMapRect, AAppearance);
  end else if Supports(AData, IGeometryLonLatLine, VItemLine) then begin
    Result := DrawPath(ABitmapInited, ATargetBmp, VItemLine, AProjection, AMapRect, AAppearance, AFixedPointArray);
  end else if Supports(AData, IGeometryLonLatPolygon, VItemPoly) then begin
    Result := DrawPoly(ABitmapInited, ATargetBmp, VItemPoly, AProjection, AMapRect, AAppearance, AFixedPointArray);
  end else begin
    Result := False;
  end;
end;

procedure TVectorTileRenderer.InitBitmap(
  ATargetBmp: TCustomBitmap32;
  const ASize: TPoint
);
begin
  ATargetBmp.SetSize(ASize.X, ASize.Y);
  ATargetBmp.Clear(0);
  ATargetBmp.CombineMode := cmMerge;
end;

function TVectorTileRenderer.RenderVectorTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjection: IProjection;
  const ATile: TPoint;
  const ASource: IVectorItemSubset
): IBitmap32Static;
var
  i: Integer;
  VItem: IVectorDataItem;
  VMapPixelRect: TRect;
  VBitmapInited: Boolean;
  VBitmap: TBitmap32ByStaticBitmap;
  VIsEmpty: Boolean;
  VFixedPointArray: TArrayOfFixedPoint;
begin
  Result := nil;
  if not AProjection.CheckTilePosStrict(ATile) then begin
    Exit;
  end;
  VMapPixelRect := AProjection.TilePos2PixelRect(ATile);

  VBitmapInited := False;
  if (ASource <> nil) and (ASource.Count > 0) then begin
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      VIsEmpty := True;
      for i := 0 to ASource.Count - 1 do begin
        VItem := ASource.Items[i];
        if DrawWikiElement(VBitmapInited, VBitmap, VItem.Geometry, AProjection, VMapPixelRect, VItem.Appearance, VFixedPointArray) then begin
          VIsEmpty := False;
        end;
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
      end;
      if not VIsEmpty then begin
        Result := VBitmap.MakeAndClear;
      end;
    finally
      VBitmap.Free;
    end;
  end;
end;

end.
