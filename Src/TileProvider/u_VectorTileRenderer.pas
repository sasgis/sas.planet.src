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
  i_GeometryLonLat,
  i_GeometryProjectedProvider,
  i_NotifierOperation,
  i_MarkerDrawable,
  i_GeometryProjected,
  i_ProjectionInfo,
  i_VectorTileRenderer,
  u_BaseInterfacedObject;

type
  TVectorTileRenderer = class(TBaseInterfacedObject, IVectorTileRenderer)
  private
    FColorMain: TColor32;
    FColorBG: TColor32;
    FPointMarker: IMarkerDrawable;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FProjectedCache: IGeometryProjectedProvider;

    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ASize: TPoint
    );
    function DrawPoint(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const APoint: IGeometryLonLatPoint;
      const AProjectionInfo: IProjectionInfo;
      const AMapRect: TRect
    ): Boolean;
    function DrawPath(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const ALine: IGeometryLonLatLine;
      const AProjectionInfo: IProjectionInfo;
      const AMapRect: TRect;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
    function DrawSinglePolygon(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const APoly: IGeometryProjectedSinglePolygon;
      const AMapRect: TRect;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
    function DrawPoly(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const APoly: IGeometryLonLatPolygon;
      const AProjectionInfo: IProjectionInfo;
      const AMapRect: TRect;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
    function DrawWikiElement(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AData: IGeometryLonLat;
      const AProjectionInfo: IProjectionInfo;
      const AMapRect: TRect;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
  private
    function RenderVectorTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjectionInfo: IProjectionInfo;
      const ATile: TPoint;
      const ASource: IVectorItemSubset
    ): IBitmap32Static;
  public
    constructor Create(
      AColorMain: TColor32;
      AColorBG: TColor32;
      const APointMarker: IMarkerDrawable;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProjectedCache: IGeometryProjectedProvider
    );
  end;

implementation

uses
  GR32_Polygons,
  u_Bitmap32ByStaticBitmap,
  u_GeometryFunc;

{ TVectorTileRenderer }

constructor TVectorTileRenderer.Create(
  AColorMain: TColor32;
  AColorBG: TColor32;
  const APointMarker: IMarkerDrawable;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProjectedCache: IGeometryProjectedProvider
);
begin
  Assert(Assigned(APointMarker));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AProjectedCache));
  inherited Create;
  FColorMain := AColorMain;
  FColorBG := AColorBG;
  FPointMarker := APointMarker;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProjectedCache := AProjectedCache;
end;

function TVectorTileRenderer.DrawPath(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const ALine: IGeometryLonLatLine;
  const AProjectionInfo: IProjectionInfo;
  const AMapRect: TRect;
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VProjected: IGeometryProjectedLine;
  VPolygon: TPolygon32;
begin
  Result := False;
  VPolygon := nil;
  VProjected := FProjectedCache.GetProjectedPath(AProjectionInfo, ALine);
  ProjectedLine2GR32Polygon(
    VProjected,
    AMapRect,
    am4times,
    AFixedPointArray,
    VPolygon
  );
  try
    if VPolygon <> nil then begin
      if not ABitmapInited then begin
        InitBitmap(ATargetBmp, Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top));
        ABitmapInited := True;
      end;

      with VPolygon.Outline do try
        with Grow(GR32.Fixed(0.5), 0.5) do try
          FillMode := pfWinding;
          DrawFill(ATargetBmp, FColorBG);
        finally
          free;
        end;
      finally
        free;
      end;
      VPolygon.DrawEdge(ATargetBmp, FColorMain);

      Result := True;
    end;
  finally
    VPolygon.Free;
  end;
end;

function TVectorTileRenderer.DrawPoint(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const APoint: IGeometryLonLatPoint;
  const AProjectionInfo: IProjectionInfo;
  const AMapRect: TRect
): Boolean;
var
  VPointLL: TDoublePoint;
  VMapPixelPos: TDoublePoint;
  VLocalPos: TDoublePoint;
  VRect: TRect;
begin
  Result := False;
  VPointLL := APoint.Point;
  AProjectionInfo.ProjectionType.ValidateLonLatPos(VPointLL);
  VMapPixelPos := AProjectionInfo.LonLat2PixelPosFloat(VPointLL);
  VLocalPos.X := VMapPixelPos.X - AMapRect.Left;
  VLocalPos.Y := VMapPixelPos.Y - AMapRect.Top;
  VRect := FPointMarker.GetBoundsForPosition(VLocalPos);
  if Types.IntersectRect(VRect, Rect(0, 0, AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top), VRect) then begin
    if not ABitmapInited then begin
      InitBitmap(ATargetBmp, Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top));
      ABitmapInited := True;
    end;
    Result := FPointMarker.DrawToBitmap(ATargetBmp, VLocalPos);
  end;
end;

function TVectorTileRenderer.DrawPoly(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const APoly: IGeometryLonLatPolygon;
  const AProjectionInfo: IProjectionInfo;
  const AMapRect: TRect;
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VProjected: IGeometryProjectedPolygon;
  VProjectedSingle: IGeometryProjectedSinglePolygon;
  VProjectedMulti: IGeometryProjectedMultiPolygon;
  i: Integer;
begin
  Result := False;
  VProjected := FProjectedCache.GetProjectedPolygon(AProjectionInfo, APoly);
  if Assigned(VProjected) then begin
    if Supports(VProjected, IGeometryProjectedSinglePolygon, VProjectedSingle) then begin
      Result := DrawSinglePolygon(ABitmapInited, ATargetBmp, VProjectedSingle, AMapRect, AFixedPointArray);
    end else if Supports(VProjected, IGeometryProjectedMultiPolygon, VProjectedMulti) then begin
      for i := 0 to VProjectedMulti.Count - 1 do begin
        VProjectedSingle := VProjectedMulti.Item[i];
        if DrawSinglePolygon(ABitmapInited, ATargetBmp, VProjectedSingle, AMapRect, AFixedPointArray) then begin
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
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VPolygon: TPolygon32;
begin
  VPolygon := nil;
  Result := False;
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
      with VPolygon.Outline do try
        with Grow(GR32.Fixed(0.5), 0.5) do try
          FillMode := pfWinding;
          DrawFill(ATargetBmp, FColorBG);
        finally
          free;
        end;
      finally
        free;
      end;
      VPolygon.DrawEdge(ATargetBmp, FColorMain);
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
  const AProjectionInfo: IProjectionInfo;
  const AMapRect: TRect;
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VItemPoint: IGeometryLonLatPoint;
  VItemLine: IGeometryLonLatLine;
  VItemPoly: IGeometryLonLatPolygon;
begin
  if Supports(AData, IGeometryLonLatPoint, VItemPoint) then begin
    Result := DrawPoint(ABitmapInited, ATargetBmp, VItemPoint, AProjectionInfo, AMapRect);
  end else if Supports(AData, IGeometryLonLatLine, VItemLine) then begin
    Result := DrawPath(ABitmapInited, ATargetBmp, VItemLine, AProjectionInfo, AMapRect, AFixedPointArray);
  end else if Supports(AData, IGeometryLonLatPolygon, VItemPoly) then begin
    Result := DrawPoly(ABitmapInited, ATargetBmp, VItemPoly, AProjectionInfo, AMapRect, AFixedPointArray);
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
  const AProjectionInfo: IProjectionInfo;
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
  if not AProjectionInfo.CheckTilePosStrict(ATile) then begin
    Exit;
  end;
  VMapPixelRect := AProjectionInfo.TilePos2PixelRect(ATile);

  VBitmapInited := False;
  if (ASource <> nil) and (ASource.Count > 0) then begin
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      VIsEmpty := True;
      for i := 0 to ASource.Count - 1 do begin
        VItem := ASource.Items[i];
        if DrawWikiElement(VBitmapInited, VBitmap, VItem.Geometry, AProjectionInfo, VMapPixelRect, VFixedPointArray) then begin
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
