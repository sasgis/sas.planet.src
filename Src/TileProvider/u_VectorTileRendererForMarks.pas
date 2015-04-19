{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_VectorTileRendererForMarks;

interface

uses
  GR32,
  WinTypes,
  Types,
  t_GeoTypes,
  i_MarkerProviderForVectorItem,
  i_GeometryLonLat,
  i_GeometryProjectedProvider,
  i_Appearance,
  i_ProjectionInfo,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_MarksDrawConfig,
  i_VectorDataItemSimple,
  i_GeometryProjected,
  i_VectorItemSubset,
  i_VectorTileRenderer,
  u_BaseInterfacedObject;

type
  TVectorTileRendererForMarks = class(TBaseInterfacedObject, IVectorTileRenderer)
  private
    FCaptionDrawConfigStatic: ICaptionDrawConfigStatic;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
    FProjectedCache: IGeometryProjectedProvider;

    function DrawSubset(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AMarksSubset: IVectorItemSubset;
      ATargetBmp: TCustomBitmap32;
      const AProjection: IProjectionInfo;
      const AMapRect: TRect;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
    function DrawPath(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AProjection: IProjectionInfo;
      const AMapRect: TRect;
      const AAppearance: IAppearance;
      const ALine: IGeometryLonLatLine;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
    function DrawPoly(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AProjection: IProjectionInfo;
      const AMapRect: TRect;
      const AAppearance: IAppearance;
      const APoly: IGeometryLonLatPolygon;
      var AFixedPointArray: TArrayOfFixedPoint
    ): Boolean;
    function DrawPoint(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AProjection: IProjectionInfo;
      const AMapRect: TRect;
      const AGeometry: IGeometryLonLatPoint;
      const APoint: IVectorDataItem
    ): Boolean;
    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ASize: TPoint
    );
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
      const ACaptionDrawConfigStatic: ICaptionDrawConfigStatic;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProjectedCache: IGeometryProjectedProvider;
      const AMarkerProviderForVectorItem: IMarkerProviderForVectorItem
    );
  end;

implementation

uses
  ActiveX,
  SysUtils,
  GR32_Polygons,
  i_MarkerDrawable,
  i_AppearanceOfVectorItem,
  i_CoordConverter,
  u_Bitmap32ByStaticBitmap,
  u_GeometryFunc;

{ TMapMarksBitmapLayerProviderByMarksSubset }

constructor TVectorTileRendererForMarks.Create(
  const ACaptionDrawConfigStatic: ICaptionDrawConfigStatic;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProjectedCache: IGeometryProjectedProvider;
  const AMarkerProviderForVectorItem: IMarkerProviderForVectorItem
);
begin
  Assert(Assigned(ACaptionDrawConfigStatic));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AProjectedCache));
  Assert(Assigned(AMarkerProviderForVectorItem));
  inherited Create;
  FCaptionDrawConfigStatic := ACaptionDrawConfigStatic;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProjectedCache := AProjectedCache;
  FMarkerProviderForVectorItem := AMarkerProviderForVectorItem;
end;

function TVectorTileRendererForMarks.DrawPath(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const AProjection: IProjectionInfo;
  const AMapRect: TRect;
  const AAppearance: IAppearance;
  const ALine: IGeometryLonLatLine;
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
        with VPolygon.Outline do try
          with Grow(GR32.Fixed(VAppearanceLine.LineWidth / 2), 0.5) do try
            FillMode := pfWinding;
            DrawFill(ATargetBmp, VAppearanceLine.LineColor);
          finally
            free;
          end;
        finally
          free;
        end;
      end;
      Result := True;
    end;
  finally
    VPolygon.Free;
  end;
end;

function TVectorTileRendererForMarks.DrawPoly(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const AProjection: IProjectionInfo;
  const AMapRect: TRect;
  const AAppearance: IAppearance;
  const APoly: IGeometryLonLatPolygon;
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VPolygon: TPolygon32;
  VProjected: IGeometryProjectedPolygon;
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
begin
  Result := False;
  VProjected := FProjectedCache.GetProjectedPolygon(AProjection, APoly);
  VPolygon := nil;
  try
    ProjectedPolygon2GR32Polygon(
      VProjected,
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
      if Supports(AAppearance, IAppearancePolygonFill, VAppearanceFill) then begin
        VPolygon.DrawFill(ATargetBmp, VAppearanceFill.FillColor);
      end;
      if Supports(AAppearance, IAppearancePolygonBorder, VAppearanceBorder) then begin
        with VPolygon.Outline do try
          with Grow(GR32.Fixed(VAppearanceBorder.LineWidth / 2), 0.5) do try
            FillMode := pfWinding;
            DrawFill(ATargetBmp, VAppearanceBorder.LineColor);
          finally
            free;
          end;
        finally
          free;
        end;
      end;
      Result := True;
    end;
  finally
    VPolygon.Free;
  end;
end;

function TVectorTileRendererForMarks.DrawPoint(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const AProjection: IProjectionInfo;
  const AMapRect: TRect;
  const AGeometry: IGeometryLonLatPoint;
  const APoint: IVectorDataItem
): Boolean;
var
  VLocalPoint: TDoublePoint;
  VMapPoint: TDoublePoint;
  VLonLat: TDoublePoint;
  VMarker: IMarkerDrawable;
begin
  Result := False;
  VMarker := FMarkerProviderForVectorItem.GetMarker(FCaptionDrawConfigStatic, APoint);
  if VMarker <> nil then begin
    VLonLat := AGeometry.Point;
    AProjection.GeoConverter.ValidateLonLatPos(VLonLat);
    VMapPoint := AProjection.GeoConverter.LonLat2PixelPosFloat(VLonLat, AProjection.Zoom);
    VLocalPoint.X := VMapPoint.X - AMapRect.Left;
    VLocalPoint.Y := VMapPoint.Y - AMapRect.Top;
    if not ABitmapInited then begin
      InitBitmap(ATargetBmp, Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top));
      ABitmapInited := True;
    end;
    Result := VMarker.DrawToBitmap(ATargetBmp, VLocalPoint);
  end;
end;

function TVectorTileRendererForMarks.DrawSubset(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AMarksSubset: IVectorItemSubset;
  ATargetBmp: TCustomBitmap32;
  const AProjection: IProjectionInfo;
  const AMapRect: TRect;
  var AFixedPointArray: TArrayOfFixedPoint
): Boolean;
var
  VEnumMarks: IEnumUnknown;
  VMark: IVectorDataItem;
  i: Cardinal;
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatLine;
  VPoly: IGeometryLonLatPolygon;
  VBitmapInited: Boolean;
begin
  Result := False;
  VBitmapInited := False;
  VEnumMarks := AMarksSubset.GetEnum;
  while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Break;
    end;
    if Supports(VMark.Geometry, IGeometryLonLatPolygon, VPoly) then begin
      if DrawPoly(VBitmapInited, ATargetBmp, AProjection, AMapRect, VMark.Appearance, VPoly, AFixedPointArray) then begin
        Result := True;
      end;
    end;
  end;
  VEnumMarks.Reset;
  while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Break;
    end;
    if Supports(VMark.Geometry, IGeometryLonLatLine, VLine) then begin
      if DrawPath(VBitmapInited, ATargetBmp, AProjection, AMapRect, VMark.Appearance, VLine, AFixedPointArray) then begin
        Result := True;
      end;
    end;
  end;
  VEnumMarks.Reset;
  while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Break;
    end;
    if Supports(VMark.Geometry, IGeometryLonLatPoint, VPoint) then begin
      if DrawPoint(VBitmapInited, ATargetBmp, AProjection, AMapRect, VPoint, VMark) then begin
        Result := True;
      end;
    end;
  end;
end;

function TVectorTileRendererForMarks.RenderVectorTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjectionInfo;
  const ATile: TPoint;
  const ASource: IVectorItemSubset
): IBitmap32Static;
var
  VMapRect: TRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VBitmap: TBitmap32ByStaticBitmap;
  VFixedPointArray: TArrayOfFixedPoint;
begin
  Result := nil;
  if Assigned(ASource) and not ASource.IsEmpty then begin
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      VZoom := AProjectionInfo.Zoom;
      VConverter := AProjectionInfo.GeoConverter;
      VMapRect := VConverter.TilePos2PixelRect(ATile, VZoom);
      if DrawSubset(AOperationID, ACancelNotifier, ASource, VBitmap, AProjectionInfo, VMapRect, VFixedPointArray) then begin
        Result := VBitmap.MakeAndClear;
      end;
    finally
      VBitmap.Free;
    end;
  end;
end;

procedure TVectorTileRendererForMarks.InitBitmap(
  ATargetBmp: TCustomBitmap32;
  const ASize: TPoint
);
begin
  ATargetBmp.SetSize(ASize.X, ASize.Y);
  ATargetBmp.Clear(0);
  ATargetBmp.CombineMode := cmMerge;
end;

end.

