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

unit u_BitmapLayerProviderByMarksSubset;

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
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByMarksSubset = class(TBaseInterfacedObject, IBitmapTileUniProvider)
  private
    FDrawOrderConfigStatic: IMarksDrawOrderConfigStatic;
    FCaptionDrawConfigStatic: ICaptionDrawConfigStatic;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
    FMarksSubset: IVectorItemSubset;
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
    function GetTile(AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjectionInfo: IProjectionInfo;
      const ATile: TPoint): IBitmap32Static;
  public
    constructor Create(
      const ADrawOrderConfigStatic: IMarksDrawOrderConfigStatic;
      const ACaptionDrawConfigStatic: ICaptionDrawConfigStatic;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProjectedCache: IGeometryProjectedProvider;
      const AMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
      const AMarksSubset: IVectorItemSubset
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

constructor TBitmapLayerProviderByMarksSubset.Create(
  const ADrawOrderConfigStatic: IMarksDrawOrderConfigStatic;
  const ACaptionDrawConfigStatic: ICaptionDrawConfigStatic;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProjectedCache: IGeometryProjectedProvider;
  const AMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
  const AMarksSubset: IVectorItemSubset
);
begin
  Assert(Assigned(ADrawOrderConfigStatic));
  Assert(Assigned(ACaptionDrawConfigStatic));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AProjectedCache));
  Assert(Assigned(AMarkerProviderForVectorItem));
  Assert(Assigned(AMarksSubset));
  inherited Create;
  FDrawOrderConfigStatic := ADrawOrderConfigStatic;
  FCaptionDrawConfigStatic := ACaptionDrawConfigStatic;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FMarksSubset := AMarksSubset;
  FProjectedCache := AProjectedCache;
  FMarkerProviderForVectorItem := AMarkerProviderForVectorItem;
end;

function TBitmapLayerProviderByMarksSubset.DrawPath(
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

function TBitmapLayerProviderByMarksSubset.DrawPoly(
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

function TBitmapLayerProviderByMarksSubset.DrawPoint(
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

function TBitmapLayerProviderByMarksSubset.DrawSubset(
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
  if FDrawOrderConfigStatic.UseSimpleDrawOrder then begin
    while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Break;
      end;
      if Supports(VMark.Geometry, IGeometryLonLatPoint, VPoint) then begin
        if DrawPoint(VBitmapInited, ATargetBmp, AProjection, AMapRect, VPoint, VMark) then begin
          Result := True;
        end;
      end else if Supports(VMark.Geometry, IGeometryLonLatLine, VLine) then begin
        if DrawPath(VBitmapInited, ATargetBmp, AProjection, AMapRect, VMark.Appearance, VLine, AFixedPointArray) then begin
          Result := True;
        end;
      end else if Supports(VMark.Geometry, IGeometryLonLatPolygon, VPoly) then begin
        if DrawPoly(VBitmapInited, ATargetBmp, AProjection, AMapRect, VMark.Appearance, VPoly, AFixedPointArray) then begin
          Result := True;
        end;
      end;
    end;
  end else begin
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
end;

function TBitmapLayerProviderByMarksSubset.GetTile(AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjectionInfo;
  const ATile: TPoint
): IBitmap32Static;
var
  VRectWithDelta: TRect;
  VMapRect: TRect;
  VLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMarksSubset: IVectorItemSubset;
  VDeltaSizeInPixel: TRect;
  VBitmap: TBitmap32ByStaticBitmap;
  VFixedPointArray: TArrayOfFixedPoint;
begin
  VZoom := AProjectionInfo.Zoom;
  VConverter := AProjectionInfo.GeoConverter;
  VMapRect := VConverter.TilePos2PixelRect(ATile, VZoom);
  VDeltaSizeInPixel := FDrawOrderConfigStatic.OverSizeRect;
  VRectWithDelta.Left := VMapRect.Left - VDeltaSizeInPixel.Left;
  VRectWithDelta.Top := VMapRect.Top - VDeltaSizeInPixel.Top;
  VRectWithDelta.Right := VMapRect.Right + VDeltaSizeInPixel.Right;
  VRectWithDelta.Bottom := VMapRect.Bottom + VDeltaSizeInPixel.Bottom;
  VConverter.ValidatePixelRect(VRectWithDelta, VZoom);
  VLonLatRect := VConverter.PixelRect2LonLatRect(VRectWithDelta, VZoom);
  VMarksSubset := FMarksSubset.GetSubsetByLonLatRect(VLonLatRect);
  Result := nil;
  if Assigned(VMarksSubset) and not VMarksSubset.IsEmpty then begin
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      if DrawSubset(AOperationID, ACancelNotifier, VMarksSubset, VBitmap, AProjectionInfo, VMapRect, VFixedPointArray) then begin
        Result := VBitmap.MakeAndClear;
      end;
    finally
      VBitmap.Free;
    end;
  end;
end;

procedure TBitmapLayerProviderByMarksSubset.InitBitmap(
  ATargetBmp: TCustomBitmap32;
  const ASize: TPoint
);
begin
  ATargetBmp.SetSize(ASize.X, ASize.Y);
  ATargetBmp.Clear(0);
  ATargetBmp.CombineMode := cmMerge;
end;

end.

