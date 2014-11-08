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
  t_GeoTypes,
  i_MarkerProviderForVectorItem,
  i_GeometryLonLat,
  i_GeometryProjectedProvider,
  i_Appearance,
  i_LocalCoordConverter,
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
  TBitmapLayerProviderByMarksSubset = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FDrawOrderConfigStatic: IMarksDrawOrderConfigStatic;
    FCaptionDrawConfigStatic: ICaptionDrawConfigStatic;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
    FMarksSubset: IVectorItemSubset;
    FProjectedCache: IGeometryProjectedProvider;

    FFixedPointArray: TArrayOfFixedPoint;

    function DrawSubset(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AMarksSubset: IVectorItemSubset;
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawPath(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const AAppearance: IAppearance;
      const ALine: IGeometryLonLatLine
    ): Boolean;
    function DrawPoly(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const AAppearance: IAppearance;
      const APoly: IGeometryLonLatPolygon
    ): Boolean;
    function DrawPoint(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const AGeometry: IGeometryLonLatPoint;
      const APoint: IVectorDataItem
    ): Boolean;
    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    );
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
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
  const ALocalConverter: ILocalCoordConverter;
  const AAppearance: IAppearance;
  const ALine: IGeometryLonLatLine
): Boolean;
var
  VPolygon: TPolygon32;
  VProjected: IGeometryProjectedLine;
  VAppearanceLine: IAppearanceLine;
begin
  Result := False;
  VPolygon := nil;
  VProjected := FProjectedCache.GetProjectedPath(ALocalConverter.ProjectionInfo, ALine);
  ProjectedLine2GR32Polygon(
    VProjected,
    ALocalConverter,
    am4times,
    FFixedPointArray,
    VPolygon
  );
  try
    if Assigned(VPolygon) then begin
      if not ABitmapInited then begin
        InitBitmap(ATargetBmp, ALocalConverter);
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
  const ALocalConverter: ILocalCoordConverter;
  const AAppearance: IAppearance;
  const APoly: IGeometryLonLatPolygon
): Boolean;
var
  VPolygon: TPolygon32;
  VProjected: IGeometryProjectedPolygon;
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
begin
  Result := False;
  VProjected := FProjectedCache.GetProjectedPolygon(ALocalConverter.ProjectionInfo, APoly);
  VPolygon := nil;
  try
    ProjectedPolygon2GR32Polygon(
      VProjected,
      ALocalConverter,
      am4times,
      FFixedPointArray,
      VPolygon
    );
    if VPolygon <> nil then begin
      if not ABitmapInited then begin
        InitBitmap(ATargetBmp, ALocalConverter);
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
  const ALocalConverter: ILocalCoordConverter;
  const AGeometry: IGeometryLonLatPoint;
  const APoint: IVectorDataItem
): Boolean;
var
  VLocalPoint: TDoublePoint;
  VLonLat: TDoublePoint;
  VMarker: IMarkerDrawable;
begin
  Result := False;
  VMarker := FMarkerProviderForVectorItem.GetMarker(FCaptionDrawConfigStatic, APoint);
  if VMarker <> nil then begin
    VLonLat := AGeometry.Point;
    ALocalConverter.GeoConverter.CheckLonLatPos(VLonLat);
    VLocalPoint := ALocalConverter.LonLat2LocalPixelFloat(VLonLat);
    if not ABitmapInited then begin
      InitBitmap(ATargetBmp, ALocalConverter);
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
  const ALocalConverter: ILocalCoordConverter
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
        if DrawPoint(VBitmapInited, ATargetBmp, ALocalConverter, VPoint, VMark) then begin
          Result := True;
        end;
      end else if Supports(VMark.Geometry, IGeometryLonLatLine, VLine) then begin
        if DrawPath(VBitmapInited, ATargetBmp, ALocalConverter, VMark.Appearance, VLine) then begin
          Result := True;
        end;
      end else if Supports(VMark.Geometry, IGeometryLonLatPolygon, VPoly) then begin
        if DrawPoly(VBitmapInited, ATargetBmp, ALocalConverter, VMark.Appearance, VPoly) then begin
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
        if DrawPoly(VBitmapInited, ATargetBmp, ALocalConverter, VMark.Appearance, VPoly) then begin
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
        if DrawPath(VBitmapInited, ATargetBmp, ALocalConverter, VMark.Appearance, VLine) then begin
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
        if DrawPoint(VBitmapInited, ATargetBmp, ALocalConverter, VPoint, VMark) then begin
          Result := True;
        end;
      end;
    end;
  end;
end;

function TBitmapLayerProviderByMarksSubset.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VRectWithDelta: TRect;
  VLocalRect: TRect;
  VTargetRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMarksSubset: IVectorItemSubset;
  VDeltaSizeInPixel: TRect;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  VLocalRect := ALocalConverter.GetLocalRect;
  VDeltaSizeInPixel := FDrawOrderConfigStatic.OverSizeRect;
  VRectWithDelta.Left := VLocalRect.Left - VDeltaSizeInPixel.Left;
  VRectWithDelta.Top := VLocalRect.Top - VDeltaSizeInPixel.Top;
  VRectWithDelta.Right := VLocalRect.Right + VDeltaSizeInPixel.Right;
  VRectWithDelta.Bottom := VLocalRect.Bottom + VDeltaSizeInPixel.Bottom;
  VTargetRect := ALocalConverter.LocalRect2MapRectFloat(VRectWithDelta);
  VZoom := ALocalConverter.GetZoom;
  VConverter := ALocalConverter.GetGeoConverter;
  VConverter.CheckPixelRectFloat(VTargetRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VTargetRect, VZoom);
  VMarksSubset := FMarksSubset.GetSubsetByLonLatRect(VLonLatRect);
  Result := nil;
  if Assigned(VMarksSubset) and not VMarksSubset.IsEmpty then begin
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      if DrawSubset(AOperationID, ACancelNotifier, VMarksSubset, VBitmap, ALocalConverter) then begin
        Result := VBitmap.MakeAndClear;
      end;
    finally
      VBitmap.Free;
    end;
  end;
end;

procedure TBitmapLayerProviderByMarksSubset.InitBitmap(
  ATargetBmp: TCustomBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VSize: TPoint;
begin
  VSize := ALocalConverter.GetLocalRectSize;
  ATargetBmp.SetSize(VSize.X, VSize.Y);
  ATargetBmp.Clear(0);
  ATargetBmp.CombineMode := cmMerge;
end;

end.

