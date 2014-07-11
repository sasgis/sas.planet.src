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
  i_DoublePointsAggregator,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByMarksSubset = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FDrawOrderConfigStatic: IMarksDrawOrderConfigStatic;
    FCaptionDrawConfigStatic: ICaptionDrawConfigStatic;
    FBitmapFactory: IBitmap32BufferFactory;
    FMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
    FMarksSubset: IVectorItemSubset;
    FProjectedCache: IGeometryProjectedProvider;

    FPreparedPointsAggreagtor: IDoublePointsAggregator;
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
      const ABitmapFactory: IBitmap32BufferFactory;
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
  i_EnumDoublePoint,
  u_Bitmap32ByStaticBitmap,
  u_DoublePointsAggregator,
  u_EnumDoublePointClosePoly,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_GeoFunc;

const
  CMaxFontSize = 20;

{ TMapMarksBitmapLayerProviderByMarksSubset }

constructor TBitmapLayerProviderByMarksSubset.Create(
  const ADrawOrderConfigStatic: IMarksDrawOrderConfigStatic;
  const ACaptionDrawConfigStatic: ICaptionDrawConfigStatic;
  const ABitmapFactory: IBitmap32BufferFactory;
  const AProjectedCache: IGeometryProjectedProvider;
  const AMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
  const AMarksSubset: IVectorItemSubset
);
begin
  inherited Create;
  FDrawOrderConfigStatic := ADrawOrderConfigStatic;
  FCaptionDrawConfigStatic := ACaptionDrawConfigStatic;
  FBitmapFactory := ABitmapFactory;
  FMarksSubset := AMarksSubset;
  FProjectedCache := AProjectedCache;
  FMarkerProviderForVectorItem := AMarkerProviderForVectorItem;

  FPreparedPointsAggreagtor := TDoublePointsAggregator.Create;
end;

procedure SingleLine2GR32Polygon(
  const ALine: IGeometryProjectedSingleLine;
  const ALocalConverter: ILocalCoordConverter;
  const ARectWithDelta: TDoubleRect;
  const AMapRect: TDoubleRect;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);
var
  VEnum: IEnumLocalPoint;
  VPoint: TDoublePoint;
  VPointsProcessedCount: Integer;
  VIndex: Integer;
  i: Integer;
begin
  if IsIntersecProjectedRect(AMapRect, ALine.Bounds) then begin
    APreparedPointsAggreagtor.Clear;
    VEnum :=
      TEnumDoublePointMapPixelToLocalPixel.Create(
        ALocalConverter,
        ALine.GetEnum
      );
    VEnum :=
      TEnumLocalPointClipByRect.Create(
        False,
        ARectWithDelta,
        VEnum
      );
    VEnum := TEnumLocalPointFilterEqual.Create(VEnum);
    while VEnum.Next(VPoint) do begin
      APreparedPointsAggreagtor.Add(VPoint);
    end;
    VPointsProcessedCount := APreparedPointsAggreagtor.Count;
    if VPointsProcessedCount > 0 then begin
      if APolygon = nil then begin
        APolygon := TPolygon32.Create;
        APolygon.Antialiased := true;
        APolygon.AntialiasMode := am4times;
        APolygon.Closed := False;
      end else begin
        APolygon.NewLine;
      end;
      if Length(AFixedPointArray) < VPointsProcessedCount then begin
        SetLength(AFixedPointArray, VPointsProcessedCount);
      end;
      VIndex := 0;
      for i := 0 to VPointsProcessedCount - 1 do begin
        VPoint := APreparedPointsAggreagtor.Points[i];
        if PointIsEmpty(VPoint) then begin
          APolygon.AddPoints(AFixedPointArray[0], VIndex);
          APolygon.NewLine;
          VIndex := 0;
        end else begin
          AFixedPointArray[VIndex] := FixedPoint(VPoint.X, VPoint.Y);
          Inc(VIndex);
        end;
      end;
      APolygon.AddPoints(AFixedPointArray[0], VIndex);
    end;
  end;
end;

procedure Line2GR32Polygon(
  const ALine: IGeometryProjectedLine;
  const ALocalConverter: ILocalCoordConverter;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);
var
  VMapRect: TDoubleRect;
  VLocalRect: TDoubleRect;
  VRectWithDelta: TDoubleRect;
  VLineIndex: Integer;
  VSingleLine: IGeometryProjectedSingleLine;
  VMultiLine: IGeometryProjectedMultiLine;
begin
  if Assigned(APolygon) then begin
    APolygon.Clear;
  end;

  if ALine <> nil then begin
    if not ALine.IsEmpty then begin
      VMapRect := ALocalConverter.GetRectInMapPixelFloat;
      if IsIntersecProjectedRect(VMapRect, ALine.Bounds) then begin
        VLocalRect := DoubleRect(ALocalConverter.GetLocalRect);
        VRectWithDelta.Left := VLocalRect.Left - 10;
        VRectWithDelta.Top := VLocalRect.Top - 10;
        VRectWithDelta.Right := VLocalRect.Right + 10;
        VRectWithDelta.Bottom := VLocalRect.Bottom + 10;
        if Supports(ALine, IGeometryProjectedSingleLine, VSingleLine) then begin
          SingleLine2GR32Polygon(
            VSingleLine,
            ALocalConverter,
            VRectWithDelta,
            VMapRect,
            APreparedPointsAggreagtor,
            AFixedPointArray,
            APolygon
          );
        end else if Supports(ALine, IGeometryProjectedMultiLine, VMultiLine) then begin
          for VLineIndex := 0 to VMultiLine.Count - 1 do begin
            VSingleLine := VMultiLine.Item[VLineIndex];
            SingleLine2GR32Polygon(
              VSingleLine,
              ALocalConverter,
              VRectWithDelta,
              VMapRect,
              APreparedPointsAggreagtor,
              AFixedPointArray,
              APolygon
            );
          end;
        end;
      end;
    end;
  end;
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
  VProjected: IGeometryProjectedMultiLine;
  VAppearanceLine: IAppearanceLine;
begin
  Result := False;
  VPolygon := nil;
  VProjected := FProjectedCache.GetProjectedPath(ALocalConverter.ProjectionInfo, ALine);
  Line2GR32Polygon(
    VProjected,
    ALocalConverter,
    FPreparedPointsAggreagtor,
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

procedure SinglePoly2GR32Polygon(
  const ALine: IGeometryProjectedSinglePolygon;
  const ALocalConverter: ILocalCoordConverter;
  const ARectWithDelta: TDoubleRect;
  const AMapRect: TDoubleRect;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);
var
  VEnum: IEnumLocalPoint;
  VPoint: TDoublePoint;
  VPointsProcessedCount: Integer;
  i: Integer;
begin
  if IsIntersecProjectedRect(AMapRect, ALine.Bounds) then begin
    APreparedPointsAggreagtor.Clear;
    VEnum :=
      TEnumDoublePointMapPixelToLocalPixel.Create(
        ALocalConverter,
        ALine.GetEnum
      );
    VEnum :=
      TEnumLocalPointClipByRect.Create(
        True,
        ARectWithDelta,
        VEnum
      );
    VEnum := TEnumLocalPointFilterEqual.Create(VEnum);
    VEnum := TEnumLocalPointClosePoly.Create(VEnum);
    while VEnum.Next(VPoint) do begin
      APreparedPointsAggreagtor.Add(VPoint);
    end;
    VPointsProcessedCount := APreparedPointsAggreagtor.Count;
    if VPointsProcessedCount > 0 then begin
      if APolygon = nil then begin
        APolygon := TPolygon32.Create;
        APolygon.Antialiased := true;
        APolygon.AntialiasMode := am4times;
        APolygon.Closed := True;
      end else begin
        APolygon.NewLine;
      end;
      if Length(AFixedPointArray) < VPointsProcessedCount then begin
        SetLength(AFixedPointArray, VPointsProcessedCount);
      end;
      for i := 0 to VPointsProcessedCount - 1 do begin
        VPoint := APreparedPointsAggreagtor.Points[i];
        AFixedPointArray[i] := FixedPoint(VPoint.X, VPoint.Y);
      end;
      APolygon.AddPoints(AFixedPointArray[0], VPointsProcessedCount);
    end;
  end;
end;

procedure Polygon2GR32Polygon(
  const ALine: IGeometryProjectedPolygon;
  const ALocalConverter: ILocalCoordConverter;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);
var
  VMapRect: TDoubleRect;
  VLocalRect: TDoubleRect;
  VRectWithDelta: TDoubleRect;
  VProjectedMultiLine: IGeometryProjectedMultiPolygon;
  VProjectedSingleLine: IGeometryProjectedSinglePolygon;
  VLineIndex: Integer;
begin
  if Assigned(APolygon) then begin
    APolygon.Clear;
  end;

  if ALine <> nil then begin
    if not ALine.IsEmpty then begin
      VMapRect := ALocalConverter.GetRectInMapPixelFloat;
      if IsIntersecProjectedRect(VMapRect, ALine.Bounds) then begin
        VLocalRect := DoubleRect(ALocalConverter.GetLocalRect);
        VRectWithDelta.Left := VLocalRect.Left - 10;
        VRectWithDelta.Top := VLocalRect.Top - 10;
        VRectWithDelta.Right := VLocalRect.Right + 10;
        VRectWithDelta.Bottom := VLocalRect.Bottom + 10;
        if Supports(ALine, IGeometryProjectedSinglePolygon, VProjectedSingleLine) then begin
          SinglePoly2GR32Polygon(
            VProjectedSingleLine,
            ALocalConverter,
            VRectWithDelta,
            VMapRect,
            APreparedPointsAggreagtor,
            AFixedPointArray,
            APolygon
          );
        end else if Supports(ALine, IGeometryProjectedMultiPolygon, VProjectedMultiLine) then begin
          for VLineIndex := 0 to VProjectedMultiLine.Count - 1 do begin
            VProjectedSingleLine := VProjectedMultiLine.Item[VLineIndex];
            SinglePoly2GR32Polygon(
              VProjectedSingleLine,
              ALocalConverter,
              VRectWithDelta,
              VMapRect,
              APreparedPointsAggreagtor,
              AFixedPointArray,
              APolygon
            );
          end;
        end;
      end;
    end;
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
    Polygon2GR32Polygon(
      VProjected,
      ALocalConverter,
      FPreparedPointsAggreagtor,
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
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
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

