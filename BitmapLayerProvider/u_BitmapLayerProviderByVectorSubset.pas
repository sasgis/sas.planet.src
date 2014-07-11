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

unit u_BitmapLayerProviderByVectorSubset;

interface

uses
  Types,
  SysUtils,
  GR32,
  t_GeoTypes,
  i_CoordConverter,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_GeometryLonLat,
  i_GeometryProjectedProvider,
  i_LocalCoordConverter,
  i_NotifierOperation,
  i_MarkerDrawable,
  i_GeometryProjected,
  i_DoublePointsAggregator,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByVectorSubset = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FColorMain: TColor32;
    FColorBG: TColor32;
    FPointMarker: IMarkerDrawable;
    FBitmapFactory: IBitmap32BufferFactory;
    FVectorItems: IVectorItemSubset;
    FProjectedCache: IGeometryProjectedProvider;

    FPreparedPointsAggreagtor: IDoublePointsAggregator;
    FFixedPointArray: TArrayOfFixedPoint;

    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    );
    function DrawPoint(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const APoint: IGeometryLonLatPoint;
      const ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawPath(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const ALine: IGeometryLonLatLine;
      const ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawPoly(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const APoly: IGeometryLonLatPolygon;
      const ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawWikiElement(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AData: IGeometryLonLat;
      const ALocalConverter: ILocalCoordConverter
    ): Boolean;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      AColorMain: TColor32;
      AColorBG: TColor32;
      const APointMarker: IMarkerDrawable;
      const ABitmapFactory: IBitmap32BufferFactory;
      const AProjectedCache: IGeometryProjectedProvider;
      const AVectorItems: IVectorItemSubset
    );
  end;

implementation

uses
  GR32_Polygons,
  i_EnumDoublePoint,
  i_LonLatRect,
  u_Bitmap32ByStaticBitmap,
  u_DoublePointsAggregator,
  u_EnumDoublePointClosePoly,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_GeoFunc;

{ TBitmapLayerProviderByVectorSubset }

constructor TBitmapLayerProviderByVectorSubset.Create(
  AColorMain: TColor32;
  AColorBG: TColor32;
  const APointMarker: IMarkerDrawable;
  const ABitmapFactory: IBitmap32BufferFactory;
  const AProjectedCache: IGeometryProjectedProvider;
  const AVectorItems: IVectorItemSubset
);
begin
  inherited Create;
  FColorMain := AColorMain;
  FColorBG := AColorBG;
  FPointMarker := APointMarker;
  FBitmapFactory := ABitmapFactory;
  FProjectedCache := AProjectedCache;
  FVectorItems := AVectorItems;

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

function TBitmapLayerProviderByVectorSubset.DrawPath(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const ALine: IGeometryLonLatLine;
  const ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VProjected: IGeometryProjectedLine;
  VPolygon: TPolygon32;
begin
  Result := False;
  if not ALine.IsEmpty then begin
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
      if VPolygon <> nil then begin
        if not ABitmapInited then begin
          InitBitmap(ATargetBmp, ALocalConverter);
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
end;

function TBitmapLayerProviderByVectorSubset.DrawPoint(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const APoint: IGeometryLonLatPoint;
  const ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VConverter: ICoordConverter;
  VPointLL: TDoublePoint;
  VLocalPos: TDoublePoint;
  VRect: TRect;
begin
  Result := False;
  VConverter := ALocalConverter.GetGeoConverter;
  VPointLL := APoint.Point;
  VConverter.CheckLonLatPos(VPointLL);
  VLocalPos := ALocalConverter.LonLat2LocalPixelFloat(VPointLL);
  VRect := FPointMarker.GetBoundsForPosition(VLocalPos);
  if IntersectRect(VRect, ALocalConverter.GetLocalRect, VRect) then begin
    if not ABitmapInited then begin
      InitBitmap(ATargetBmp, ALocalConverter);
      ABitmapInited := True;
    end;
    Result := FPointMarker.DrawToBitmap(ATargetBmp, VLocalPos);
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

function TBitmapLayerProviderByVectorSubset.DrawPoly(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const APoly: IGeometryLonLatPolygon;
  const ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VPolygon: TPolygon32;
  VProjected: IGeometryProjectedPolygon;
begin
  VPolygon := nil;
  Result := False;
  VProjected := FProjectedCache.GetProjectedPolygon(ALocalConverter.ProjectionInfo, APoly);
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

function TBitmapLayerProviderByVectorSubset.DrawWikiElement(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const AData: IGeometryLonLat;
  const ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VItemPoint: IGeometryLonLatPoint;
  VItemLine: IGeometryLonLatLine;
  VItemPoly: IGeometryLonLatPolygon;
begin
  if Supports(AData, IGeometryLonLatPoint, VItemPoint) then begin
    Result := DrawPoint(ABitmapInited, ATargetBmp, VItemPoint, ALocalConverter);
  end else if Supports(AData, IGeometryLonLatLine, VItemLine) then begin
    Result := DrawPath(ABitmapInited, ATargetBmp, VItemLine, ALocalConverter);
  end else if Supports(AData, IGeometryLonLatPolygon, VItemPoly) then begin
    Result := DrawPoly(ABitmapInited, ATargetBmp, VItemPoly, ALocalConverter);
  end else begin
    Result := False;
  end;
end;

function TBitmapLayerProviderByVectorSubset.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  i: Integer;
  VItem: IVectorDataItem;
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
  VMapPixelRect: TDoubleRect;
  VLLRect: TDoubleRect;
  VBitmapInited: Boolean;
  VBitmap: TBitmap32ByStaticBitmap;
  VIsEmpty: Boolean;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VMapPixelRect, VZoom);
  VLLRect := VGeoConvert.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

  VBitmapInited := False;
  Result := nil;
  if (FVectorItems <> nil) and (FVectorItems.Count > 0) then begin
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
    try
      VIsEmpty := True;
      for i := 0 to FVectorItems.Count - 1 do begin
        VItem := FVectorItems.GetItem(i);
        if VItem.Geometry.Bounds.IsIntersecWithRect(VLLRect) then begin
          if DrawWikiElement(VBitmapInited, VBitmap, VItem.Geometry, ALocalConverter) then begin
            VIsEmpty := False;
          end;
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            Break;
          end;
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

procedure TBitmapLayerProviderByVectorSubset.InitBitmap(
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
