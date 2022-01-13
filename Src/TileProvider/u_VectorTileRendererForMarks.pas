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

unit u_VectorTileRendererForMarks;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_NotifierOperation,
  i_Appearance,
  i_Projection,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_GeometryProjectedProvider,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_MarksDrawConfig,
  i_MarkerProviderForVectorItem,
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
      const AProjection: IProjection;
      const AMapRect: TRect;
      var APointArray: TArrayOfFloatPoint
    ): Boolean;
    function DrawPoint(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AProjection: IProjection;
      const AMapRect: TRect;
      const AGeometry: IGeometryLonLatPoint;
      const APoint: IVectorDataItem
    ): Boolean;
    function DrawPath(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AProjection: IProjection;
      const AMapRect: TRect;
      const AAppearance: IAppearance;
      const ALine: IGeometryLonLatLine;
      var APointArray: TArrayOfFloatPoint
    ): Boolean;
    function DrawPoly(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AProjection: IProjection;
      const AMapRect: TRect;
      const AAppearance: IAppearance;
      const APoly: IGeometryLonLatPolygon;
      var APointArray: TArrayOfFloatPoint
    ): Boolean;
    function DrawSinglePolygon(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      const AMapRect: TRect;
      const AAppearance: IAppearance;
      const APoly: IGeometryProjectedSinglePolygon;
      var APointArray: TArrayOfFloatPoint
    ): Boolean;

    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ASize: TPoint
    );
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
      const ACaptionDrawConfigStatic: ICaptionDrawConfigStatic;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProjectedCache: IGeometryProjectedProvider;
      const AMarkerProviderForVectorItem: IMarkerProviderForVectorItem
    );
  end;

implementation

uses
  ActiveX,
  Math,
  SysUtils,
  GR32_Polygons,
  i_MarkerDrawable,
  i_AppearanceOfVectorItem,
  u_Bitmap32ByStaticBitmap,
  u_GeometryFunc;

{ TVectorTileRendererForMarks }

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

function TVectorTileRendererForMarks.DrawPoint(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const AProjection: IProjection;
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
    AProjection.ProjectionType.ValidateLonLatPos(VLonLat);
    VMapPoint := AProjection.LonLat2PixelPosFloat(VLonLat);
    VLocalPoint.X := VMapPoint.X - AMapRect.Left;
    VLocalPoint.Y := VMapPoint.Y - AMapRect.Top;
    if not ABitmapInited then begin
      InitBitmap(ATargetBmp, Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top));
      ABitmapInited := True;
    end;
    Result := VMarker.DrawToBitmap(ATargetBmp, VLocalPoint);
  end;
end;

function TVectorTileRendererForMarks.DrawPath(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const AProjection: IProjection;
  const AMapRect: TRect;
  const AAppearance: IAppearance;
  const ALine: IGeometryLonLatLine;
  var APointArray: TArrayOfFloatPoint
): Boolean;
var
  VPolygon: TArrayOfArrayOfFloatPoint;
  VProjected: IGeometryProjectedLine;
  VAppearanceLine: IAppearanceLine;
begin
  Result := False;
  VProjected := FProjectedCache.GetProjectedPath(AProjection, ALine);
  VPolygon := ProjectedLine2ArrayOfArray(VProjected, AMapRect, APointArray);
  if Assigned(VPolygon) then begin
    if not ABitmapInited then begin
      InitBitmap(ATargetBmp, Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top));
      ABitmapInited := True;
    end;
    if Supports(AAppearance, IAppearanceLine, VAppearanceLine) then begin
      PolyPolylineFS(ATargetBmp, VPolygon, VAppearanceLine.LineColor, False, VAppearanceLine.LineWidth);
    end;
    Result := True;
  end;
end;

function TVectorTileRendererForMarks.DrawSinglePolygon(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const AMapRect: TRect;
  const AAppearance: IAppearance;
  const APoly: IGeometryProjectedSinglePolygon;
  var APointArray: TArrayOfFloatPoint
): Boolean;
var
  VPolygon: TArrayOfArrayOfFloatPoint;
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
begin
  Result := False;
  VPolygon := ProjectedPolygon2ArrayOfArray(APoly, AMapRect, APointArray);
  if VPolygon <> nil then begin
    if not ABitmapInited then begin
      InitBitmap(ATargetBmp, Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top));
      ABitmapInited := True;
    end;
    if Supports(AAppearance, IAppearancePolygonFill, VAppearanceFill) then begin
      PolyPolygonFS(ATargetBmp, VPolygon, VAppearanceFill.FillColor);
    end;
    if Supports(AAppearance, IAppearancePolygonBorder, VAppearanceBorder) then begin
      PolyPolylineFS(ATargetBmp, VPolygon, VAppearanceBorder.LineColor, True, VAppearanceBorder.LineWidth);
    end;
    Result := True;
  end;
end;

function TVectorTileRendererForMarks.DrawPoly(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  const AProjection: IProjection;
  const AMapRect: TRect;
  const AAppearance: IAppearance;
  const APoly: IGeometryLonLatPolygon;
  var APointArray: TArrayOfFloatPoint
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
      Result := DrawSinglePolygon(ABitmapInited, ATargetBmp, AMapRect, AAppearance, VProjectedSingle, APointArray);
    end else if Supports(VProjected, IGeometryProjectedMultiPolygon, VProjectedMulti) then begin
      for i := 0 to VProjectedMulti.Count - 1 do begin
        VProjectedSingle := VProjectedMulti.Item[i];
        if DrawSinglePolygon(ABitmapInited, ATargetBmp, AMapRect, AAppearance, VProjectedSingle, APointArray) then begin
          Result := True;
        end;
      end;
    end else begin
      Assert(False);
    end;
  end;
end;

function TVectorTileRendererForMarks.DrawSubset(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AMarksSubset: IVectorItemSubset;
  ATargetBmp: TCustomBitmap32;
  const AProjection: IProjection;
  const AMapRect: TRect;
  var APointArray: TArrayOfFloatPoint
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
      if DrawPoly(VBitmapInited, ATargetBmp, AProjection, AMapRect, VMark.Appearance, VPoly, APointArray) then begin
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
      if DrawPath(VBitmapInited, ATargetBmp, AProjection, AMapRect, VMark.Appearance, VLine, APointArray) then begin
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
  const AProjection: IProjection;
  const ATile: TPoint;
  const ASource: IVectorItemSubset
): IBitmap32Static;
var
  VMapRect: TRect;
  VBitmap: TBitmap32ByStaticBitmap;
  VFixedPointArray: TArrayOfFloatPoint;
begin
  Result := nil;
  if Assigned(ASource) and not ASource.IsEmpty then begin
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      VMapRect := AProjection.TilePos2PixelRect(ATile);
      if DrawSubset(AOperationID, ACancelNotifier, ASource, VBitmap, AProjection, VMapRect, VFixedPointArray) then begin
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
