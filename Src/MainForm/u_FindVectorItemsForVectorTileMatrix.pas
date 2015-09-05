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

unit u_FindVectorItemsForVectorTileMatrix;

interface

uses
  Types,
  i_InternalPerformanceCounter,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_LocalCoordConverter,
  i_GeometryProjectedProvider,
  i_VectorTileMatrixChangeable,
  i_FindVectorItems,
  u_BaseInterfacedObject;

type
  TFindVectorItemsForVectorTileMatrix = class(TBaseInterfacedObject, IFindVectorItems)
  private
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FProjectedProvider: IGeometryProjectedProvider;
    FVectorMatrix: IVectorTileMatrixChangeable;
    FFindItemsCounter: IInternalPerformanceCounter;
    FRectHalfSize: Double;
  private
    function FindItems(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AProjectedProvider: IGeometryProjectedProvider;
      const AVectorMatrix: IVectorTileMatrixChangeable;
      const AFindItemsCounter: IInternalPerformanceCounter;
      const ARectSize: Double
    );
  end;

implementation

uses
  SysUtils,
  Math,
  t_GeoTypes,
  i_ProjectionInfo,
  i_VectorTileMatrix,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_GeometryProjected,
  u_GeoFunc;

{ TFindVectorItemsForVectorTileMatrix }

constructor TFindVectorItemsForVectorTileMatrix.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjectedProvider: IGeometryProjectedProvider;
  const AVectorMatrix: IVectorTileMatrixChangeable;
  const AFindItemsCounter: IInternalPerformanceCounter;
  const ARectSize: Double
);
begin
  Assert(Assigned(AVectorItemSubsetBuilderFactory));
  Assert(Assigned(AProjectedProvider));
  Assert(Assigned(AVectorMatrix));
  Assert(Assigned(AFindItemsCounter));
  Assert(ARectSize > 0);
  Assert(ARectSize < 100);
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FProjectedProvider := AProjectedProvider;
  FVectorMatrix := AVectorMatrix;
  FFindItemsCounter := AFindItemsCounter;
  FRectHalfSize := ARectSize / 2;
end;

function TFindVectorItemsForVectorTileMatrix.FindItems(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VMatrix: IVectorTileMatrix;
  VRect: TDoubleRect;
  VProjection: IProjectionInfo;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  VLonLatPos: TDoublePoint;
  VTile: TPoint;
  i: integer;
  VItems: IVectorItemSubset;
  VItem: IVectorDataItem;
  VProjectdPath: IGeometryProjectedLine;
  VProjectdPolygon: IGeometryProjectedPolygon;
  VGeometry: IGeometryLonLat;
  VGeometryLine: IGeometryLonLatLine;
  VGeometryPoly: IGeometryLonLatPolygon;
  Vtmp: IVectorItemSubsetBuilder;
begin
  Result := nil;
  VCounterContext := FFindItemsCounter.StartOperation;
  try
    VMatrix := FVectorMatrix.GetStatic;
    if Assigned(VMatrix) then begin
      Vtmp := FVectorItemSubsetBuilderFactory.Build;
      VRect.Left := ALocalPoint.X - FRectHalfSize;
      VRect.Top := ALocalPoint.Y - FRectHalfSize;
      VRect.Right := ALocalPoint.X + FRectHalfSize;
      VRect.Bottom := ALocalPoint.Y + FRectHalfSize;

      VProjection := AVisualConverter.ProjectionInfo;
      VMapRect := AVisualConverter.LocalRectFloat2MapRectFloat(VRect);
      VProjection.ValidatePixelRectFloat(VMapRect);
      VLonLatRect := VProjection.PixelRectFloat2LonLatRect(VMapRect);
      VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(ALocalPoint);
      VProjection.ValidatePixelPosFloatStrict(VPixelPos, False);
      VLonLatPos := VProjection.PixelPosFloat2LonLat(VPixelPos);
      VTile := PointFromDoublePoint(VMatrix.TileRect.ProjectionInfo.LonLat2TilePosFloat(VLonLatPos), prToTopLeft);
      VItems := VMatrix.GetElementByTile(VTile);
      if Assigned(VItems) then begin
        // check element
        for i := 0 to VItems.Count - 1 do begin
          VItem := VItems.GetItem(i);
          VGeometry := VItem.Geometry;
          if VGeometry.Bounds.IsIntersecWithRect(VLonLatRect) then begin
            if Supports(VGeometry, IGeometryLonLatPoint) then begin
              Vtmp.add(VItem);
            end else if Supports(VGeometry, IGeometryLonLatLine, VGeometryLine) then begin
              VProjectdPath := FProjectedProvider.GetProjectedPath(VProjection, VGeometryLine);
              if Assigned(VProjectdPath) then begin
                if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
                  Vtmp.add(VItem);
                end;
              end;
            end else if Supports(VGeometry, IGeometryLonLatPolygon, VGeometryPoly) then begin
              VProjectdPolygon := FProjectedProvider.GetProjectedPolygon(VProjection, VGeometryPoly);
              if Assigned(VProjectdPolygon) then begin
                if VProjectdPolygon.IsPointInPolygon(VPixelPos) or
                  VProjectdPolygon.IsPointOnBorder(VPixelPos, 3) then begin
                  Vtmp.add(VItem);
                end;
              end;
            end;
          end;
        end;
        Result := Vtmp.MakeStaticAndClear;
      end;
    end;
  finally
    FFindItemsCounter.FinishOperation(VCounterContext);
  end;
end;

end.
