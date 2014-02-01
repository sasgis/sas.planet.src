unit u_FindVectorItemsForVectorMaps;

interface

uses
  Types,
  i_InternalPerformanceCounter,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_VectorItemSubsetChangeable,
  i_LocalCoordConverter,
  i_ProjectedGeometryProvider,
  i_FindVectorItems,
  u_BaseInterfacedObject;

type
  TFindVectorItemsForVectorMaps = class(TBaseInterfacedObject, IFindVectorItems)
  private
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FProjectedProvider: IProjectedGeometryProvider;
    FVectorItems: IVectorItemSubsetChangeable;
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
      const AProjectedProvider: IProjectedGeometryProvider;
      const AVectorItems: IVectorItemSubsetChangeable;
      const AFindItemsCounter: IInternalPerformanceCounter;
      const ARectSize: Double
    );
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_CoordConverter,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_GeometryProjected;

{ TFindVectorItemsForVectorMaps }

constructor TFindVectorItemsForVectorMaps.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjectedProvider: IProjectedGeometryProvider;
  const AVectorItems: IVectorItemSubsetChangeable;
  const AFindItemsCounter: IInternalPerformanceCounter;
  const ARectSize: Double
);
begin
  Assert(Assigned(AVectorItemSubsetBuilderFactory));
  Assert(Assigned(AProjectedProvider));
  Assert(Assigned(AVectorItems));
  Assert(Assigned(AFindItemsCounter));
  Assert(ARectSize > 0);
  Assert(ARectSize < 100);
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FProjectedProvider := AProjectedProvider;
  FVectorItems := AVectorItems;
  FFindItemsCounter := AFindItemsCounter;
  FRectHalfSize := ARectSize / 2;
end;

function TFindVectorItemsForVectorMaps.FindItems(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VElements: IVectorItemSubset;
  VRect: TDoubleRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  i: integer;
  VItem: IVectorDataItemSimple;
  VProjectdPath: IGeometryProjectedMultiLine;
  VProjectdPolygon: IGeometryProjectedMultiPolygon;
  VGeometry: IGeometryLonLat;
  VGeometryLine: IGeometryLonLatMultiLine;
  VGeometryPoly: IGeometryLonLatMultiPolygon;
  Vtmp: IVectorItemSubsetBuilder;
begin
  Result := nil;
  VCounterContext := FFindItemsCounter.StartOperation;
  try
    VElements := FVectorItems.GetStatic;
    if VElements <> nil then begin
      if VElements.Count > 0 then begin
        Vtmp := FVectorItemSubsetBuilderFactory.Build;
        VRect.Left := ALocalPoint.X - FRectHalfSize;
        VRect.Top := ALocalPoint.Y - FRectHalfSize;
        VRect.Right := ALocalPoint.X + FRectHalfSize;
        VRect.Bottom := ALocalPoint.Y + FRectHalfSize;

        VConverter := AVisualConverter.GetGeoConverter;
        VZoom := AVisualConverter.GetZoom;
        VMapRect := AVisualConverter.LocalRectFloat2MapRectFloat(VRect);
        VConverter.CheckPixelRectFloat(VMapRect, VZoom);
        VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
        VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(ALocalPoint);

        // check element
        for i := 0 to VElements.Count - 1 do begin
          VItem := VElements.GetItem(i);
          VGeometry := VItem.Geometry;
          if VGeometry.Bounds.IsIntersecWithRect(VLonLatRect) then begin
            if Supports(VGeometry, IGeometryLonLatPoint) then begin
              Vtmp.add(VItem);
            end else if Supports(VGeometry, IGeometryLonLatMultiLine, VGeometryLine) then begin
              VProjectdPath := FProjectedProvider.GetProjectedPath(AVisualConverter.ProjectionInfo,  VGeometryLine);
              if Assigned(VProjectdPath) then begin
                if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
                  Vtmp.add(VItem);
                end;
              end;
            end else if Supports(VGeometry, IGeometryLonLatMultiPolygon, VGeometryPoly) then begin
              VProjectdPolygon := FProjectedProvider.GetProjectedPolygon(AVisualConverter.ProjectionInfo,  VGeometryPoly);
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
