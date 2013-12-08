unit u_FindVectorItemsForVectorMaps;

interface

uses
  Types,
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
    function MouseOnElements(
      const AVisualConverter: ILocalCoordConverter;
      const ACopiedElements: IVectorItemSubset;
      const xy: TPoint
    ): IVectorItemSubset;
  private
    function FindItems(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AProjectedProvider: IProjectedGeometryProvider;
      const AVectorItems: IVectorItemSubsetChangeable
    );
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_CoordConverter,
  i_VectorDataItemSimple,
  i_VectorItemProjected;

{ TFindVectorItemsForVectorMaps }

constructor TFindVectorItemsForVectorMaps.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjectedProvider: IProjectedGeometryProvider;
  const AVectorItems: IVectorItemSubsetChangeable);
begin
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FProjectedProvider := AProjectedProvider;
  FVectorItems := AVectorItems;
end;

function TFindVectorItemsForVectorMaps.FindItems(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;
var
  VElements: IVectorItemSubset;
begin
  Result := nil;
  VElements := FVectorItems.GetStatic;
  if VElements <> nil then begin
    Result := MouseOnElements(AVisualConverter, VElements, ALocalPoint);
  end;
end;

function TFindVectorItemsForVectorMaps.MouseOnElements(
  const AVisualConverter: ILocalCoordConverter;
  const ACopiedElements: IVectorItemSubset;
  const xy: TPoint
): IVectorItemSubset;
var
  VRect: TRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  i: integer;
  VItem: IVectorDataItemSimple;
  VProjectdPath: IProjectedPath;
  VItemLine: IVectorDataItemLine;
  VItemPoly: IVectorDataItemPoly;
  VProjectdPolygon: IProjectedPolygon;
  Vtmp: IVectorItemSubsetBuilder;
begin
  Result := nil;
  Vtmp := FVectorItemSubsetBuilderFactory.Build;

  if ACopiedElements.Count > 0 then begin
    VRect.Left := xy.X - 3;
    VRect.Top := xy.Y - 3;
    VRect.Right := xy.X + 3;
    VRect.Bottom := xy.Y + 3;

    VConverter := AVisualConverter.GetGeoConverter;
    VZoom := AVisualConverter.GetZoom;
    VMapRect := AVisualConverter.LocalRect2MapRectFloat(VRect);
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(xy);

    // check element
    for i := 0 to ACopiedElements.Count - 1 do begin
      VItem := ACopiedElements.GetItem(i);
      if VItem.LLRect.IsIntersecWithRect(VLonLatRect) then begin
        if Supports(VItem, IVectorDataItemPoint) then begin
          Vtmp.add(VItem);
        end else if Supports(VItem, IVectorDataItemLine, VItemLine) then begin
          VProjectdPath := FProjectedProvider.GetProjectedPath(AVisualConverter.ProjectionInfo,  VItemLine.Line);
          if Assigned(VProjectdPath) then begin
            if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
              Vtmp.add(VItem);
            end;
          end;
        end else if Supports(VItem, IVectorDataItemPoly, VItemPoly) then begin
          VProjectdPolygon := FProjectedProvider.GetProjectedPolygon(AVisualConverter.ProjectionInfo,  VItemPoly.Line);
          if Assigned(VProjectdPolygon) then begin
            if VProjectdPolygon.IsPointInPolygon(VPixelPos) then begin
              Vtmp.add(VItem);
            end;
          end;
        end;
      end;
    end;
  end;
  Result := Vtmp.MakeStaticAndClear;
end;

end.
