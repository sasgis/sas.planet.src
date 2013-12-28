unit u_FindVectorItemsFromSearchResults;

interface

uses
  Types,
  i_LastSearchResultConfig,
  i_LocalCoordConverter,
  i_VectorDataFactory,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_FindVectorItems,
  u_BaseInterfacedObject;

type
  TFindVectorItemsFromSearchResults = class(TBaseInterfacedObject, IFindVectorItems)
  private
    FVectorDataFactory: IVectorDataFactory;
    FLastSearchResults: ILastSearchResultConfig;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  private
    function FindItems(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const ALastSearchResults: ILastSearchResultConfig
    );
  end;

implementation

uses
  ActiveX,
  t_GeoTypes,
  i_CoordConverter,
  i_GeoCoder,
  i_VectorDataItemSimple,
  u_GeoFun;

{ TFindVectorItemsFromSearchResults }

constructor TFindVectorItemsFromSearchResults.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const ALastSearchResults: ILastSearchResultConfig
);
begin
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FVectorDataFactory := AVectorDataFactory;
  FLastSearchResults := ALastSearchResults;
end;

function TFindVectorItemsFromSearchResults.FindItems(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;
var
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VPixelPos: TDoublePoint;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  i: integer;
  VEnum: IEnumUnknown;
  VPlacemark: IVectorDataItemSimple;
  VSearchResults: IGeoCodeResult;
  Vtmp: IVectorItemSubsetBuilder;
begin
  Result := nil;
  VSearchResults := FLastSearchResults.GeoCodeResult;
  if (VSearchResults <> nil) and (VSearchResults.Count > 0) then begin
    Vtmp := FVectorItemSubsetBuilderFactory.Build;
    VRect.Left := ALocalPoint.X - 5;
    VRect.Top := ALocalPoint.Y - 5;
    VRect.Right := ALocalPoint.X + 5;
    VRect.Bottom := ALocalPoint.Y + 5;
    VConverter := AVisualConverter.GetGeoConverter;
    VZoom := AVisualConverter.GetZoom;
    VMapRect := AVisualConverter.LocalRect2MapRectFloat(VRect);
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(ALocalPoint);
    VEnum := VSearchResults.GetEnum;
    while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
      if VPlacemark.Geometry.Bounds.IsIntersecWithRect(VLonLatRect) then begin
        Vtmp.add(VPlacemark);
      end;
    end;
    Result := Vtmp.MakeStaticAndClear;
  end;
end;

end.
