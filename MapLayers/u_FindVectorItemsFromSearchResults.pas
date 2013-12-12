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
  SysUtils,
  t_GeoTypes,
  c_InternalBrowser,
  i_CoordConverter,
  i_GeoCoder,
  i_VectorDataItemSimple,
  u_GeoCodePlacemarkWithUrlDecorator,
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
  VPlacemark: IVectorDataItemPoint;
  VPlacemarkInfo: IGeoCodePlacemarkInfo;
  VSearchResults: IGeoCodeResult;
  VIndex: Integer;
  Vtmp: IVectorItemSubsetBuilder;
  VTempItem: IVectorDataItemPoint;
begin
  Result := nil;
  VSearchResults := FLastSearchResults.GeoCodeResult;
  if (VSearchResults <> nil) and (VSearchResults.GetPlacemarksCount > 0) then begin
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
    VIndex := 0;
    VEnum := VSearchResults.GetPlacemarks;
    while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
      if Supports(VPlacemark.MainInfo, IGeoCodePlacemarkInfo, VPlacemarkInfo) then begin
        if LonLatPointInRect(VPlacemark.GetPoint.Point, VLonLatRect) then begin
          VTempItem :=
            FVectorDataFactory.BuildPoint(
              TGeoCodePlacemarkInfoWithUrlDecorator.Create(VPlacemarkInfo, CLastSearchResultsInternalURL + IntToStr(VIndex) + '/'),
              VPlacemark.Appearance,
              VPlacemark.Point
            );
          Vtmp.add(VTempItem);
        end;
      end;
      Inc(VIndex);
    end;
    Result := Vtmp.MakeStaticAndClear;
  end;
end;

end.
