unit u_MapLayerSearchResults;

interface

uses
  Windows,
  ActiveX,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_Notifier,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_LastSearchResultConfig,
  i_BitmapMarker,
  i_ViewPortState,
  i_VectorDataItemSimple,
  i_FindVectorItems,
  i_GeoCoder,
  u_VectorDataItemPoint,
  u_HtmlToHintTextConverterStuped,
  u_MapLayerBasic;

type
  TSearchResultsLayer = class(TMapLayerBasicNoBitmap, IFindVectorItems)
  private
    FLastSearchResults: ILastSearchResultConfig;
    FMarkerProvider: IBitmapMarkerProviderChangeable;
    FMarkerProviderStatic: IBitmapMarkerProvider;
    procedure OnLastSearchResultsChange;
    procedure OnConfigChange;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure StartThreads; override;
  private
    function FindItem(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint;
      out AItemS: Double
    ): IVectorDataItemSimple;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const ALastSearchResults: ILastSearchResultConfig;
      const AMarkerProvider: IBitmapMarkerProviderChangeable
    );
  end;

implementation

uses
  Types,
  GR32_Resamplers,
  i_CoordConverter,
  u_ListenerByEvent,
  u_GeoFun;

{ TSearchResultsLayer }

constructor TSearchResultsLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const ALastSearchResults: ILastSearchResultConfig;
  const AMarkerProvider: IBitmapMarkerProviderChangeable
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState
  );
  FLastSearchResults := ALastSearchResults;
  FMarkerProvider := AMarkerProvider;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLastSearchResultsChange),
    FLastSearchResults.GetChangeNotifier
  );
end;

procedure TSearchResultsLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FMarkerProviderStatic := FMarkerProvider.GetStatic;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TSearchResultsLayer.OnLastSearchResultsChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
    SetVisible(FLastSearchResults.IsActive);
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TSearchResultsLayer.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VConverter: ICoordConverter;
  VEnum: IEnumUnknown;
  VPlacemark: IGeoCodePlacemark;
  VTargetPointFloat: TDoublePoint;
  VTargetPoint: TPoint;
  VFixedOnView: TDoublePoint;
  VMarker: IBitmapMarker;
  i: integer;
  VSearchResults: IGeoCodeResult;
begin
  VConverter := ALocalConverter.GetGeoConverter;
  VMarker := FMarkerProviderStatic.GetMarker;
  VSearchResults := FLastSearchResults.GeoCodeResult;
  if VSearchResults <> nil then begin
    VEnum := VSearchResults.GetPlacemarks;
    while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
      VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(VPlacemark.GetPoint);
      VTargetPointFloat :=
        DoublePoint(
          VFixedOnView.X - VMarker.AnchorPoint.X,
          VFixedOnView.Y - VMarker.AnchorPoint.Y
        );
      VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);
      if PtInRect(ALocalConverter.GetLocalRect, VTargetPoint) then begin
        BlockTransfer(
          ABuffer,
          VTargetPoint.X, VTargetPoint.Y,
          ABuffer.ClipRect,
          VMarker.Bitmap,
          VMarker.Bitmap.BoundsRect,
          dmBlend,
          cmBlend
        );
      end;
    end;
  end;
end;

function TSearchResultsLayer.FindItem(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint;
  out AItemS: Double
): IVectorDataItemSimple;
var
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VPixelPos: TDoublePoint;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  i: integer;
  VEnum: IEnumUnknown;
  VPlacemark: IGeoCodePlacemark;
  VMarker: IBitmapMarker;
  VSearchResults: IGeoCodeResult;
begin
  Result := nil;
  AItemS := 0;
  VSearchResults := FLastSearchResults.GeoCodeResult;
  if VSearchResults <> nil then begin
    VMarker := FMarkerProviderStatic.GetMarker;
    VRect.Left := ALocalPoint.X - (VMarker.Bitmap.Width div 2);
    VRect.Top := ALocalPoint.Y - (VMarker.Bitmap.Height div 2);
    VRect.Right := ALocalPoint.X + (VMarker.Bitmap.Width div 2);
    VRect.Bottom := ALocalPoint.Y + (VMarker.Bitmap.Height div 2);
    VConverter := AVisualConverter.GetGeoConverter;
    VZoom := AVisualConverter.GetZoom;
    VMapRect := AVisualConverter.LocalRect2MapRectFloat(VRect);
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(ALocalPoint);
    VEnum := VSearchResults.GetPlacemarks;
    while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
      if LonLatPointInRect(VPlacemark.GetPoint, VLonLatRect) then begin
        Result :=
          TVectorDataItemPoint.Create(
            THtmlToHintTextConverterStuped.Create,
            VPlacemark.GetAddress + #13#10 + VPlacemark.GetDesc,
            VPlacemark.GetFullDesc,
            VPlacemark.GetPoint
          );
        AItemS := 0;
        exit;
      end;
    end;
  end;
end;

procedure TSearchResultsLayer.StartThreads;
begin
  inherited;
  OnLastSearchResultsChange;
  OnConfigChange;
end;

end.
