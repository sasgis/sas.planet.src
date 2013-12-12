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
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_LastSearchResultConfig,
  i_MarkerDrawable,
  i_VectorDataItemSimple,
  i_GeoCoder,
  u_MapLayerBasic;

type
  TSearchResultsLayer = class(TMapLayerBasicNoBitmap)
  private
    FLastSearchResults: ILastSearchResultConfig;
    FMarker: IMarkerDrawableChangeable;
    procedure OnLastSearchResultsChange;
    procedure OnConfigChange;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const ALastSearchResults: ILastSearchResultConfig;
      const AMarker: IMarkerDrawableChangeable
    );
  end;

implementation

uses
  i_CoordConverter,
  u_ListenerByEvent;

{ TSearchResultsLayer }

constructor TSearchResultsLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ALastSearchResults: ILastSearchResultConfig;
  const AMarker: IMarkerDrawableChangeable
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FLastSearchResults := ALastSearchResults;
  FMarker := AMarker;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FMarker.GetChangeNotifier
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
    Visible := FLastSearchResults.IsActive;
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
  VPlacemark: IVectorDataItemPoint;
  VFixedOnView: TDoublePoint;
  VMarker: IMarkerDrawable;
  i: integer;
  VSearchResults: IGeoCodeResult;
begin
  VSearchResults := FLastSearchResults.GeoCodeResult;
  if (VSearchResults <> nil) and (VSearchResults.GetPlacemarksCount > 0) then begin
    VMarker := FMarker.GetStatic;
    VConverter := ALocalConverter.GetGeoConverter;
    VEnum := VSearchResults.GetPlacemarks;
    while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
      VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(VPlacemark.GetPoint.Point);
      VMarker.DrawToBitmap(ABuffer, VFixedOnView);
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
