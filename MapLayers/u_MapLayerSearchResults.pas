unit u_MapLayerSearchResults;

interface

uses
  Windows,
  Types,
  ActiveX,
  Classes,
  Controls,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_LastSearchResultConfig,
  i_BitmapMarker,
  i_ViewPortState,
  i_GeoCoder,
  u_MapLayerBasic;

type
  TSearchResultsLayer = class(TMapLayerBasicNoBitmap)
  private
    FLastSearchResults: ILastSearchResultConfig;
    FMarkerProvider: IBitmapMarkerProviderChangeable;
    FMarkerProviderStatic: IBitmapMarkerProvider;
    FSearchResultsForm:TControl;
    procedure OnLastSearchResultsChange(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ALastSearchResults: ILastSearchResultConfig;
      AMarkerProvider: IBitmapMarkerProviderChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Math,
  i_CoordConverter,
  u_GeoFun,
  u_NotifyEventListener;

{ TSearchResultsLayer }

constructor TSearchResultsLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ALastSearchResults: ILastSearchResultConfig;
  AMarkerProvider: IBitmapMarkerProviderChangeable
);
begin
  inherited Create(AParentMap, AViewPortState);
  FLastSearchResults:=ALastSearchResults;
  FMarkerProvider := AMarkerProvider;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnLastSearchResultsChange),
    FLastSearchResults.GetChangeNotifier
  );
end;

destructor TSearchResultsLayer.Destroy;
begin
  inherited;
end;

procedure TSearchResultsLayer.OnConfigChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    FMarkerProviderStatic := FMarkerProvider.GetStatic;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TSearchResultsLayer.OnLastSearchResultsChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
    SetVisible(FLastSearchResults.IsActive);
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TSearchResultsLayer.PaintLayer(ABuffer: TBitmap32;
  ALocalConverter: ILocalCoordConverter);
var
  VConverter: ICoordConverter;
  VEnum: IEnumUnknown;
  VPlacemark: IGeoCodePlacemark;
  VVisualConverter: ILocalCoordConverter;
  VTargetPoint: TDoublePoint;
  VFixedOnView: TDoublePoint;
  VMarker: IBitmapMarker;
  i:integer;
begin
  VVisualConverter := ViewCoordConverter;
  VConverter := VVisualConverter.GetGeoConverter;
  VMarker := FMarkerProviderStatic.GetMarker;

  if FLastSearchResults.GeoCodeResult<>nil then begin
    VEnum:=FLastSearchResults.GeoCodeResult.GetPlacemarks;
    while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
      VFixedOnView :=  VVisualConverter.LonLat2LocalPixelFloat(VPlacemark.GetPoint);
      VTargetPoint.X := VFixedOnView.X - (VMarker.BitmapSize.X div 2);
      VTargetPoint.Y := VFixedOnView.Y - (VMarker.BitmapSize.Y div 2);
      ABuffer.Draw(Trunc(VTargetPoint.X), Trunc(VTargetPoint.Y), VMarker.Bitmap);
    end;
  end;
end;

procedure TSearchResultsLayer.StartThreads;
begin
  inherited;
  OnLastSearchResultsChange(nil);
  OnConfigChange(nil);
end;

end.
