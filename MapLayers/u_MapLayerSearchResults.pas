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
  i_VectorDataItemSimple,
  i_GeoCoder,
  u_VectorDataItemPoint,
  u_HtmlToHintTextConverterStuped,
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
    procedure MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple; out AItemS: Double);
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
      VTargetPoint.X := VFixedOnView.X - VMarker.AnchorPoint.X;
      VTargetPoint.Y := VFixedOnView.Y - VMarker.AnchorPoint.Y;
      ABuffer.Draw(Trunc(VTargetPoint.X), Trunc(VTargetPoint.Y), VMarker.Bitmap);
    end;
  end;
end;

procedure TSearchResultsLayer.MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple;
  out AItemS: Double);
var
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VMarkLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
  VVisualConverter: ILocalCoordConverter;
  i: integer;
  VEnum: IEnumUnknown;
  VPlacemark: IGeoCodePlacemark;
  VMarker: IBitmapMarker;
begin
  AItem := nil;
  AItemS := 0;
  if FLastSearchResults.GeoCodeResult<>nil then begin
    VMarker := FMarkerProviderStatic.GetMarker;
    VRect.Left := xy.X - (VMarker.Bitmap.Width div 2);
    VRect.Top := xy.Y - (VMarker.Bitmap.Height div 2);
    VRect.Right := xy.X + (VMarker.Bitmap.Width div 2);
    VRect.Bottom := xy.Y + (VMarker.Bitmap.Height div 2);
    VLocalConverter := LayerCoordConverter;
    VConverter := VLocalConverter.GetGeoConverter;
    VZoom := VLocalConverter.GetZoom;
    VVisualConverter := ViewCoordConverter;
    VMapRect := VVisualConverter.LocalRect2MapRectFloat(VRect);
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VPixelPos := VVisualConverter.LocalPixel2MapPixelFloat(xy);
    VEnum:=FLastSearchResults.GeoCodeResult.GetPlacemarks;
    while VEnum.Next(1, VPlacemark, @i) = S_OK do begin
      if((VLonLatRect.Right>VPlacemark.GetPoint.X)and(VLonLatRect.Left<VPlacemark.GetPoint.X)and
        (VLonLatRect.Bottom<VPlacemark.GetPoint.Y)and(VLonLatRect.Top>VPlacemark.GetPoint.Y))then begin
         AItem := TVectorDataItemPoint.Create(THtmlToHintTextConverterStuped.Create,VPlacemark.GetAddress+#13#10+VPlacemark.GetDesc,VPlacemark.GetFullDesc,VPlacemark.GetPoint);
         AItemS := 0;
         exit;
      end;
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
