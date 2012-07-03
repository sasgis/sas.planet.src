unit u_PointOnMapEditLayer;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_InternalPerformanceCounter,
  i_BitmapMarker,
  i_ViewPortState,
  i_LocalCoordConverter,
  i_PointOnMapEdit,
  u_MapLayerBasic;

type
  TPointOnMapEditLayer = class(TMapLayerBasicNoBitmap)
  private
    FPointOnMap: IPointOnMapEdit;
    FMarkerProvider: IBitmapMarkerProviderChangeable;

    procedure OnPointChange;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AMarkerProvider: IBitmapMarkerProviderChangeable;
      const APointOnMap: IPointOnMapEdit
    );
  end;

implementation

uses
  Types,
  GR32_Resamplers,
  i_Notifier, i_Listener,
  i_CoordConverter,
  u_ListenerByEvent,
  u_GeoFun;

{ TPointOnMapEditLayer }

constructor TPointOnMapEditLayer.Create(
  const APerfList: IInternalPerformanceCounterList; AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AMarkerProvider: IBitmapMarkerProviderChangeable;
  const APointOnMap: IPointOnMapEdit);
var
  VListener: IListener;
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FPointOnMap := APointOnMap;
  FMarkerProvider := AMarkerProvider;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnPointChange);
  LinksList.Add(
    VListener,
    FPointOnMap.GetChangeNotifier
  );

  LinksList.Add(
    VListener,
    FMarkerProvider.GetChangeNotifier
  );
end;

procedure TPointOnMapEditLayer.OnPointChange;
var
  VPoint: TDoublePoint;
begin
  VPoint := FPointOnMap.Point;
  ViewUpdateLock;
  try
    SetVisible(not PointIsEmpty(VPoint));
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TPointOnMapEditLayer.PaintLayer(ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter);
var
  VConverter: ICoordConverter;
  VMarker: IBitmapMarker;
  VGotoLonLat: TDoublePoint;
  VFixedOnView: TDoublePoint;
  VTargetPoint: TPoint;
  VTargetPointFloat: TDoublePoint;
begin
  inherited;
  VGotoLonLat := FPointOnMap.Point;
  if not PointIsEmpty(VGotoLonLat) then begin
    VConverter := ALocalConverter.GetGeoConverter;
    VMarker := FMarkerProvider.GetStatic.GetMarker;
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(VGotoLonLat);
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

procedure TPointOnMapEditLayer.StartThreads;
begin
  inherited;
  OnPointChange;
end;

end.


