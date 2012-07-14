unit u_PointOnMapEditLayer;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_MarkerDrawable,
  i_ViewPortState,
  i_LocalCoordConverter,
  i_PointOnMapEdit,
  u_MapLayerBasic;

type
  TPointOnMapEditLayer = class(TMapLayerBasicNoBitmap)
  private
    FPointOnMap: IPointOnMapEdit;
    FMarker: IMarkerDrawableChangeable;

    procedure OnPointChange;
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
      const AViewPortState: IViewPortState;
      const AMarker: IMarkerDrawableChangeable;
      const APointOnMap: IPointOnMapEdit
    );
  end;

implementation

uses
  i_Listener,
  i_CoordConverter,
  u_ListenerByEvent,
  u_GeoFun;

{ TPointOnMapEditLayer }

constructor TPointOnMapEditLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AMarker: IMarkerDrawableChangeable;
  const APointOnMap: IPointOnMapEdit);
var
  VListener: IListener;
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState
  );
  FPointOnMap := APointOnMap;
  FMarker := AMarker;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnPointChange);
  LinksList.Add(
    VListener,
    FPointOnMap.GetChangeNotifier
  );

  LinksList.Add(
    VListener,
    FMarker.ChangeNotifier
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
  VMarker: IMarkerDrawable;
  VGotoLonLat: TDoublePoint;
  VFixedOnView: TDoublePoint;
begin
  inherited;
  VGotoLonLat := FPointOnMap.Point;
  if not PointIsEmpty(VGotoLonLat) then begin
    VConverter := ALocalConverter.GetGeoConverter;
    VMarker := FMarker.GetStatic;
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(VGotoLonLat);
    VMarker.DrawToBitmap(ABuffer, VFixedOnView);
  end;
end;

procedure TPointOnMapEditLayer.StartThreads;
begin
  inherited;
  OnPointChange;
end;

end.


