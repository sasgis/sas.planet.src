unit u_PointOnMapEditLayer;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_MarkerDrawable,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_PointOnMapEdit,
  u_WindowLayerWithPos;

type
  TPointOnMapEditLayer = class(TWindowLayerBasicBase)
  private
    FLocalConverter: ILocalCoordConverterChangeable;
    FPointOnMap: IPointOnMapEdit;
    FMarker: IMarkerDrawableChangeable;

    procedure OnPointChange;
    procedure OnPosChange;
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const ALocalConverter: ILocalCoordConverterChangeable;
      const AMarker: IMarkerDrawableChangeable;
      const APointOnMap: IPointOnMapEdit
    );
  end;

implementation

uses
  GR32_Layers,
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
  const ALocalConverter: ILocalCoordConverterChangeable;
  const AMarker: IMarkerDrawableChangeable;
  const APointOnMap: IPointOnMapEdit);
var
  VListener: IListener;
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TCustomLayer.Create(AParentMap.Layers)
  );
  FPointOnMap := APointOnMap;
  FLocalConverter := ALocalConverter;
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
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalConverter.ChangeNotifier
  );
end;

procedure TPointOnMapEditLayer.OnPointChange;
var
  VPoint: TDoublePoint;
begin
  VPoint := FPointOnMap.Point;
  ViewUpdateLock;
  try
    Visible := not PointIsEmpty(VPoint);
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TPointOnMapEditLayer.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TPointOnMapEditLayer.PaintLayer(ABuffer: TBitmap32);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VMarker: IMarkerDrawable;
  VLonLat: TDoublePoint;
  VFixedOnView: TDoublePoint;
begin
  inherited;
  VLocalConverter := FLocalConverter.GetStatic;
  VLonLat := FPointOnMap.Point;
  if not PointIsEmpty(VLonLat) then begin
    VConverter := VLocalConverter.GetGeoConverter;
    VConverter.CheckLonLatPos(VLonLat);
    VMarker := FMarker.GetStatic;
    VFixedOnView := VLocalConverter.LonLat2LocalPixelFloat(VLonLat);
    VMarker.DrawToBitmap(ABuffer, VFixedOnView);
  end;
end;

procedure TPointOnMapEditLayer.StartThreads;
begin
  inherited;
  OnPointChange;
end;

end.


