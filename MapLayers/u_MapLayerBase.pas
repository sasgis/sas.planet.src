unit u_MapLayerBase;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  i_ICoordConverter,
  i_IPosChangeMessage,
  u_MapViewPortState,
  u_WindowLayerBasic;

type
  TMapLayerBase = class(TWindowLayerBasic)
  protected
    FScreenCenterPos: TPoint;
    FZoom: Byte;
    FGeoConvert: ICoordConverter;
    FViewSize: TPoint;

    FMapPosChangeListener: IJclListener;
    FViewScaleChangeListener: IJclListener;
    procedure ProcessPosChange(AMessage: IPosChangeMessage); virtual;
    procedure ProcessViewScaleChange(AMessage: IJclNotificationMessage); virtual;
    procedure UpdatelLayerLocation; virtual; abstract;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
  end;

implementation

uses
  u_JclNotify;

{ TListenerOfMapLayer }

type
  TListenerOfMapLayer = class(TJclBaseListener)
  protected
    FMapLayer: TMapLayerBase;
  public
    constructor Create(AMapLayer: TMapLayerBase);
  end;

constructor TListenerOfMapLayer.Create(AMapLayer: TMapLayerBase);
begin
  FMapLayer := AMapLayer;
end;

{ TChangePosListener }

type
  TChangePosListener = class(TListenerOfMapLayer)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TChangePosListener.Notification(
  msg: IJclNotificationMessage);
var
  VMessage: IPosChangeMessage;
begin
  VMessage := msg as IPosChangeMessage;
  FMapLayer.ProcessPosChange(VMessage);
end;

{ TChangeViewScaleListener }

type
  TChangeViewScaleListener = class(TListenerOfMapLayer)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TChangeViewScaleListener.Notification(
  msg: IJclNotificationMessage);
begin
  FMapLayer.ProcessViewScaleChange(msg);
end;

{ TMapLayerBase }

constructor TMapLayerBase.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FMapPosChangeListener := TChangePosListener.Create(Self);
  FViewPortState.PosChangeNotifier.Add(FMapPosChangeListener);
  FViewScaleChangeListener := TChangeViewScaleListener.Create(Self);
  FViewPortState.ScaleChangeNotifier.Add(FViewScaleChangeListener);
end;

destructor TMapLayerBase.Destroy;
begin
  FViewPortState.PosChangeNotifier.Remove(FMapPosChangeListener);
  FMapPosChangeListener := nil;
  FViewPortState.PosChangeNotifier.Remove(FViewScaleChangeListener);
  FViewScaleChangeListener := nil;
  inherited;
end;

procedure TMapLayerBase.ProcessPosChange(AMessage: IPosChangeMessage);
begin

end;

procedure TMapLayerBase.ProcessViewScaleChange(
  AMessage: IJclNotificationMessage);
begin

end;

end.
