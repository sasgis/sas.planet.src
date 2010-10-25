unit u_MapLayerBase;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  i_ICoordConverter,
  i_IPosChangeMessage,
  UMapType,
  u_MapViewPortState,
  u_WindowLayerBasic;

type
  TMapLayerBase = class(TWindowLayerBasic)
  protected
    FScreenCenterPos: TPoint;
    FZoom: Byte;
    FGeoConvert: ICoordConverter;
    FMapType: TMapType;
    FViewSize: TPoint;

    FMapPosChangeListener: IJclListener;
    FViewScaleChangeListener: IJclListener;
    procedure ProcessPosChange(AMessage: IPosChangeMessage); virtual;
    procedure UpdatelLayerLocation; virtual;
    procedure DoUpdatelLayerLocation; virtual; abstract;
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
  FMapLayer.UpdatelLayerLocation;
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
var
  VNeedRedraw: Boolean;
  VNewPos: TPoint;
  VNewSize: TPoint;
begin
  if Visible then begin
    VNeedRedraw := False;
    if FZoom <> AMessage.GetZoom then begin
      FZoom := AMessage.GetZoom;
      VNeedRedraw := True;
    end;

    VNewPos := AMessage.GetMapPixel;
    if (FScreenCenterPos.X <> VNewPos.X) or (FScreenCenterPos.Y <> VNewPos.Y) then begin
      FScreenCenterPos := VNewPos;
      VNeedRedraw := True;
    end;

    if not FGeoConvert.IsSameConverter(AMessage.GetCoordConverter) then begin
      FGeoConvert := AMessage.GetCoordConverter;
      VNeedRedraw := True;
    end;

    VNewSize := AMessage.GetViewSize;
    if (FViewSize.X <> VNewSize.X) or (FViewSize.Y <> VNewSize.Y) then begin
      FViewSize := VNewSize;
      VNeedRedraw := True;
    end;

    if FMapType <> AMessage.GetMap then begin
      FMapType := AMessage.GetMap;
      VNeedRedraw := True;
    end;

    if VNeedRedraw then begin
      Redraw;
    end;
    UpdatelLayerLocation;
  end;
end;

procedure TMapLayerBase.UpdatelLayerLocation;
begin
  if Visible then begin
    DoUpdatelLayerLocation;
  end;
end;

end.
