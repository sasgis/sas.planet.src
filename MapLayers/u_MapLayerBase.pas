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
    procedure OnChangeViewScale(Sender: TObject);
    procedure ProcessPosChange(AMessage: IPosChangeMessage); virtual;
    procedure UpdatelLayerLocation; virtual;
    procedure DoUpdatelLayerLocation; virtual; abstract;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
  end;

implementation

uses
  u_PosChangeListener,
  u_NotifyEventListener;

{ TMapLayerBase }

constructor TMapLayerBase.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FMapPosChangeListener := TPosChangeListener.Create(Self.ProcessPosChange);
  FViewPortState.PosChangeNotifier.Add(FMapPosChangeListener);
  FViewScaleChangeListener := TNotifyEventListener.Create(Self.OnChangeViewScale);
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

procedure TMapLayerBase.OnChangeViewScale(Sender: TObject);
begin
  UpdatelLayerLocation;
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

    if (FGeoConvert = nil) or not FGeoConvert.IsSameConverter(AMessage.GetCoordConverter) then begin
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
