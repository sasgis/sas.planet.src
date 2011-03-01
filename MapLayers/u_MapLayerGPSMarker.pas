unit u_MapLayerGPSMarker;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Transforms,
  GR32_Image,
  i_IMapLayerGPSMarkerConfig,
  i_IGPSModule,
  i_IViewPortState,
  u_MapLayerBasic;


type
  TMapLayerGPSMarker = class(TMapLayerFixedWithBitmap)
  private
    FConfig: IMapLayerGPSMarkerConfig;
    FGPSModule: IGPSModule;
    FTransform: TAffineTransformation;
    FAngle: Double;
    FSpeed: Double;
    procedure GPSReceiverReceive(Sender: TObject);
    procedure GPSReceiverDisconnect(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DoRedraw; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IMapLayerGPSMarkerConfig;
      AGPSModule: IGPSModule
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_GPS,
  u_NotifyEventListener;

{ TMapLayerGPSMarker }

constructor TMapLayerGPSMarker.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IMapLayerGPSMarkerConfig;
  AGPSModule: IGPSModule
);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  FGPSModule := AGPSModule;
  FTransform := TAffineTransformation.Create;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.GPSReceiverDisconnect),
    FGPSModule.DisconnectedNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.GPSReceiverReceive),
    FGPSModule.DataReciveNotifier
  );
end;

destructor TMapLayerGPSMarker.Destroy;
begin
  FreeAndNil(FTransform);
  inherited;
end;

procedure TMapLayerGPSMarker.DoRedraw;
var
  VSize: TPoint;
  VMarker: TCustomBitmap32;
begin
  inherited;
  if FSpeed > FConfig.MinMoveSpeed then begin
    FConfig.LockRead;
    try
      VMarker := FConfig.GetMarkerMoved;
      FTransform.SrcRect := FloatRect(0, 0, VMarker.Width, VMarker.Height);
      FTransform.Clear;
      FTransform.Translate(-VMarker.Width / 2, -VMarker.Height / 2);
      FTransform.Rotate(0, 0, -FAngle);
      FLayer.Bitmap.Lock;
      try
        VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
        FTransform.Translate(VSize.X / 2, VSize.Y / 2);
        FLayer.Bitmap.Clear(0);
        Transform(FLayer.Bitmap, VMarker, FTransform);
      finally
        FLayer.Bitmap.Unlock;
      end;
    finally
      FConfig.UnlockRead;
    end;
  end else begin
    FLayer.Bitmap.Lock;
    try
      FConfig.LockRead;
      try
        VMarker := FConfig.GetMarkerStoped;
        FLayer.Bitmap.Lock;
        try
          VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
          VMarker.DrawTo(
            FLayer.Bitmap,
            trunc(VSize.X / 2 - VMarker.Width / 2),
            trunc(VSize.Y / 2 - VMarker.Height / 2)
          );
        finally
          FLayer.Bitmap.Unlock;
        end;
      finally
        FConfig.UnlockRead;
      end;
    finally
      FLayer.Bitmap.Unlock;
    end;
  end;
end;

procedure TMapLayerGPSMarker.GPSReceiverDisconnect(Sender: TObject);
begin
  Hide;
end;

procedure TMapLayerGPSMarker.GPSReceiverReceive(Sender: TObject);
var
  VGPSPosition: IGPSPosition;
begin
  VGPSPosition := FGPSModule.Position;
  if VGPSPosition.IsFix = 0 then begin
    Hide;
  end else begin
    FFixedLonLat := VGPSPosition.Position;
    FAngle := VGPSPosition.Heading;
    FSpeed := VGPSPosition.Speed_KMH;
    Redraw;
    UpdateLayerLocation(GetMapLayerLocationRect);
    Show;
  end;
end;

procedure TMapLayerGPSMarker.OnConfigChange(Sender: TObject);
var
  VSize: TPoint;
  VMarker: TCustomBitmap32;
begin
  FConfig.LockRead;
  try
    VMarker := FConfig.GetMarkerMoved;
    VSize := Point(VMarker.Width, VMarker.Height);
    VMarker := FConfig.GetMarkerStoped;
    if VSize.X < VMarker.Width then begin
      VSize.X := VMarker.Width;
    end;
    if VSize.Y < VMarker.Height then begin
      VSize.Y := VMarker.Height;
    end;
    FFixedOnBitmap.X := VSize.X / 2;
    FFixedOnBitmap.Y := VSize.Y / 2;
  finally
    FConfig.UnlockRead;
  end;
  DoUpdateLayerSize(VSize);
  Redraw;
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
