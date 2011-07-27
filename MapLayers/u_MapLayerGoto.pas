unit u_MapLayerGoto;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_ViewPortState,
  i_GotoLayerConfig,
  u_MapLayerBasic;

type
  TGotoLayer = class(TMapLayerFixedWithBitmap)
  private
    FConfig: IGotoLayerConfig;
    FShowTickCount: Cardinal;
    FShowTick: Cardinal;

    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IGotoLayerConfig
    );
    procedure ShowGotoIcon(APoint: TDoublePoint);
  end;



implementation

uses
  SysUtils,
  u_NotifyEventListener,
  u_GeoFun;

{ TGotoLayer }

constructor TGotoLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IGotoLayerConfig
);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  OnConfigChange(nil);
end;

procedure TGotoLayer.DoUpdateLayerLocation(ANewLocation: TFloatRect);
var
  VCurrTime: Cardinal;
begin
  if FShowTick <> 0 then begin
    VCurrTime := GetTickCount;
    if (FShowTick <= VCurrTime) then begin
      if (VCurrTime < FShowTick + FShowTickCount) then begin
        inherited;
      end else begin
        Visible := False;
        FShowTick := 0;
      end;
    end else begin
      Visible := False;
      FShowTick := 0;
    end;
  end else begin
    Visible := False;
  end;
end;

procedure TGotoLayer.OnConfigChange(Sender: TObject);
var
  VBitmap: TCustomBitmap32;
  VBitmapSize: TPoint;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      VBitmap := FConfig.GetMarker;
      try
        FLayer.Bitmap.Assign(VBitmap);
        FLayer.Bitmap.DrawMode := dmBlend;
        VBitmapSize.X := VBitmap.Width;
        VBitmapSize.Y := VBitmap.Height;
        FFixedOnBitmap := DoublePoint(FConfig.MarkerFixedPoint);
        FShowTickCount := FConfig.ShowTickCount;
      finally
        VBitmap.Free;
      end;
    finally
      FConfig.UnlockRead;
    end;
    DoUpdateLayerSize(VBitmapSize);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TGotoLayer.ShowGotoIcon(APoint: TDoublePoint);
begin
  FFixedLonLat := APoint;
  FShowTick := GetTickCount;
  Visible := True;
  UpdateLayerLocation;
end;

end.
