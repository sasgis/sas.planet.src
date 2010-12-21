unit u_MapLayerBasic;

interface

uses
  Windows,
  GR32,
  GR32_Layers,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  u_MapViewPortState,
  u_WindowLayerBasic,
  u_WindowLayerWithPos;

type
  TMapLayerBasicNoBitmap = class(TWindowLayerWithPos)
  protected
    FScaleChangeListener: IJclListener;

    procedure OnScaleChange(Sender: TObject);
    procedure DoShow; override;
    function GetMapLayerLocationRect: TFloatRect; override;
  public
    constructor Create(ALayer: TPositionedLayer; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
  end;

  TMapLayerFixedWithBitmap = class(TWindowLayerBasicFixedSizeWithBitmap)
  protected
    FVisualCoordConverter: ILocalCoordConverter;
    FPosChangeListener: IJclListener;
    FScaleChangeListener: IJclListener;
    FFixedLonLat: TDoublePoint;
    FFixedOnBitmap: TDoublePoint;
    procedure OnScaleChange(Sender: TObject);
    procedure OnPosChange(Sender: TObject); virtual;
    procedure PosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    function GetMapLayerLocationRect: TFloatRect; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
  end;

  TMapLayerBasic = class(TMapLayerBasicNoBitmap)
  protected
    FBitmapCoordConverter: ILocalCoordConverter;
    FLayer: TBitmapLayer;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoUpdateLayerSize(ANewSize: TPoint); override;
    procedure DoHide; override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
  end;

implementation

uses
  Types,
  Forms,
  Graphics,
  u_NotifyEventListener,
  Ugeofun,
  u_GlobalState;

{ TMapLayerBasicNoBitmap }

constructor TMapLayerBasicNoBitmap.Create(ALayer: TPositionedLayer; AViewPortState: TMapViewPortState);
begin
  inherited;
  FScaleChangeListener := TNotifyEventListener.Create(Self.OnScaleChange);
  FViewPortState.ScaleChangeNotifier.Add(FScaleChangeListener);
end;

destructor TMapLayerBasicNoBitmap.Destroy;
begin
  FViewPortState.ScaleChangeNotifier.Remove(FScaleChangeListener);
  FScaleChangeListener := nil;
  FVisualCoordConverter := nil;
  inherited;
end;

procedure TMapLayerBasicNoBitmap.OnScaleChange(Sender: TObject);
begin
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter;
  UpdateLayerLocation(GetMapLayerLocationRect);
end;

procedure TMapLayerBasicNoBitmap.DoShow;
begin
  inherited;
  OnViewSizeChange(nil);
end;

function TMapLayerBasicNoBitmap.GetMapLayerLocationRect: TFloatRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := MapViewSize.X;
  Result.Bottom := MapViewSize.Y;
end;

{ TMapLayerBasic }

constructor TMapLayerBasic.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);
  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
  FBitmapCoordConverter := FVisualCoordConverter;
end;

procedure TMapLayerBasic.DoUpdateLayerSize(ANewSize: TPoint);
var
  VBitmapSizeInPixel: TPoint;
begin
  inherited;
  VBitmapSizeInPixel := MapViewSize;
  FLayer.Bitmap.Lock;
  try
    if (FLayer.Bitmap.Width <> VBitmapSizeInPixel.X) or (FLayer.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
      FLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
    end;
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

function TMapLayerBasic.GetMapLayerLocationRect: TFloatRect;
var
  VBitmapRect: TDoubleRect;
  VBitmapOnMapRect: TDoubleRect;
  VBitmapOnVisualRect: TDoubleRect;
begin
  VBitmapRect := DoubleRect(0, 0, MapViewSize.X, MapViewSize.Y);
  VBitmapOnMapRect := FBitmapCoordConverter.LocalRectFloat2MapRectFloat(VBitmapRect);
  VBitmapOnVisualRect := FVisualCoordConverter.MapRectFloat2LocalRectFloat(VBitmapOnMapRect);
  Result := FloatRect(VBitmapOnVisualRect.Left, VBitmapOnVisualRect.Top, VBitmapOnVisualRect.Right, VBitmapOnVisualRect.Bottom);
end;

procedure TMapLayerBasic.DoHide;
begin
  inherited;
  FLayer.Bitmap.Lock;
  try
    FLayer.Bitmap.SetSize(0, 0);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

procedure TMapLayerBasic.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  FBitmapCoordConverter :=  FVisualCoordConverter;
  UpdateLayerLocation(GetMapLayerLocationRect);
  Redraw;
end;

{ TMapLayerFixedWithBitmap }

constructor TMapLayerFixedWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FPosChangeListener := TNotifyEventListener.Create(Self.OnPosChange);
  FViewPortState.PosChangeNotifier.Add(FPosChangeListener);
  FScaleChangeListener := TNotifyEventListener.Create(Self.OnScaleChange);
  FViewPortState.ScaleChangeNotifier.Add(FScaleChangeListener);
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter
end;

destructor TMapLayerFixedWithBitmap.Destroy;
begin
  FViewPortState.PosChangeNotifier.Remove(FPosChangeListener);
  FPosChangeListener := nil;
  FViewPortState.ScaleChangeNotifier.Remove(FScaleChangeListener);
  FScaleChangeListener := nil;
  FVisualCoordConverter := nil;
  inherited;
end;

procedure TMapLayerFixedWithBitmap.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  FVisualCoordConverter := ANewVisualCoordConverter;
  UpdateLayerLocation(GetMapLayerLocationRect);
end;

function TMapLayerFixedWithBitmap.GetMapLayerLocationRect: TFloatRect;
var
  VFixedVisualPixel: TDoublePoint;
  VBitmapSize: TPoint;
begin
  VBitmapSize := GetBitmapSizeInPixel;
  VFixedVisualPixel := FVisualCoordConverter.LonLat2LocalPixelFloat(FFixedLonLat);
  if (Abs(VFixedVisualPixel.X) < (1 shl 15)) and (Abs(VFixedVisualPixel.Y) < (1 shl 15)) then begin
    Result.Left := VFixedVisualPixel.X - FFixedOnBitmap.X;
    Result.Top := VFixedVisualPixel.Y - FFixedOnBitmap.Y;
    Result.Right := Result.Left + VBitmapSize.X;
    Result.Bottom := Result.Top + VBitmapSize.Y;
  end else begin
    Result.Left := - VBitmapSize.X;
    Result.Top := - VBitmapSize.Y;
    Result.Right := 0;
    Result.Bottom := 0;
  end;
end;

procedure TMapLayerFixedWithBitmap.OnPosChange(Sender: TObject);
begin
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter;
  UpdateLayerLocation(GetMapLayerLocationRect);
end;

procedure TMapLayerFixedWithBitmap.OnScaleChange(Sender: TObject);
begin
  PosChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TMapLayerFixedWithBitmap.PosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  if Visible then begin
    DoPosChange(ANewVisualCoordConverter);
  end;
end;

end.
