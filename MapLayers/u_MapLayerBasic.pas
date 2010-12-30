unit u_MapLayerBasic;

interface

uses
  Windows,
  GR32,
  GR32_Layers,
  GR32_Image,
  t_GeoTypes,
  i_ILocalCoordConverter,
  u_MapViewPortState,
  u_WindowLayerWithPos;

type
  TMapLayerBase = class(TWindowLayerBasic)
  private
    procedure OnScaleChange(Sender: TObject);
  protected
    procedure ScaleChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoScaleChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
  public
    constructor Create(ALayer: TPositionedLayer; AViewPortState: TMapViewPortState);
  end;

  TMapLayerBasicFullView = class(TMapLayerBase)
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    function GetLayerSizeForViewSize(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
  end;

  TMapLayerFixedWithBitmap = class(TMapLayerBase)
  protected
    FLayer: TBitmapLayer;
    FFixedLonLat: TDoublePoint;
    FFixedOnBitmap: TDoublePoint;
    function GetMapLayerLocationRect: TFloatRect; override;
    function GetLayerSizeForViewSize(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
  end;

  TMapLayerBasic = class(TMapLayerBasicFullView)
  protected
    FBitmapCoordConverter: ILocalCoordConverter;
    FLayer: TBitmapLayer;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoUpdateLayerSize(ANewSize: TPoint); override;
    procedure DoHide; override;
    procedure DoShow; override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
  end;

implementation

uses
  Types,
  Graphics,
  u_NotifyEventListener,
  Ugeofun;

{ TMapLayerBase }

constructor TMapLayerBase.Create(ALayer: TPositionedLayer;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnScaleChange),
    FViewPortState.ScaleChangeNotifier
  );
end;

procedure TMapLayerBase.DoScaleChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  FVisualCoordConverter := ANewVisualCoordConverter;
  UpdateLayerLocation(GetMapLayerLocationRect);
end;

procedure TMapLayerBase.OnScaleChange(Sender: TObject);
begin
  ScaleChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TMapLayerBase.ScaleChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  if Visible then begin
    DoScaleChange(ANewVisualCoordConverter);
  end;
end;

{ TMapLayerBasicNoBitmap }

function TMapLayerBasicFullView.GetLayerSizeForViewSize(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  Result := ANewVisualCoordConverter.GetLocalRectSize;
end;

function TMapLayerBasicFullView.GetMapLayerLocationRect: TFloatRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := LayerSize.X;
  Result.Bottom := LayerSize.Y;
end;

{ TMapLayerFixedWithBitmap }

constructor TMapLayerFixedWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

procedure TMapLayerFixedWithBitmap.DoRedraw;
begin
  inherited;
end;

function TMapLayerFixedWithBitmap.GetLayerSizeForViewSize(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  Result := LayerSize;
end;

function TMapLayerFixedWithBitmap.GetMapLayerLocationRect: TFloatRect;
var
  VFixedVisualPixel: TDoublePoint;
  VBitmapSize: TPoint;
begin
  VBitmapSize := LayerSize;
  if FVisualCoordConverter <> nil then begin
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
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
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
  VBitmapSizeInPixel := LayerSize;
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
  if (FBitmapCoordConverter <> nil) and (FVisualCoordConverter <> nil) then begin
    VBitmapRect := DoubleRect(0, 0, LayerSize.X, LayerSize.Y);
    VBitmapOnMapRect := FBitmapCoordConverter.LocalRectFloat2MapRectFloat(VBitmapRect);
    VBitmapOnVisualRect := FVisualCoordConverter.MapRectFloat2LocalRectFloat(VBitmapOnMapRect);
    Result := FloatRect(VBitmapOnVisualRect.Left, VBitmapOnVisualRect.Top, VBitmapOnVisualRect.Right, VBitmapOnVisualRect.Bottom);
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMapLayerBasic.DoHide;
begin
  inherited;
  UpdateLayerSize(Point(0, 0));
end;

procedure TMapLayerBasic.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  FBitmapCoordConverter :=  FVisualCoordConverter;
  UpdateLayerLocation(GetMapLayerLocationRect);
  Redraw;
end;

procedure TMapLayerBasic.DoShow;
begin
  inherited;
  FBitmapCoordConverter := FVisualCoordConverter;
end;

end.
