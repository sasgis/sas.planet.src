unit u_MapLayerBasic;

interface

uses
  Windows,
  GR32,
  GR32_Layers,
  GR32_Image,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_ViewPortState,
  u_WindowLayerWithPos;

type
  TMapLayerBase = class(TWindowLayerBasic)
  public
    constructor Create(ALayer: TPositionedLayer; AViewPortState: IViewPortState);
  end;

  TMapLayerBasicFullView = class(TMapLayerBase)
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
  end;

  TMapLayerFixedWithBitmap = class(TMapLayerBase)
  private
    FLayerSize: TPoint;
  protected
    FLayer: TBitmapLayer;
    FFixedLonLat: TDoublePoint;
    FFixedOnBitmap: TDoublePoint;
    function GetLayerSizeForViewSize(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; virtual;
    procedure UpdateLayerSize(ANewSize: TPoint); virtual;
    procedure DoUpdateLayerSize(ANewSize: TPoint); virtual;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoRedraw; override;
    procedure DoShow; override;
    procedure AfterPosChange; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState);
  end;

  TMapLayerBasic = class(TMapLayerBasicFullView)
  private
    FBitmapCoordConverter: ILocalCoordConverter;
  protected
    FLayer: TBitmapLayer;
    function CreateBitmapCoordConverter(ANewVisualCoordConverter: ILocalCoordConverter): ILocalCoordConverter; virtual;
    procedure DoUpdateBitmapCoordConverter(ANewBitmapCoordConverter: ILocalCoordConverter); virtual;
    procedure UpdateBitmapConverterByVisual(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    property BitmapCoordConverter: ILocalCoordConverter read FBitmapCoordConverter;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoHide; override;
    procedure DoShow; override;
    procedure AfterPosChange; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState);
  end;

implementation

uses
  Types,
  Graphics,
  u_NotifyEventListener;

{ TMapLayerBase }

constructor TMapLayerBase.Create(ALayer: TPositionedLayer;
  AViewPortState: IViewPortState);
begin
  inherited Create(ALayer, AViewPortState, True);
end;

procedure TMapLayerBase.DoScaleChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  SetNeedUpdateLocation;
end;

{ TMapLayerBasicNoBitmap }

function TMapLayerBasicFullView.GetMapLayerLocationRect: TFloatRect;
begin
  if VisualCoordConverter <> nil then begin
    Result := FloatRect(VisualCoordConverter.GetLocalRect);
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

{ TMapLayerFixedWithBitmap }

constructor TMapLayerFixedWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

procedure TMapLayerFixedWithBitmap.AfterPosChange;
begin
  inherited;
  UpdateLayerSize(GetLayerSizeForViewSize(VisualCoordConverter));
end;

procedure TMapLayerFixedWithBitmap.DoRedraw;
begin
  inherited;
end;

procedure TMapLayerFixedWithBitmap.DoShow;
begin
  inherited;
  UpdateLayerSize(GetLayerSizeForViewSize(VisualCoordConverter));
end;

procedure TMapLayerFixedWithBitmap.DoUpdateLayerSize(ANewSize: TPoint);
var
  VBitmapSizeInPixel: TPoint;
begin
  FLayerSize := ANewSize;
  VBitmapSizeInPixel := FLayerSize;
  FLayer.Bitmap.Lock;
  try
    if (FLayer.Bitmap.Width <> VBitmapSizeInPixel.X) or (FLayer.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
      FLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
    end;
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

function TMapLayerFixedWithBitmap.GetLayerSizeForViewSize(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  Result := FLayerSize;
end;

function TMapLayerFixedWithBitmap.GetMapLayerLocationRect: TFloatRect;
var
  VFixedVisualPixel: TDoublePoint;
  VBitmapSize: TPoint;
  VVisualCoordConverter: ILocalCoordConverter;
begin
  VVisualCoordConverter := VisualCoordConverter;
  if VVisualCoordConverter <> nil then begin
    VFixedVisualPixel := VVisualCoordConverter.LonLat2LocalPixelFloat(FFixedLonLat);
    if (Abs(VFixedVisualPixel.X) < (1 shl 15)) and (Abs(VFixedVisualPixel.Y) < (1 shl 15)) then begin
      VBitmapSize := FLayerSize;
      Result.Left := VFixedVisualPixel.X - FFixedOnBitmap.X;
      Result.Top := VFixedVisualPixel.Y - FFixedOnBitmap.Y;
      Result.Right := Result.Left + VBitmapSize.X;
      Result.Bottom := Result.Top + VBitmapSize.Y;
    end else begin
      Result := FloatRect(0, 0, 0, 0);
    end;
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMapLayerFixedWithBitmap.UpdateLayerSize(ANewSize: TPoint);
begin
  if Visible then begin
    if (FLayerSize.X <> ANewSize.X) or (FLayerSize.Y <> ANewSize.Y) then begin
      DoUpdateLayerSize(ANewSize);
      UpdateLayerLocation;
    end;
  end;
end;

{ TMapLayerBasic }

constructor TMapLayerBasic.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);
  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
  FBitmapCoordConverter := VisualCoordConverter;
end;

function TMapLayerBasic.GetMapLayerLocationRect: TFloatRect;
var
  VBitmapOnMapRect: TDoubleRect;
  VBitmapOnVisualRect: TDoubleRect;
  VBitmapConverter: ILocalCoordConverter;
  VVisualConverter: ILocalCoordConverter;
begin
  VBitmapConverter := FBitmapCoordConverter;
  VVisualConverter := VisualCoordConverter;
  if (VBitmapConverter <> nil) and (VVisualConverter <> nil) then begin
    VBitmapOnMapRect := VBitmapConverter.GetRectInMapPixelFloat;
    VBitmapOnVisualRect := VVisualConverter.MapRectFloat2LocalRectFloat(VBitmapOnMapRect);
    Result := FloatRect(VBitmapOnVisualRect.Left, VBitmapOnVisualRect.Top, VBitmapOnVisualRect.Right, VBitmapOnVisualRect.Bottom);
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMapLayerBasic.UpdateBitmapConverterByVisual(
  ANewVisualCoordConverter: ILocalCoordConverter);
var
  VOldBitmapCoordConverter: ILocalCoordConverter;
  VNewBitmapCoordConverter: ILocalCoordConverter;
  VNeedUpdateConverter: Boolean;
begin
  if not Visible then begin
    VNewBitmapCoordConverter := nil;
  end else begin
    VNewBitmapCoordConverter := CreateBitmapCoordConverter(ANewVisualCoordConverter);
  end;
  VOldBitmapCoordConverter := FBitmapCoordConverter;
  VNeedUpdateConverter := True;
  if (VOldBitmapCoordConverter <> nil) and (VNewBitmapCoordConverter <> nil) then begin
    if VOldBitmapCoordConverter.GetIsSameConverter(VNewBitmapCoordConverter) then begin
      VNeedUpdateConverter := False;
    end;
  end;
  if VNeedUpdateConverter then begin
    DoUpdateBitmapCoordConverter(VNewBitmapCoordConverter);
    Redraw;
  end;
end;

function TMapLayerBasic.CreateBitmapCoordConverter(
  ANewVisualCoordConverter: ILocalCoordConverter): ILocalCoordConverter;
begin
  Result := ANewVisualCoordConverter;
end;

procedure TMapLayerBasic.DoHide;
begin
  inherited;
  UpdateBitmapConverterByVisual(VisualCoordConverter);
end;

procedure TMapLayerBasic.AfterPosChange;
begin
  inherited;
  UpdateBitmapConverterByVisual(VisualCoordConverter);
  UpdateLayerLocation;
end;

procedure TMapLayerBasic.DoShow;
begin
  inherited;
  UpdateBitmapConverterByVisual(VisualCoordConverter);
end;

procedure TMapLayerBasic.DoUpdateBitmapCoordConverter(
  ANewBitmapCoordConverter: ILocalCoordConverter);
var
  VBitmapSizeInPixel: TPoint;
begin
  FBitmapCoordConverter := ANewBitmapCoordConverter;
  if ANewBitmapCoordConverter <> nil then begin
    VBitmapSizeInPixel := ANewBitmapCoordConverter.GetLocalRectSize;
  end else begin
    VBitmapSizeInPixel := Point(0, 0);
  end;
  FLayer.Bitmap.Lock;
  try
    if (FLayer.Bitmap.Width <> VBitmapSizeInPixel.X) or (FLayer.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
      FLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
    end;
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

end.
