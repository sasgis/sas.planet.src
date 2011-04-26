unit u_MapLayerBasic;

interface

uses
  Windows,
  SyncObjs,
  GR32,
  GR32_Layers,
  GR32_Image,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_ViewPortState,
  u_WindowLayerWithPos;

type
  TMapLayerBase = class(TWindowLayerBasic)
  protected
    procedure SetViewCoordConverter(AValue: ILocalCoordConverter); override;
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
  public
    constructor Create(ALayer: TPositionedLayer; AViewPortState: IViewPortState);
  end;

  TMapLayerBasicFullView = class(TMapLayerBase)
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    function GetLayerCoordConverterByViewConverter(ANewViewCoordConverter: ILocalCoordConverter): ILocalCoordConverter; override;
  end;

  TMapLayerFixedWithBitmap = class(TWindowLayerWithBitmap)
  protected
    FFixedLonLat: TDoublePoint;
    FFixedOnBitmap: TDoublePoint;
  protected
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState);
  end;

  TMapLayerBasic = class(TMapLayerBasicFullView)
  private
    FLayer: TBitmapLayer;
    FNeedUpdateLayerSize: Boolean;
    FNeedUpdateLayerSizeCS: TCriticalSection;
  protected
    procedure SetNeedUpdateLayerSize; virtual;
    procedure UpdateLayerSize; virtual;
    procedure UpdateLayerSizeIfNeed; virtual;
    procedure DoUpdateLayerSize(ANewSize: TPoint); virtual;
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; virtual;
    property Layer: TBitmapLayer read FLayer;
  protected
    function GetLayerCoordConverterByViewConverter(ANewViewCoordConverter: ILocalCoordConverter): ILocalCoordConverter; override;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure AfterLayerStateChange; override;
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Types,
  Graphics,
  u_NotifyEventListener;

{ TMapLayerBase }

constructor TMapLayerBase.Create(ALayer: TPositionedLayer;
  AViewPortState: IViewPortState);
begin
  inherited Create(ALayer, AViewPortState, True);
end;

procedure TMapLayerBase.SetLayerCoordConverter(AValue: ILocalCoordConverter);
begin
  if (LayerPositioned = nil) or (not LayerCoordConverter.GetIsSameConverter(AValue)) then begin
    SetNeedRedraw;
  end;
  inherited;
end;

procedure TMapLayerBase.SetViewCoordConverter(AValue: ILocalCoordConverter);
begin
  if (ViewCoordConverter = nil) or (not ViewCoordConverter.GetIsSameConverter(AValue)) then begin
    SetNeedUpdateLocation;
  end;
  inherited;
end;

{ TMapLayerBasicFullView }

function TMapLayerBasicFullView.GetLayerCoordConverterByViewConverter(
  ANewViewCoordConverter: ILocalCoordConverter): ILocalCoordConverter;
begin
  Result := ANewViewCoordConverter;
end;

function TMapLayerBasicFullView.GetMapLayerLocationRect: TFloatRect;
begin
  if ViewCoordConverter <> nil then begin
    Result := FloatRect(ViewCoordConverter.GetLocalRect);
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;
{ TMapLayerFixedWithBitmap }

constructor TMapLayerFixedWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState);
begin
  inherited Create(AParentMap, AViewPortState);
end;

procedure TMapLayerFixedWithBitmap.DoRedraw;
begin
  inherited;
end;

function TMapLayerFixedWithBitmap.GetLayerSizeForView(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  FLayer.Bitmap.Lock;
  try
    Result := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

function TMapLayerFixedWithBitmap.GetMapLayerLocationRect: TFloatRect;
var
  VFixedVisualPixel: TDoublePoint;
  VBitmapSize: TPoint;
  VVisualCoordConverter: ILocalCoordConverter;
begin
  VVisualCoordConverter := ViewCoordConverter;
  if VVisualCoordConverter <> nil then begin
    VFixedVisualPixel := VVisualCoordConverter.LonLat2LocalPixelFloat(FFixedLonLat);
    if (Abs(VFixedVisualPixel.X) < (1 shl 15)) and (Abs(VFixedVisualPixel.Y) < (1 shl 15)) then begin
      FLayer.Bitmap.Lock;
      try
        VBitmapSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
      finally
        FLayer.Bitmap.Unlock;
      end;
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

{ TMapLayerBasic }

procedure TMapLayerBasic.AfterLayerStateChange;
begin
  UpdateLayerSizeIfNeed;
  inherited;
end;

constructor TMapLayerBasic.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);
  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
  FNeedUpdateLayerSizeCS := TCriticalSection.Create;
end;

function TMapLayerBasic.GetMapLayerLocationRect: TFloatRect;
var
  VBitmapOnMapRect: TDoubleRect;
  VBitmapOnVisualRect: TDoubleRect;
  VBitmapConverter: ILocalCoordConverter;
  VVisualConverter: ILocalCoordConverter;
begin
  VBitmapConverter := LayerCoordConverter;
  VVisualConverter := ViewCoordConverter;
  if (VBitmapConverter <> nil) and (VVisualConverter <> nil) then begin
    VBitmapOnMapRect := VBitmapConverter.GetRectInMapPixelFloat;
    VBitmapOnVisualRect := VVisualConverter.MapRectFloat2LocalRectFloat(VBitmapOnMapRect);
    Result := FloatRect(VBitmapOnVisualRect.Left, VBitmapOnVisualRect.Top, VBitmapOnVisualRect.Right, VBitmapOnVisualRect.Bottom);
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMapLayerBasic.SetLayerCoordConverter(AValue: ILocalCoordConverter);
var
  VNewSize: TPoint;
begin
  VNewSize := GetLayerSizeForView(AValue);
  Layer.Bitmap.Lock;
  try
    if (Layer.Bitmap.Width <> VNewSize.X) or (Layer.Bitmap.Height <> VNewSize.Y) then begin
      SetNeedUpdateLayerSize;
    end;
  finally
    Layer.Bitmap.Unlock;
  end;
  inherited;
end;

procedure TMapLayerBasic.SetNeedUpdateLayerSize;
begin
  FNeedUpdateLayerSizeCS.Acquire;
  try
    FNeedUpdateLayerSize := True;
  finally
    FNeedUpdateLayerSizeCS.Release;
  end;
end;

destructor TMapLayerBasic.Destroy;
begin
  FreeAndNil(FNeedUpdateLayerSizeCS);
  inherited;
end;

procedure TMapLayerBasic.DoUpdateLayerSize(ANewSize: TPoint);
var
  VNedRedraw: Boolean;
begin
  FLayer.Bitmap.Lock;
  try
    VNedRedraw := FLayer.Bitmap.SetSize(ANewSize.X, ANewSize.Y);
  finally
    FLayer.Bitmap.Unlock;
  end;
  if VNedRedraw then begin
    SetNeedRedraw;
  end;
end;

procedure TMapLayerBasic.UpdateLayerSize;
begin
  if Visible then begin
    FNeedUpdateLayerSizeCS.Acquire;
    try
      FNeedUpdateLayerSize := False;
    finally
      FNeedUpdateLayerSizeCS.Release;
    end;
    DoUpdateLayerSize(GetLayerSizeForView(LayerCoordConverter));
  end;
end;

procedure TMapLayerBasic.UpdateLayerSizeIfNeed;
var
  VNeed: Boolean;
begin
  VNeed := False;
  FNeedUpdateLayerSizeCS.Acquire;
  try
    if FNeedUpdateLayerSize then begin
      FNeedUpdateLayerSize := False;
      VNeed := True;
    end;
  finally
    FNeedUpdateLayerSizeCS.Release;
  end;
  if VNeed then begin
    UpdateLayerSize;
  end;
end;

function TMapLayerBasic.GetLayerCoordConverterByViewConverter(
  ANewViewCoordConverter: ILocalCoordConverter): ILocalCoordConverter;
begin
  if not Visible then begin
    Result := nil;
  end else begin
    Result := ANewViewCoordConverter;
  end;
end;

function TMapLayerBasic.GetLayerSizeForView(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  Result := ANewVisualCoordConverter.GetLocalRectSize;
end;

end.
