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
  i_LocalCoordConverterFactorySimpe,
  i_ViewPortState,
  i_InternalPerformanceCounter,
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
  end;

  TMapLayerBasicNoBitmap = class(TMapLayerBasicFullView)
  private
    FOnPaintCounter: IInternalPerformanceCounter;
    procedure OnPaintLayer(Sender: TObject; Buffer: TBitmap32);
  protected
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); virtual; abstract;
  protected
    procedure DoRedraw; override;
    procedure SetViewCoordConverter(AValue: ILocalCoordConverter); override;
    procedure SetPerfList(const Value: IInternalPerformanceCounterList); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState);
    procedure StartThreads; override;
  end;

  TMapLayerFixedWithBitmap = class(TWindowLayerWithBitmap)
  protected
    FFixedLonLat: TDoublePoint;
    FFixedOnBitmap: TDoublePoint;
  protected
    procedure SetViewCoordConverter(AValue: ILocalCoordConverter); override;
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
    FConverterFactory: ILocalCoordConverterFactorySimpe;
  protected
    procedure SetNeedUpdateLayerSize; virtual;
    procedure UpdateLayerSize; virtual;
    procedure UpdateLayerSizeIfNeed; virtual;

    procedure ClearLayerBitmap; virtual;
    procedure DoUpdateLayerSize(ANewSize: TPoint); virtual;
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; virtual;
    property Layer: TBitmapLayer read FLayer;
    property ConverterFactory: ILocalCoordConverterFactorySimpe read FConverterFactory;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoViewUpdate; override;
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    function GetLayerCoordConverterByViewConverter(ANewViewCoordConverter: ILocalCoordConverter): ILocalCoordConverter; override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure DoRedraw; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ACoordConverterFactory: ILocalCoordConverterFactorySimpe
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Types,
  i_CoordConverter,
  u_LocalCoordConverterFactorySimpe,
  u_GeoFun;

{ TMapLayerBase }

constructor TMapLayerBase.Create(
  ALayer: TPositionedLayer;
  AViewPortState: IViewPortState
);
begin
  inherited Create(ALayer, AViewPortState, True);
end;

procedure TMapLayerBase.SetLayerCoordConverter(AValue: ILocalCoordConverter);
begin
  if (LayerCoordConverter = nil) or (not LayerCoordConverter.GetIsSameConverter(AValue)) then begin
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
  // Ничего не делаем по-умолчанию.
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

procedure TMapLayerFixedWithBitmap.SetViewCoordConverter(
  AValue: ILocalCoordConverter);
begin
  if (ViewCoordConverter = nil) or (not ViewCoordConverter.GetIsSameConverter(AValue)) then begin
    SetNeedUpdateLocation;
  end;
  inherited;
end;

{ TMapLayerBasic }

procedure TMapLayerBasic.ClearLayerBitmap;
begin
  if Visible then begin
    Layer.Bitmap.Lock;
    try
      Layer.Bitmap.Clear(0);
    finally
      Layer.Bitmap.UnLock;
    end;
  end;
end;

constructor TMapLayerBasic.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ACoordConverterFactory: ILocalCoordConverterFactorySimpe
);
begin
  FConverterFactory := ACoordConverterFactory;
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);
  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FNeedUpdateLayerSizeCS := TCriticalSection.Create;
end;

procedure TMapLayerBasic.DoViewUpdate;
begin
  UpdateLayerSizeIfNeed;
  inherited;
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

procedure TMapLayerBasic.DoHide;
begin
  inherited;
  SetNeedUpdateLayerSize;
end;

procedure TMapLayerBasic.DoRedraw;
begin
  ClearLayerBitmap;
  inherited;
end;

procedure TMapLayerBasic.DoShow;
begin
  inherited;
  SetNeedUpdateLayerSize;
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
  FNeedUpdateLayerSizeCS.Acquire;
  try
    FNeedUpdateLayerSize := False;
  finally
    FNeedUpdateLayerSizeCS.Release;
  end;
  DoUpdateLayerSize(GetLayerSizeForView(LayerCoordConverter));
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
var
  VZoom: Byte;
  VSourcePixelRect: TDoubleRect;
  VConverter: ICoordConverter;
  VTileRect: TRect;
  VResultPixelRect: TRect;
begin
  VConverter := ANewViewCoordConverter.GetGeoConverter;
  VZoom := ANewViewCoordConverter.GetZoom;
  VSourcePixelRect := ANewViewCoordConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VSourcePixelRect, VZoom);
  VTileRect := VConverter.PixelRectFloat2TileRect(VSourcePixelRect, VZoom);
  VResultPixelRect := VConverter.TileRect2PixelRect(VTileRect, VZoom);

  Result := FConverterFactory.CreateConverter(
    Rect(0, 0, VResultPixelRect.Right - VResultPixelRect.Left, VResultPixelRect.Bottom - VResultPixelRect.Top),
    VZoom,
    VConverter,
    DoublePoint(1, 1),
    DoublePoint(VResultPixelRect.TopLeft)
  );
end;

function TMapLayerBasic.GetLayerSizeForView(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  if Visible then begin
    Result := ANewVisualCoordConverter.GetLocalRectSize;
  end else begin
    Result := Point(0, 0);
  end;
end;

{ TMapLayerBasicNoBitmap }

constructor TMapLayerBasicNoBitmap.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState);
begin
  inherited Create(TPositionedLayer.Create(AParentMap.Layers), AViewPortState);
end;

procedure TMapLayerBasicNoBitmap.DoRedraw;
begin
  inherited;
  LayerPositioned.Changed;
end;

procedure TMapLayerBasicNoBitmap.OnPaintLayer(Sender: TObject;
  Buffer: TBitmap32);
var
  VLocalConverter: ILocalCoordConverter;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VLocalConverter := ViewCoordConverter;
  if VLocalConverter <> nil then begin
    VCounterContext := FOnPaintCounter.StartOperation;
    try
      PaintLayer(Buffer, VLocalConverter);
    finally
      FOnPaintCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TMapLayerBasicNoBitmap.SetPerfList(
  const Value: IInternalPerformanceCounterList);
begin
  inherited;
  FOnPaintCounter := Value.CreateAndAddNewCounter('OnPaint');
end;

procedure TMapLayerBasicNoBitmap.SetViewCoordConverter(
  AValue: ILocalCoordConverter);
begin
  if (ViewCoordConverter = nil) or (not ViewCoordConverter.GetIsSameConverter(AValue)) then begin
    SetNeedRedraw;
  end;
  inherited;
end;

procedure TMapLayerBasicNoBitmap.StartThreads;
begin
  inherited;
  LayerPositioned.OnPaint := OnPaintLayer;
end;

end.
