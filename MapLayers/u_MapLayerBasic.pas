unit u_MapLayerBasic;

interface

uses
  SysUtils,
  GR32,
  GR32_Layers,
  GR32_Image,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_ViewPortState,
  i_ImageResamplerConfig,
  i_InternalPerformanceCounter,
  u_WindowLayerWithPos;

type
  TMapLayerBase = class(TWindowLayerBasic)
  protected
    procedure SetLayerCoordConverter(
      const AValue: ILocalCoordConverter
    ); override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      ALayer: TCustomLayer;
      const AViewPortState: IViewPortState
    );
  end;

  TMapLayerBasicFullView = class(TMapLayerBase)
  private
    FLayer: TPositionedLayer;

    FNeedUpdateLocation: Boolean;
    FNeedUpdateLocationCS: IReadWriteSync;
  protected
    function GetMapLayerLocationRect: TFloatRect; virtual;
    procedure UpdateLayerLocationIfNeed; virtual;
    procedure UpdateLayerLocation; virtual;
    procedure DoUpdateLayerLocation(const ANewLocation: TFloatRect); virtual;
  protected
    procedure SetViewCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure SetNeedRedraw; override;
    procedure SetNeedUpdateLocation; virtual;
    procedure DoViewUpdate; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      ALayer: TPositionedLayer;
      const AViewPortState: IViewPortState
    );
  end;

  TMapLayerBasicNoBitmap = class(TMapLayerBase)
  private
    FOnPaintCounter: IInternalPerformanceCounter;
    procedure OnPaintLayer(Sender: TObject; Buffer: TBitmap32);
  protected
    procedure PaintLayer(ABuffer: TBitmap32; const ALocalConverter: ILocalCoordConverter); virtual; abstract;
  protected
    procedure DoRedraw; override;
    procedure SetViewCoordConverter(const AValue: ILocalCoordConverter); override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState
    );
    procedure StartThreads; override;
  end;

  TMapLayerBasic = class(TMapLayerBasicFullView)
  private
    FLayer: TBitmapLayer;
    FNeedUpdateLayerSize: Boolean;
    FNeedUpdateLayerSizeCS: IReadWriteSync;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
  protected
    procedure SetNeedUpdateLayerSize; virtual;
    procedure UpdateLayerSize; virtual;
    procedure UpdateLayerSizeIfNeed; virtual;

    procedure ClearLayerBitmap; virtual;
    procedure DoUpdateLayerSize(const ANewSize: TPoint); virtual;
    function GetLayerSizeForView(
      const ANewVisualCoordConverter: ILocalCoordConverter
    ): TPoint; virtual;
    property Layer: TBitmapLayer read FLayer;
    property ConverterFactory: ILocalCoordConverterFactorySimpe read FConverterFactory;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoViewUpdate; override;
    procedure SetLayerCoordConverter(const AValue: ILocalCoordConverter); override;
    function GetLayerCoordConverterByViewConverter(
      const ANewViewCoordConverter: ILocalCoordConverter
    ): ILocalCoordConverter; override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure DoRedraw; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const ACoordConverterFactory: ILocalCoordConverterFactorySimpe
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  Types;

{ TMapLayerBase }

constructor TMapLayerBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  ALayer: TCustomLayer;
  const AViewPortState: IViewPortState
);
begin
  inherited Create(APerfList, ALayer, AViewPortState, True);
end;

procedure TMapLayerBase.SetLayerCoordConverter(const AValue: ILocalCoordConverter);
begin
  if (LayerCoordConverter = nil) or (not LayerCoordConverter.GetIsSameConverter(AValue)) then begin
    SetNeedRedraw;
  end;
  inherited;
end;

{ TMapLayerBasicFullView }

constructor TMapLayerBasicFullView.Create(
  const APerfList: IInternalPerformanceCounterList;
  ALayer: TPositionedLayer;
  const AViewPortState: IViewPortState
);
begin
  inherited Create(APerfList, ALayer, AViewPortState);
  FLayer := ALayer;
  FNeedUpdateLocationCS := MakeSyncFake(Self);
  FNeedUpdateLocation := True;
end;

procedure TMapLayerBasicFullView.DoUpdateLayerLocation(
  const ANewLocation: TFloatRect
);
begin
  FLayer.Location := ANewLocation;
end;

procedure TMapLayerBasicFullView.DoViewUpdate;
begin
  inherited;
  UpdateLayerLocationIfNeed;
end;

function TMapLayerBasicFullView.GetMapLayerLocationRect: TFloatRect;
begin
  if ViewCoordConverter <> nil then begin
    Result := FloatRect(ViewCoordConverter.GetLocalRect);
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMapLayerBasicFullView.SetNeedRedraw;
begin
  inherited;
  SetNeedUpdateLocation;
end;

procedure TMapLayerBasicFullView.SetNeedUpdateLocation;
begin
  FNeedUpdateLocationCS.BeginWrite;
  try
    FNeedUpdateLocation := True;
  finally
    FNeedUpdateLocationCS.EndWrite;
  end;
end;

procedure TMapLayerBasicFullView.SetViewCoordConverter(
  const AValue: ILocalCoordConverter
);
begin
  if (ViewCoordConverter = nil) or (not ViewCoordConverter.GetIsSameConverter(AValue)) then begin
    SetNeedUpdateLocation;
  end;
  inherited;
end;

procedure TMapLayerBasicFullView.UpdateLayerLocation;
begin
  if Visible then begin
    FNeedUpdateLocationCS.BeginWrite;
    try
      FNeedUpdateLocation := False;
    finally
      FNeedUpdateLocationCS.EndWrite;
    end;
    DoUpdateLayerLocation(GetMapLayerLocationRect);
  end;
end;

procedure TMapLayerBasicFullView.UpdateLayerLocationIfNeed;
var
  VNeed: Boolean;
begin
  VNeed := False;
  FNeedUpdateLocationCS.BeginWrite;
  try
    if FNeedUpdateLocation then begin
      FNeedUpdateLocation := False;
      VNeed := True;
    end;
  finally
    FNeedUpdateLocationCS.EndWrite;
  end;
  if VNeed then begin
    UpdateLayerLocation;
  end;
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
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const ACoordConverterFactory: ILocalCoordConverterFactorySimpe
);
begin
  FConverterFactory := ACoordConverterFactory;
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(APerfList, FLayer, AViewPortState);
  FLayer.Bitmap.DrawMode := dmBlend;
//  FLayer.Bitmap.Resampler := AResamplerConfig.GetActiveFactory.CreateResampler;
  FNeedUpdateLayerSizeCS := MakeSyncFake(Self);
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

procedure TMapLayerBasic.SetLayerCoordConverter(const AValue: ILocalCoordConverter);
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
  FNeedUpdateLayerSizeCS.BeginWrite;
  try
    FNeedUpdateLayerSize := True;
  finally
    FNeedUpdateLayerSizeCS.EndWrite;
  end;
end;

destructor TMapLayerBasic.Destroy;
begin
  FNeedUpdateLayerSizeCS := nil;
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

procedure TMapLayerBasic.DoUpdateLayerSize(const ANewSize: TPoint);
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
  FNeedUpdateLayerSizeCS.BeginWrite;
  try
    FNeedUpdateLayerSize := False;
  finally
    FNeedUpdateLayerSizeCS.EndWrite;
  end;
  DoUpdateLayerSize(GetLayerSizeForView(LayerCoordConverter));
end;

procedure TMapLayerBasic.UpdateLayerSizeIfNeed;
var
  VNeed: Boolean;
begin
  VNeed := False;
  FNeedUpdateLayerSizeCS.BeginWrite;
  try
    if FNeedUpdateLayerSize then begin
      FNeedUpdateLayerSize := False;
      VNeed := True;
    end;
  finally
    FNeedUpdateLayerSizeCS.EndWrite;
  end;
  if VNeed then begin
    UpdateLayerSize;
  end;
end;

function TMapLayerBasic.GetLayerCoordConverterByViewConverter(
  const ANewViewCoordConverter: ILocalCoordConverter
): ILocalCoordConverter;
begin
  Result := FConverterFactory.CreateBySourceWithStableTileRect(ANewViewCoordConverter);
end;

function TMapLayerBasic.GetLayerSizeForView(
  const ANewVisualCoordConverter: ILocalCoordConverter
): TPoint;
begin
  if Visible then begin
    Result := ANewVisualCoordConverter.GetLocalRectSize;
  end else begin
    Result := Point(0, 0);
  end;
end;

{ TMapLayerBasicNoBitmap }

constructor TMapLayerBasicNoBitmap.Create(
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState
);
begin
  inherited Create(APerfList, TCustomLayer.Create(AParentMap.Layers), AViewPortState);
  FOnPaintCounter := PerfList.CreateAndAddNewCounter('OnPaint');
end;

procedure TMapLayerBasicNoBitmap.DoRedraw;
begin
  inherited;
  Layer.Changed;
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

procedure TMapLayerBasicNoBitmap.SetViewCoordConverter(
  const AValue: ILocalCoordConverter
);
begin
  if (ViewCoordConverter = nil) or (not ViewCoordConverter.GetIsSameConverter(AValue)) then begin
    SetNeedRedraw;
  end;
  inherited;
end;

procedure TMapLayerBasicNoBitmap.StartThreads;
begin
  inherited;
  Layer.OnPaint := OnPaintLayer;
end;

end.
