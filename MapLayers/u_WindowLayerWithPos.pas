unit u_WindowLayerWithPos;

interface

uses
  Windows,
  SyncObjs,
  GR32,
  GR32_Layers,
  GR32_Image,
  i_LocalCoordConverter,
  i_ViewPortState,
  u_WindowLayerBasic;

type
  TWindowLayerWithPosBase = class(TWindowLayerAbstract)
  private
    FViewPortState: IViewPortState;
    FViewCoordConverter: ILocalCoordConverter;
    FLayerCoordConverter: ILocalCoordConverter;
    procedure OnViewPortPosChange(Sender: TObject);
    procedure OnViewPortScaleChange(Sender: TObject);
  protected
    procedure SetViewCoordConverter(AValue: ILocalCoordConverter); virtual;
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); virtual;

    function GetLayerCoordConverterByViewConverter(ANewViewCoordConverter: ILocalCoordConverter): ILocalCoordConverter; virtual; abstract;

    procedure PosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;

    procedure ScaleChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoScaleChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;

    procedure AfterLayerStateChange; virtual;

    property ViewPortState: IViewPortState read FViewPortState;
    property ViewCoordConverter: ILocalCoordConverter read FViewCoordConverter;
    property LayerCoordConverter: ILocalCoordConverter read FLayerCoordConverter;
  public
    constructor Create(AViewPortState: IViewPortState; AListenScaleChange: Boolean);
    destructor Destroy; override;

    procedure StartThreads; override;
  end;


  TWindowLayerBasic = class(TWindowLayerWithPosBase)
  private
    FVisible: Boolean;
    FLayer: TPositionedLayer;

    FNeedRedraw: Boolean;
    FNeedUpdateLocation: Boolean;
    FNeedRedrawCS: TCriticalSection;
  protected
    function GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean); virtual;

    procedure SetNeedRedraw; virtual;
    procedure SetNeedUpdateLocation; virtual;

    function GetVisibleForNewPos(ANewVisualCoordConverter: ILocalCoordConverter): Boolean; virtual;

    function GetMapLayerLocationRect: TFloatRect; virtual; abstract;
    procedure UpdateLayerLocationIfNeed; virtual;
    procedure UpdateLayerLocation; virtual;
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); virtual;

    procedure Show; virtual;
    procedure DoShow; virtual;
    procedure Hide; virtual;
    procedure DoHide; virtual;
    procedure DoRedraw; virtual; abstract;
    procedure RedrawIfNeed; virtual;

    property LayerPositioned: TPositionedLayer read FLayer;
    property Visible: Boolean read GetVisible write SetVisible;
  protected
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    procedure AfterLayerStateChange; override;
  public
    constructor Create(ALayer: TPositionedLayer; AViewPortState: IViewPortState; AListenScaleChange: Boolean);
    destructor Destroy; override;
    procedure Redraw; virtual;
  end;

  TWindowLayerWithBitmap = class(TWindowLayerBasic)
  private
    FNeedUpdateLayerSize: Boolean;
  protected
    FLayer: TBitmapLayer;
    procedure SetNeedUpdateLayerSize; virtual;

    procedure UpdateLayerSize; virtual;
    procedure UpdateLayerSizeIfNeed; virtual;
    procedure DoUpdateLayerSize(ANewSize: TPoint); virtual;
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; virtual; abstract;
  protected
    procedure DoHide; override;
    procedure DoShow; override;
    procedure AfterLayerStateChange; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState);
  end;

  TWindowLayerFixedSizeWithBitmap = class(TWindowLayerWithBitmap)
  protected
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    procedure DoRedraw; override;
  end;

implementation

uses
  Types,
  SysUtils,
  Graphics,
  u_NotifyEventListener;

{ TWindowLayerWithPosBase }

constructor TWindowLayerWithPosBase.Create(AViewPortState: IViewPortState;
  AListenScaleChange: Boolean);
begin
  inherited Create;
  FViewPortState := AViewPortState;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnViewPortPosChange),
    FViewPortState.GetChangeNotifier
  );
  if AListenScaleChange then begin
    LinksList.Add(
      TNotifyEventListener.Create(Self.OnViewPortScaleChange),
      ViewPortState.ScaleChangeNotifier
    );
  end;
end;

destructor TWindowLayerWithPosBase.Destroy;
begin
  FViewCoordConverter := nil;
  FLayerCoordConverter := nil;
  FViewPortState := nil;
  inherited;
end;

procedure TWindowLayerWithPosBase.OnViewPortPosChange(Sender: TObject);
begin
  PosChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerWithPosBase.PosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  DoPosChange(ANewVisualCoordConverter);
  AfterLayerStateChange;
end;

procedure TWindowLayerWithPosBase.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  SetViewCoordConverter(ANewVisualCoordConverter);
  SetLayerCoordConverter(GetLayerCoordConverterByViewConverter(ANewVisualCoordConverter));
end;

procedure TWindowLayerWithPosBase.OnViewPortScaleChange(Sender: TObject);
begin
  ScaleChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerWithPosBase.ScaleChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  DoScaleChange(ANewVisualCoordConverter);
  AfterLayerStateChange;
end;

procedure TWindowLayerWithPosBase.DoScaleChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  SetViewCoordConverter(ANewVisualCoordConverter);
end;

procedure TWindowLayerWithPosBase.AfterLayerStateChange;
begin
end;

procedure TWindowLayerWithPosBase.SetLayerCoordConverter(
  AValue: ILocalCoordConverter);
begin
  FLayerCoordConverter := AValue;
end;

procedure TWindowLayerWithPosBase.SetViewCoordConverter(
  AValue: ILocalCoordConverter);
begin
  FViewCoordConverter := AValue;
end;

procedure TWindowLayerWithPosBase.StartThreads;
begin
  inherited;
  OnViewPortPosChange(nil);
end;

{ TWindowLayerBasic }

constructor TWindowLayerBasic.Create(ALayer: TPositionedLayer; AViewPortState: IViewPortState; AListenScaleChange: Boolean);
begin
  inherited Create(AViewPortState, AListenScaleChange);
  FLayer := ALayer;

  FLayer.MouseEvents := false;
  FLayer.Visible := false;
  FVisible := False;
  FNeedRedraw := True;
  FNeedRedrawCS := TCriticalSection.Create;
end;

destructor TWindowLayerBasic.Destroy;
begin
  FreeAndNil(FNeedRedrawCS);
  FLayer := nil;
  inherited;
end;

procedure TWindowLayerBasic.AfterLayerStateChange;
begin
  inherited;
  RedrawIfNeed;
  UpdateLayerLocationIfNeed;
end;

procedure TWindowLayerBasic.Hide;
begin
  if FVisible then begin
    DoHide;
  end;
  AfterLayerStateChange;
end;

procedure TWindowLayerBasic.DoHide;
begin
  FVisible := False;
  FLayer.Visible := False;
  SetNeedUpdateLocation;
  SetNeedRedraw;
end;

procedure TWindowLayerBasic.Show;
begin
  if not Visible then begin
    DoShow;
  end;
  AfterLayerStateChange;
end;

procedure TWindowLayerBasic.DoShow;
begin
  FVisible := True;
  FLayer.Visible := True;
  SetNeedUpdateLocation;
  SetNeedRedraw;
end;

procedure TWindowLayerBasic.DoUpdateLayerLocation(ANewLocation: TFloatRect);
begin
  FLayer.Location := ANewLocation;
end;

function TWindowLayerBasic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TWindowLayerBasic.GetVisibleForNewPos(
  ANewVisualCoordConverter: ILocalCoordConverter): Boolean;
begin
  Result := FVisible;
end;

procedure TWindowLayerBasic.UpdateLayerLocation;
begin
  if FVisible then begin
    FNeedRedrawCS.Acquire;
    try
      FNeedUpdateLocation := False;
    finally
      FNeedRedrawCS.Release;
    end;
    DoUpdateLayerLocation(GetMapLayerLocationRect);
  end;
end;

procedure TWindowLayerBasic.UpdateLayerLocationIfNeed;
var
  VNeed: Boolean;
begin
  VNeed := False;
  FNeedRedrawCS.Acquire;
  try
    if FNeedUpdateLocation then begin
      FNeedUpdateLocation := False;
      VNeed := True;
    end;
  finally
    FNeedRedrawCS.Release;
  end;
  if VNeed then begin
    UpdateLayerLocation;
  end;
end;

procedure TWindowLayerBasic.Redraw;
var
  VPerformanceCounterBegin: Int64;
  VPerformanceCounterEnd: Int64;
  VPerformanceCounterFr: Int64;
  VUpdateTime: TDateTime;
begin
  if FVisible then begin
    try
      FNeedRedrawCS.Acquire;
      try
        FNeedRedraw := False;
      finally
        FNeedRedrawCS.Release;
      end;
      QueryPerformanceCounter(VPerformanceCounterBegin);
      DoRedraw;
    finally
      QueryPerformanceCounter(VPerformanceCounterEnd);
      QueryPerformanceFrequency(VPerformanceCounterFr);
      VUpdateTime := (VPerformanceCounterEnd - VPerformanceCounterBegin) / VPerformanceCounterFr/24/60/60;
      IncRedrawCounter(VUpdateTime);
    end;
  end;
end;

procedure TWindowLayerBasic.RedrawIfNeed;
var
  VNeed: Boolean;
begin
  VNeed := False;
  FNeedRedrawCS.Acquire;
  try
    if FNeedRedraw then begin
      FNeedRedraw := False;
      VNeed := True;
    end;
  finally
    FNeedRedrawCS.Release;
  end;
  if VNeed then begin
    Redraw;
  end;
end;

procedure TWindowLayerBasic.SetLayerCoordConverter(
  AValue: ILocalCoordConverter);
begin
  inherited;
  SetVisible(GetVisibleForNewPos(AValue));
end;

procedure TWindowLayerBasic.SetNeedRedraw;
begin
  FNeedRedrawCS.Acquire;
  try
    FNeedRedraw := True;
  finally
    FNeedRedrawCS.Release;
  end;
  SetNeedUpdateLocation;
end;

procedure TWindowLayerBasic.SetNeedUpdateLocation;
begin
  FNeedRedrawCS.Acquire;
  try
    FNeedUpdateLocation := True;
  finally
    FNeedRedrawCS.Release;
  end;
end;

procedure TWindowLayerBasic.SetVisible(const Value: Boolean);
begin
  if Value then begin
    Show;
  end else begin
    Hide;
  end;
end;

{ TWindowLayerWithBitmap }

procedure TWindowLayerWithBitmap.AfterLayerStateChange;
begin
  UpdateLayerSizeIfNeed;
  inherited;
end;

constructor TWindowLayerWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState, False);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

procedure TWindowLayerWithBitmap.DoHide;
begin
  inherited;
  SetNeedUpdateLayerSize;
end;

procedure TWindowLayerWithBitmap.DoShow;
begin
  inherited;
  SetNeedUpdateLayerSize;
end;

procedure TWindowLayerWithBitmap.DoUpdateLayerSize(ANewSize: TPoint);
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

procedure TWindowLayerWithBitmap.SetNeedUpdateLayerSize;
begin
  FNeedRedrawCS.Acquire;
  try
    FNeedUpdateLayerSize := True;
  finally
    FNeedRedrawCS.Release;
  end;
end;

procedure TWindowLayerWithBitmap.UpdateLayerSize;
begin
  if FVisible then begin
    FNeedRedrawCS.Acquire;
    try
      FNeedUpdateLayerSize := False;
    finally
      FNeedRedrawCS.Release;
    end;
    DoUpdateLayerSize(GetLayerSizeForView(LayerCoordConverter));
  end;
end;

procedure TWindowLayerWithBitmap.UpdateLayerSizeIfNeed;
var
  VNeed: Boolean;
begin
  VNeed := False;
  FNeedRedrawCS.Acquire;
  try
    if FNeedUpdateLayerSize then begin
      FNeedUpdateLayerSize := False;
      VNeed := True;
    end;
  finally
    FNeedRedrawCS.Release;
  end;
  if VNeed then begin
    UpdateLayerSize;
  end;
end;

{ TWindowLayerFixedSizeWithBitmap }

procedure TWindowLayerFixedSizeWithBitmap.DoRedraw;
begin
  // По-умолчанию ничего не делаем.
end;

function TWindowLayerFixedSizeWithBitmap.GetLayerSizeForView(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  FLayer.Bitmap.Lock;
  try
    Result := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

end.
