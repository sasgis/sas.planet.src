unit u_WindowLayerWithPos;

interface

uses
  Windows,
  GR32,
  GR32_Layers,
  GR32_Image,
  i_JclNotify,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ILocalCoordConverter,
  i_IViewPortState,
  u_WindowLayerBasic;

type
  TWindowLayerBasic = class(TWindowLayerAbstract)
  private
    FVisible: Boolean;
    FVisibleChangeNotifier: IJclNotifier;
    FLayer: TPositionedLayer;
  protected
    FVisualCoordConverter: ILocalCoordConverter;
    FViewPortState: IViewPortState;

    function GetVisible: Boolean; override;
    procedure SetVisible(const Value: Boolean); virtual;

    procedure OnPosChange(Sender: TObject); virtual;
    procedure PosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;

    procedure UpdateLayerLocation(ANewLocation: TFloatRect); virtual;
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); virtual;
    procedure DoShow; virtual;
    procedure DoHide; virtual;
    procedure DoRedraw; virtual; abstract;
    function GetMapLayerLocationRect: TFloatRect; virtual; abstract;

    property LayerPositioned: TPositionedLayer read FLayer;
    property Visible: Boolean read GetVisible write SetVisible;
  public
    constructor Create(ALayer: TPositionedLayer; AViewPortState: IViewPortState);
    destructor Destroy; override;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure Redraw; override;
    property VisibleChangeNotifier: IJclNotifier read FVisibleChangeNotifier;
  end;

  TWindowLayerWithBitmap = class(TWindowLayerBasic)
  protected
    FLayer: TBitmapLayer;
    procedure UpdateLayerSize(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoUpdateLayerSize(ANewSize: TPoint); virtual;
    function GetLayerSizeForViewSize(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; virtual; abstract;
  protected
    procedure DoShow; override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState);
  end;

  TWindowLayerFixedSizeWithBitmap = class(TWindowLayerWithBitmap)
  protected
    function GetLayerSizeForViewSize(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    procedure DoRedraw; override;
  end;

implementation

uses
  Types,
  Graphics,
  u_JclNotify,
  u_NotifyEventListener;

{ TWindowLayerBasic }

constructor TWindowLayerBasic.Create(ALayer: TPositionedLayer; AViewPortState: IViewPortState);
begin
  inherited Create;
  FViewPortState := AViewPortState;

  FLayer := ALayer;

  FLayer.MouseEvents := false;
  FLayer.Visible := false;
  FVisible := False;

  FVisibleChangeNotifier := TJclBaseNotifier.Create;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnPosChange),
    FViewPortState.GetChangeNotifier
  );
end;

destructor TWindowLayerBasic.Destroy;
begin
  FViewPortState := nil;
  FLayer := nil;
  FVisibleChangeNotifier := nil;
  inherited;
end;

procedure TWindowLayerBasic.DoHide;
begin
  FVisible := False;
  FLayer.Visible := False;
end;

procedure TWindowLayerBasic.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  FVisualCoordConverter := ANewVisualCoordConverter;
end;

procedure TWindowLayerBasic.DoShow;
begin
  FVisible := True;
  FLayer.Visible := True;
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter;
end;

procedure TWindowLayerBasic.DoUpdateLayerLocation(ANewLocation: TFloatRect);
begin
  FLayer.Location := ANewLocation;
end;

function TWindowLayerBasic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TWindowLayerBasic.Hide;
begin
  if FVisible then begin
    DoHide;
    FVisibleChangeNotifier.Notify(nil);
  end;
end;

procedure TWindowLayerBasic.OnPosChange(Sender: TObject);
begin
  PosChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerBasic.PosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  if Visible then begin
    DoPosChange(ANewVisualCoordConverter);
    UpdateLayerLocation(GetMapLayerLocationRect);
  end;
end;

procedure TWindowLayerBasic.UpdateLayerLocation(ANewLocation: TFloatRect);
begin
  if FVisible then begin
    DoUpdateLayerLocation(ANewLocation);
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
      QueryPerformanceCounter(VPerformanceCounterBegin);
      DoRedraw;
    finally
      QueryPerformanceCounter(VPerformanceCounterEnd);
      QueryPerformanceFrequency(VPerformanceCounterFr);
      VUpdateTime := (VPerformanceCounterEnd - VPerformanceCounterBegin) / VPerformanceCounterFr;
      IncRedrawCounter(VUpdateTime);
    end;
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

procedure TWindowLayerBasic.Show;
begin
  if not Visible then begin
    DoShow;
    UpdateLayerLocation(GetMapLayerLocationRect);
    Redraw;
    FVisibleChangeNotifier.Notify(nil);
  end;
end;

{ TWindowLayerWithBitmap }

constructor TWindowLayerWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

procedure TWindowLayerWithBitmap.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  UpdateLayerSize(FVisualCoordConverter);
end;

procedure TWindowLayerWithBitmap.DoShow;
begin
  inherited;
  UpdateLayerSize(FVisualCoordConverter);
end;

procedure TWindowLayerWithBitmap.DoUpdateLayerSize(ANewSize: TPoint);
begin
  FLayer.Bitmap.Lock;
  try
    FLayer.Bitmap.SetSize(ANewSize.X, ANewSize.Y);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

procedure TWindowLayerWithBitmap.UpdateLayerSize(ANewVisualCoordConverter: ILocalCoordConverter);
var
  VNewSize: TPoint;
begin
  if FVisible then begin
    VNewSize := GetLayerSizeForViewSize(ANewVisualCoordConverter);
    if (FLayer.Bitmap.Width <> VNewSize.X) or (FLayer.Bitmap.Height <> VNewSize.Y) then begin
      DoUpdateLayerSize(VNewSize);
      UpdateLayerLocation(GetMapLayerLocationRect);
    end;
  end;
end;

{ TWindowLayerFixedSizeWithBitmap }

procedure TWindowLayerFixedSizeWithBitmap.DoRedraw;
begin
  // По-умолчанию ничего не делаем.
end;

function TWindowLayerFixedSizeWithBitmap.GetLayerSizeForViewSize(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  Result := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
end;

end.
