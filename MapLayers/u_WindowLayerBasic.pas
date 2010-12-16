unit u_WindowLayerBasic;

interface

uses
  Windows,
  SyncObjs,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_JclNotify,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState;

type
  TWindowLayerAbstract = class
  private
    FCS: TCriticalSection;
    FRedrawCounter: Cardinal;
    FRedrawTime: TDateTime;
  protected
    procedure IncRedrawCounter(ATime: TDateTime);
    function GetVisible: Boolean; virtual; abstract;
  public
    constructor Create();
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); virtual; abstract;
    procedure StartThreads; virtual; abstract;
    procedure SendTerminateToThreads; virtual; abstract;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); virtual; abstract;
    procedure Redraw; virtual; abstract;
    property Visible: Boolean read GetVisible;
    property RedrawCounter: Cardinal read FRedrawCounter;
    property RedrawTime: TDateTime read FRedrawTime;
  end;



  TWindowLayerBasic = class(TWindowLayerAbstract)
  protected
    FLayerPositioned: TPositionedLayer;
    FViewPortState: TMapViewPortState;
    FVisible: Boolean;
    FVisibleChangeNotifier: IJclNotifier;

    FViewSizeChangeListener: IJclListener;

    function GetVisible: Boolean; override;
    procedure SetVisible(const Value: Boolean); virtual;

    procedure OnViewSizeChange(Sender: TObject); virtual; abstract;
    procedure UpdateLayerLocation; virtual;
    procedure DoUpdateLayerLocation; virtual;
    procedure DoShow; virtual;
    procedure DoHide; virtual;
    procedure DoRedraw; virtual; abstract;
    function GetMapLayerLocationRect: TFloatRect; virtual; abstract;
  public
    constructor Create(ALayer: TPositionedLayer; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure Redraw; override;
    property Visible: Boolean read GetVisible write SetVisible;
    property VisibleChangeNotifier: IJclNotifier read FVisibleChangeNotifier;
  end;

  TWindowLayerBasicFixedSize = class(TWindowLayerBasic)
  protected
    FMapViewSize: TPoint;
    procedure OnViewSizeChange(Sender: TObject); override;
  public
    constructor Create(ALayer: TPositionedLayer; AViewPortState: TMapViewPortState);
  end;

  TWindowLayerBasicScaledSize = class(TWindowLayerBasic)
  protected
    procedure OnViewSizeChange(Sender: TObject); override;
    procedure UpdateLayerSize; virtual;
    procedure DoUpdateLayerSize; virtual; abstract;
  end;

  TWindowLayerBasicFixedSizeWithBitmap = class(TWindowLayerBasicFixedSize)
  protected
    FParentMap: TImage32;
    FLayer: TBitmapLayer;
    function GetBitmapSizeInPixel: TPoint; virtual; abstract;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
  end;

  TWindowLayerBasicWithBitmap = class(TWindowLayerBasicScaledSize)
  protected
    FMapViewSize: TPoint;
    FParentMap: TImage32;
    FLayer: TBitmapLayer;
    procedure DoUpdateLayerSize; override;
    procedure DoHide; override;
    procedure DoShow; override;
    function GetBitmapSizeInPixel: TPoint; virtual; abstract;
    procedure OnViewSizeChange(Sender: TObject); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
  end;

implementation

uses
  SysUtils,
  Forms,
  Types,
  u_NotifyEventListener,
  u_JclNotify;

{ TWindowLayerAbstract }

constructor TWindowLayerAbstract.Create;
begin
  FCS := TCriticalSection.Create;
  FRedrawCounter := 0;
  FRedrawTime  := 0;
end;

destructor TWindowLayerAbstract.Destroy;
begin
  FreeAndNil(FCS);
  inherited;
end;

procedure TWindowLayerAbstract.IncRedrawCounter(ATime: TDateTime);
begin
  FCS.Acquire;
  try
    Inc(FRedrawCounter);
    FRedrawTime := FRedrawTime + ATime;
  finally
    FCS.Release;
  end;
end;

{ TWindowLayerBasic }

constructor TWindowLayerBasic.Create(ALayer: TPositionedLayer; AViewPortState: TMapViewPortState);
begin
  inherited Create;
  FViewPortState := AViewPortState;

  FLayerPositioned := ALayer;

  FLayerPositioned.MouseEvents := false;
  FLayerPositioned.Visible := false;
  FVisible := False;

  FVisibleChangeNotifier := TJclBaseNotifier.Create;
  FViewSizeChangeListener := TNotifyEventListener.Create(Self.OnViewSizeChange);
  FViewPortState.ViewSizeChangeNotifier.Add(FViewSizeChangeListener);
end;

destructor TWindowLayerBasic.Destroy;
begin
  FViewPortState.ViewSizeChangeNotifier.Remove(FViewSizeChangeListener);
  FViewSizeChangeListener := nil;
  FViewPortState := nil;
  FLayerPositioned := nil;
  FVisibleChangeNotifier := nil;
  inherited;
end;

procedure TWindowLayerBasic.DoHide;
begin
  FVisible := False;
  FLayerPositioned.Visible := False;
end;

procedure TWindowLayerBasic.DoShow;
begin
  FVisible := True;
  FLayerPositioned.Visible := True;
end;

procedure TWindowLayerBasic.DoUpdateLayerLocation;
begin
  FLayerPositioned.Location := GetMapLayerLocationRect;
end;

function TWindowLayerBasic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TWindowLayerBasic.Hide;
begin
  if Visible then begin
    DoHide;
    FVisibleChangeNotifier.Notify(nil);
  end;
end;

procedure TWindowLayerBasic.LoadConfig(AConfigProvider: IConfigDataProvider);
begin
  // По умолчанию ничего не делаем
end;

procedure TWindowLayerBasic.UpdateLayerLocation;
begin
  if Visible then begin
    DoUpdateLayerLocation;
  end;
end;

procedure TWindowLayerBasic.Redraw;
var
  VPerformanceCounterBegin: Int64;
  VPerformanceCounterEnd: Int64;
  VPerformanceCounterFr: Int64;
  VUpdateTime: TDateTime;
begin
  if Visible then begin
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

procedure TWindowLayerBasic.SaveConfig(
  AConfigProvider: IConfigDataWriteProvider);
begin
  // По умолчанию ничего не делаем
end;

procedure TWindowLayerBasic.SendTerminateToThreads;
begin
  // По умолчанию ничего не делаем
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
    FVisibleChangeNotifier.Notify(nil);
  end;
end;

procedure TWindowLayerBasic.StartThreads;
begin
  // По умолчанию ничего не делаем
end;

{ TWindowLayerBasicScaledSize }

procedure TWindowLayerBasicScaledSize.OnViewSizeChange(Sender: TObject);
begin
  UpdateLayerSize;
  UpdateLayerLocation;
  Redraw;
end;

procedure TWindowLayerBasicScaledSize.UpdateLayerSize;
begin
  if Visible then begin
    DoUpdateLayerSize;
  end;
end;

{ TWindowLayerBasicWithBitmap }

constructor TWindowLayerBasicWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  FParentMap := AParentMap;
  FLayer := TBitmapLayer.Create(FParentMap.Layers);
  inherited Create(FLayer, AViewPortState);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
  FMapViewSize := FViewPortState.GetViewSizeInVisiblePixel;
end;

procedure TWindowLayerBasicWithBitmap.DoHide;
begin
  inherited;
  FLayer.Bitmap.Lock;
  try
    FLayer.Bitmap.SetSize(0, 0);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

procedure TWindowLayerBasicWithBitmap.DoShow;
begin
  inherited;
  UpdateLayerSize;
  UpdateLayerLocation;
  Redraw;
end;

procedure TWindowLayerBasicWithBitmap.DoUpdateLayerSize;
var
  VBitmapSizeInPixel: TPoint;
begin
  inherited;
  VBitmapSizeInPixel := GetBitmapSizeInPixel;
  FLayer.Bitmap.Lock;
  try
    if (FLayer.Bitmap.Width <> VBitmapSizeInPixel.X) or (FLayer.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
      FLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
    end;
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

procedure TWindowLayerBasicWithBitmap.OnViewSizeChange(Sender: TObject);
begin
  FMapViewSize := FViewPortState.GetViewSizeInVisiblePixel;
  inherited;
end;

{ TWindowLayerBasicFixedSize }

constructor TWindowLayerBasicFixedSize.Create(ALayer: TPositionedLayer;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FMapViewSize := FViewPortState.GetViewSizeInVisiblePixel;
end;

procedure TWindowLayerBasicFixedSize.OnViewSizeChange(Sender: TObject);
begin
  FMapViewSize := FViewPortState.GetViewSizeInVisiblePixel;
  UpdateLayerLocation;
end;

{ TWindowLayerBasicFixedSizeWithBitmap }

constructor TWindowLayerBasicFixedSizeWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  FParentMap := AParentMap;
  FLayer := TBitmapLayer.Create(FParentMap.Layers);
  inherited Create(FLayer, AViewPortState);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

procedure TWindowLayerBasicFixedSizeWithBitmap.DoRedraw;
begin
end;

end.
