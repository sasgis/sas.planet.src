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
  i_ILocalCoordConverter,
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
  private
    FVisible: Boolean;
    FVisibleChangeNotifier: IJclNotifier;
    FViewSizeChangeListener: IJclListener;
    FLayerSize: TPoint;
    FLayer: TPositionedLayer;
  protected
    FVisualCoordConverter: ILocalCoordConverter;
    FViewPortState: TMapViewPortState;

    function GetVisible: Boolean; override;
    procedure SetVisible(const Value: Boolean); virtual;

    procedure OnViewSizeChange(Sender: TObject); virtual;
    procedure UpdateLayerSize(ANewSize: TPoint); virtual;
    procedure DoUpdateLayerSize(ANewSize: TPoint); virtual;
    function GetLayerSizeForViewSize(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; virtual; abstract;

    procedure UpdateLayerLocation(ANewLocation: TFloatRect); virtual;
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); virtual;
    procedure DoShow; virtual;
    procedure DoHide; virtual;
    procedure DoRedraw; virtual; abstract;
    function GetMapLayerLocationRect: TFloatRect; virtual; abstract;

    property LayerPositioned: TPositionedLayer read FLayer;
    property LayerSize: TPoint read FLayerSize;
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

  TWindowLayerFixedSizeWithBitmap = class(TWindowLayerBasic)
  protected
    FLayer: TBitmapLayer;
    function GetLayerSizeForViewSize(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    procedure DoRedraw; override;
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

  FLayer := ALayer;

  FLayer.MouseEvents := false;
  FLayer.Visible := false;
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
  FLayer := nil;
  FVisibleChangeNotifier := nil;
  inherited;
end;

procedure TWindowLayerBasic.DoHide;
begin
  FVisible := False;
  FLayer.Visible := False;
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

procedure TWindowLayerBasic.DoUpdateLayerSize(ANewSize: TPoint);
begin
  FLayerSize := ANewSize;
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

procedure TWindowLayerBasic.LoadConfig(AConfigProvider: IConfigDataProvider);
begin
  // По умолчанию ничего не делаем
end;

procedure TWindowLayerBasic.OnViewSizeChange(Sender: TObject);
var
  VNewConverter: ILocalCoordConverter;
begin
  VNewConverter := FViewPortState.GetVisualCoordConverter;
  UpdateLayerSize(GetLayerSizeForViewSize(VNewConverter));
  UpdateLayerLocation(GetMapLayerLocationRect);
end;

procedure TWindowLayerBasic.UpdateLayerLocation(ANewLocation: TFloatRect);
begin
  if FVisible then begin
    DoUpdateLayerLocation(ANewLocation);
  end;
end;

procedure TWindowLayerBasic.UpdateLayerSize(ANewSize: TPoint);
begin
  if FVisible then begin
    if (FLayerSize.X <> ANewSize.X) or (FLayerSize.Y <> ANewSize.Y) then begin
      DoUpdateLayerSize(ANewSize);
      UpdateLayerLocation(GetMapLayerLocationRect);
    end;
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
    UpdateLayerSize(GetLayerSizeForViewSize(FVisualCoordConverter));
    UpdateLayerLocation(GetMapLayerLocationRect);
    Redraw;
    FVisibleChangeNotifier.Notify(nil);
  end;
end;

procedure TWindowLayerBasic.StartThreads;
begin
  // По умолчанию ничего не делаем
end;

{ TWindowLayerBasicFixedSizeWithBitmap }

constructor TWindowLayerFixedSizeWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

procedure TWindowLayerFixedSizeWithBitmap.DoRedraw;
begin
end;

function TWindowLayerFixedSizeWithBitmap.GetLayerSizeForViewSize(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  Result := FLayerSize;
end;

end.
