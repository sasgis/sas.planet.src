unit u_WindowLayerWithPos;

interface

uses
  Windows,
  GR32,
  GR32_Layers,
  GR32_Image,
  i_JclNotify,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  u_MapViewPortState,
  u_WindowLayerBasic,
  t_GeoTypes;

type
  TWindowLayerWithPos = class(TWindowLayerBasic)
  private
    FPosChangeListener: IJclListener;
  protected
    FVisualCoordConverter: ILocalCoordConverter;
    procedure OnPosChange(Sender: TObject); virtual;
    procedure PosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoShow; override;
  public
    constructor Create(ALayer: TPositionedLayer; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
  end;

  TWindowLayerWithPosWithBitmap = class(TWindowLayerWithPos)
  protected
    FLayer: TBitmapLayer;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
  end;

  TWindowLayerFixedSizeWithPosWithBitmap = class(TWindowLayerWithPosWithBitmap)
  protected
    function GetLayerSizeForViewSize(AViewSize: TPoint): TPoint; override;
    procedure DoRedraw; override;
  end;



implementation

uses
  Types,
  Forms,
  Graphics,
  u_NotifyEventListener,
  Ugeofun,
  u_GlobalState;

{ TWindowLayerWithPos }

constructor TWindowLayerWithPos.Create(ALayer: TPositionedLayer;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FPosChangeListener := TNotifyEventListener.Create(Self.OnPosChange);
  FViewPortState.PosChangeNotifier.Add(FPosChangeListener);
  PosChange(FViewPortState.GetVisualCoordConverter);
end;

destructor TWindowLayerWithPos.Destroy;
begin
  FViewPortState.PosChangeNotifier.Remove(FPosChangeListener);
  FPosChangeListener := nil;
  inherited;
end;

procedure TWindowLayerWithPos.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  FVisualCoordConverter := ANewVisualCoordConverter;
end;

procedure TWindowLayerWithPos.DoShow;
begin
  inherited;
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter;
end;

procedure TWindowLayerWithPos.OnPosChange(Sender: TObject);
begin
  PosChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerWithPos.PosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  if Visible then begin
    DoPosChange(ANewVisualCoordConverter);
    UpdateLayerLocation(GetMapLayerLocationRect);
  end;
end;

{ TWindowLayerWithPosWithBitmap }

constructor TWindowLayerWithPosWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

{ TWindowLayerFixedSizeWithPosWithBitmap }

procedure TWindowLayerFixedSizeWithPosWithBitmap.DoRedraw;
begin
  inherited;
end;

function TWindowLayerFixedSizeWithPosWithBitmap.GetLayerSizeForViewSize(
  AViewSize: TPoint): TPoint;
begin
  Result := LayerSize;
end;

end.
