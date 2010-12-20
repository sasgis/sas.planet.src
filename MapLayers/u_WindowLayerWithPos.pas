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
  protected
    FVisualCoordConverter: ILocalCoordConverter;
    FPosChangeListener: IJclListener;
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
    FParentMap: TImage32;
    FLayer: TBitmapLayer;
    procedure DoUpdateLayerSize(ANewSize: TPoint); override;
    procedure DoHide; override;
    procedure DoShow; override;
    function GetBitmapSizeInPixel: TPoint; virtual; abstract;
    procedure OnViewSizeChange(Sender: TObject); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
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
  DoPosChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerWithPos.OnPosChange(Sender: TObject);
begin
  PosChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerWithPos.PosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  if FVisible then begin
    DoPosChange(ANewVisualCoordConverter);
    UpdateLayerLocation(GetMapLayerLocationRect);
  end;
end;

{ TWindowLayerWithPosWithBitmap }

constructor TWindowLayerWithPosWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  FParentMap := AParentMap;
  FLayer := TBitmapLayer.Create(FParentMap.Layers);
  inherited Create(FLayer, AViewPortState);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

procedure TWindowLayerWithPosWithBitmap.DoHide;
begin
  inherited;

end;

procedure TWindowLayerWithPosWithBitmap.DoShow;
begin
  inherited;

end;

procedure TWindowLayerWithPosWithBitmap.DoUpdateLayerSize(ANewSize: TPoint);
begin
  inherited;

end;

procedure TWindowLayerWithPosWithBitmap.OnViewSizeChange(Sender: TObject);
begin
  inherited;

end;

end.
