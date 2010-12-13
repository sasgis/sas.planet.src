unit u_MapLayerBasic;

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
  TMapLayerBasicNoBitmap = class(TWindowLayerBasic)
  protected
    FMapViewSize: TPoint;
    FVisualCoordConverter: ILocalCoordConverter;

    FPosChangeListener: IJclListener;
    FScaleChangeListener: IJclListener;

    procedure OnScaleChange(Sender: TObject);
    procedure OnPosChange(Sender: TObject); virtual;
    procedure OnViewSizeChange(Sender: TObject); override;
    function GetMapLayerLocationRect: TFloatRect; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure Redraw; override;
    property VisibleCoordConverter: ILocalCoordConverter read FVisualCoordConverter;
  end;

  TMapLayerBasic = class(TMapLayerBasicNoBitmap)
  protected
    FBitmapCoordConverter: ILocalCoordConverter;

    FLayer: TBitmapLayer;
    function CreateLayer(ALayerCollection: TLayerCollection): TPositionedLayer; override;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure OnPosChange(Sender: TObject); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure Hide; override;
  end;

implementation

uses
  Types,
  Forms,
  Graphics,
  u_NotifyEventListener,
  u_GlobalState;

{ TGPSTrackLayer }

constructor TMapLayerBasicNoBitmap.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FPosChangeListener := TNotifyEventListener.Create(Self.OnPosChange);
  FViewPortState.PosChangeNotifier.Add(FPosChangeListener);
  FScaleChangeListener := TNotifyEventListener.Create(Self.OnScaleChange);
  FViewPortState.ScaleChangeNotifier.Add(FScaleChangeListener);
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter
end;

destructor TMapLayerBasicNoBitmap.Destroy;
begin
  FViewPortState.PosChangeNotifier.Remove(FPosChangeListener);
  FPosChangeListener := nil;
  FViewPortState.ScaleChangeNotifier.Remove(FScaleChangeListener);
  FScaleChangeListener := nil;
  FVisualCoordConverter := nil;
  inherited;
end;

function TMapLayerBasicNoBitmap.GetMapLayerLocationRect: TFloatRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := FMapViewSize.X;
  Result.Bottom := FMapViewSize.Y;
end;

procedure TMapLayerBasicNoBitmap.OnPosChange(Sender: TObject);
begin
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter;
  Redraw;
end;

procedure TMapLayerBasicNoBitmap.OnScaleChange(Sender: TObject);
begin
  FLayerPositioned.Location := GetMapLayerLocationRect;
end;

procedure TMapLayerBasicNoBitmap.OnViewSizeChange(Sender: TObject);
begin
  FMapViewSize := FViewPortState.GetViewSizeInVisiblePixel;
  inherited;
end;

procedure TMapLayerBasicNoBitmap.Redraw;
begin
  if FVisualCoordConverter <> nil then begin
    inherited;
  end;
end;

{ TMapLayerBasic }

constructor TMapLayerBasic.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FLayer := TBitmapLayer(FLayerPositioned);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

function TMapLayerBasic.CreateLayer(
  ALayerCollection: TLayerCollection): TPositionedLayer;
begin
  Result := TBitmapLayer.Create(ALayerCollection);
end;

function TMapLayerBasic.GetMapLayerLocationRect: TFloatRect;
begin

end;

//procedure TMapLayerBasic.DoResizeBitmap;
//var
//  VBitmapSizeInPixel: TPoint;
//begin
//  inherited;
//  VBitmapSizeInPixel := GetBitmapSizeInPixel;
//  if (FLayer.Bitmap.Width <> VBitmapSizeInPixel.X) or (FLayer.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
//    FLayer.Bitmap.Lock;
//    try
//      FLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
//    finally
//      FLayer.Bitmap.Unlock;
//    end;
//  end;
//end;

procedure TMapLayerBasic.Hide;
begin
  inherited;
  FLayer.Bitmap.Lock;
  try
    FLayer.Bitmap.SetSize(0, 0);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

procedure TMapLayerBasic.OnPosChange(Sender: TObject);
begin
  inherited;

end;

//function TMapLayerBasic.GetBitmapSizeInPixel: TPoint;
//begin
//  Result.X := GState.ScreenSize.X + 2 * 256 * GState.TilesOut;
//  Result.Y := GState.ScreenSize.Y + 2 * 256 * GState.TilesOut;
//end;
//
end.
