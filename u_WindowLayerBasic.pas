unit u_WindowLayerBasic;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_JclNotify,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState;

type

  TWindowLayerBasic = class
  protected
    FParentMap: TImage32;
    FLayerPositioned: TPositionedLayer;
    FViewPortState: TMapViewPortState;
    FMapPosChangeListener: IJclListener;
    FVisibleChangeNotifier: IJclNotifier;

    function GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean); virtual;

    function CreateLayer(ALayerCollection: TLayerCollection): TPositionedLayer; virtual;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); virtual;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); virtual;
    procedure Show; virtual; abstract;
    procedure Hide; virtual;
    procedure Redraw; virtual; abstract;
    property Visible: Boolean read GetVisible write SetVisible;
    property VisibleChangeNotifier: IJclNotifier read FVisibleChangeNotifier;
  end;

  TWindowLayerBasicOld = class(TWindowLayerBasic)
  protected

{
 Используемые системы координат:
 VisualPixel - координаты в пикселах компонента ParentMap
 BitmapPixel - координаты в пикселах битмапа текущего слоя
}

    // Получает размер отображаемого изображения. По сути коордниаты картинки в системе VisualPixel
    function GetVisibleSizeInPixel: TPoint; virtual;
    // Размеры битмапки текущего слоя. по сути координаты правого нижнего угла картинки в системе BitmapPixel
    function GetBitmapSizeInPixel: TPoint; virtual; abstract;

    // Коэффициент масштабирования
    function GetScale: double; virtual;

    // Координаты зафиксированной точки в сисетме VisualPixel
    function GetFreezePointInVisualPixel: TPoint; virtual; abstract;
    // Координаты зафиксированной точки в сисетме BitmapPixel
    function GetFreezePointInBitmapPixel: TPoint; virtual; abstract;

    function BitmapPixel2VisiblePixel(Pnt: TPoint): TPoint; overload; virtual;
    function BitmapPixel2VisiblePixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    function VisiblePixel2BitmapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function VisiblePixel2BitmapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;

    // Переводит координаты прямоугольника битмапки в координаты VisualPixel
    function GetMapLayerLocationRect: TRect; virtual;

    procedure DoRedraw; virtual; abstract;
    procedure DoResizeBitmap; virtual;
    procedure DoResize; virtual;
  public
    procedure Show; override;
    procedure Resize; virtual;
    procedure Redraw; override;
  end;

  TWindowLayerBasicWithBitmap = class(TWindowLayerBasicOld)
  protected
    FLayer: TBitmapLayer;
    function CreateLayer(ALayerCollection: TLayerCollection): TPositionedLayer; override;
    procedure DoResizeBitmap; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure Hide; override;
  end;

implementation

uses
  Forms,
  Types,
  u_JclNotify;

{ TWindowLayerBasic }

constructor TWindowLayerBasic.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  FParentMap := AParentMap;
  FViewPortState := AViewPortState;

  FLayerPositioned := TBitmapLayer.Create(FParentMap.Layers);

  FLayerPositioned.MouseEvents := false;
  FLayerPositioned.Visible := false;

  FVisibleChangeNotifier := TJclBaseNotifier.Create;
end;

function TWindowLayerBasic.CreateLayer(
  ALayerCollection: TLayerCollection): TPositionedLayer;
begin
  Result := TPositionedLayer.Create(ALayerCollection);
end;

destructor TWindowLayerBasic.Destroy;
begin
  FMapPosChangeListener := nil;
  FViewPortState := nil;
  FParentMap := nil;
  FLayerPositioned := nil;
  FVisibleChangeNotifier := nil;
  inherited;
end;

function TWindowLayerBasic.GetVisible: Boolean;
begin
  Result := FLayerPositioned.Visible;
end;

procedure TWindowLayerBasic.Hide;
begin
  if FLayerPositioned.Visible then begin
    FLayerPositioned.Visible := False;
    FVisibleChangeNotifier.Notify(nil);
  end;
end;

procedure TWindowLayerBasic.LoadConfig(AConfigProvider: IConfigDataProvider);
begin
  // По умолчанию ничего не делаем
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

procedure TWindowLayerBasic.StartThreads;
begin
  // По умолчанию ничего не делаем
end;

{ TWindowLayerBasicOld }

procedure TWindowLayerBasicOld.Resize;
begin
  if FLayerPositioned.Visible then begin
    DoResize;
  end;
end;

procedure TWindowLayerBasicOld.Show;
begin
  if not FLayerPositioned.Visible then begin
    FLayerPositioned.Visible := True;
    FVisibleChangeNotifier.Notify(nil);
    Resize;
    Redraw;
  end;
end;

function TWindowLayerBasicOld.GetScale: double;
begin
  Result := 1;
end;

procedure TWindowLayerBasicOld.Redraw;
begin
  if Visible then begin
    DoResizeBitmap;
    DoRedraw;
  end;
end;

procedure TWindowLayerBasicOld.DoResizeBitmap;
begin
end;

procedure TWindowLayerBasicOld.DoResize;
begin
  FLayerPositioned.Location := floatrect(GetMapLayerLocationRect);
end;

function TWindowLayerBasicOld.GetVisibleSizeInPixel: TPoint;
begin
  Result.X := FParentMap.Width;
  Result.Y := FParentMap.Height;
end;

function TWindowLayerBasicOld.GetMapLayerLocationRect: TRect;
var
  VBitmapSize: TPoint;
begin
  VBitmapSize := GetBitmapSizeInPixel;
  Result.TopLeft := BitmapPixel2VisiblePixel(Point(0, 0));
  Result.BottomRight := BitmapPixel2VisiblePixel(VBitmapSize);
end;

function TWindowLayerBasicOld.VisiblePixel2BitmapPixel(
  Pnt: TExtendedPoint): TExtendedPoint;
var
  VFreezePointInVisualPixel: TPoint;
  VFreezePointInBitmapPixel: TPoint;
  VScale: double;
begin
  VFreezePointInVisualPixel := GetFreezePointInVisualPixel;
  VFreezePointInBitmapPixel := GetFreezePointInBitmapPixel;
  VScale := GetScale;

  Result.X := (Pnt.X - VFreezePointInVisualPixel.X) / VScale + VFreezePointInBitmapPixel.X;
  Result.Y := (Pnt.Y - VFreezePointInVisualPixel.Y) / VScale + VFreezePointInBitmapPixel.Y;
end;

function TWindowLayerBasicOld.VisiblePixel2BitmapPixel(Pnt: TPoint): TPoint;
var
  VFreezePointInVisualPixel: TPoint;
  VFreezePointInBitmapPixel: TPoint;
  VScale: double;
begin
  VFreezePointInVisualPixel := GetFreezePointInVisualPixel;
  VFreezePointInBitmapPixel := GetFreezePointInBitmapPixel;
  VScale := GetScale;

  Result.X := Trunc((Pnt.X - VFreezePointInVisualPixel.X) / VScale + VFreezePointInBitmapPixel.X);
  Result.Y := Trunc((Pnt.Y - VFreezePointInVisualPixel.Y) / VScale + VFreezePointInBitmapPixel.Y);
end;


function TWindowLayerBasicOld.BitmapPixel2VisiblePixel(Pnt: TPoint): TPoint;
var
  VFreezePointInVisualPixel: TPoint;
  VFreezePointInBitmapPixel: TPoint;
  VScale: double;
begin
  VFreezePointInVisualPixel := GetFreezePointInVisualPixel;
  VFreezePointInBitmapPixel := GetFreezePointInBitmapPixel;
  VScale := GetScale;

  Result.X := Trunc((Pnt.X - VFreezePointInBitmapPixel.X) * VScale + VFreezePointInVisualPixel.X);
  Result.Y := Trunc((Pnt.Y - VFreezePointInBitmapPixel.Y) * VScale + VFreezePointInVisualPixel.Y);
end;

function TWindowLayerBasicOld.BitmapPixel2VisiblePixel(
  Pnt: TExtendedPoint): TExtendedPoint;
var
  VFreezePointInVisualPixel: TPoint;
  VFreezePointInBitmapPixel: TPoint;
  VScale: double;
begin
  VFreezePointInVisualPixel := GetFreezePointInVisualPixel;
  VFreezePointInBitmapPixel := GetFreezePointInBitmapPixel;
  VScale := GetScale;

  Result.X := (Pnt.X - VFreezePointInBitmapPixel.X) * VScale + VFreezePointInVisualPixel.X;
  Result.Y := (Pnt.Y - VFreezePointInBitmapPixel.Y) * VScale + VFreezePointInVisualPixel.Y;
end;

{ TWindowLayerBasicWithBitmap }

constructor TWindowLayerBasicWithBitmap.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FLayer := TBitmapLayer(FLayerPositioned);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
end;

function TWindowLayerBasicWithBitmap.CreateLayer(ALayerCollection: TLayerCollection): TPositionedLayer;
begin
  Result := TBitmapLayer.Create(ALayerCollection);
end;

procedure TWindowLayerBasicWithBitmap.DoResizeBitmap;
var
  VBitmapSizeInPixel: TPoint;
begin
  inherited;
  VBitmapSizeInPixel := GetBitmapSizeInPixel;
  if (FLayer.Bitmap.Width <> VBitmapSizeInPixel.X) or (FLayer.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
    FLayer.Bitmap.Lock;
    try
      FLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
    finally
      FLayer.Bitmap.Unlock;
    end;
  end;
end;

procedure TWindowLayerBasicWithBitmap.Hide;
begin
  inherited;
  FLayer.Bitmap.Lock;
  try
    FLayer.Bitmap.SetSize(0, 0);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

end.
