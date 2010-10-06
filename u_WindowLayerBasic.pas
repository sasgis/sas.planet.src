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
  u_MapViewPortState,
  u_BitmapLayerWithSortIndex;

type
  TWindowLayerBasic = class
  protected
    FParentMap: TImage32;
    FLayer: TBitmapLayerWithSortIndex;
    FViewPortState: TMapViewPortState;
    FMapPosChangeListener: IJclListener;

    function GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean); virtual;

    // В наследниках нужно перекрывать если уровень слоя выше чем бекграунд.
    function GetSortIndexForLayer: Integer; virtual;

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

    procedure DoResizeBitmap; virtual;
    procedure DoRedraw; virtual; abstract;
    procedure DoResize; virtual;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); virtual;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); virtual;
    procedure Resize; virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure Redraw; virtual;
    property Visible: Boolean read GetVisible write SetVisible;
  end;


implementation

uses
  Forms,
  Types;

constructor TWindowLayerBasic.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  FParentMap := AParentMap;
  FLayer := TBitmapLayerWithSortIndex.Create(FParentMap.Layers, GetSortIndexForLayer);

  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FLayer.bitmap.Font.Charset := RUSSIAN_CHARSET;
  FLayer.MouseEvents := false;
  FLayer.Visible := false;
end;

function TWindowLayerBasic.GetVisible: Boolean;
begin
  Result := FLayer.Visible;
end;

procedure TWindowLayerBasic.Hide;
begin
  FLayer.Visible := False;
  FLayer.Bitmap.Lock;
  try
    FLayer.Bitmap.SetSize(0, 0);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

procedure TWindowLayerBasic.LoadConfig(AConfigProvider: IConfigDataProvider);
begin
  // По умолчанию ничего не делаем
end;

procedure TWindowLayerBasic.Resize;
begin
  if FLayer.Visible then begin
    DoResize;
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
  if not FLayer.Visible then begin
    FLayer.Visible := True;
    Resize;
    Redraw;
  end;
end;

procedure TWindowLayerBasic.StartThreads;
begin
  // По умолчанию ничего не делаем
end;

function TWindowLayerBasic.GetScale: double;
begin
  Result := 1;
end;

procedure TWindowLayerBasic.Redraw;
begin
  if Visible then begin
    DoResizeBitmap;
    DoRedraw;
  end;
end;

procedure TWindowLayerBasic.DoResizeBitmap;
var
  VBitmapSizeInPixel: TPoint;
begin
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


procedure TWindowLayerBasic.DoResize;
begin
  FLayer.Location := floatrect(GetMapLayerLocationRect);
end;

function TWindowLayerBasic.GetVisibleSizeInPixel: TPoint;
begin
  Result.X := FParentMap.Width;
  Result.Y := FParentMap.Height;
end;

function TWindowLayerBasic.GetMapLayerLocationRect: TRect;
var
  VBitmapSize: TPoint;
begin
  VBitmapSize := GetBitmapSizeInPixel;
  Result.TopLeft := BitmapPixel2VisiblePixel(Point(0, 0));
  Result.BottomRight := BitmapPixel2VisiblePixel(VBitmapSize);
end;


function TWindowLayerBasic.VisiblePixel2BitmapPixel(
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

function TWindowLayerBasic.VisiblePixel2BitmapPixel(Pnt: TPoint): TPoint;
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


function TWindowLayerBasic.BitmapPixel2VisiblePixel(Pnt: TPoint): TPoint;
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

function TWindowLayerBasic.BitmapPixel2VisiblePixel(
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

function TWindowLayerBasic.GetSortIndexForLayer: Integer;
begin
  Result := 0;
end;

end.
