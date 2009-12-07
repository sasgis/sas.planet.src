unit u_WindowLayerBasic;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  GR32_Layers,
  t_GeoTypes;

type
  TWindowLayerBasic =  class
  protected
    FParentMap: TImage32;
    FLayer: TBitmapLayer;
{
 Используемые системы координат:
 VisualPixel - координаты в пикселах компонента ParentMap
 BitmapPixel - координаты в пикселах битмапа текущего слоя
}
    function GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean); virtual;

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

    procedure  DoRedraw; virtual; abstract;
    procedure  DoResize; virtual;
  public
    constructor Create(AParentMap: TImage32);
    procedure Resize; virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure Redraw; virtual;
    property Visible: Boolean read GetVisible write SetVisible;
  end;


implementation

uses
  Forms,
  u_GlobalState, Types;

constructor TWindowLayerBasic.Create(AParentMap: TImage32);
begin
  FParentMap := AParentMap;
  FLayer := TBitmapLayer.Create(FParentMap.Layers);

  FLayer.Bitmap.DrawMode:=dmBlend;
  FLayer.Bitmap.CombineMode:=cmMerge;
  FLayer.bitmap.Font.Charset:=RUSSIAN_CHARSET;
  FLayer.Visible:=false;
end;

function TWindowLayerBasic.GetVisible: Boolean;
begin
  Result := FLayer.Visible;
end;

procedure TWindowLayerBasic.Hide;
begin
  FLayer.Visible := False;
  FLayer.SendToBack;
  FLayer.Bitmap.SetSize(0, 0);
end;

procedure TWindowLayerBasic.Resize;
begin
  if FLayer.Visible then begin
    DoResize;
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
  FLayer.Visible := True;
  FLayer.BringToFront;
  Resize;
  Redraw;
end;

function TWindowLayerBasic.GetScale: double;
begin
  Result := 1;
end;

procedure TWindowLayerBasic.Redraw;
begin
  if Visible then begin
    DoRedraw;
  end;
end;

procedure TWindowLayerBasic.DoResize;
var
  VBitmapSizeInPixel: TPoint;
begin
  VBitmapSizeInPixel := GetBitmapSizeInPixel;
  FLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
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

end.
