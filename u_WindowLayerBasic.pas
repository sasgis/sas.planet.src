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
    FScreenCenterPos: TPoint;
    FScale: Double;
    FScaleCenter: TPoint;

    function GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean); virtual;

    function GetBitmapSizeInPixel: TPoint; virtual;
    function GetVisibleSizeInPixel: TPoint; virtual;
    function GetCenterMove: TPoint; virtual;

    function GetMapLayerLocationRect: TRect; virtual;

    function BitmapPixel2VisiblePixel(Pnt: TPoint): TPoint; overload; virtual;
    function BitmapPixel2VisiblePixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    function VisiblePixel2BitmapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function VisiblePixel2BitmapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
  public
    constructor Create(AParentMap: TImage32);
    procedure Resize; virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure Redraw; virtual; abstract;
    property Visible: Boolean read GetVisible write SetVisible;
    property MapLayerVisibleLocationRect: TRect read GetMapLayerLocationRect;
  end;


implementation

uses
  Forms,
  u_GlobalState;

constructor TWindowLayerBasic.Create(AParentMap: TImage32);
var
  VBitmapSizeInPixel: TPoint;
begin
  FScale := 1;
  FParentMap := AParentMap;
  FLayer := TBitmapLayer.Create(FParentMap.Layers);

  VBitmapSizeInPixel := GetBitmapSizeInPixel;

  FLayer.Bitmap.Width := VBitmapSizeInPixel.X;
  FLayer.Bitmap.Height := VBitmapSizeInPixel.Y;
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
  FLayer.Bitmap.Width := 0;
  FLayer.Bitmap.Height := 0;
end;

procedure TWindowLayerBasic.Resize;
begin
  if FLayer.Visible then begin
    FLayer.Location := floatrect(GetMapLayerLocationRect);
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
var
  VBitmapSizeInPixel: TPoint;
begin
  VBitmapSizeInPixel := GetBitmapSizeInPixel;

  FLayer.Bitmap.Width := VBitmapSizeInPixel.X;
  FLayer.Bitmap.Height := VBitmapSizeInPixel.Y;

  FLayer.Visible := True;
  FLayer.BringToFront;
  Resize;
  Redraw;
end;


function TWindowLayerBasic.GetVisibleSizeInPixel: TPoint;
begin
  Result.X := FParentMap.Width;
  Result.Y := FParentMap.Height;
end;

function TWindowLayerBasic.GetBitmapSizeInPixel: TPoint;
begin
  Result.X := Screen.Width + 2 * 256 * GState.TilesOut;
  Result.Y := Screen.Height + 2 * 256 * GState.TilesOut;
end;

function TWindowLayerBasic.GetMapLayerLocationRect: TRect;
var
  VBitmapSize: TPoint;
begin
  VBitmapSize := GetBitmapSizeInPixel;
  Result.TopLeft := BitmapPixel2VisiblePixel(Point(0, 0));
  Result.BottomRight := BitmapPixel2VisiblePixel(VBitmapSize);
end;


function TWindowLayerBasic.VisiblePixel2BitmapPixel(Pnt: TPoint): TPoint;
var
  VVisibleSize: TPoint;
  VBitmapSize: TPoint;
  VCenterMove: TPoint;
begin
  VVisibleSize := GetVisibleSizeInPixel;
  VBitmapSize := GetBitmapSizeInPixel;
  VCenterMove := GetCenterMove;

  Result.X := Trunc(((Pnt.X + VCenterMove.X) - FScaleCenter.X) / FScale + FScaleCenter.X + (VBitmapSize.X - VVisibleSize.X) / 2);
  Result.Y := Trunc(((Pnt.Y + VCenterMove.Y) - FScaleCenter.Y) / FScale + FScaleCenter.Y + (VBitmapSize.Y - VVisibleSize.Y) / 2);
end;

function TWindowLayerBasic.VisiblePixel2BitmapPixel(
  Pnt: TExtendedPoint): TExtendedPoint;
var
  VVisibleSize: TPoint;
  VBitmapSize: TPoint;
  VCenterMove: TPoint;
begin
  VVisibleSize := GetVisibleSizeInPixel;
  VBitmapSize := GetBitmapSizeInPixel;
  VCenterMove := GetCenterMove;

  Result.X := ((Pnt.X + VCenterMove.X) - FScaleCenter.X) / FScale + FScaleCenter.X + (VBitmapSize.X - VVisibleSize.X) / 2;
  Result.Y := ((Pnt.Y + VCenterMove.Y) - FScaleCenter.Y) / FScale + FScaleCenter.Y + (VBitmapSize.Y - VVisibleSize.Y) / 2;
end;

function TWindowLayerBasic.BitmapPixel2VisiblePixel(Pnt: TPoint): TPoint;
var
  VVisibleSize: TPoint;
  VBitmapSize: TPoint;
  VCenterMove: TPoint;
begin
  VVisibleSize := GetVisibleSizeInPixel;
  VBitmapSize := GetBitmapSizeInPixel;
  VCenterMove := GetCenterMove;

  Result.X := trunc(((Pnt.X - (VBitmapSize.X - VVisibleSize.X) / 2) - FScaleCenter.X) * FScale + FScaleCenter.X - VCenterMove.X);
  Result.Y := trunc(((Pnt.Y - (VBitmapSize.Y - VVisibleSize.Y) / 2) - FScaleCenter.Y) * FScale + FScaleCenter.Y - VCenterMove.Y);
end;

function TWindowLayerBasic.BitmapPixel2VisiblePixel(
  Pnt: TExtendedPoint): TExtendedPoint;
var
  VVisibleSize: TPoint;
  VBitmapSize: TPoint;
  VCenterMove: TPoint;
begin
  VVisibleSize := GetVisibleSizeInPixel;
  VBitmapSize := GetBitmapSizeInPixel;
  VCenterMove := GetCenterMove;

  Result.X := ((Pnt.X - (VBitmapSize.X - VVisibleSize.X) / 2) - FScaleCenter.X) * FScale + FScaleCenter.X - VCenterMove.X;
  Result.Y := ((Pnt.Y - (VBitmapSize.Y - VVisibleSize.Y) / 2) - FScaleCenter.Y) * FScale + FScaleCenter.Y - VCenterMove.Y;
end;

function TWindowLayerBasic.GetCenterMove: TPoint;
begin
  Result := Point(0, 0);
end;

end.
