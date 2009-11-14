unit u_MapLayerBasic;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  GR32_Layers,
  u_WindowLayerBasic,
  t_GeoTypes;


type
  TMapLayerBasic =  class(TWindowLayerBasic)
  protected
    FCenterMove: TPoint;
    FScreenCenterPos: TPoint;
    function GetCenterMove: TPoint; override;
    procedure SetScreenCenterPos(const Value: TPoint);

    function VisiblePixel2MapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function VisiblePixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    function MapPixel2VisiblePixel(Pnt: TPoint): TPoint; overload; virtual;
    function MapPixel2VisiblePixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;

    function BitmapPixel2MapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function BitmapPixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    function MapPixel2BitmapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function MapPixel2BitmapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    procedure MoveTo(Pnt: TPoint); virtual;
    procedure ScaleTo(AScale: Double; ACenterPoint: TPoint); virtual;
    property ScreenCenterPos: TPoint read FScreenCenterPos write SetScreenCenterPos;
  end;
implementation

uses
  Types,
  Forms,
  Graphics,
  Math,
  u_GlobalState;

{ TGPSTrackLayer }

constructor TMapLayerBasic.Create(AParentMap: TImage32; ACenter: TPoint);
begin
  inherited Create(AParentMap);
  FScreenCenterPos := ACenter;
end;

procedure TMapLayerBasic.SetScreenCenterPos(const Value: TPoint);
begin
  FScreenCenterPos := Value;
  FCenterMove := Point(0, 0);
  FScale := 1;
  Redraw;
  Resize;
end;

function TMapLayerBasic.BitmapPixel2MapPixel(Pnt: TPoint): TPoint;
var
  VSizeInPixel: TPoint;
begin
  VSizeInPixel := GetBitmapSizeInPixel;
  Result.X := ScreenCenterPos.X - VSizeInPixel.X div 2 + Pnt.X;
  Result.Y := ScreenCenterPos.X - VSizeInPixel.X div 2 + Pnt.y;
end;

function TMapLayerBasic.BitmapPixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VSizeInPixel: TPoint;
begin
  VSizeInPixel := GetBitmapSizeInPixel;
  Result.X := ScreenCenterPos.X - VSizeInPixel.X / 2 + Pnt.X;
  Result.Y := ScreenCenterPos.X - VSizeInPixel.X / 2 + Pnt.y;
end;

function TMapLayerBasic.MapPixel2BitmapPixel(Pnt: TPoint): TPoint;
var
  VSize: TPoint;
begin
  VSize := GetBitmapSizeInPixel;
  Result.X := Pnt.X - ScreenCenterPos.X + (VSize.X div 2);
  Result.Y := Pnt.Y - ScreenCenterPos.Y + (VSize.Y div 2);
end;

function TMapLayerBasic.MapPixel2BitmapPixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VSize: TPoint;
begin
  VSize := GetBitmapSizeInPixel;
  Result.X := Pnt.X - ScreenCenterPos.X + (VSize.X / 2);
  Result.Y := Pnt.Y - ScreenCenterPos.Y + (VSize.Y / 2);
end;

function TMapLayerBasic.MapPixel2VisiblePixel(Pnt: TPoint): TPoint;
var
  VPoint: TPoint;
begin
  VPoint := MapPixel2BitmapPixel(Pnt);
  Result := BitmapPixel2VisiblePixel(Pnt);
end;

function TMapLayerBasic.MapPixel2VisiblePixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VPoint: TExtendedPoint;
begin
  VPoint := MapPixel2BitmapPixel(Pnt);
  Result := BitmapPixel2VisiblePixel(VPoint);
end;

function TMapLayerBasic.VisiblePixel2MapPixel(Pnt:TPoint):TPoint;
var
  VPoint: TPoint;
begin
  VPoint := VisiblePixel2BitmapPixel(Pnt);
  Result := BitmapPixel2MapPixel(VPoint);
end;

function TMapLayerBasic.VisiblePixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VPoint: TExtendedPoint;
begin
  VPoint := VisiblePixel2BitmapPixel(Pnt);
  Result := BitmapPixel2MapPixel(VPoint);
end;

procedure TMapLayerBasic.MoveTo(Pnt: TPoint);
begin
  FCenterMove := Pnt;
  Resize;
end;

procedure TMapLayerBasic.ScaleTo(AScale: Double; ACenterPoint: TPoint);
begin
  FScaleCenter := ACenterPoint;
  FScale := AScale;
  Resize;
end;

function TMapLayerBasic.GetCenterMove: TPoint;
begin
  Result := FCenterMove;
end;


end.
