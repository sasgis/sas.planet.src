unit u_MapLayerBasic;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  GR32_Layers,
  t_GeoTypes;


type
  TMapLayerBasic =  class
  protected
    FParentMap: TImage32;
    FLayer: TBitmapLayer;
    FScreenCenterPos: TPoint;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    procedure SetScreenCenterPos(const Value: TPoint);

    function GetLoadedPixelRect: TRect; virtual;
    function GetVisiblePixelRect: TRect; virtual;
    function GetLoadedSizeInPixel: TPoint; virtual;
    function GetLoadedSizeInTile: TPoint; virtual;
    function GetVisibleTopLeft: TPoint; virtual;
    function GetVisibleSizeInPixel: TPoint; virtual;
    function GetMapLayerLocationRect: TRect; virtual;
    function GetLoadedTopLeft: TPoint; virtual;

    function VisiblePixel2MapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function VisiblePixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    function MapPixel2VisiblePixel(Pnt: TPoint): TPoint; overload; virtual;
    function MapPixel2VisiblePixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    function VisiblePixel2LoadedPixel(Pnt: TPoint): TPoint; virtual;

    function LoadedPixel2MapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function LoadedPixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    function MapPixel2LoadedPixel(Pnt: TPoint): TPoint; overload; virtual;
    function MapPixel2LoadedPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;

    property VisibleTopLeft: TPoint read GetVisibleTopLeft;
    property VisibleSizeInPixel: TPoint read GetVisibleSizeInPixel;
    property VisiblePixelRect: TRect read GetVisiblePixelRect;

    property LoadedTopLeft: TPoint read GetLoadedTopLeft;
    property LoadedPixelRect: TRect read GetLoadedPixelRect;
    property LoadedSizeInTile: TPoint read GetLoadedSizeInTile;
    property LoadedSizeInPixel: TPoint read GetLoadedSizeInPixel;

    property MapLayerLocationRect: TRect read GetMapLayerLocationRect;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    procedure Resize; virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure Redraw; virtual;
    property Visible: Boolean read GetVisible write SetVisible;
    property ScreenCenterPos: TPoint read FScreenCenterPos write SetScreenCenterPos;
  end;
implementation

uses
  Types,
  Forms,
  Graphics,
  u_GlobalState;

{ TGPSTrackLayer }

constructor TMapLayerBasic.Create(AParentMap: TImage32; ACenter: TPoint);
var
  VLoadedSizeInPixel: TPoint;
begin
  FParentMap := AParentMap;
  FLayer := TBitmapLayer.Create(FParentMap.Layers);
  FScreenCenterPos := ACenter;

  VLoadedSizeInPixel := LoadedSizeInPixel;

  FLayer.Bitmap.Width := VLoadedSizeInPixel.X;
  FLayer.Bitmap.Height := VLoadedSizeInPixel.Y;
  FLayer.Bitmap.DrawMode:=dmBlend;
  FLayer.Bitmap.CombineMode:=cmMerge;
  FLayer.bitmap.Font.Charset:=RUSSIAN_CHARSET;
  FLayer.Visible:=false;

end;

function TMapLayerBasic.GetVisible: Boolean;
begin
  Result := FLayer.Visible;
end;

procedure TMapLayerBasic.Hide;
begin
  FLayer.Visible := False;
  FLayer.SendToBack;
  FLayer.Bitmap.Clear(clBlack);
end;

procedure TMapLayerBasic.Redraw;
begin
  FLayer.Location := floatrect(MapLayerLocationRect);

end;

procedure TMapLayerBasic.Resize;
begin
  FLayer.Location := floatrect(MapLayerLocationRect);
end;

procedure TMapLayerBasic.SetScreenCenterPos(const Value: TPoint);
begin
  FScreenCenterPos := Value;
  Redraw;
end;

procedure TMapLayerBasic.SetVisible(const Value: Boolean);
begin
  if Value then begin
    Show;
  end else begin
    Hide;
  end;
end;

procedure TMapLayerBasic.Show;
begin
  FLayer.Visible := True;
  FLayer.BringToFront;
  Resize;
  Redraw;
end;


function TMapLayerBasic.GetVisiblePixelRect: TRect;
begin
  Result.Left := ScreenCenterPos.X - FParentMap.Width div 2;
  Result.Top := ScreenCenterPos.Y - FParentMap.Height div 2;
  Result.Right := ScreenCenterPos.X + FParentMap.Width div 2;
  Result.Bottom := ScreenCenterPos.Y + FParentMap.Height div 2;
end;

function TMapLayerBasic.GetVisibleSizeInPixel: TPoint;
begin
  Result.X := FParentMap.Width;
  Result.Y := FParentMap.Height;
end;

function TMapLayerBasic.GetVisibleTopLeft: TPoint;
begin
  Result.X := ScreenCenterPos.X - FParentMap.Width div 2;
  Result.Y := ScreenCenterPos.Y - FParentMap.Height div 2;
end;

function TMapLayerBasic.GetLoadedPixelRect: TRect;
var
  VSizeInPixel: TPoint;
begin
  VSizeInPixel := GetLoadedSizeInPixel;
  Result.Left := ScreenCenterPos.X - VSizeInPixel.X div 2;
  Result.Top := ScreenCenterPos.Y - VSizeInPixel.Y div 2;
  Result.Right := ScreenCenterPos.X + VSizeInPixel.X div 2;
  Result.Bottom := ScreenCenterPos.Y + VSizeInPixel.Y div 2;
end;

function TMapLayerBasic.GetLoadedSizeInPixel: TPoint;
var
  VSizeInTile: TPoint;
begin
  if GState.TilesOut=0 then begin
    Result.X := Screen.Width;
    Result.Y := Screen.Height;
  end else begin
    VSizeInTile := GetLoadedSizeInTile;
    Result.X := VSizeInTile.X * 256;
    Result.Y := VSizeInTile.Y * 256;
  end;
end;

function TMapLayerBasic.GetLoadedSizeInTile: TPoint;
begin
// Result.X := Ceil(Screen.Width / 256) + GState.TilesOut;
  Result.X := round(Screen.Width / 256)+(integer((Screen.Width mod 256)>0))+GState.TilesOut;
// Result.Y := Ceil(Screen.Height / 256) + GState.TilesOut;
  Result.Y := round(Screen.Height / 256)+(integer((Screen.height mod 256)>0))+GState.TilesOut;
end;


function TMapLayerBasic.GetMapLayerLocationRect: TRect;
var
  VLoadedSize: TPoint;
  VVisibleSize: TPoint;
begin
  VLoadedSize := GetLoadedSizeInPixel;
  VVisibleSize := GetVisibleSizeInPixel;
  Result := bounds(
    (VVisibleSize.X - VLoadedSize.X) div 2,
    (VVisibleSize.Y - VLoadedSize.Y) div 2,
    VLoadedSize.X,
    VLoadedSize.Y
  );
end;

function TMapLayerBasic.GetLoadedTopLeft: TPoint;
var
  VSizeInPixel: TPoint;
begin
  VSizeInPixel := GetLoadedSizeInPixel;
  Result.X := ScreenCenterPos.X - VSizeInPixel.X div 2;
  Result.Y := ScreenCenterPos.Y - VSizeInPixel.Y div 2;
end;


function TMapLayerBasic.VisiblePixel2MapPixel(Pnt:TPoint):TPoint;
begin
  Result := GetVisibleTopLeft;
  Result.X := Result.X + Pnt.X;
  Result.Y := Result.Y + Pnt.y;
end;


function TMapLayerBasic.MapPixel2VisiblePixel(Pnt: TPoint): TPoint;
var
  VVisibleSize: TPoint;
begin
  VVisibleSize := GetVisibleSizeInPixel;
  Result.X := Pnt.X - ScreenCenterPos.X + (VVisibleSize.X div 2);
  Result.Y := Pnt.Y - ScreenCenterPos.Y + (VVisibleSize.Y div 2);
end;

function TMapLayerBasic.LoadedPixel2MapPixel(Pnt: TPoint): TPoint;
begin
  Result := GetLoadedTopLeft;
  Result.X := Result.X + Pnt.X;
  Result.Y := Result.Y + Pnt.y;
end;

function TMapLayerBasic.MapPixel2LoadedPixel(Pnt: TPoint): TPoint;
var
  VSize: TPoint;
begin
  VSize := GetLoadedSizeInPixel;
  Result.X := Pnt.X - ScreenCenterPos.X + (VSize.X div 2);
  Result.Y := Pnt.Y - ScreenCenterPos.Y + (VSize.Y div 2);
end;

function TMapLayerBasic.LoadedPixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VTopLeft: TPoint;
begin
  VTopLeft := GetLoadedTopLeft;
  Result.X := VTopLeft.X + Pnt.X;
  Result.Y := VTopLeft.Y + Pnt.y;
end;

function TMapLayerBasic.MapPixel2LoadedPixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VSize: TPoint;
begin
  VSize := GetLoadedSizeInPixel;
  Result.X := Pnt.X - ScreenCenterPos.X + (VSize.X / 2);
  Result.Y := Pnt.Y - ScreenCenterPos.Y + (VSize.Y / 2);
end;

function TMapLayerBasic.MapPixel2VisiblePixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VSize: TPoint;
begin
  VSize := GetVisibleSizeInPixel;
  Result.X := Pnt.X - ScreenCenterPos.X + (VSize.X / 2);
  Result.Y := Pnt.Y - ScreenCenterPos.Y + (VSize.Y / 2);
end;

function TMapLayerBasic.VisiblePixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VTopLeft: TPoint;
begin
  VTopLeft := GetVisibleTopLeft;
  Result.X := VTopLeft.X + Pnt.X;
  Result.Y := VTopLeft.Y + Pnt.y;
end;

function TMapLayerBasic.VisiblePixel2LoadedPixel(Pnt: TPoint): TPoint;
var
  VVisibleSize: TPoint;
  VLoadedSize: TPoint;
begin
  VVisibleSize := GetVisibleSizeInPixel;
  VLoadedSize := GetLoadedSizeInPixel;

  Result.X := Pnt.X + (VLoadedSize.X - VVisibleSize.X) div 2;
  Result.Y := Pnt.Y + (VLoadedSize.Y - VVisibleSize.Y) div 2;
end;

end.
