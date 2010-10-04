unit u_MapLayerBasic;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  i_ICoordConverter,
  u_MapViewPortState,
  u_WindowLayerBasic,
  t_GeoTypes;

type
  TMapLayerBasic = class(TWindowLayerBasic)
  protected

    FScale: Double;
    FScaleCenterInVisualPixel: TPoint;
    FScaleCenterInBitmapPixel: TPoint;
    FFreezeInCenter: Boolean;
    FCenterMove: TPoint;

    FScreenCenterPos: TPoint;
    FZoom: Byte;
    FGeoConvert: ICoordConverter;

    function GetBitmapSizeInPixel: TPoint; override;
    function GetFreezePointInVisualPixel: TPoint; override;
    function GetFreezePointInBitmapPixel: TPoint; override;
    function GetScale: double; override;

    function GetScreenCenterInBitmapPixels: TPoint; virtual;
    function IsNeedFullRedraw(ANewCenterPos: TPoint): Boolean;
    procedure RedrawPartial(ANewCenterPos: TPoint); virtual;

    function VisiblePixel2MapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function VisiblePixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    function MapPixel2VisiblePixel(Pnt: TPoint): TPoint; overload; virtual;
    function MapPixel2VisiblePixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;

    function BitmapPixel2MapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function BitmapPixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    function MapPixel2BitmapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function MapPixel2BitmapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure Redraw; override;
    procedure MoveTo(Pnt: TPoint); virtual;
    procedure ScaleTo(AScale: Double; ACenterPoint: TPoint); overload; virtual;
    procedure ScaleTo(AScale: Double); overload; virtual;
    procedure SetScreenCenterPos(const AScreenCenterPos: TPoint; const AZoom: byte; AGeoConvert: ICoordConverter); virtual;
    property ScreenCenterPos: TPoint read FScreenCenterPos;
    property Zoom: Byte read FZoom;
    property GeoConvert: ICoordConverter read FGeoConvert;
  end;

implementation

uses
  Types,
  Forms,
  Graphics,
  u_GlobalState;

{ TGPSTrackLayer }

constructor TMapLayerBasic.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FScreenCenterPos := AViewPortState.GetCenterMapPixel;
end;

function TMapLayerBasic.IsNeedFullRedraw(ANewCenterPos: TPoint): Boolean;
begin
  Result := (FScreenCenterPos.X <> ANewCenterPos.X) or (FScreenCenterPos.Y <> ANewCenterPos.Y);
end;

procedure TMapLayerBasic.RedrawPartial(ANewCenterPos: TPoint);
begin
  // ѕо-умолчанию, не делаем ничего. ≈сли карта поддерживает частичное обновление то должна перекрывать этот метод.
end;

procedure TMapLayerBasic.SetScreenCenterPos(const AScreenCenterPos: TPoint; const AZoom: byte; AGeoConvert: ICoordConverter);
var
  VFullRedraw: Boolean;
begin
  VFullRedraw := False;
  if (FGeoConvert = nil) or ((FGeoConvert.GetProjectionEPSG() <> 0) and (FGeoConvert.GetProjectionEPSG <> AGeoConvert.GetProjectionEPSG)) then begin
    VFullRedraw := True;
  end;
  if FZoom <> AZoom then begin
    VFullRedraw := True;
  end;
  if (FScreenCenterPos.X <> AScreenCenterPos.X) or (FScreenCenterPos.Y <> AScreenCenterPos.Y) then begin
    if not VFullRedraw then begin
      if IsNeedFullRedraw(AScreenCenterPos) then begin
        VFullRedraw := True;
      end else begin
        FScreenCenterPos := AScreenCenterPos;
      end;
    end;
  end;

  FScale := 1;
  FCenterMove := Point(0, 0);
  FFreezeInCenter := True;

  if VFullRedraw then begin
    FGeoConvert := AGeoConvert;
    FZoom := AZoom;
    FScreenCenterPos := AScreenCenterPos;
    Redraw;
  end else begin
    RedrawPartial(AScreenCenterPos);
    FScreenCenterPos := AScreenCenterPos;
  end;
  Resize;
end;

function TMapLayerBasic.GetScreenCenterInBitmapPixels: TPoint;
var
  VSizeInPixel: TPoint;
begin
  VSizeInPixel := GetBitmapSizeInPixel;
  Result.X := VSizeInPixel.X div 2;
  Result.Y := VSizeInPixel.Y div 2;
end;

function TMapLayerBasic.BitmapPixel2MapPixel(Pnt: TPoint): TPoint;
var
  VScreenCenterInBitmap: TPoint;
begin
  VScreenCenterInBitmap := GetScreenCenterInBitmapPixels;
  Result.X := ScreenCenterPos.X - VScreenCenterInBitmap.X + Pnt.X;
  Result.Y := ScreenCenterPos.Y - VScreenCenterInBitmap.Y + Pnt.y;
end;

function TMapLayerBasic.BitmapPixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VScreenCenterInBitmap: TPoint;
begin
  VScreenCenterInBitmap := GetScreenCenterInBitmapPixels;
  Result.X := ScreenCenterPos.X - VScreenCenterInBitmap.X + Pnt.X;
  Result.Y := ScreenCenterPos.Y - VScreenCenterInBitmap.Y + Pnt.y;
end;

function TMapLayerBasic.MapPixel2BitmapPixel(Pnt: TPoint): TPoint;
var
  VScreenCenterInBitmap: TPoint;
begin
  VScreenCenterInBitmap := GetScreenCenterInBitmapPixels;
  Result.X := Pnt.X - ScreenCenterPos.X + VScreenCenterInBitmap.X;
  Result.Y := Pnt.Y - ScreenCenterPos.Y + VScreenCenterInBitmap.Y;
end;

function TMapLayerBasic.MapPixel2BitmapPixel(Pnt: TExtendedPoint): TExtendedPoint;
var
  VScreenCenterInBitmap: TPoint;
begin
  VScreenCenterInBitmap := GetScreenCenterInBitmapPixels;
  Result.X := Pnt.X - ScreenCenterPos.X + VScreenCenterInBitmap.X;
  Result.Y := Pnt.Y - ScreenCenterPos.Y + VScreenCenterInBitmap.Y;
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

function TMapLayerBasic.VisiblePixel2MapPixel(Pnt: TPoint): TPoint;
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
  if Visible then begin
    FCenterMove := Pnt;
    FFreezeInCenter := True;
    Resize;
  end;
end;

procedure TMapLayerBasic.ScaleTo(AScale: Double);
var
  VCenterPoint: TPoint;
begin
  VCenterPoint := GetVisibleSizeInPixel;
  VCenterPoint.X := VCenterPoint.X div 2;
  VCenterPoint.Y := VCenterPoint.Y div 2;
  ScaleTo(AScale, VCenterPoint);
end;

procedure TMapLayerBasic.ScaleTo(AScale: Double; ACenterPoint: TPoint);
begin
  if Visible then begin
    FScaleCenterInBitmapPixel := VisiblePixel2BitmapPixel(ACenterPoint);
    FScaleCenterInVisualPixel := ACenterPoint;
    FScale := AScale;
    FFreezeInCenter := False;
    Resize;
  end;
end;


function TMapLayerBasic.GetFreezePointInBitmapPixel: TPoint;
begin
  if FFreezeInCenter then begin
    Result := GetScreenCenterInBitmapPixels;
  end else begin
    Result := FScaleCenterInBitmapPixel;
  end;
end;

function TMapLayerBasic.GetFreezePointInVisualPixel: TPoint;
var
  VVisibleSize: TPoint;
begin
  if FFreezeInCenter then begin
    VVisibleSize := GetVisibleSizeInPixel;
    Result := Point(VVisibleSize.X div 2 - FCenterMove.X, VVisibleSize.Y div 2 - FCenterMove.Y);
  end else begin
    Result := Point(FScaleCenterInVisualPixel.X - FCenterMove.X, FScaleCenterInVisualPixel.Y - FCenterMove.Y);
  end;
end;

function TMapLayerBasic.GetScale: double;
begin
  Result := FScale;
end;

function TMapLayerBasic.GetBitmapSizeInPixel: TPoint;
begin
  Result.X := GState.ScreenSize.X + 2 * 256 * GState.TilesOut;
  Result.Y := GState.ScreenSize.Y + 2 * 256 * GState.TilesOut;
end;

procedure TMapLayerBasic.Redraw;
begin
  if FGeoConvert <> nil then begin
    inherited;
  end;
end;

end.
