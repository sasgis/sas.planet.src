unit u_MapLayerGoto;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TGotoLayer =  class(TMapLayerBasic)
  protected
    FGotoPoint: TExtendedPoint;
    FHideAfterTime: Cardinal;
    FBitmapSize: TPoint;
    procedure DoRedraw; override;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetScreenCenterInBitmapPixels: TPoint; override;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    procedure ShowGotoIcon(APoint: TExtendedPoint);
  end;



implementation

uses
  u_GlobalState, u_WindowLayerBasic, Types;
{ TGotoLayer }

constructor TGotoLayer.Create(AParentMap: TImage32; ACenter: TPoint);
begin
  inherited Create(AParentMap, ACenter);
  FBitmapSize.X := GState.GOToSelIcon.Width;
  FBitmapSize.Y := GState.GOToSelIcon.Height;
end;

procedure TGotoLayer.DoRedraw;
var
  VGotoPoint: TPoint;
  VCurrTime: Cardinal;
begin
  if FHideAfterTime <> 0 then begin
    VCurrTime := GetTickCount;
    if (VCurrTime < FHideAfterTime) then begin
      VGotoPoint := FGeoConvert.LonLat2PixelPos(FGotoPoint, FZoom);
      if (abs(VGotoPoint.X - FScreenCenterPos.X) < (1 shl 20)) and
        (abs(VGotoPoint.Y - FScreenCenterPos.Y) < (1 shl 20)) then begin
        FLayer.Bitmap.Draw(0, 0, GState.GOToSelIcon);
      end else begin
        Visible := False;
      end;
    end else begin
      Visible := False;
    end;
  end else begin
    Visible := False;
  end;
end;

function TGotoLayer.GetBitmapSizeInPixel: TPoint;
begin
  Result := FBitmapSize;
end;

function TGotoLayer.GetScreenCenterInBitmapPixels: TPoint;
var
  VGotoPoint: TPoint;
begin
  Result.X := 7;
  Result.Y := 6;
  VGotoPoint := FGeoConvert.LonLat2PixelPos(FGotoPoint, FZoom);
  Result.X := Result.X + (FScreenCenterPos.X - VGotoPoint.X);
  Result.Y := Result.Y + (FScreenCenterPos.Y - VGotoPoint.Y);;
end;

procedure TGotoLayer.ShowGotoIcon(APoint: TExtendedPoint);
begin
  FGotoPoint := APoint;
  FHideAfterTime := GetTickCount + 100000;
  Visible := True;
  Resize;
end;

end.
 