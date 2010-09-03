unit u_MapLayerShowError;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  UMapType,
  u_MapLayerBasic;

type
  TTileErrorInfoLayer = class(TMapLayerBasic)
  protected
    FTilePos: TPoint;
    FTileZoom: Byte;
    FMapType: TMapType;
    FText: string;
    FHideAfterTime: Cardinal;
    FBitmapSize: TPoint;
    procedure RenderText;
    procedure DoRedraw; override;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetScreenCenterInBitmapPixels: TPoint; override;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    procedure ShowError(APoint: TPoint; AZoom: Byte; AMapType: TMapType; AText: string);
  end;

implementation

uses
  Graphics,
  Types,
  u_WindowLayerBasic;

{ TTileErrorInfoLayer }

constructor TTileErrorInfoLayer.Create(AParentMap: TImage32;
  ACenter: TPoint);
begin
  inherited Create(AParentMap, ACenter);
  FBitmapSize.X := 256;
  FBitmapSize.Y := 100;
end;

procedure TTileErrorInfoLayer.DoRedraw;
var
  VCurrTime: Cardinal;
  VTilePixel: TPoint;
begin
  if FHideAfterTime <> 0 then begin
    VCurrTime := GetTickCount;
    if (VCurrTime < FHideAfterTime) then begin
      if FZoom = FTileZoom then begin
        VTilePixel := FMapType.GeoConvert.TilePos2PixelPos(FTilePos, FZoom);
        VTilePixel := FMapType.GeoConvert.PixelPos2OtherMap(VTilePixel, FZoom, FGeoConvert);
        if (abs(VTilePixel.X - FScreenCenterPos.X) < (1 shl 15)) and
          (abs(VTilePixel.Y - FScreenCenterPos.Y) < (1 shl 15)) then
        begin
          RenderText;
        end else begin
          Visible := False;
        end;
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

function TTileErrorInfoLayer.GetBitmapSizeInPixel: TPoint;
begin
  Result := FBitmapSize;
end;

function TTileErrorInfoLayer.GetScreenCenterInBitmapPixels: TPoint;
var
  VTileRect: TRect;
  VTileCenter: TPoint;
begin
  if FTileZoom = FZoom then begin
    VTileRect := FMapType.GeoConvert.TilePos2PixelRect(FTilePos, FZoom);
    VTileCenter.X := VTileRect.Left + (VTileRect.Right - VTileRect.Left + 1) div 2;
    VTileCenter.Y := VTileRect.Top + (VTileRect.Bottom - VTileRect.Top + 1) div 2;
    VTileCenter := FMapType.GeoConvert.PixelPos2OtherMap(VTileCenter, FZoom, FGeoConvert);
    Result := GetBitmapSizeInPixel;
    Result.X := Result.X div 2 + (FScreenCenterPos.X - VTileCenter.X);
    Result.Y := Result.Y div 2 + (FScreenCenterPos.Y - VTileCenter.Y);
  end else begin
    Result := Point(10000, 10000);
  end;
end;

procedure TTileErrorInfoLayer.RenderText;
var
  VTextWidth: integer;
  VSize: TPoint;
begin
  VSize := GetBitmapSizeInPixel;
  FLayer.Bitmap.Clear(clBlack);
  if FMapType <> nil then begin
    VTextWidth := FLayer.Bitmap.TextWidth(FMapType.name);
    FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, VSize.Y div 4, FMapType.name, 0, clBlack32);

    VTextWidth := FLayer.Bitmap.TextWidth(FText);
    FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, (VSize.Y div 4) * 3, FText, 0, clBlack32);
  end else begin
    VTextWidth := FLayer.Bitmap.TextWidth(FText);
    FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, (VSize.Y div 2), FText, 0, clBlack32);
  end;
end;

procedure TTileErrorInfoLayer.ShowError(APoint: TPoint; AZoom: Byte;
  AMapType: TMapType; AText: string);
begin
  FTilePos := APoint;
  FTileZoom := AZoom;
  FMapType := AMapType;
  FText := AText;
  FHideAfterTime := GetTickCount + 10000;
  Visible := true;
  Resize;
  Redraw;
end;

end.
 