unit u_MapLayerShowError;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  i_ViewPortState,
  u_MapType,
  u_MapLayerBasic;

type
  TTileErrorInfoLayer = class(TMapLayerFixedWithBitmap)
  private
    FHideAfterTime: Cardinal;
    FZoom: Byte;
    FTile: TPoint;
    FMapType: TMapType;
    procedure RenderText(AMapType: TMapType; AText: string);
  protected
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState);
    procedure ShowError(ATile: TPoint; AZoom: Byte; AMapType: TMapType; AText: string);
    procedure SetNoError(ATile: TPoint; AZoom: Byte; AMapType: TMapType);
  end;

implementation

uses
  Graphics,
  Types,
  i_CoordConverter,
  u_GeoFun;


{ TTileErrorInfoLayer }

constructor TTileErrorInfoLayer.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState);
var
  VBitmapSize: TPoint;
begin
  inherited;
  FMapType := nil;
  FZoom := 0;
  FTile := Point(0, 0);
  VBitmapSize.X := 256;
  VBitmapSize.Y := 100;
  FFixedOnBitmap.X := VBitmapSize.X / 2;
  FFixedOnBitmap.Y := VBitmapSize.Y / 2;
  FLayer.Bitmap.SetSize(VBitmapSize.X, VBitmapSize.Y);
  DoUpdateLayerSize(VBitmapSize);
end;

procedure TTileErrorInfoLayer.DoUpdateLayerLocation(ANewLocation: TFloatRect);
var
  VCurrTime: Cardinal;
begin
  if FHideAfterTime <> 0 then begin
    VCurrTime := GetTickCount;
    if (VCurrTime < FHideAfterTime) then begin
      if (VCurrTime < FHideAfterTime) then begin
        if FZoom = LayerCoordConverter.GetZoom then begin
          inherited;
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

procedure TTileErrorInfoLayer.RenderText(AMapType: TMapType; AText: string);
var
  VTextWidth: integer;
  VSize: TPoint;
begin
  VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  FLayer.Bitmap.Clear(clBlack);
  if AMapType <> nil then begin
    VTextWidth := FLayer.Bitmap.TextWidth(AMapType.name);
    FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, VSize.Y div 4, AMapType.name, 0, clBlack32);

    VTextWidth := FLayer.Bitmap.TextWidth(AText);
    FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, (VSize.Y div 4) * 3, AText, 0, clBlack32);
  end else begin
    VTextWidth := FLayer.Bitmap.TextWidth(AText);
    FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, (VSize.Y div 2), AText, 0, clBlack32);
  end;
end;

procedure TTileErrorInfoLayer.SetNoError(ATile: TPoint; AZoom: Byte;
  AMapType: TMapType);
begin
  if FMapType = AMapType then begin
    if (FTile.X = ATile.X) and (FTile.Y = ATile.Y) then begin
      if FZoom = AZoom then begin
        Hide;
      end;
    end;
  end;
end;

procedure TTileErrorInfoLayer.ShowError(ATile: TPoint; AZoom: Byte; AMapType: TMapType; AText: string);
var
  VConverter: ICoordConverter;
begin
  FMapType := AMapType;
  FTile := ATile;
  FZoom := AZoom;
  VConverter := AMapType.GeoConvert;
  FHideAfterTime := GetTickCount + 10000;
  FFixedLonLat := VConverter.PixelPosFloat2LonLat(RectCenter(VConverter.TilePos2PixelRect(ATile, AZoom)), AZoom);
  RenderText(AMapType, AText);
  Visible := true;
  UpdateLayerLocation;
end;

end.
