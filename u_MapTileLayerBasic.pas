unit u_MapTileLayerBasic;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  GR32_Layers,
  u_MapLayerBasic,
  uMapType,
  t_GeoTypes;

type
  TMapTileLayerBasic =  class(TMapLayerBasic)
  protected
    FMapType: TMapType;
    FZoom: Byte;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetFreezePointInBitmapPixel: TPoint; override;
  public
    procedure SetMapZoomAndPos(AMapType: TMapType; AXY: TPoint; AZoom: byte); virtual;
  end;
implementation

uses
  Types,
  i_ICoordConverter,
  u_GlobalState,
  u_WindowLayerBasic;

{ TMapTileLayerBasic }

function TMapTileLayerBasic.GetBitmapSizeInPixel: TPoint;
var
  VConverter: ICoordConverter;
  VVisibleSize: TPoint;
  VVisibleMapRect: TRect;
  VVisibleTilesRect: TRect;
  VMapPixelsRect: TRect;
  VZoom: byte;
  VMapSize: Integer;
begin
  VZoom := FZoom;
  VConverter := FMapType.GeoConvert;
  VMapSize := VConverter.PixelsAtZoom(VZoom);
  VVisibleSize := GetVisibleSizeInPixel;
  if (VVisibleSize.X >= VMapSize) and (VVisibleSize.Y >= VMapSize) then begin
    Result := VVisibleSize;
  end else begin
    if (VVisibleSize.X >= VMapSize) or (VVisibleSize.Y >= VMapSize) then begin
      if (VVisibleSize.X >= VMapSize) then begin
        VVisibleMapRect.Left := 0;
        VVisibleMapRect.Top := FScreenCenterPos.Y - VVisibleSize.Y div 2;
        VVisibleMapRect.Right := VMapSize - 1;
        VVisibleMapRect.Bottom := VVisibleMapRect.Top + VVisibleSize.Y;
      end else begin
        VVisibleMapRect.Left := FScreenCenterPos.X - VVisibleSize.X div 2;
        VVisibleMapRect.Top := 0;
        VVisibleMapRect.Right := VVisibleMapRect.Left + VVisibleSize.X;
        VVisibleMapRect.Bottom := VMapSize - 1;
      end;
    end else begin
      VVisibleMapRect.Left := FScreenCenterPos.X - VVisibleSize.X div 2;
      VVisibleMapRect.Top := FScreenCenterPos.Y - VVisibleSize.Y div 2;
      VVisibleMapRect.Right := VVisibleMapRect.Left + VVisibleSize.X;
      VVisibleMapRect.Bottom := VVisibleMapRect.Top + VVisibleSize.Y;
    end;

    VConverter.CheckPixelRect(VVisibleMapRect, VZoom, GState.CiclMap);
    VVisibleTilesRect := VConverter.PixelRect2TileRect(VVisibleMapRect, VZoom);
    VMapPixelsRect := VConverter.TileRect2PixelRect(VVisibleTilesRect, VZoom);
    Result.X := VMapPixelsRect.Right - VMapPixelsRect.Left + 1;
    Result.Y := VMapPixelsRect.Bottom - VMapPixelsRect.Top + 1;
    if VMapPixelsRect.Left > VMapPixelsRect.Right then begin
      Result.X := Result.X + VMapSize;
    end;
    if VMapPixelsRect.Top > VMapPixelsRect.Bottom then begin
      Result.Y := Result.Y + VMapSize;
    end;
  end;
end;

function TMapTileLayerBasic.GetFreezePointInBitmapPixel: TPoint;
var
  VBitmapSize: TPoint;
begin
  if FFreezeInCenter then begin
    VBitmapSize := GetBitmapSizeInPixel;
    Result := Point(VBitmapSize.X div 2, VBitmapSize.Y div 2);
  end else begin
    Result := FScaleCenterInBitmapPixel;
  end;
end;

procedure TMapTileLayerBasic.SetMapZoomAndPos(AMapType: TMapType;
  AXY: TPoint; AZoom: byte);
begin
  FMapType := AMapType;
  FScreenCenterPos := AXY;
  FZoom := AZoom;
end;

end.
 