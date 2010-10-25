unit u_MapTileLayerBasic;

interface

uses
  Windows,
  GR32,
  u_MapLayerBasic,
  uMapType;

type
  TMapTileLayerBasic = class(TMapLayerBasic)
  protected
    FMapType: TMapType;
    FZoom: Byte;
    function GetBitmapRectInTiles: TRect; virtual;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetScreenCenterInBitmapPixels: TPoint; override;
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

function TMapTileLayerBasic.GetBitmapRectInTiles: TRect;
var
  VConverter: ICoordConverter;
  VVisibleSize: TPoint;
  VVisibleMapRect: TRect;
  VZoom: byte;
  VMapSize: Integer;
  VMapSizeInTiles: Integer;
begin
  VZoom := FZoom;
  VConverter := FMapType.GeoConvert;
  VMapSize := VConverter.PixelsAtZoom(VZoom);
  VMapSizeInTiles := VConverter.TilesAtZoom(VZoom);

  VVisibleSize := GetVisibleSizeInPixel;
  if (VVisibleSize.X >= VMapSize) and (VVisibleSize.Y >= VMapSize) then begin
    Result := Rect(0, 0, VMapSizeInTiles - 1, VMapSizeInTiles - 1);
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

    VConverter.CheckPixelRect(VVisibleMapRect, VZoom, False);
    Result := VConverter.PixelRect2TileRect(VVisibleMapRect, VZoom);
    if GState.TilesOut > 0 then begin
      if (Result.Left > Result.Right) and (Result.Left - Result.Right <= GState.TilesOut * 2) then begin
      end else begin
        Dec(Result.Left, GState.TilesOut);
        Inc(Result.Right, GState.TilesOut);
      end;
      Dec(Result.Top, GState.TilesOut);
      Inc(Result.Bottom, GState.TilesOut);
      VConverter.CheckTileRect(Result, VZoom, False);
    end;
  end;
end;

function TMapTileLayerBasic.GetBitmapSizeInPixel: TPoint;
var
  VConverter: ICoordConverter;
  VVisibleTilesRect: TRect;
  VMapPixelsRect: TRect;
  VZoom: byte;
  VVisibleSize: TPoint;
  VMapSize: Integer;
begin
  VZoom := FZoom;
  VConverter := FMapType.GeoConvert;
  VVisibleSize := GetVisibleSizeInPixel;
  VMapSize := VConverter.PixelsAtZoom(VZoom);
  VVisibleTilesRect := GetBitmapRectInTiles;
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

function TMapTileLayerBasic.GetScreenCenterInBitmapPixels: TPoint;
var
  VConverter: ICoordConverter;
  VVisibleSize: TPoint;
  VBitmapSize: TPoint;
  VZoom: byte;
  VMapSize: Integer;
begin
  VZoom := FZoom;
  VConverter := FMapType.GeoConvert;
  VMapSize := VConverter.PixelsAtZoom(VZoom);
  VVisibleSize := GetVisibleSizeInPixel;
  VBitmapSize := GetBitmapSizeInPixel;
  if (VVisibleSize.Y >= VMapSize) then begin
    Result.Y := FScreenCenterPos.Y;
  end else begin
    if FScreenCenterPos.Y < VVisibleSize.Y div 2 then begin
      Result.Y := FScreenCenterPos.Y;
    end else begin
      if VMapSize - FScreenCenterPos.Y < VVisibleSize.Y then begin
        Result.Y := VBitmapSize.Y - (VMapSize - FScreenCenterPos.Y);
      end;
    end;
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
