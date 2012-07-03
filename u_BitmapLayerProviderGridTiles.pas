unit u_BitmapLayerProviderGridTiles;

interface

uses
  GR32,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_Bitmap32Static,
  i_BitmapLayerProvider;

type
  TBitmapLayerProviderGridTiles = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FColor: TColor32;
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
    FShowText: Boolean;
    FShowLines: Boolean;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      AColor: TColor32;
      AUseRelativeZoom: Boolean;
      AZoom: Integer;
      AShowText: Boolean;
      AShowLines: Boolean
    );
  end;

implementation

uses
  t_GeoTypes,
  i_CoordConverter;

{ TBitmapLayerProviderGridTiles }

constructor TBitmapLayerProviderGridTiles.Create(AColor: TColor32;
  AUseRelativeZoom: Boolean; AZoom: Integer; AShowText, AShowLines: Boolean);
begin
  inherited Create;
  FColor := AColor;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FShowText := AShowText;
  FShowLines := AShowLines;
end;

function TBitmapLayerProviderGridTiles.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VCurrentZoom: Byte;
  VGridZoom: Byte;
  VGeoConvert: ICoordConverter;
  VMapRect: TDoubleRect;
  VRelativeRect: TDoubleRect;
  VTilesRect: TRect;
  VTilesLineRect: TRect;
  i, j: integer;
  VTileRelativeRect: TDoubleRect;
  VTileRect: TRect;
  VTileScreenRect: TRect;
  VLocalRect: TRect;
  VBitmap: TCustomBitmap32;
begin
  Result := nil;
  VCurrentZoom := ALocalConverter.GetZoom;
  if FUseRelativeZoom then begin
    VGridZoom := VCurrentZoom + FZoom;
  end else begin
    VGridZoom := FZoom;
  end;
  VGeoConvert := ALocalConverter.GetGeoConverter;
  if not VGeoConvert.CheckZoom(VGridZoom) then begin
    Exit;
  end;

  VLocalRect := ALocalConverter.GetLocalRect;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VMapRect, VCurrentZoom);

  VRelativeRect := VGeoConvert.PixelRectFloat2RelativeRect(VMapRect, VCurrentZoom);
  VTilesRect := VGeoConvert.RelativeRect2TileRect(VRelativeRect, VGridZoom);

  VTilesLineRect.Left := VTilesRect.Left;
  VTilesLineRect.Right := VTilesRect.Right;
  VBitmap := TCustomBitmap32.Create;
  try
    for i := VTilesRect.Top to VTilesRect.Bottom do begin
      VTilesLineRect.Top := i;
      VTilesLineRect.Bottom := i;

      VTileRelativeRect := VGeoConvert.TileRect2RelativeRect(VTilesLineRect, VGridZoom);
      VTileRect := VGeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
      VTileScreenRect := ALocalConverter.MapRect2LocalRect(VTileRect);

      VTileScreenRect.Left := VLocalRect.Left;
      VTileScreenRect.Right := VLocalRect.Right;

      if VTileScreenRect.Top < VLocalRect.Top then begin
        VTileScreenRect.Top := VLocalRect.Top;
        VTileScreenRect.Bottom := VTileScreenRect.Top;
      end;

      if VTileScreenRect.Top > VLocalRect.Bottom then begin
        VTileScreenRect.Top := VLocalRect.Bottom;
        VTileScreenRect.Bottom := VTileScreenRect.Top;
      end;

      VBitmap.HorzLineTS(VTileScreenRect.Left, VTileScreenRect.Top,
        VTileScreenRect.Right, FColor);
    end;

    VTilesLineRect.Top := VTilesRect.Top;
    VTilesLineRect.Bottom := VTilesRect.Bottom;
    for j := VTilesRect.Left to VTilesRect.Right do begin
      VTilesLineRect.Left := j;
      VTilesLineRect.Right := j;

      VTileRelativeRect := VGeoConvert.TileRect2RelativeRect(VTilesLineRect, VGridZoom);
      VTileRect := VGeoConvert.RelativeRect2PixelRect(VTileRelativeRect, VCurrentZoom);
      VTileScreenRect := ALocalConverter.MapRect2LocalRect(VTileRect);

      VTileScreenRect.Top := VLocalRect.Top;
      VTileScreenRect.Bottom := VLocalRect.Bottom;

      if VTileScreenRect.Left < VLocalRect.Left then begin
        VTileScreenRect.Left := VLocalRect.Left;
        VTileScreenRect.Right := VTileScreenRect.Left;
      end;

      if VTileScreenRect.Left > VLocalRect.Right then begin
        VTileScreenRect.Left := VLocalRect.Right;
        VTileScreenRect.Right := VTileScreenRect.Left;
      end;

      VBitmap.VertLineTS(VTileScreenRect.Left, VTileScreenRect.Top,
        VTileScreenRect.Bottom, FColor);
    end;
  finally
    VBitmap.Free;
  end;
end;

end.
