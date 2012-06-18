unit u_BitmapLayerProviderGridTiles;

interface

uses
  GR32,
  i_OperationNotifier,
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
      const ACancelNotifier: IOperationNotifier;
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
  const ACancelNotifier: IOperationNotifier;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VCurrentZoom: Byte;
  VGridZoom: Byte;
  VGeoConvert: ICoordConverter;
  VMapRect: TDoubleRect;
  VRelativeRect: TDoubleRect;
  VTilesRect: TRect;
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

  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VMapRect, VCurrentZoom);

  VRelativeRect := VGeoConvert.PixelRectFloat2RelativeRect(VMapRect, VCurrentZoom);
  VTilesRect := VGeoConvert.RelativeRect2TileRect(VRelativeRect, VGridZoom);

end;

end.
