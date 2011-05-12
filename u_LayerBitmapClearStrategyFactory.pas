unit u_LayerBitmapClearStrategyFactory;

interface

uses
  GR32,
  i_LocalCoordConverter,
  i_LayerBitmapClearStrategy;

type
  TLayerBitmapClearStrategyFactory = class(TInterfacedObject, ILayerBitmapClearStrategyFactory)
  protected
    function GetStrategy(ASourceConverter, ATargetConverter: ILocalCoordConverter; ASourceBitmap: TCustomBitmap32): ILayerBitmapClearStrategy;
  end;

implementation

uses
  t_GeoTypes,
  u_LayerBitmapClearStrategy;

{ TLayerBitmapClearStrategyFactory }

function TLayerBitmapClearStrategyFactory.GetStrategy(ASourceConverter,
  ATargetConverter: ILocalCoordConverter;
  ASourceBitmap: TCustomBitmap32): ILayerBitmapClearStrategy;
var
  VZoomSource: Byte;
  VZoomTarget: Byte;
  VLocalSizeSource: TPoint;
  VLocalSizeTarget: TPoint;
  VMapCenterSource: TDoublePoint;
  VMapCenterTarget: TDoublePoint;
  VDelta: TPoint;
begin

  if
    (ATargetConverter = nil) or
    (ASourceConverter = nil) or
    (ASourceBitmap = nil) or
    ASourceBitmap.Empty or
    ASourceConverter.GetIsSameConverter(ATargetConverter)
  then begin
    Result := TLayerBitmapClearStrategyNOP.Create;
    Exit;
  end;

  if not ASourceConverter.GetGeoConverter.IsSameConverter(ATargetConverter.GetGeoConverter) then begin
    Result := TLayerBitmapClearStrategySimpleClear.Create;
    Exit;
  end;

  VLocalSizeSource := ASourceConverter.GetLocalRectSize;
  if (VLocalSizeSource.X <> ASourceBitmap.Width) or
    (VLocalSizeSource.Y <> ASourceBitmap.Height)
  then begin
    Result := TLayerBitmapClearStrategySimpleClear.Create;
    Exit;
  end;

  VZoomSource := ASourceConverter.GetZoom;
  VZoomTarget := ATargetConverter.GetZoom;

  if (VZoomSource + 1 < VZoomTarget) or
    (VZoomSource > VZoomTarget + 1)
  then begin
    Result := TLayerBitmapClearStrategySimpleClear.Create;
    Exit;
  end;

  if VZoomSource = VZoomTarget then begin
    VLocalSizeTarget := ATargetConverter.GetLocalRectSize;
    if (VLocalSizeSource.X = VLocalSizeTarget.X) and
      (VLocalSizeSource.Y = VLocalSizeTarget.Y)
    then begin
      VMapCenterSource := ASourceConverter.GetCenterMapPixelFloat;
      VMapCenterTarget := ATargetConverter.GetCenterMapPixelFloat;
      VDelta.X := Trunc(VMapCenterSource.X - VMapCenterTarget.X);
      VDelta.Y := Trunc(VMapCenterSource.Y - VMapCenterTarget.Y);
      Result := TLayerBitmapClearStrategyMoveImage.Create(VDelta);
      Exit;
    end else begin

//      Result := TLayerBitmapClearStrategyImageResize.Create(ASourceBitmap, );
    end;
  end else if VZoomSource + 1 = VZoomTarget then begin

  end else if VZoomSource = VZoomTarget + 1 then begin

  end;
  Result := TLayerBitmapClearStrategySimpleClear.Create;
end;

end.
