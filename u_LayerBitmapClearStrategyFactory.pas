unit u_LayerBitmapClearStrategyFactory;

interface

uses
  GR32,
  i_LocalCoordConverter,
  i_ImageResamplerConfig,
  i_LayerBitmapClearStrategy;

type
  TLayerBitmapClearStrategyFactory = class(TInterfacedObject, ILayerBitmapClearStrategyFactory)
  private
    FResamplerConfig: IImageResamplerConfig;
    FSimpleClearStrategy: ILayerBitmapClearStrategy;
    function GetStrategeForZoomChange(ASourceConverter, ATargetConverter: ILocalCoordConverter; ASourceBitmap: TCustomBitmap32): ILayerBitmapClearStrategy;
    function GetStrategeForSameZoom(ASourceConverter, ATargetConverter: ILocalCoordConverter; ASourceBitmap: TCustomBitmap32): ILayerBitmapClearStrategy;
  protected
    function GetStrategy(
      ASourceConverter, ATargetConverter: ILocalCoordConverter;
      ASourceBitmap: TCustomBitmap32;
      APrevStrategy: ILayerBitmapClearStrategy
    ): ILayerBitmapClearStrategy;
  public
    constructor Create(AResamplerConfig: IImageResamplerConfig);
  end;

implementation

uses
  t_GeoTypes,
  i_CoordConverter,
  u_LayerBitmapClearStrategy;

{ TLayerBitmapClearStrategyFactory }

constructor TLayerBitmapClearStrategyFactory.Create(
  AResamplerConfig: IImageResamplerConfig);
begin
  FResamplerConfig := AResamplerConfig;
  FSimpleClearStrategy := TLayerBitmapClearStrategySimpleClear.Create;
end;

function TLayerBitmapClearStrategyFactory.GetStrategeForSameZoom(
  ASourceConverter, ATargetConverter: ILocalCoordConverter;
  ASourceBitmap: TCustomBitmap32): ILayerBitmapClearStrategy;
var
  VLocalSizeSource: TPoint;
  VLocalSizeTarget: TPoint;
  VMapCenterSource: TDoublePoint;
  VMapCenterTarget: TDoublePoint;
  VMapRectSource: TRect;
  VMapRectTarget: TRect;
  VTargetRectInSource: TRect;
  VDelta: TPoint;
begin
  Result := nil;
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
    VMapRectSource := ASourceConverter.GetRectInMapPixel;
    VMapRectTarget := ATargetConverter.GetRectInMapPixel;
    VTargetRectInSource.Left := VMapRectTarget.Left - VMapRectSource.Left;
    VTargetRectInSource.Top := VMapRectTarget.Top - VMapRectSource.Top;
    VTargetRectInSource.Right := VMapRectTarget.Right - VMapRectSource.Left;
    VTargetRectInSource.Bottom := VMapRectTarget.Bottom - VMapRectSource.Top;
    Result := TLayerBitmapClearStrategyImageResize.Create(ASourceBitmap, VTargetRectInSource);
    Exit;
  end;
end;

function TLayerBitmapClearStrategyFactory.GetStrategeForZoomChange(
  ASourceConverter, ATargetConverter: ILocalCoordConverter;
  ASourceBitmap: TCustomBitmap32): ILayerBitmapClearStrategy;
var
  VConverterSource: ICoordConverter;
  VConverterTarget: ICoordConverter;
  VZoomSource: Byte;
  VZoomTarget: Byte;
  VMapRectSource: TRect;
  VMapRectTarget: TRect;
  VRelativeRect: TDoubleRect;
  VMapRectTargetValid: TRect;
  VMapRectValidTargetAtSource: TRect;
  VMapRectValidTargetCropped: TRect;
  VSourceRect: TRect;
  VTargetRect: TRect;
  VResampler: TCustomResampler;
begin
  VConverterSource := ASourceConverter.GetGeoConverter;
  VConverterTarget := ATargetConverter.GetGeoConverter;

  VZoomSource := ASourceConverter.GetZoom;
  VZoomTarget := ATargetConverter.GetZoom;

  VMapRectTarget := ATargetConverter.GetRectInMapPixel;
  VMapRectSource := ASourceConverter.GetRectInMapPixel;

  VMapRectTargetValid := VMapRectTarget;
  VConverterTarget.CheckPixelRect(VMapRectTargetValid, VZoomTarget);
  VRelativeRect := VConverterTarget.PixelRect2RelativeRect(VMapRectTargetValid, VZoomTarget);
  VMapRectValidTargetAtSource := VConverterSource.RelativeRect2PixelRect(VRelativeRect, VZoomSource);

  if not IntersectRect(VMapRectValidTargetAtSource, VMapRectValidTargetAtSource, VMapRectSource) then begin
    Result := FSimpleClearStrategy;
    Exit;
  end;

  VRelativeRect := VConverterSource.PixelRect2RelativeRect(VMapRectValidTargetAtSource, VZoomSource);
  VMapRectValidTargetCropped := VConverterTarget.RelativeRect2PixelRect(VRelativeRect, VZoomTarget);

  VSourceRect := VMapRectValidTargetAtSource;
  Dec(VSourceRect.Left, VMapRectSource.Left);
  Dec(VSourceRect.Top, VMapRectSource.Top);
  Dec(VSourceRect.Right, VMapRectSource.Left);
  Dec(VSourceRect.Bottom, VMapRectSource.Top);

  VTargetRect := VMapRectValidTargetCropped;
  Dec(VTargetRect.Left, VMapRectTarget.Left);
  Dec(VTargetRect.Top, VMapRectTarget.Top);
  Dec(VTargetRect.Right, VMapRectTarget.Left);
  Dec(VTargetRect.Bottom, VMapRectTarget.Top);

  VResampler := nil;
  if FResamplerConfig <> nil then begin
    VResampler := FResamplerConfig.GetActiveFactory.CreateResampler;
  end;
  Result := TLayerBitmapClearStrategyZoomChange.Create(VResampler, ASourceBitmap, VSourceRect, VTargetRect);
end;

function TLayerBitmapClearStrategyFactory.GetStrategy(
  ASourceConverter,
  ATargetConverter: ILocalCoordConverter;
  ASourceBitmap: TCustomBitmap32;
  APrevStrategy: ILayerBitmapClearStrategy
): ILayerBitmapClearStrategy;
var
  VConverterSource: ICoordConverter;
  VConverterTarget: ICoordConverter;
  VZoomSource: Byte;
  VZoomTarget: Byte;
  VLocalSizeSource: TPoint;
begin
  if
    (ATargetConverter = nil) or
    (ASourceConverter = nil) or
    (ASourceBitmap = nil) or
    ASourceBitmap.Empty or
    ASourceConverter.GetIsSameConverter(ATargetConverter)
  then begin
    Result := nil;
    Exit;
  end;
  if APrevStrategy <> nil then begin
    Result := FSimpleClearStrategy;
    Exit;
  end;

  VConverterSource := ASourceConverter.GetGeoConverter;
  VConverterTarget := ATargetConverter.GetGeoConverter;
  if not VConverterSource.IsSameConverter(VConverterSource) then begin
    Result := FSimpleClearStrategy;
    Exit;
  end;

  VLocalSizeSource := ASourceConverter.GetLocalRectSize;
  if (VLocalSizeSource.X <> ASourceBitmap.Width) or
    (VLocalSizeSource.Y <> ASourceBitmap.Height)
  then begin
    Result := FSimpleClearStrategy;
    Exit;
  end;

  VZoomSource := ASourceConverter.GetZoom;
  VZoomTarget := ATargetConverter.GetZoom;

  if (VZoomSource + 1 < VZoomTarget) or
    (VZoomSource > VZoomTarget + 1)
  then begin
    Result := FSimpleClearStrategy;
    Exit;
  end;

  if VZoomSource = VZoomTarget then begin
    Result := GetStrategeForSameZoom(ASourceConverter, ATargetConverter, ASourceBitmap);
  end else begin
    Result := GetStrategeForZoomChange(ASourceConverter, ATargetConverter, ASourceBitmap);
  end;
  if Result <> nil then begin
    Exit;
  end;
  Result := FSimpleClearStrategy;
end;

end.
