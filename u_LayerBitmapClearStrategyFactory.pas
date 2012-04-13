{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_LayerBitmapClearStrategyFactory;

interface

uses
  GR32,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_ImageResamplerConfig,
  i_LayerBitmapClearStrategy;

type
  TLayerBitmapClearStrategyFactory = class(TInterfacedObject, ILayerBitmapClearStrategyFactory)
  private
    FResamplerConfig: IImageResamplerConfig;
    FSimpleClearStrategy: ILayerBitmapClearStrategy;

    FSimpleClearCounter: IInternalPerformanceCounter;

    FMoveImageCreateCounter: IInternalPerformanceCounter;
    FMoveImageCounter: IInternalPerformanceCounter;

    FImageResizeCreateCounter: IInternalPerformanceCounter;
    FImageResizeCounter: IInternalPerformanceCounter;

    FZoomChangeCreateCounter: IInternalPerformanceCounter;
    FZoomChangeCounter: IInternalPerformanceCounter;

    function GetStrategeForZoomChange(ASourceConverter, ATargetConverter: ILocalCoordConverter; ASourceBitmap: TCustomBitmap32): ILayerBitmapClearStrategy;
    function GetStrategeForSameZoom(ASourceConverter, ATargetConverter: ILocalCoordConverter; ASourceBitmap: TCustomBitmap32): ILayerBitmapClearStrategy;
  protected
    function GetStrategy(
      ASourceConverter, ATargetConverter: ILocalCoordConverter;
      ASourceBitmap: TCustomBitmap32;
      APrevStrategy: ILayerBitmapClearStrategy
    ): ILayerBitmapClearStrategy;
  public
    constructor Create(
      AResamplerConfig: IImageResamplerConfig;
      APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  t_GeoTypes,
  i_CoordConverter,
  u_GeoFun,
  u_LayerBitmapClearStrategy;

{ TLayerBitmapClearStrategyFactory }

constructor TLayerBitmapClearStrategyFactory.Create(
  AResamplerConfig: IImageResamplerConfig;
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  FResamplerConfig := AResamplerConfig;

  FSimpleClearCounter := APerfCounterList.CreateAndAddNewCounter('SimpleClear');
  FMoveImageCreateCounter := APerfCounterList.CreateAndAddNewCounter('MoveImageCreate');
  FMoveImageCounter := APerfCounterList.CreateAndAddNewCounter('MoveImageClear');
  FImageResizeCreateCounter := APerfCounterList.CreateAndAddNewCounter('ImageResizeCreate');
  FImageResizeCounter := APerfCounterList.CreateAndAddNewCounter('ImageResizeClear');
  FZoomChangeCreateCounter := APerfCounterList.CreateAndAddNewCounter('ZoomChangeCreate');
  FZoomChangeCounter := APerfCounterList.CreateAndAddNewCounter('ZoomChangeClear');

  FSimpleClearStrategy := TLayerBitmapClearStrategySimpleClear.Create(FSimpleClearCounter);
end;

function TLayerBitmapClearStrategyFactory.GetStrategeForSameZoom(
  ASourceConverter, ATargetConverter: ILocalCoordConverter;
  ASourceBitmap: TCustomBitmap32): ILayerBitmapClearStrategy;
var
  VLocalSizeSource: TPoint;
  VLocalSizeTarget: TPoint;
  VMapCenterSource: TPoint;
  VMapCenterTarget: TPoint;
  VMapRectSource: TRect;
  VMapRectTarget: TRect;
  VTargetRectInSource: TRect;
  VDelta: TPoint;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := nil;
  VLocalSizeSource := ASourceConverter.GetLocalRectSize;
  VLocalSizeTarget := ATargetConverter.GetLocalRectSize;
  if (VLocalSizeSource.X = VLocalSizeTarget.X) and
    (VLocalSizeSource.Y = VLocalSizeTarget.Y)
  then begin
    VCounterContext := FMoveImageCreateCounter.StartOperation;
    try
      VMapCenterSource := PointFromDoublePoint(ASourceConverter.GetCenterMapPixelFloat, prToTopLeft);
      VMapCenterTarget := PointFromDoublePoint(ATargetConverter.GetCenterMapPixelFloat, prToTopLeft);
      VDelta.X := VMapCenterSource.X - VMapCenterTarget.X;
      VDelta.Y := VMapCenterSource.Y - VMapCenterTarget.Y;
      Result := TLayerBitmapClearStrategyMoveImage.Create(FMoveImageCounter, VDelta);
    finally
      FMoveImageCreateCounter.FinishOperation(VCounterContext);
    end;
  end else begin
    VCounterContext := FImageResizeCreateCounter.StartOperation;
    try
      VMapRectSource := ASourceConverter.GetRectInMapPixel;
      VMapRectTarget := ATargetConverter.GetRectInMapPixel;
      VTargetRectInSource.Left := VMapRectTarget.Left - VMapRectSource.Left;
      VTargetRectInSource.Top := VMapRectTarget.Top - VMapRectSource.Top;
      VTargetRectInSource.Right := VMapRectTarget.Right - VMapRectSource.Left;
      VTargetRectInSource.Bottom := VMapRectTarget.Bottom - VMapRectSource.Top;
      Result := TLayerBitmapClearStrategyImageResize.Create(FImageResizeCounter, ASourceBitmap, VTargetRectInSource);
    finally
      FImageResizeCreateCounter.FinishOperation(VCounterContext);
    end;
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
  VCounterContext: TInternalPerformanceCounterContext;
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
  end else begin
    VCounterContext := FZoomChangeCreateCounter.StartOperation;
    try
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
      Result := TLayerBitmapClearStrategyZoomChange.Create(FZoomChangeCounter, VResampler, ASourceBitmap, VSourceRect, VTargetRect);
    finally
      FZoomChangeCreateCounter.FinishOperation(VCounterContext);
    end;
  end;
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
  if not VConverterSource.IsSameConverter(VConverterTarget) then begin
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
