unit u_BitmapLayerProviderSimpleForCombine;

interface

uses
  GR32,
  i_OperationNotifier,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  i_BitmapPostProcessingConfig,
  u_MapType;

type
  TBitmapLayerProviderSimpleForCombine = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FRecolorConfig: IBitmapPostProcessingConfigStatic;
    FMapTypeMain: TMapType;
    FMapTypeHybr: TMapType;
    FMarksImageProvider: IBitmapLayerProvider;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;

    FTempBitmap: TCustomBitmap32;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
  public
    constructor Create(
      ARecolorConfig: IBitmapPostProcessingConfigStatic;
      AMapTypeMain: TMapType;
      AMapTypeHybr: TMapType;
      AMarksImageProvider: IBitmapLayerProvider;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers;

{ TBitmapLayerProviderSimpleForCombine }

constructor TBitmapLayerProviderSimpleForCombine.Create(
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AMapTypeMain, AMapTypeHybr: TMapType;
  AMarksImageProvider: IBitmapLayerProvider;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean
);
begin
  FMapTypeMain := AMapTypeMain;
  FMapTypeHybr := AMapTypeHybr;
  FMarksImageProvider := AMarksImageProvider;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;
  FRecolorConfig := ARecolorConfig;

  FTempBitmap := TCustomBitmap32.Create;
end;

destructor TBitmapLayerProviderSimpleForCombine.Destroy;
begin
  FreeAndNil(FTempBitmap);
  inherited;
end;

function TBitmapLayerProviderSimpleForCombine.GetBitmapRect(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter
): Boolean;
begin
  Result := False;
  if FMapTypeMain <> nil then begin
    Result := FMapTypeMain.LoadBtimapUni(ATargetBmp, ALocalConverter.GetRectInMapPixel, ALocalConverter.GetZoom, ALocalConverter.GetGeoConverter, FUsePrevZoomAtMap, True, True);
  end;
  if FMapTypeHybr <> nil then begin
    if Result then begin
      if
        FMapTypeHybr.LoadBtimapUni(FTempBitmap, ALocalConverter.GetRectInMapPixel, ALocalConverter.GetZoom, ALocalConverter.GetGeoConverter, FUsePrevZoomAtLayer, True, True)
      then begin
        BlockTransfer(
          ATargetBmp,
          0,
          0,
          ATargetBmp.ClipRect,
          FTempBitmap,
          FTempBitmap.BoundsRect,
          dmBlend
        );
      end;
    end else begin
      Result := FMapTypeHybr.LoadBtimapUni(ATargetBmp, ALocalConverter.GetRectInMapPixel, ALocalConverter.GetZoom, ALocalConverter.GetGeoConverter, FUsePrevZoomAtLayer, True, True);
    end;
  end;
  if Result then begin
    if FRecolorConfig <> nil then begin
      FRecolorConfig.ProcessBitmap(ATargetBmp);
    end;
  end;
  if FMarksImageProvider <> nil then begin
    if Result then begin
      if
        FMarksImageProvider.GetBitmapRect(AOperationID, ACancelNotifier, FTempBitmap, ALocalConverter)
      then begin
        BlockTransfer(
          ATargetBmp,
          0,
          0,
          ATargetBmp.ClipRect,
          FTempBitmap,
          FTempBitmap.BoundsRect,
          dmBlend
        );
      end;
    end else begin
      Result := FMarksImageProvider.GetBitmapRect(AOperationID, ACancelNotifier, ATargetBmp, ALocalConverter);
    end;
  end;
end;

end.
