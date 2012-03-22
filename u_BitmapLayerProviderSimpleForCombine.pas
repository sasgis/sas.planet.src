unit u_BitmapLayerProviderSimpleForCombine;

interface

uses
  GR32,
  i_OperationNotifier,
  i_Bitmap32Static,
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
      ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
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
  GR32_Resamplers,
  u_Bitmap32Static;

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
  ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VLayer: IBitmap32Static;
  VResultBmp: TCustomBitmap32;
begin
  Result := nil;
  VLayer := nil;
  if FMapTypeMain <> nil then begin
    Result := FMapTypeMain.LoadBtimapUni(ALocalConverter.GetRectInMapPixel, ALocalConverter.GetZoom, ALocalConverter.GetGeoConverter, FUsePrevZoomAtMap, True, True);
  end;

  if FMapTypeHybr <> nil then begin
    VLayer := FMapTypeHybr.LoadBtimapUni(ALocalConverter.GetRectInMapPixel, ALocalConverter.GetZoom, ALocalConverter.GetGeoConverter, FUsePrevZoomAtLayer, True, True);
  end;

  if Result <> nil then begin
    if VLayer <> nil then begin
      VResultBmp := TCustomBitmap32.Create;
      try
        VResultBmp.Assign(Result.Bitmap);
        BlockTransfer(
          VResultBmp,
          0, 0,
          VResultBmp.ClipRect,
          VLayer.Bitmap,
          VLayer.Bitmap.BoundsRect,
          dmBlend
        );
        Result := TBitmap32Static.CreateWithOwn(VResultBmp);
        VResultBmp := nil;
      finally
        VResultBmp.Free;
      end;
    end;
  end else begin
    Result := VLayer;
  end;
  if Result <> nil then begin
    if FRecolorConfig <> nil then begin
      Result := FRecolorConfig.Process(Result);
    end;
  end;
  if FMarksImageProvider <> nil then begin
    VLayer := FMarksImageProvider.GetBitmapRect(AOperationID, ACancelNotifier, ALocalConverter);
  end;
  if Result <> nil then begin
    if VLayer <> nil then begin
      VResultBmp := TCustomBitmap32.Create;
      try
        VResultBmp.Assign(Result.Bitmap);
        BlockTransfer(
          VResultBmp,
          0, 0,
          VResultBmp.ClipRect,
          VLayer.Bitmap,
          VLayer.Bitmap.BoundsRect,
          dmBlend
        );
        Result := TBitmap32Static.CreateWithOwn(VResultBmp);
        VResultBmp := nil;
      finally
        VResultBmp.Free;
      end;
    end;
  end else begin
    Result := VLayer;
  end;
end;

end.
