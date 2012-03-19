unit u_BitmapLayerProviderMapWithLayer;

interface

uses
  GR32,
  i_OperationNotifier,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  u_MapType;

type
  TBitmapLayerProviderMapWithLayer = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FMapTypeMain: TMapType;
    FMapTypeHybr: TMapType;
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
      AMapTypeMain: TMapType;
      AMapTypeHybr: TMapType;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers;

{ TBitmapLayerProviderMapWithLayer }

constructor TBitmapLayerProviderMapWithLayer.Create(AMapTypeMain,
  AMapTypeHybr: TMapType; AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean);
begin
  FMapTypeMain := AMapTypeMain;
  FMapTypeHybr := AMapTypeHybr;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;

  FTempBitmap := TCustomBitmap32.Create;
end;

destructor TBitmapLayerProviderMapWithLayer.Destroy;
begin
  FreeAndNil(FTempBitmap);
  inherited;
end;

function TBitmapLayerProviderMapWithLayer.GetBitmapRect(AOperationID: Integer;
  ACancelNotifier: IOperationNotifier; ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter): Boolean;
begin
  Result := False;
  if FMapTypeMain <> nil then begin
    Result := FMapTypeMain.LoadBtimapUni(ATargetBmp, ALocalConverter.GetRectInMapPixel, ALocalConverter.GetZoom, ALocalConverter.GetGeoConverter, FUsePrevZoomAtMap, True, True);
  end;
  if FMapTypeHybr <> nil then begin
    if Result then begin
      Result := FMapTypeHybr.LoadBtimapUni(FTempBitmap, ALocalConverter.GetRectInMapPixel, ALocalConverter.GetZoom, ALocalConverter.GetGeoConverter, FUsePrevZoomAtLayer, True, True);
      if Result then begin
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
end;

end.
