unit u_BitmapLayerProviderMapWithLayer;

interface

uses
  GR32,
  i_OperationNotifier,
  i_Bitmap32Static,
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
  private
    function GetBitmapRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      AMapTypeMain: TMapType;
      AMapTypeHybr: TMapType;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean
    );
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers,
  u_Bitmap32Static;

{ TBitmapLayerProviderMapWithLayer }

constructor TBitmapLayerProviderMapWithLayer.Create(AMapTypeMain,
  AMapTypeHybr: TMapType; AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean);
begin
  FMapTypeMain := AMapTypeMain;
  FMapTypeHybr := AMapTypeHybr;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;
end;

function TBitmapLayerProviderMapWithLayer.GetBitmapRect(
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
end;

end.
