unit u_BitmapLayerProviderMapWithLayer;

interface

uses
  GR32,
  i_NotifierOperation,
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
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
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
  GR32_Resamplers,
  u_Bitmap32Static;

{ TBitmapLayerProviderMapWithLayer }

constructor TBitmapLayerProviderMapWithLayer.Create(
  AMapTypeMain,
  AMapTypeHybr: TMapType;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean
);
begin
  inherited Create;
  FMapTypeMain := AMapTypeMain;
  FMapTypeHybr := AMapTypeHybr;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;
end;

function TBitmapLayerProviderMapWithLayer.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VLayer: IBitmap32Static;
  VBitmap: TCustomBitmap32;
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
      VBitmap := TCustomBitmap32.Create;
      try
        VBitmap.Assign(Result.Bitmap);
        BlockTransfer(
          VBitmap,
          0, 0,
          VBitmap.ClipRect,
          VLayer.Bitmap,
          VLayer.Bitmap.BoundsRect,
          dmBlend
        );
        Result := TBitmap32Static.CreateWithOwn(VBitmap);
        VBitmap := nil;
      finally
        VBitmap.Free;
      end;
    end;
  end else begin
    Result := VLayer;
  end;
end;

end.
