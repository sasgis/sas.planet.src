unit u_BitmapLayerProviderMapWithLayer;

interface

uses
  GR32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  u_MapType,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderMapWithLayer = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FBitmapFactory: IBitmap32StaticFactory;
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
      const ABitmapFactory: IBitmap32StaticFactory;
      AMapTypeMain: TMapType;
      AMapTypeHybr: TMapType;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean
    );
  end;

implementation

uses
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderMapWithLayer }

constructor TBitmapLayerProviderMapWithLayer.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  AMapTypeMain, AMapTypeHybr: TMapType;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean
);
begin
  inherited Create;
  FBitmapFactory := ABitmapFactory;
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
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := nil;
  VLayer := nil;
  if FMapTypeMain <> nil then begin
    Result := FMapTypeMain.LoadBitmapUni(ALocalConverter.GetRectInMapPixel, ALocalConverter.GetZoom, ALocalConverter.GetGeoConverter, FUsePrevZoomAtMap, True, True);
  end;

  if FMapTypeHybr <> nil then begin
    VLayer := FMapTypeHybr.LoadBitmapUni(ALocalConverter.GetRectInMapPixel, ALocalConverter.GetZoom, ALocalConverter.GetGeoConverter, FUsePrevZoomAtLayer, True, True);
  end;

  if Result <> nil then begin
    if VLayer <> nil then begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
      try
        AssignStaticToBitmap32(VBitmap, Result);
        BlockTransferFull(
          VBitmap,
          0, 0,
          VLayer,
          dmBlend
        );
        Result := VBitmap.BitmapStatic;
      finally
        VBitmap.Free;
      end;
    end;
  end else begin
    Result := VLayer;
  end;
end;

end.
