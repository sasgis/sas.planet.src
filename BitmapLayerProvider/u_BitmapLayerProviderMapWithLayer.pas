unit u_BitmapLayerProviderMapWithLayer;

interface

uses
  GR32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  i_MapVersionRequest,
  i_MapTypes,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderMapWithLayer = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FMapTypeMain: IMapType;
    FMapTypeMainVersion: IMapVersionRequest;
    FMapTypeHybr: IMapType;
    FMapTypeHybrVersion: IMapVersionRequest;
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
      const AMapTypeMain: IMapType;
      const AMapTypeMainVersion: IMapVersionRequest;
      const AMapTypeHybr: IMapType;
      const AMapTypeHybrVersion: IMapVersionRequest;
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
  const AMapTypeMain: IMapType;
  const AMapTypeMainVersion: IMapVersionRequest;
  const AMapTypeHybr: IMapType;
  const AMapTypeHybrVersion: IMapVersionRequest;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean
);
begin
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FMapTypeMain := AMapTypeMain;
  FMapTypeMainVersion := AMapTypeMainVersion;
  FMapTypeHybr := AMapTypeHybr;
  FMapTypeHybrVersion := AMapTypeHybrVersion;
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
    Result :=
      FMapTypeMain.LoadBitmapUni(
        ALocalConverter.GetRectInMapPixel,
        ALocalConverter.GetZoom,
        FMapTypeMainVersion,
        ALocalConverter.GetGeoConverter,
        FUsePrevZoomAtMap,
        True,
        True
      );
  end;

  if FMapTypeHybr <> nil then begin
    VLayer :=
      FMapTypeHybr.LoadBitmapUni(
        ALocalConverter.GetRectInMapPixel,
        ALocalConverter.GetZoom,
        FMapTypeHybrVersion,
        ALocalConverter.GetGeoConverter,
        FUsePrevZoomAtLayer,
        True,
        True
      );
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
