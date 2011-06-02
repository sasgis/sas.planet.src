unit u_MapMainLayer;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_CommonTypes,
  i_CoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_ViewPortState,
  i_ImageResamplerConfig,
  i_GlobalViewMainConfig,
  i_BitmapPostProcessingConfig,
  i_TileError,
  u_MapType,
  u_MapLayerWithThreadDraw;

type
  TMapMainLayer = class(TMapLayerTiledWithThreadDraw)
  private
    FErrorLogger: ITileErrorLogger;
    FMapsConfig: IMainMapsConfig;
    FPostProcessingConfig:IBitmapPostProcessingConfig;
    FViewConfig: IGlobalViewMainConfig;
    FMainMap: IMapType;
    FLayersList: IMapTypeList;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    function DrawMap(
      ATargetBmp: TCustomBitmap32;
      AMapType: TMapType;
      AGeoConvert: ICoordConverter;
      AZoom: Byte;
      ATile: TPoint;
      ADrawMode: TDrawMode;
      AUsePre: Boolean;
      ARecolorConfig: IBitmapPostProcessingConfigStatic
    ): Boolean;
    procedure OnMainMapChange(Sender: TObject);
    procedure OnLayerSetChange(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DrawBitmap(AIsStop: TIsCancelChecker); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      AMapsConfig: IMainMapsConfig;
      AResamplerConfig: IImageResamplerConfig;
      APostProcessingConfig:IBitmapPostProcessingConfig;
      AViewConfig: IGlobalViewMainConfig;
      AErrorLogger: ITileErrorLogger;
      ATimerNoifier: IJclNotifier
    );
    procedure StartThreads; override;
  end;

implementation

uses
  ActiveX,
  Classes,
  SysUtils,
  i_LocalCoordConverter,
  i_TileIterator,
  u_ResStrings,
  u_TileErrorInfo,
  u_NotifyEventListener,
  u_TileIteratorSpiralByRect;

{ TMapMainLayer }

constructor TMapMainLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  AMapsConfig: IMainMapsConfig;
  AResamplerConfig: IImageResamplerConfig;
  APostProcessingConfig: IBitmapPostProcessingConfig;
  AViewConfig: IGlobalViewMainConfig;
  AErrorLogger: ITileErrorLogger;
  ATimerNoifier: IJclNotifier
);
begin
  inherited Create(AParentMap, AViewPortState, AConverterFactory, AResamplerConfig, ATimerNoifier, tpNormal);
  FMapsConfig := AMapsConfig;
  FErrorLogger := AErrorLogger;
  FPostProcessingConfig := APostProcessingConfig;
  FViewConfig := AViewConfig;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnMainMapChange),
    FMapsConfig.GetActiveMap.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnLayerSetChange),
    FMapsConfig.GetBitmapLayersSet.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FViewConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FPostProcessingConfig.GetChangeNotifier
  );
end;

procedure TMapMainLayer.DrawBitmap(AIsStop: TIsCancelChecker);
var
  i: Cardinal;
  VMapType: TMapType;
  VGUID: TGUID;
  VItem: IMapType;
  VEnum: IEnumGUID;
  VHybrList: IMapTypeList;
  VRecolorConfig: IBitmapPostProcessingConfigStatic;
  VTileToDrawBmp: TCustomBitmap32;

  VGeoConvert: ICoordConverter;
  VBitmapConverter: ILocalCoordConverter;
  VTileIterator: ITileIterator;
  VZoom: Byte;
  { Прямоугольник пикселей растра в координатах основного конвертера }
  VBitmapOnMapPixelRect: TRect;
  { Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    основного конвертера }
  VTileSourceRect: TRect;
  { Текущий тайл в кооординатах основного конвертера }
  VTile: TPoint;
  { Прямоугольник пикслов текущего тайла в кооординатах основного конвертера }
  VCurrTilePixelRect: TRect;
  { Прямоугольник тайла подлежащий отображению на текущий растр }
  VTilePixelsToDraw: TRect;
  { Прямоугольник пикселов в которые будет скопирован текущий тайл }
  VCurrTileOnBitmapRect: TRect;
  VTileIsEmpty: Boolean;
begin
  VRecolorConfig := FPostProcessingConfig.GetStatic;

  VBitmapConverter := LayerCoordConverter;
  VGeoConvert := VBitmapConverter.GetGeoConverter;
  VZoom := VBitmapConverter.GetZoom;

  VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;
  VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

  VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
  VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);

  VTileToDrawBmp := TCustomBitmap32.Create;
  try
    if not AIsStop then begin
      while VTileIterator.Next(VTile) do begin
        if AIsStop then begin
          break;
        end;
        VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);

        VTilePixelsToDraw.TopLeft := Point(0, 0);
        VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
        VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

        VCurrTileOnBitmapRect.TopLeft := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
        VCurrTileOnBitmapRect.BottomRight := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);

        VTileToDrawBmp.SetSize(VTilePixelsToDraw.Right, VTilePixelsToDraw.Bottom);
        VTileIsEmpty := True;
        if FMainMap <> nil then begin
          if DrawMap(VTileToDrawBmp, FMainMap.MapType, VGeoConvert, VZoom, VTile, dmOpaque, FUsePrevZoomAtMap, VRecolorConfig) then begin
            VTileIsEmpty := False;
          end else begin
            VTileToDrawBmp.Clear(0);
          end;
        end;
        if AIsStop then begin
          break;
        end;

        VHybrList := FLayersList;
        if VHybrList <> nil then begin
          VEnum := VHybrList.GetIterator;
          while VEnum.Next(1, VGUID, i) = S_OK do begin
            VItem := VHybrList.GetMapTypeByGUID(VGUID);
            VMapType := VItem.GetMapType;
            if VMapType.IsBitmapTiles then begin
              if DrawMap(VTileToDrawBmp, VMapType, VGeoConvert, VZoom, VTile, dmBlend, FUsePrevZoomAtLayer, VRecolorConfig) then begin
                VTileIsEmpty := False;
              end;
            end;
            if AIsStop then begin
              break;
            end;
          end;
        end;

        if not VTileIsEmpty then begin
          VRecolorConfig.ProcessBitmap(VTileToDrawBmp);
          if AIsStop then begin
            break;
          end;
        end;

        Layer.Bitmap.Lock;
        try
          if AIsStop then begin
            break;
          end;
          Layer.Bitmap.Draw(VCurrTileOnBitmapRect, VTilePixelsToDraw, VTileToDrawBmp);
          SetBitmapChanged;
        finally
          Layer.Bitmap.UnLock;
        end;
      end;
    end;
  finally
    VTileToDrawBmp.Free;
  end;
end;

function TMapMainLayer.DrawMap(
  ATargetBmp: TCustomBitmap32;
  AMapType: TMapType;
  AGeoConvert: ICoordConverter;
  AZoom: Byte;
  ATile: TPoint;
  ADrawMode: TDrawMode;
  AUsePre: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic
): Boolean;
var
  VBmp: TCustomBitmap32;
  VErrorString: string;
begin
  Result := False;
  VBmp := TCustomBitmap32.Create;
  try
    VErrorString := '';
    try
      if AMapType.LoadTileUni(VBmp, ATile, AZoom, true, AGeoConvert, AUsePre, True, False) then begin
        VBmp.DrawMode := ADrawMode;
        VBmp.DrawTo(ATargetBmp);
        Result := True;
      end;
    except
      on E: Exception do begin
        VErrorString := E.Message;
      end;
    else
      VErrorString := SAS_ERR_TileDownloadUnexpectedError;
    end;
    if VErrorString <> '' then begin
      FErrorLogger.LogError(
        TTileErrorInfo.Create(
          AMapType,
          AZoom,
          ATile,
          VErrorString
        )
      );
    end;
  finally
    VBmp.Free;
  end;
end;

procedure TMapMainLayer.OnConfigChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    FViewConfig.LockRead;
    try
      FUsePrevZoomAtMap := FViewConfig.UsePrevZoomAtMap;
      FUsePrevZoomAtLayer := FViewConfig.UsePrevZoomAtLayer;
    finally
      FViewConfig.UnlockRead;
    end;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TMapMainLayer.OnLayerSetChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    FLayersList := FMapsConfig.GetBitmapLayersSet.GetSelectedMapsList;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TMapMainLayer.OnMainMapChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    FMainMap := FMapsConfig.GetSelectedMapType;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TMapMainLayer.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
  OnMainMapChange(nil);
  OnLayerSetChange(nil);
  Visible := True;
end;

end.


