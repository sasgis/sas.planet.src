unit u_MapMainLayer;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  i_CoordConverter,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_ViewPortState,
  i_GlobalViewMainConfig,
  i_BitmapPostProcessingConfig,
  i_TileError,
  u_MapType,
  u_MapLayerBasic;

type
  TMapMainLayer = class(TMapLayerBasic)
  private
    FErrorLogger: ITileErrorLogger;
    FMapsConfig: IMainMapsConfig;
    FPostProcessingConfig:IBitmapPostProcessingConfig;
    FViewConfig: IGlobalViewMainConfig;
    FMainMap: IMapType;
    FLayersList: IMapTypeList;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    procedure DrawMap(
      ATargetBmp: TCustomBitmap32;
      AMapType: TMapType;
      AGeoConvert: ICoordConverter;
      AZoom: Byte;
      ATile: TPoint;
      ADrawMode: TDrawMode;
      AUsePre: Boolean;
      ARecolorConfig: IBitmapPostProcessingConfigStatic
    );
    procedure OnMainMapChange(Sender: TObject);
    procedure OnLayerSetChange(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DoRedraw; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AMapsConfig: IMainMapsConfig;
      APostProcessingConfig:IBitmapPostProcessingConfig;
      AViewConfig: IGlobalViewMainConfig;
      AErrorLogger: ITileErrorLogger
    );
    procedure StartThreads; override;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  i_LocalCoordConverter,
  i_TileIterator,
  u_ResStrings,
  u_TileErrorInfo,
  u_TileIteratorByRect,
  u_NotifyEventListener;

{ TMapMainLayer }

constructor TMapMainLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AMapsConfig: IMainMapsConfig;
  APostProcessingConfig: IBitmapPostProcessingConfig;
  AViewConfig: IGlobalViewMainConfig;
  AErrorLogger: ITileErrorLogger
);
begin
  inherited Create(AParentMap, AViewPortState);
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

procedure TMapMainLayer.DoRedraw;
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
begin
  inherited;
  Layer.Bitmap.Clear(0);

  VRecolorConfig := FPostProcessingConfig.GetStatic;

  VBitmapConverter := LayerCoordConverter;
  VGeoConvert := VBitmapConverter.GetGeoConverter;
  VZoom := VBitmapConverter.GetZoom;

  VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;
  VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

  VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);

  VTileToDrawBmp := TCustomBitmap32.Create;
  try
    VTileToDrawBmp.DrawMode := dmOpaque;
    while VTileIterator.Next(VTile) do begin
      VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);

      VTilePixelsToDraw.TopLeft := Point(0, 0);
      VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
      VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

      VCurrTileOnBitmapRect.TopLeft := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
      VCurrTileOnBitmapRect.BottomRight := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);

      VTileToDrawBmp.SetSize(VTilePixelsToDraw.Right, VTilePixelsToDraw.Bottom);

      if FMainMap <> nil then begin
        DrawMap(VTileToDrawBmp, FMainMap.MapType, VGeoConvert, VZoom, VTile, dmOpaque, FUsePrevZoomAtMap, VRecolorConfig);
      end;

      VHybrList := FLayersList;
      if VHybrList <> nil then begin
        VEnum := VHybrList.GetIterator;
        while VEnum.Next(1, VGUID, i) = S_OK do begin
          VItem := VHybrList.GetMapTypeByGUID(VGUID);
          VMapType := VItem.GetMapType;
          if VMapType.IsBitmapTiles then begin
            DrawMap(VTileToDrawBmp, VMapType, VGeoConvert, VZoom, VTile, dmBlend, FUsePrevZoomAtLayer, VRecolorConfig);
          end;
        end;
      end;

      VRecolorConfig.ProcessBitmap(VTileToDrawBmp);

      Layer.Bitmap.Lock;
      try
        Layer.Bitmap.Draw(VCurrTileOnBitmapRect, VTilePixelsToDraw, VTileToDrawBmp);
      finally
        Layer.Bitmap.UnLock;
      end;
    end;
  finally
    VTileToDrawBmp.Free;
  end;
end;

procedure TMapMainLayer.DrawMap(
  ATargetBmp: TCustomBitmap32;
  AMapType: TMapType;
  AGeoConvert: ICoordConverter;
  AZoom: Byte;
  ATile: TPoint;
  ADrawMode: TDrawMode;
  AUsePre: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic
);
var
  VBmp: TCustomBitmap32;
  VErrorString: string;
begin
  VBmp := TCustomBitmap32.Create;
  try
    VErrorString := '';
    try
      if AMapType.LoadTileUni(VBmp, ATile, AZoom, true, AGeoConvert, AUsePre, True, False) then begin
        VBmp.DrawMode := ADrawMode;
        VBmp.DrawTo(ATargetBmp);
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


