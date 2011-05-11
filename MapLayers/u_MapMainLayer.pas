unit u_MapMainLayer;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
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
      AMapType: TMapType;
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
  i_CoordConverter,
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
begin
  inherited;
  Layer.Bitmap.Clear(0);

  VRecolorConfig := FPostProcessingConfig.GetStatic;

  if FMainMap <> nil then begin
    DrawMap(FMainMap.MapType, dmOpaque, FUsePrevZoomAtMap, VRecolorConfig);
  end;

  VHybrList := FLayersList;
  if VHybrList <> nil then begin
    VEnum := VHybrList.GetIterator;
    while VEnum.Next(1, VGUID, i) = S_OK do begin
      VItem := VHybrList.GetMapTypeByGUID(VGUID);
      VMapType := VItem.GetMapType;
      if VMapType.IsBitmapTiles then begin
        DrawMap(VMapType, dmBlend, FUsePrevZoomAtLayer, VRecolorConfig);
      end;
    end;
  end;
end;

procedure TMapMainLayer.DrawMap(
  AMapType: TMapType;
  ADrawMode: TDrawMode;
  AUsePre: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic
);
var
  VZoom: Byte;
  VBmp: TCustomBitmap32;

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

  VGeoConvert: ICoordConverter;
  VBitmapConverter: ILocalCoordConverter;
  VTileIterator: ITileIterator;
  VErrorString: string;
begin
  VBitmapConverter := LayerCoordConverter;
  VGeoConvert := VBitmapConverter.GetGeoConverter;
  VZoom := VBitmapConverter.GetZoom;

  VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;
  VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

  VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);
  VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;

  VBmp := TCustomBitmap32.Create;
  try
    while VTileIterator.Next(VTile) do begin
        VErrorString := '';
        try
          if AMapType.LoadTileUni(VBmp, VTile, VZoom, true, VGeoConvert, AUsePre, True, False) then begin
            ARecolorConfig.ProcessBitmap(VBmp);

            VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);
          
            VTilePixelsToDraw.TopLeft := Point(0, 0);
            VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
            VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

            VCurrTileOnBitmapRect.TopLeft := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
            VCurrTileOnBitmapRect.BottomRight := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);

            Layer.Bitmap.Lock;
            try
              VBmp.DrawMode := ADrawMode;
              Layer.Bitmap.Draw(VCurrTileOnBitmapRect, VTilePixelsToDraw, Vbmp);
            finally
              Layer.Bitmap.UnLock;
            end;
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
              VZoom,
              VTile,
              VErrorString
            )
          );
        end;
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


