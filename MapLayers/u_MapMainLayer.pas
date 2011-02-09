unit u_MapMainLayer;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_MapTypes,
  i_IActiveMapsConfig,
  i_IViewPortState,
  u_MapLayerShowError,
  UMapType,
  u_MapLayerBasic;

type
  TMapMainLayer = class(TMapLayerBasic)
  private
    FErrorShowLayer: TTileErrorInfoLayer;
    FMapsConfig: IMainMapsConfig;
    FMainMap: IMapType;
    FLayersList: IMapTypeList;
  protected
    procedure DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
    procedure DoRedraw; override;
    procedure OnMainMapChange(Sender: TObject);
    procedure OnLayerSetChange(Sender: TObject);
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState; AMapsConfig: IMainMapsConfig);
    procedure StartThreads; override;
    property ErrorShowLayer: TTileErrorInfoLayer read FErrorShowLayer write FErrorShowLayer;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  i_ITileIterator,
  i_IBitmapPostProcessingConfig,
  Uimgfun,
  UResStrings,
  u_TileIteratorByRect,
  u_NotifyEventListener,
  u_GlobalState;

{ TMapMainLayer }

constructor TMapMainLayer.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState; AMapsConfig: IMainMapsConfig);
begin
  inherited Create(AParentMap, AViewPortState);
  FMapsConfig := AMapsConfig;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnMainMapChange),
    FMapsConfig.GetActiveMap.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnLayerSetChange),
    FMapsConfig.GetBitmapLayersSet.GetChangeNotifier
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
begin
  inherited;
  FLayer.Bitmap.Clear(0);
  if FMainMap <> nil then begin
    DrawMap(FMainMap.MapType, dmOpaque);
  end;

  VHybrList := FLayersList;
  if VHybrList <> nil then begin
    VEnum := VHybrList.GetIterator;
    while VEnum.Next(1, VGUID, i) = S_OK do begin
      VItem := VHybrList.GetMapTypeByGUID(VGUID);
      VMapType := VItem.GetMapType;
      if VMapType.IsBitmapTiles then begin
        DrawMap(VMapType, dmBlend);
      end;
    end;
  end;
end;

procedure TMapMainLayer.DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
var
  VZoom: Byte;
  VSourceMapType: TMapType;
  VBmp: TCustomBitmap32;

  {
    Прямоугольник пикселей растра в координатах текущей основной карты
  }
  VBitmapOnMapPixelRect: TDoubleRect;

  {
    Географические координаты растра
  }
  VSourceLonLatRect: TDoubleRect;

  {
    Прямоугольник пикселов текущего зума, покрывающий растр, в кооординатах
    карты для которой строится слой
  }
  VPixelSourceRect: TRect;

  {
    Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    карты, для которой строится слой
  }
  VTileSourceRect: TRect;

  {
    Текущий тайл в кооординатах карты, для которой строится слой
  }
  VTile: TPoint;

  {
    Прямоугольник пикслов текущего тайла в кооординатах карты,
    для которой строится слой
  }
  VCurrTilePixelRectSource: TRect;

  {
    Прямоугольник пикслов текущего тайла в кооординатах текущей карты
  }
  VCurrTilePixelRect: TRect;

  {
    Прямоугольник пикслов текущего тайла в кооординатах текущего растра
  }
  VCurrTilePixelRectAtBitmap: TRect;

  {
    Прямоугольник тайла подлежащий отображению на текущий растр
  }
  VTilePixelsToDraw: TRect;


  VSourceGeoConvert: ICoordConverter;
  VGeoConvert: ICoordConverter;
  VUsePre: Boolean;
  VLocalConverter: ILocalCoordConverter;
  VTileIterator: ITileIterator;
  VRecolorConfig: IBitmapPostProcessingConfigStatic;
begin
  if AMapType.asLayer then begin
    VUsePre := GState.UsePrevZoomLayer;
  end else begin
    VUsePre := GState.UsePrevZoom;
  end;
  VRecolorConfig := GState.BitmapPostProcessingConfig.GetStatic;

  VLocalConverter := BitmapCoordConverter;
  VGeoConvert := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VSourceMapType := AMapType;
  VSourceGeoConvert := VSourceMapType.GeoConvert;

  VBitmapOnMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VBitmapOnMapPixelRect, VZoom);

  VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
  VPixelSourceRect := VSourceGeoConvert.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);
  VTileSourceRect := VSourceGeoConvert.PixelRect2TileRect(VPixelSourceRect, VZoom);

  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);

  VBmp := TCustomBitmap32.Create;
  try
    while VTileIterator.Next(VTile) do begin
        VCurrTilePixelRectSource := VSourceGeoConvert.TilePos2PixelRect(VTile, VZoom);
        VTilePixelsToDraw.TopLeft := Point(0, 0);
        VTilePixelsToDraw.Right := VCurrTilePixelRectSource.Right - VCurrTilePixelRectSource.Left;
        VTilePixelsToDraw.Bottom := VCurrTilePixelRectSource.Bottom - VCurrTilePixelRectSource.Top;

        if VCurrTilePixelRectSource.Left < VPixelSourceRect.Left then begin
          VTilePixelsToDraw.Left := VPixelSourceRect.Left - VCurrTilePixelRectSource.Left;
          VCurrTilePixelRectSource.Left := VPixelSourceRect.Left;
        end;

        if VCurrTilePixelRectSource.Top < VPixelSourceRect.Top then begin
          VTilePixelsToDraw.Top := VPixelSourceRect.Top - VCurrTilePixelRectSource.Top;
          VCurrTilePixelRectSource.Top := VPixelSourceRect.Top;
        end;

        if VCurrTilePixelRectSource.Right > VPixelSourceRect.Right then begin
          VTilePixelsToDraw.Right := VTilePixelsToDraw.Right - (VCurrTilePixelRectSource.Right - VPixelSourceRect.Right);
          VCurrTilePixelRectSource.Right := VPixelSourceRect.Right;
        end;

        if VCurrTilePixelRectSource.Bottom > VPixelSourceRect.Bottom then begin
          VTilePixelsToDraw.Bottom := VTilePixelsToDraw.Bottom - (VCurrTilePixelRectSource.Bottom - VPixelSourceRect.Bottom);
          VCurrTilePixelRectSource.Bottom := VPixelSourceRect.Bottom;
        end;

        VCurrTilePixelRect.TopLeft := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.TopLeft, VZoom, VGeoConvert);
        VCurrTilePixelRect.BottomRight := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.BottomRight, VZoom, VGeoConvert);

        VCurrTilePixelRectAtBitmap.TopLeft := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
        VCurrTilePixelRectAtBitmap.BottomRight := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);
        try
          if VSourceMapType.LoadTileOrPreZ(VBmp, VTile, VZoom, true, False, VUsePre) then begin
            Gamma(VBmp, VRecolorConfig.ContrastN, VRecolorConfig.GammaN, VRecolorConfig.InvertColor);
            FLayer.Bitmap.Lock;
            try
              VBmp.DrawMode := ADrawMode;
              FLayer.Bitmap.Draw(VCurrTilePixelRectAtBitmap, VTilePixelsToDraw, Vbmp);
            finally
              FLayer.Bitmap.UnLock;
            end;
          end;
        except
          FErrorShowLayer.ShowError(VTile, VZoom, VSourceMapType, SAS_ERR_BadFile);
        end;
    end;
  finally
    VBmp.Free;
  end;
end;

procedure TMapMainLayer.OnLayerSetChange(Sender: TObject);
begin
  FMainMap := FMapsConfig.GetActiveMap.GetMapsList.GetMapTypeByGUID(FMapsConfig.GetActiveMap.GetSelectedGUID);
  Redraw;
end;

procedure TMapMainLayer.OnMainMapChange(Sender: TObject);
begin
  FLayersList := FMapsConfig.GetBitmapLayersSet.GetSelectedMapsList;
  Redraw;
end;

procedure TMapMainLayer.StartThreads;
begin
  inherited;
  OnMainMapChange(nil);
  OnLayerSetChange(nil);
  Visible := True;
end;

end.


