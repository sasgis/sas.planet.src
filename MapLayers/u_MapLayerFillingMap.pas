unit u_MapLayerFillingMap;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_CommonTypes,
  i_LocalCoordConverter,
  i_ViewPortState,
  i_FillingMapLayerConfig,
  u_MapType,
  u_MapLayerWithThreadDraw;

type
  TMapLayerFillingMap = class(TMapLayerWithThreadDraw)
  private
    FConfig: IFillingMapLayerConfig;
    FConfigStatic: IFillingMapLayerConfigStatic;
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DrawBitmap(AIsStop: TIsCancelChecker); override;
    function GetVisibleForNewPos(ANewVisualCoordConverter: ILocalCoordConverter): Boolean; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ATimerNoifier: IJclNotifier;
      AConfig: IFillingMapLayerConfig
    );
    procedure StartThreads; override;
  end;

implementation

uses
  Classes,
  Graphics,
  SysUtils,
  t_GeoTypes,
  i_CoordConverter,
  i_TileIterator,
  u_NotifyEventListener,
  u_TileIteratorSpiralByRect;

{ TMapLayerFillingMap }

constructor TMapLayerFillingMap.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ATimerNoifier: IJclNotifier;
  AConfig: IFillingMapLayerConfig
);
begin
  inherited Create(AParentMap, AViewPortState, nil, ATimerNoifier, tpLowest);
  FConfig := AConfig;

  LinksList.Add(
    TNotifyEventListener.Create(OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

procedure TMapLayerFillingMap.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

procedure TMapLayerFillingMap.DrawBitmap(AIsStop: TIsCancelChecker);
var
  VZoom: Byte;
  VZoomSource: Byte;
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
    карты для которой строится слой заполнения
  }
  VPixelSourceRect: TRect;

  {
    Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    карты, для которой строится слой заполнения
  }
  VTileSourceRect: TRect;
  {
    Текущий тайл в кооординатах карты, для которой строится слой заполнения
  }
  VTile: TPoint;
  {
    Прямоугольник пикслов текущего тайла в кооординатах карты,
    для которой строится слой заполнения
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

  VLocalConverter: ILocalCoordConverter;
  VSourceGeoConvert: ICoordConverter;
  VGeoConvert: ICoordConverter;
  VTileIterator: ITileIterator;
  VConfig: IFillingMapLayerConfigStatic;
begin
  inherited;

  VConfig := FConfigStatic;
  VLocalConverter := LayerCoordConverter;
  if (VConfig <> nil) and (VLocalConverter <> nil) then begin
    VBmp := TCustomBitmap32.Create;
    try
      VZoom := VLocalConverter.GetZoom;
      VZoomSource := VConfig.SourceZoom;
      VSourceMapType := VConfig.SourceMap.MapType;
      VSourceGeoConvert := VSourceMapType.GeoConvert;
      VGeoConvert := VLocalConverter.GetGeoConverter;

      VBitmapOnMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
      if not AIsStop then begin
        VGeoConvert.CheckPixelRectFloat(VBitmapOnMapPixelRect, VZoom);
        VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
        VSourceGeoConvert.CheckLonLatRect(VSourceLonLatRect);
        VPixelSourceRect := VSourceGeoConvert.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);
        VTileSourceRect := VSourceGeoConvert.PixelRect2TileRect(VPixelSourceRect, VZoom);
        VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);
        while VTileIterator.Next(VTile) do begin
          if AIsStop then begin
            break;
          end;
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
            VTilePixelsToDraw.Right := VPixelSourceRect.Right - VCurrTilePixelRectSource.Left;
            VCurrTilePixelRectSource.Right := VPixelSourceRect.Right;
          end;

          if VCurrTilePixelRectSource.Bottom > VPixelSourceRect.Bottom then begin
            VTilePixelsToDraw.Bottom := VPixelSourceRect.Bottom - VCurrTilePixelRectSource.Top;
            VCurrTilePixelRectSource.Bottom := VPixelSourceRect.Bottom;
          end;

          VCurrTilePixelRect.TopLeft := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.TopLeft, VZoom, VGeoConvert);
          VCurrTilePixelRect.BottomRight := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.BottomRight, VZoom, VGeoConvert);

          if AIsStop then begin
            break;
          end;
          VCurrTilePixelRectAtBitmap.TopLeft := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
          VCurrTilePixelRectAtBitmap.BottomRight := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);
          if AIsStop then begin
            break;
          end;
          if VSourceMapType.LoadFillingMap(VBmp, VTile, VZoom, VZoomSource, AIsStop, VConfig.NoTileColor, VConfig.ShowTNE, VConfig.TNEColor) then begin
            Layer.Bitmap.Lock;
            try
              if not AIsStop then begin
                Layer.Bitmap.Draw(VCurrTilePixelRectAtBitmap, VTilePixelsToDraw, Vbmp);
                SetBitmapChanged;
              end;
            finally
              Layer.Bitmap.UnLock;
            end;
          end;
        end;
      end;
    finally
      VBmp.Free;
    end;
  end;
end;

function TMapLayerFillingMap.GetVisibleForNewPos(
  ANewVisualCoordConverter: ILocalCoordConverter): Boolean;
begin
  Result := False;
  if FConfigStatic <> nil then begin
    Result := FConfigStatic.Visible;
    if Result then begin
      Result := ANewVisualCoordConverter.GetZoom <= FConfigStatic.SourceZoom;
    end;
  end;
end;

procedure TMapLayerFillingMap.OnConfigChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
    FConfigStatic := FConfig.GetStatic;
    SetVisible(GetVisibleForNewPos(ViewCoordConverter));
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

end.
