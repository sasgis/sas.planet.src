unit u_MapLayerFillingMap;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_BackgroundTaskLayerDraw,
  i_LocalCoordConverter,
  u_BackgroundTaskLayerDrawBase,
  i_IViewPortState,
  i_FillingMapLayerConfig,
  UMapType,
  u_MapLayerWithThreadDraw;

type
  IBackgroundTaskFillingMap = interface(IBackgroundTaskLayerDraw)
    ['{3BE65F32-4F6F-41F0-83CC-5B4E6646B3FF}']
    procedure ChangeConfig(AConfig: IFillingMapLayerConfigStatic);
  end;

  TBackgroundTaskFillingMap = class(TBackgroundTaskLayerDrawBase, IBackgroundTaskFillingMap)
  private
    FConfig: IFillingMapLayerConfigStatic;
  protected
    procedure DrawBitmap; override;
    procedure ExecuteTask; override;
  protected
    procedure ChangeConfig(AConfig: IFillingMapLayerConfigStatic);
  end;

  TBackgroundTaskFillingMapFactory = class(TInterfacedObject, IBackgroundTaskLayerDrawFactory)
  protected
    function GetTask(ABitmap: TCustomBitmap32): IBackgroundTaskLayerDraw;
  end;

  TMapLayerFillingMap = class(TMapLayerWithThreadDraw)
  private
    FConfig: IFillingMapLayerConfig;
    FConfigStatic: IFillingMapLayerConfigStatic;
    FDrawTask: IBackgroundTaskFillingMap;
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure PosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState; AConfig: IFillingMapLayerConfig);
    procedure StartThreads; override;
  end;

implementation

uses
  Graphics,
  SysUtils,
  t_GeoTypes,
  i_CoordConverter,
  i_ITileIterator,
  u_NotifyEventListener,
  u_TileIteratorSpiralByRect;

{ TBackgroundTaskFillingMap }

procedure TBackgroundTaskFillingMap.ChangeConfig(
  AConfig: IFillingMapLayerConfigStatic);
begin
  StopExecute;
  try
    FConfig := AConfig;
  finally
    StartExecute;
  end;
end;

procedure TBackgroundTaskFillingMap.DrawBitmap;
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

  Bitmap.Lock;
  try
    Bitmap.Clear(0);
  finally
    Bitmap.UnLock;
  end;

  VBmp := TCustomBitmap32.Create;
  try
    VConfig := FConfig;
    VLocalConverter := Converter;
    VZoom := VLocalConverter.GetZoom;
    VZoomSource := VConfig.SourceZoom;
    VSourceMapType := VConfig.SourceMap.MapType;
    VSourceGeoConvert := VSourceMapType.GeoConvert;
    VGeoConvert := VLocalConverter.GetGeoConverter;

    VBitmapOnMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
    if not FNeedStopExecute then begin
      VGeoConvert.CheckPixelRectFloat(VBitmapOnMapPixelRect, VZoom);
      VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
      VSourceGeoConvert.CheckLonLatRect(VSourceLonLatRect);
      VPixelSourceRect := VSourceGeoConvert.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);
      VTileSourceRect := VSourceGeoConvert.PixelRect2TileRect(VPixelSourceRect, VZoom);
      VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);
      while VTileIterator.Next(VTile) do begin
        if FNeedStopExecute then begin
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

        if FNeedStopExecute then begin
          break;
        end;
        VCurrTilePixelRectAtBitmap.TopLeft := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
        VCurrTilePixelRectAtBitmap.BottomRight := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);
        if FNeedStopExecute then begin
          break;
        end;
        if VSourceMapType.LoadFillingMap(VBmp, VTile, VZoom, VZoomSource, @FNeedStopExecute, VConfig.NoTileColor, VConfig.ShowTNE, VConfig.TNEColor) then begin
          Bitmap.Lock;
          try
            Bitmap.Draw(VCurrTilePixelRectAtBitmap, VTilePixelsToDraw, Vbmp);
          finally
            Bitmap.UnLock;
          end;
        end;
      end;
    end;
  finally
    VBmp.Free;
  end;
end;

procedure TBackgroundTaskFillingMap.ExecuteTask;
begin
  if FConfig <> nil then begin
    inherited;
  end;
end;

{ TMapLayerFillingMap }

constructor TMapLayerFillingMap.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState; AConfig: IFillingMapLayerConfig);
var
  VFactory: IBackgroundTaskLayerDrawFactory;
begin
  VFactory := TBackgroundTaskFillingMapFactory.Create;
  inherited Create(AParentMap, AViewPortState, VFactory);
  FConfig := AConfig;
  FDrawTask := (inherited DrawTask) as IBackgroundTaskFillingMap;

  LinksList.Add(
    TNotifyEventListener.Create(OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

procedure TMapLayerFillingMap.PosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  if not FConfigStatic.Visible then begin
    Hide;
  end else begin
    if ANewVisualCoordConverter.GetZoom > FConfigStatic.SourceZoom then begin
      Hide;
    end else begin
      Show;
      inherited;
    end;
  end;
end;

procedure TMapLayerFillingMap.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

procedure TMapLayerFillingMap.OnConfigChange(Sender: TObject);
begin
  FConfigStatic := FConfig.GetStatic;
  if FConfigStatic.Visible then begin
    if ViewPortState.GetCurrentZoom > FConfigStatic.SourceZoom then begin
      Hide;
    end else begin
      FDrawTask.StopExecute;
      try
        Show;
        FDrawTask.ChangeConfig(FConfigStatic);
      finally
        FDrawTask.StartExecute;
      end;
    end;
  end else begin
    Hide;
  end;
end;

{ TBackgroundTaskFillingMapFactory }

function TBackgroundTaskFillingMapFactory.GetTask(
  ABitmap: TCustomBitmap32): IBackgroundTaskLayerDraw;
begin
  Result := TBackgroundTaskFillingMap.Create(ABitmap);
end;

end.
