unit u_MapLayerFillingMap;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  i_IBackgroundTaskLayerDraw,
  u_BackgroundTaskLayerDrawBase,
  u_MapViewPortState,
  UMapType,
  u_MapLayerWithThreadDraw;

type
  IBackgroundTaskFillingMap = interface(IBackgroundTaskLayerDraw)
    ['{3BE65F32-4F6F-41F0-83CC-5B4E6646B3FF}']
    procedure ChangeSoureMap(AMapType: TMapType);
    procedure ChangeSoureZoom(AZoom: Byte);
  end;

  TBackgroundTaskFillingMap = class(TBackgroundTaskLayerDrawBase)
  private
    FSourceMap: TMapType;
    FSourceZoom: Byte;
  protected
    procedure DrawBitmap; override;
    procedure ExecuteTask; override;
  protected
    procedure ChangeSoureMap(AMapType: TMapType);
    procedure ChangeSoureZoom(AZoom: Byte);
  end;

  TBackgroundTaskFillingMapFactory = class(TInterfacedObject, IBackgroundTaskLayerDrawFactory)
  protected
    function GetTask(ABitmap: TCustomBitmap32): IBackgroundTaskLayerDraw;
  end;

  TMapLayerFillingMap = class(TMapLayerWithThreadDraw)
  private
    FDrawTask: IBackgroundTaskFillingMap;
    FSourceMapType: TMapType;
    FSourceSelected: TMapType;
    FSourceZoom: integer;
    FMainMapChangeListener: IJclListener;
    FSourceMapChangeNotifier: IJclNotifier;
    procedure OnMainMapchange(Sender: TObject);
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;

    procedure SetSourceMap(AMapType: TMapType; AZoom: integer);
    property SourceSelected: TMapType read FSourceSelected;
    property SourceZoom: integer read FSourceZoom;
    property SourceMapChangeNotifier: IJclNotifier read FSourceMapChangeNotifier;
  end;

implementation

uses
  u_JclNotify,
  t_GeoTypes,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  i_ITileIterator,
  u_NotifyEventListener,
  u_TileIteratorSpiralByRect;

{ TBackgroundTaskFillingMap }

procedure TBackgroundTaskFillingMap.ChangeSoureMap(AMapType: TMapType);
begin
  StopExecute;
  try
    FSourceMap := AMapType;
  finally
    StartExecute;
  end;
end;

procedure TBackgroundTaskFillingMap.ChangeSoureZoom(AZoom: Byte);
begin
  StopExecute;
  try
    FSourceZoom := AZoom;
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
begin
  inherited;
  VBmp := TCustomBitmap32.Create;
  try
    VLocalConverter := Converter;
    VZoom := VLocalConverter.GetZoom;
    VZoomSource := FSourceZoom;
    VSourceMapType := FSourceMap;
    VSourceGeoConvert := VSourceMapType.GeoConvert;
    VGeoConvert := VLocalConverter.GetGeoConverter;

    VBitmapOnMapPixelRect.TopLeft := VLocalConverter.LocalPixel2MapPixelFloat(Point(0, 0));
    VBitmapOnMapPixelRect.BottomRight := VLocalConverter.LocalPixel2MapPixelFloat(BitmapSize);
    if not FNeedStopExecute then begin
      VGeoConvert.CheckPixelPosFloatStrict(VBitmapOnMapPixelRect.TopLeft, VZoom, False);
      VGeoConvert.CheckPixelPosFloatStrict(VBitmapOnMapPixelRect.BottomRight, VZoom, False);
      VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
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
        if VSourceMapType.LoadFillingMap(VBmp, VTile, VZoom, VZoomSource, @FNeedStopExecute) then begin
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
  if FSourceMap <> nil then begin
    inherited;
  end;
end;

{ TMapLayerFillingMap }

constructor TMapLayerFillingMap.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
var
  VFactory: IBackgroundTaskLayerDrawFactory;
begin
  VFactory := TBackgroundTaskFillingMapFactory.Create;
  inherited Create(AParentMap, AViewPortState, VFactory);
  FDrawTask := (inherited DrawTask) as IBackgroundTaskFillingMap;
  FSourceMapType := FViewPortState.GetCurrentMap;
  FSourceSelected := nil;
  FSourceZoom := -1;
  FMainMapChangeListener := TNotifyEventListener.Create(OnMainMapchange);
  FViewPortState.MapChangeNotifier.Add(FMainMapChangeListener);
  FSourceMapChangeNotifier := TJclBaseNotifier.Create;
end;

destructor TMapLayerFillingMap.Destroy;
begin
  FViewPortState.MapChangeNotifier.Remove(FMainMapChangeListener);
  FMainMapChangeListener := nil;
  inherited;
end;

procedure TMapLayerFillingMap.OnMainMapchange(Sender: TObject);
begin

end;

procedure TMapLayerFillingMap.SetSourceMap(AMapType: TMapType; AZoom: integer);
var
  VFullRedraw: Boolean;
  VNewSource: TMapType;
begin
  VFullRedraw := false;
  if (AMapType <> nil) then begin
    VNewSource := AMapType;
  end else begin
    VNewSource := FViewPortState.GetCurrentMap;
  end;
  if (FSourceSelected <> VNewSource) then begin
    VFullRedraw := True;
  end;
  if FSourceZoom <> AZoom then begin
    VFullRedraw := True;
  end;
  if VFullRedraw then begin
    FSourceSelected := AMapType;
    FSourceMapType := VNewSource;
    FSourceZoom := AZoom;
    FDrawTask.StopExecute;
    try
      FDrawTask.ChangeSoureMap(VNewSource);
      FDrawTask.ChangeSoureZoom(FSourceZoom);
    finally
      FDrawTask.StartExecute;
    end;
    FSourceMapChangeNotifier.Notify(nil);
  end;
end;

{ TBackgroundTaskFillingMapFactory }

function TBackgroundTaskFillingMapFactory.GetTask(
  ABitmap: TCustomBitmap32): IBackgroundTaskLayerDraw;
begin
  Result := TBackgroundTaskFillingMap.Create(ABitmap);
end;

end.
