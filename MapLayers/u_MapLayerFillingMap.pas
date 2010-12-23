unit u_MapLayerFillingMap;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IBackgroundTaskLayerDraw,
  i_ILocalCoordConverter,
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
  protected
    procedure PosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;

    procedure SetSourceMap(AMapType: TMapType; AZoom: integer);
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
    property SourceSelected: TMapType read FSourceSelected;
    property SourceZoom: integer read FSourceZoom;
    property SourceMapChangeNotifier: IJclNotifier read FSourceMapChangeNotifier;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  t_GeoTypes,
  i_ICoordConverter,
  i_ITileIterator,
  u_GlobalState,
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

    VBitmapOnMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
    if not FNeedStopExecute then begin
      VGeoConvert.CheckPixelRectFloat(VBitmapOnMapPixelRect, VZoom);
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

procedure TMapLayerFillingMap.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
  VGUID: TGUID;
  VGUIDString: string;
  VFillingmaptype: TMapType;
  VZoom: Integer;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('FillingMap');
  if VConfigProvider <> nil then begin
    try
      VGUIDString := VConfigProvider.ReadString('Map','');
      if VGUIDString <> '' then begin
        VGUID := StringToGUID(VGUIDString);
        VFillingmaptype:=GState.MapType.GetMapFromID(VGUID);
      end else begin
        VFillingmaptype := nil;
      end;
    except
      VFillingmaptype := nil;
    end;
    VZoom := VConfigProvider.ReadInteger('Zoom', SourceZoom);
    SetSourceMap(VFillingmaptype, Vzoom);
  end;
end;

procedure TMapLayerFillingMap.PosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  if FSourceZoom < 0 then begin
    Hide;
  end else begin
    if ANewVisualCoordConverter.GetZoom <= FSourceZoom then begin
      Hide;
    end else begin
      inherited;
    end;
  end;
end;

procedure TMapLayerFillingMap.OnMainMapchange(Sender: TObject);
var
  VMapType: TMapType;
begin
  if FSourceSelected = nil then begin
    VMapType := FViewPortState.GetCurrentMap;
    if FSourceMapType <> VMapType then begin
      FSourceMapType := VMapType;
      FDrawTask.ChangeSoureMap(FSourceMapType);
    end;
  end;
end;

procedure TMapLayerFillingMap.SaveConfig(
  AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('FillingMap');

  VConfigProvider.WriteInteger('Zoom', Self.SourceZoom);
  if Self.SourceSelected = nil then begin
    VConfigProvider.WriteString('Map','')
  end else begin
    VConfigProvider.WriteString('Map', Self.SourceSelected.GUIDString);
  end;
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
