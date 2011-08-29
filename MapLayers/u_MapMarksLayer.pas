unit u_MapMarksLayer;

interface

uses
  GR32,
  GR32_Image,
  i_JclNotify,
  t_CommonTypes,
  i_ImageResamplerConfig,
  i_LayerBitmapClearStrategy,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarksLayerConfig,
  i_MarksSimple,
  u_GeoFun,
  i_ViewPortState,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_InternalPerformanceCounter,
  u_MarksDbGUIHelper,
  u_MapLayerWithThreadDraw;

type
  TMapMarksLayer = class(TMapLayerTiledWithThreadDraw)
  private
    FConfig: IMarksLayerConfig;
    FConfigStatic: IUsedMarksConfigStatic;
    FDrawConfigStatic: IMarksDrawConfigStatic;
    FMarkDBGUI: TMarksDbGUIHelper;
    FMarksSubset: IMarksSubset;
    FGetMarksCounter: IInternalPerformanceCounter;
    FMouseOnRegCounter: IInternalPerformanceCounter;
    procedure OnConfigChange(Sender: TObject);
    function GetMarksSubset: IMarksSubset;
  protected
    procedure DrawBitmap(AIsStop: TIsCancelChecker); override;
    procedure SetPerfList(const Value: IInternalPerformanceCounterList); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      ATimerNoifier: IJclNotifier;
      AConfig: IMarksLayerConfig;
      AMarkDBGUI: TMarksDbGUIHelper
    );
    procedure MouseOnReg(xy: TPoint; out AMark: IMark; out AMarkS: Double); overload;
    procedure MouseOnReg(xy: TPoint; out AMark: IMark); overload;
  end;

implementation

uses
  ActiveX,
  Types,
  Classes,
  SysUtils,
  t_GeoTypes,
  i_CoordConverter,
  i_TileIterator,
  i_BitmapLayerProvider,
  u_MapMarksBitmapLayerProviderByMarksSubset,
  u_NotifyEventListener,
  u_TileIteratorSpiralByRect;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AResamplerConfig: IImageResamplerConfig;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  ATimerNoifier: IJclNotifier;
  AConfig: IMarksLayerConfig;
  AMarkDBGUI: TMarksDbGUIHelper
);
begin
  inherited Create(
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory,
    AClearStrategyFactory,
    ATimerNoifier,
    tpLower
  );
  FConfig := AConfig;
  FMarkDBGUI := AMarkDBGUI;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.MarksShowConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.MarksDrawConfig.GetChangeNotifier
  );
end;

procedure TMapMarksLayer.DrawBitmap(AIsStop: TIsCancelChecker);
var
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
  VProv: IBitmapLayerProvider;
  VMarksSubset: IMarksSubset;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FGetMarksCounter.StartOperation;
  try
    FMarksSubset := GetMarksSubset;
    VMarksSubset := FMarksSubset;
  finally
    FGetMarksCounter.FinishOperation(VCounterContext);
  end;
  VBitmapConverter := LayerCoordConverter;
  if (VMarksSubset <> nil) and (VBitmapConverter <> nil) and (not VMarksSubset.IsEmpty) then begin
    VProv :=
      TMapMarksBitmapLayerProviderByMarksSubset.Create(
        FDrawConfigStatic,
        VMarksSubset
      );
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
          VTileToDrawBmp.Clear(0);
          VProv.GetBitmapRect(
            VTileToDrawBmp,
            ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert),
          );
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
end;

procedure TMapMarksLayer.MouseOnReg(xy: TPoint; out AMark: IMark; out AMarkS: Double);
var
  VLonLatLine: TArrayOfDoublePoint;
  VLineOnBitmap: TArrayOfDoublePoint;
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VMarkLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  VZoom: Byte;
  VMark: IMark;
  VMapRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
  VVisualConverter: ILocalCoordConverter;
  VMarksSubset: IMarksSubset;
  VMarksEnum: IEnumUnknown;
  VSquare:Double;
  i: Cardinal;
  VCounterContext: TInternalPerformanceCounterContext;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
begin
  VCounterContext := FMouseOnRegCounter.StartOperation;
  try
    AMark := nil;
    AMarkS := 0;
    VMarksSubset := FMarksSubset;
    if VMarksSubset <> nil then begin
      if not VMarksSubset.IsEmpty then begin
        VRect.Left := xy.X - 8;
        VRect.Top := xy.Y - 16;
        VRect.Right := xy.X + 8;
        VRect.Bottom := xy.Y + 16;
        VLocalConverter := LayerCoordConverter;
        VConverter := VLocalConverter.GetGeoConverter;
        VZoom := VLocalConverter.GetZoom;
        VVisualConverter := ViewCoordConverter;
        VMapRect := VVisualConverter.LocalRect2MapRectFloat(VRect);
        VConverter.CheckPixelRectFloat(VMapRect, VZoom);
        VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
        VPixelPos := VVisualConverter.LocalPixel2MapPixelFloat(xy);
        VMarksEnum := VMarksSubset.GetEnum;
        while VMarksEnum.Next(1, VMark, @i) = S_OK do begin
          VMarkLonLatRect := VMark.LLRect;
          if((VLonLatRect.Right>VMarkLonLatRect.Left)and(VLonLatRect.Left<VMarkLonLatRect.Right)and
          (VLonLatRect.Bottom<VMarkLonLatRect.Top)and(VLonLatRect.Top>VMarkLonLatRect.Bottom))then begin
            if Supports(VMark, IMarkPoint) then begin
              AMark := VMark;
              AMarkS := 0;
              exit;
            end else begin
              if Supports(VMark, IMarkLine, VMarkLine) then begin
                VLonLatLine := VMarkLine.Points;
                VConverter.CheckLonLatArray(VLonLatLine);
                VLineOnBitmap := VConverter.LonLatArray2PixelArrayFloat(VLonLatLine, VZoom);
                if PointOnPath(VPixelPos, VLineOnBitmap, (VMarkLine.LineWidth / 2) + 1) then begin
                  AMark := VMark;
                  AMarkS := 0;
                  exit;
                end;
              end else if Supports(VMark, IMarkPoly, VMarkPoly) then begin
                VLonLatLine := VMarkPoly.Points;
                VConverter.CheckLonLatArray(VLonLatLine);
                VLineOnBitmap := VConverter.LonLatArray2PixelArrayFloat(VLonLatLine, VZoom);
                if (PtInRgn(VLineOnBitmap,VPixelPos)) then begin
                  VSquare := PolygonSquare(VLineOnBitmap);
                  if (AMark = nil) or (VSquare<AMarkS) then begin
                    AMark := VMark;
                    AMarkS := VSquare;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    FMouseOnRegCounter.FinishOperation(VCounterContext);
  end;
end;

function TMapMarksLayer.GetMarksSubset: IMarksSubset;
var
  VList: IInterfaceList;
  VConverter: ILocalCoordConverter;
  VZoom: Byte;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VGeoConverter: ICoordConverter;
begin
  VList := nil;
  if FConfigStatic.IsUseMarks then begin
    VConverter := LayerCoordConverter;
    if VConverter <> nil then begin
      VZoom := VConverter.GetZoom;
      if not FConfigStatic.IgnoreCategoriesVisible then begin
        VList := FMarkDBGUI.MarksDB.GetVisibleCategories(VZoom);
      end;
      try
        if (VList <> nil) and (VList.Count = 0) then begin
          Result := nil;
        end else begin
          VGeoConverter := VConverter.GetGeoConverter;
          VMapPixelRect := VConverter.GetRectInMapPixelFloat;
          VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
          VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);
          Result := FMarkDBGUI.MarksDB.MarksDb.GetMarksSubset(VLonLatRect, VList, FConfigStatic.IgnoreMarksVisible);
        end;
      finally
        VList := nil;
      end;
    end;
  end else begin
    Result := nil;
  end;
end;

procedure TMapMarksLayer.MouseOnReg(xy: TPoint; out AMark: IMark);
var
  VMarkS: Double;
begin
  MouseOnReg(xy, AMark, VMarkS);
end;

procedure TMapMarksLayer.OnConfigChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    FConfigStatic := FConfig.MarksShowConfig.GetStatic;
    FDrawConfigStatic := FConfig.MarksDrawConfig.GetStatic;
    SetVisible(FConfigStatic.IsUseMarks);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TMapMarksLayer.SetPerfList(
  const Value: IInternalPerformanceCounterList);
begin
  inherited;
  FGetMarksCounter := Value.CreateAndAddNewCounter('GetMarks');
  FMouseOnRegCounter := Value.CreateAndAddNewCounter('MouseOnReg');
end;

procedure TMapMarksLayer.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
