unit u_MapGPSLayer;

interface

uses
  Windows,
  GR32,
  GR32_Polygons,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_InternalPerformanceCounter,
  i_OperationNotifier,
  i_LayerBitmapClearStrategy,
  i_ImageResamplerConfig,
  i_GPSRecorder,
  i_MapLayerGPSTrackConfig,
  i_ViewPortState,
  u_MapLayerWithThreadDraw;

type
  TMapGPSLayer = class(TMapLayerTiledWithThreadDraw)
  private
    FConfig: IMapLayerGPSTrackConfig;
    FGPSRecorder: IGPSRecorder;

    FGetTrackCounter: IInternalPerformanceCounter;
    FGpsPosChangeCounter: Integer;
    FPoints: TGPSTrackPointArray;
    FPolygon: TPolygon32;
    procedure OnConfigChange;
    procedure OnGPSRecorderChange;
    procedure OnTimer;
    procedure DrawPath(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      ATrackColorer: ITrackColorerStatic;
      ALineWidth: Double;
      APointsCount: Integer
    );
    procedure DrawSection(
      ATargetBmp: TCustomBitmap32;
      ATrackColorer: ITrackColorerStatic;
      ALineWidth: Double;
      APointPrev, APointCurr: TDoublePoint;
      ASpeed: Double
    );
  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    ); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      ATimerNoifier: IJclNotifier;
      AConfig: IMapLayerGPSTrackConfig;
      AGPSRecorder: IGPSRecorder
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  SysUtils,
  i_CoordConverter,
  i_TileIterator,
  u_GeoFun,
  u_NotifyEventListener,
  u_TileIteratorSpiralByRect;

{ TMapGPSLayer }

constructor TMapGPSLayer.Create(
  APerfList: IInternalPerformanceCounterList;
  AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AResamplerConfig: IImageResamplerConfig;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  ATimerNoifier: IJclNotifier;
  AConfig: IMapLayerGPSTrackConfig;
  AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(
    APerfList,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory,
    AClearStrategyFactory,
    ATimerNoifier,
    tpLower
  );
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;

  FGetTrackCounter := PerfList.CreateAndAddNewCounter('GetTrack');

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnGPSRecorderChange),
    FGPSRecorder.GetChangeNotifier
  );

  FPolygon := TPolygon32.Create;
  FPolygon.Antialiased := true;
  FPolygon.AntialiasMode := am4times;
  FPolygon.Closed := false;
  FGpsPosChangeCounter := 0;
end;

destructor TMapGPSLayer.Destroy;
begin
  FreeAndNil(FPolygon);
  inherited;
end;

procedure TMapGPSLayer.DrawBitmap(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier
);
var
  VTrackColorer: ITrackColorerStatic;
  VPointsCount: Integer;
  VLineWidth: Double;
  VLocalConverter: ILocalCoordConverter;

  VTileToDrawBmp: TCustomBitmap32;
  VTileIterator: ITileIterator;
  VGeoConvert: ICoordConverter;

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
  VCounterContext: TInternalPerformanceCounterContext;
begin
  inherited;
  FConfig.LockRead;
  try
    VPointsCount := FConfig.LastPointCount;
    VLineWidth := FConfig.LineWidth;
    VTrackColorer := FConfig.TrackColorerConfig.GetStatic;
  finally
    FConfig.UnlockRead
  end;

  if (VPointsCount > 1) then begin
    VLocalConverter := LayerCoordConverter;
    VCounterContext := FGetTrackCounter.StartOperation;
    try
      VPointsCount := FGPSRecorder.LastPoints(VPointsCount, FPoints);
    finally
      FGetTrackCounter.FinishOperation(VCounterContext);
    end;
    if (VPointsCount > 1) then begin
      if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        VTileToDrawBmp := TCustomBitmap32.Create;
        VTileToDrawBmp.CombineMode:=cmMerge;
        try
          VGeoConvert := VLocalConverter.GetGeoConverter;
          VZoom := VLocalConverter.GetZoom;

          VBitmapOnMapPixelRect := VLocalConverter.GetRectInMapPixel;
          VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

          VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
          VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);
          while VTileIterator.Next(VTile) do begin
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              break;
            end;
            VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);

            VTilePixelsToDraw.TopLeft := Point(0, 0);
            VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
            VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

            VCurrTileOnBitmapRect := VLocalConverter.MapRect2LocalRect(VCurrTilePixelRect);

            VTileToDrawBmp.SetSize(VTilePixelsToDraw.Right, VTilePixelsToDraw.Bottom);
            VTileToDrawBmp.Clear(0);
            DrawPath(
              AOperationID,
              ACancelNotifier,
              VTileToDrawBmp,
              ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert),
              VTrackColorer,
              VLineWidth,
              VPointsCount
            );

            Layer.Bitmap.Lock;
            try
              if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                break;
              end;
              Layer.Bitmap.Draw(VCurrTileOnBitmapRect, VTilePixelsToDraw, VTileToDrawBmp);
              SetBitmapChanged;
            finally
              Layer.Bitmap.UnLock;
            end;
          end;
        finally
          VTileToDrawBmp.Free;
        end;
      end;
    end;
  end;
end;

procedure TMapGPSLayer.DrawPath(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter;
  ATrackColorer: ITrackColorerStatic;
  ALineWidth: Double;
  APointsCount: Integer
);
  function GetCode(ALLRect: TDoubleRect; ALL: TDoublePoint): Byte;
  //  Смысл разрядов кода:
  //
  // 1 рр = 1 - точка над верхним краем окна;
  //
  // 2 рр = 1 - точка под нижним краем окна;
  //
  // 3 рр = 1 - точка справа от правого края окна;
  //
  // 4 рр = 1 - точка слева от левого края окна.
  begin
    Result := 0;
    if ALLRect.Top < ALL.Y then begin
      Result := 1;
    end else if ALLRect.Bottom > ALL.Y then begin
      Result := 2
    end;

    if ALLRect.Left > ALL.X then begin
      Result := Result or 8;
    end else if ALLRect.Right < ALL.X then begin
      Result := Result or 4
    end;

  end;
var
  VPointPrevLL: TDoublePoint;
  VPointPrevIsEmpty: Boolean;
  VPointPrevLLCode: Byte;
  VPointPrev: TDoublePoint;
  i: Integer;
  VPointCurrLL: TDoublePoint;
  VPointCurrIsEmpty: Boolean;
  VPointCurrLLCode: Byte;
  VPointCurr: TDoublePoint;
  VIsChangePrevPoint: Boolean;

  VGeoConvert: ICoordConverter;
  VZoom: Byte;
  VMapPixelRect: TDoubleRect;
  VLLRect: TDoubleRect;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VMapPixelRect, VZoom);
  VLLRect := VGeoConvert.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

  VPointCurrLLCode := 0;
  VPointPrevLLCode := 0;
  VPointPrevLL := FPoints[APointsCount - 1].Point;
  VPointPrevIsEmpty := PointIsEmpty(VPointPrevLL);
  if not VPointPrevIsEmpty then begin
    VPointPrevLLCode := GetCode(VLLRect, VPointPrevLL);
    VPointPrev := ALocalConverter.LonLat2LocalPixelFloat(VPointPrevLL);
  end;
  for i := APointsCount - 2 downto 0 do begin
    VPointCurrLL := FPoints[i].Point;
    VPointCurrIsEmpty := PointIsEmpty(VPointCurrLL);
    if not VPointCurrIsEmpty then begin
      VPointCurrLLCode := GetCode(VLLRect, VPointCurrLL);
      VPointCurr := ALocalConverter.LonLat2LocalPixelFloat(VPointCurrLL);
      if not VPointPrevIsEmpty then begin
        if (VPointPrevLLCode and VPointCurrLLCode) = 0 then begin
          if (abs(VPointPrev.X - VPointCurr.X) > 1) or (Abs(VPointPrev.Y - VPointCurr.Y) > 1) then begin
            if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              DrawSection(ATargetBmp, ATrackColorer, ALineWidth, VPointPrev, VPointCurr,  FPoints[i].Speed);
            end;
            VIsChangePrevPoint := True;
          end else begin
            VIsChangePrevPoint := False;
          end;
        end else begin
          VIsChangePrevPoint := True;
        end;
      end else begin
        VIsChangePrevPoint := True;
      end;
    end else begin
      VIsChangePrevPoint := True;
    end;
    if VIsChangePrevPoint then begin
      VPointPrevLL := VPointCurrLL;
      VPointPrev := VPointCurr;
      VPointPrevIsEmpty := VPointCurrIsEmpty;
      VPointPrevLLCode := VPointCurrLLCode;
    end;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Break;
    end;
  end;
  if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    SetBitmapChanged;
  end;
end;

procedure TMapGPSLayer.DrawSection(
  ATargetBmp: TCustomBitmap32;
  ATrackColorer: ITrackColorerStatic;
  ALineWidth: Double;
  APointPrev, APointCurr: TDoublePoint;
  ASpeed: Double);
var
  VFixedPointsPair: array [0..10] of TFixedPoint;
  VSegmentColor: TColor32;
begin
  if (APointPrev.x < 32767) and (APointPrev.x > -32767) and (APointPrev.y < 32767) and (APointPrev.y > -32767) then begin
    VFixedPointsPair[0] := FixedPoint(APointPrev.X, APointPrev.Y);
    VFixedPointsPair[1] := FixedPoint(APointCurr.X, APointCurr.Y);
    FPolygon.Clear;
    FPolygon.AddPoints(VFixedPointsPair[0], 2);
    with FPolygon.Outline do try
      with Grow(Fixed(ALineWidth / 2), 0.5) do try
        VSegmentColor := ATrackColorer.GetColorForSpeed(ASpeed);
        DrawFill(ATargetBmp, VSegmentColor);
      finally
        free;
      end;
    finally
      free;
    end;
    FPolygon.Clear;
  end;
end;

procedure TMapGPSLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
    SetVisible(FConfig.Visible);
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TMapGPSLayer.OnGPSRecorderChange;
begin
  InterlockedIncrement(FGpsPosChangeCounter);
end;

procedure TMapGPSLayer.OnTimer;
begin
  if InterlockedExchange(FGpsPosChangeCounter, 0) > 0 then begin
    ViewUpdateLock;
    try
      SetNeedRedraw;
    finally
      ViewUpdateUnlock;
    end;
    ViewUpdate;
  end;
end;

procedure TMapGPSLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
