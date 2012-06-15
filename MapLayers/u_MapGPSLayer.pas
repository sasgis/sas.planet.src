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
    FPoints: array of TGPSTrackPoint;
    FPolygon: TPolygon32;
    procedure OnConfigChange;
    procedure OnGPSRecorderChange;
    procedure OnTimer;
    procedure DrawPath(
      AOperationID: Integer;
      const ACancelNotifier: IOperationNotifier;
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const ATrackColorer: ITrackColorerStatic;
      const ALineWidth: Double;
      APointsCount: Integer
    );
    function PrepareProjectedPointsByEnum(
      AMaxPointsCount: Integer;
      const ALocalConverter: ILocalCoordConverter;
      const AEnum: IEnumGPSTrackPoint
    ): Integer;
    procedure DrawSection(
      ATargetBmp: TCustomBitmap32;
      const ATrackColorer: ITrackColorerStatic;
      const ALineWidth: Double;
      const APointPrev, APointCurr: TDoublePoint;
      const ASpeed: Double
    );
  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: IOperationNotifier
    ); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      const ATimerNoifier: IJclNotifier;
      const AConfig: IMapLayerGPSTrackConfig;
      const AGPSRecorder: IGPSRecorder
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  SysUtils,
  GR32_Resamplers,
  i_CoordConverter,
  i_Bitmap32Static,
  i_BitmapLayerProvider,
  i_TileIterator,
  u_GeoFun,
  u_NotifyEventListener,
  u_BitmapLayerProviderByTrackPath,
  u_TileIteratorSpiralByRect;

{ TMapGPSLayer }

constructor TMapGPSLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  const ATimerNoifier: IJclNotifier;
  const AConfig: IMapLayerGPSTrackConfig;
  const AGPSRecorder: IGPSRecorder
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
    AConfig.ThreadConfig
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
  const ACancelNotifier: IOperationNotifier
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
  VEnum: IEnumGPSTrackPoint;
  VProvider: IBitmapLayerProvider;
  VBitmapStatic: IBitmap32Static;
  VTileConverter: ILocalCoordConverter;
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
      VEnum := FGPSRecorder.LastPoints(VPointsCount);
      VProvider :=
        TBitmapLayerProviderByTrackPath.Create(
          VPointsCount,
          VLineWidth,
          VTrackColorer,
          VLocalConverter.ProjectionInfo,
          VEnum
        );
    finally
      FGetTrackCounter.FinishOperation(VCounterContext);
    end;
    if (VPointsCount > 1) then begin
      if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        VTileToDrawBmp := TCustomBitmap32.Create;
        VTileToDrawBmp.CombineMode := cmMerge;
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
            VTileConverter := ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert);
            VCurrTilePixelRect := VTileConverter.GetRectInMapPixel;
            VCurrTileOnBitmapRect := VLocalConverter.MapRect2LocalRect(VCurrTilePixelRect);

            VBitmapStatic :=
              VProvider.GetBitmapRect(
                AOperationID,
                ACancelNotifier,
                VTileConverter
              );
            Layer.Bitmap.Lock;
            try
              if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                break;
              end;
              if VBitmapStatic <> nil then begin
                BlockTransfer(
                  Layer.Bitmap,
                  VCurrTileOnBitmapRect.Left,
                  VCurrTileOnBitmapRect.Top,
                  Layer.Bitmap.ClipRect,
                  VTileToDrawBmp,
                  VTilePixelsToDraw,
                  dmOpaque
                );
              end else begin
                Layer.Bitmap.FillRectS(
                  VCurrTileOnBitmapRect.Left,
                  VCurrTileOnBitmapRect.Top,
                  VCurrTileOnBitmapRect.Right,
                  VCurrTileOnBitmapRect.Bottom,
                  0
                );
              end;
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
  const ACancelNotifier: IOperationNotifier;
  ATargetBmp: TCustomBitmap32;
  const ALocalConverter: ILocalCoordConverter;
  const ATrackColorer: ITrackColorerStatic;
  const ALineWidth: Double;
  APointsCount: Integer
);
  function GetCode(
  const AMapRect: TDoubleRect;
  const APoint: TDoublePoint
  ): Byte;
    //  Смысл разрядов кода:

    // 1 рр = 1 - точка над верхним краем окна;

    // 2 рр = 1 - точка под нижним краем окна;

    // 3 рр = 1 - точка справа от правого края окна;

    // 4 рр = 1 - точка слева от левого края окна.
  begin
    Result := 0;
    if AMapRect.Top > APoint.Y then begin
      Result := 1;
    end else if AMapRect.Bottom < APoint.Y then begin
      Result := 2;
    end;

    if AMapRect.Left > APoint.X then begin
      Result := Result or 8;
    end else if AMapRect.Right < APoint.X then begin
      Result := Result or 4;
    end;
  end;

var
  VPointPrev: TDoublePoint;
  VPointPrevIsEmpty: Boolean;
  VPointPrevCode: Byte;
  VPointPrevLocal: TDoublePoint;
  i: Integer;
  VPointCurr: TDoublePoint;
  VPointCurrIsEmpty: Boolean;
  VPointCurrCode: Byte;
  VPointCurrLocal: TDoublePoint;

  VGeoConvert: ICoordConverter;
  VMapPixelRect: TDoubleRect;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;

  VPointCurrCode := 0;
  VPointPrevCode := 0;
  VPointPrev := FPoints[APointsCount - 1].Point;
  VPointPrevIsEmpty := PointIsEmpty(VPointPrev);
  if not VPointPrevIsEmpty then begin
    VPointPrevCode := GetCode(VMapPixelRect, VPointPrev);
    VPointPrevLocal := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPointPrev);
  end;
  for i := APointsCount - 2 downto 0 do begin
    VPointCurr := FPoints[i].Point;
    VPointCurrIsEmpty := PointIsEmpty(VPointCurr);
    if not VPointCurrIsEmpty then begin
      VPointCurrCode := GetCode(VMapPixelRect, VPointCurr);
      VPointCurrLocal := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPointCurr);
      if not VPointPrevIsEmpty then begin
        if (VPointPrevCode and VPointCurrCode) = 0 then begin
          if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            DrawSection(ATargetBmp, ATrackColorer, ALineWidth, VPointPrevLocal, VPointCurrLocal, FPoints[i].Speed);
          end;
        end;
      end;
    end;
    VPointPrev := VPointCurr;
    VPointPrevLocal := VPointCurrLocal;
    VPointPrevIsEmpty := VPointCurrIsEmpty;
    VPointPrevCode := VPointCurrCode;
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
  const ATrackColorer: ITrackColorerStatic;
  const ALineWidth: Double;
  const APointPrev, APointCurr: TDoublePoint;
  const ASpeed: Double
);
var
  VFixedPointsPair: array [0..10] of TFixedPoint;
  VSegmentColor: TColor32;
begin
  if (APointPrev.x < 32767) and (APointPrev.x > -32767) and (APointPrev.y < 32767) and (APointPrev.y > -32767) then begin
    VFixedPointsPair[0] := FixedPoint(APointPrev.X, APointPrev.Y);
    VFixedPointsPair[1] := FixedPoint(APointCurr.X, APointCurr.Y);
    FPolygon.Clear;
    FPolygon.AddPoints(VFixedPointsPair[0], 2);
    with FPolygon.Outline do begin
      try
        with Grow(Fixed(ALineWidth / 2), 0.5) do begin
          try
            VSegmentColor := ATrackColorer.GetColorForSpeed(ASpeed);
            DrawFill(ATargetBmp, VSegmentColor);
          finally
            free;
          end;
        end;
      finally
        free;
      end;
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
  end;
end;

function TMapGPSLayer.PrepareProjectedPointsByEnum(
  AMaxPointsCount: Integer;
  const ALocalConverter: ILocalCoordConverter;
  const AEnum: IEnumGPSTrackPoint
): Integer;
var
  i: Integer;
  VIndex: Integer;
  VPoint: TGPSTrackPoint;
  VGeoConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VCurrPointIsEmpty: Boolean;
  VPrevPointIsEmpty: Boolean;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
begin
  if Length(FPoints) < AMaxPointsCount then begin
    SetLength(FPoints, AMaxPointsCount);
  end;
  VGeoConverter := ALocalConverter.GeoConverter;
  VZoom := ALocalConverter.Zoom;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  i := 0;
  VIndex := 0;
  VPrevPointIsEmpty := True;
  while (i < AMaxPointsCount) and AEnum.Next(VPoint) do begin
    VCurrPointIsEmpty := PointIsEmpty(VPoint.Point);
    if not VCurrPointIsEmpty then begin
      VGeoConverter.CheckLonLatPos(VPoint.Point);
      VPoint.Point := VGeoConverter.LonLat2PixelPosFloat(VPoint.Point, VZoom);
      if not PixelPointInRect(VPoint.Point, VMapRect) then begin
        VPoint.Point := CEmptyDoublePoint;
        VCurrPointIsEmpty := True;
      end;
    end;

    VCurrPoint := VPoint.Point;
    if VCurrPointIsEmpty then begin
      if not VPrevPointIsEmpty then begin
        FPoints[VIndex] := VPoint;
        Inc(VIndex);
        VPrevPointIsEmpty := VCurrPointIsEmpty;
        VPrevPoint := VCurrPoint;
      end;
    end else begin
      if VPrevPointIsEmpty then begin
        FPoints[VIndex] := VPoint;
        Inc(VIndex);
        VPrevPointIsEmpty := VCurrPointIsEmpty;
        VPrevPoint := VCurrPoint;
      end else begin
        if (abs(VPrevPoint.X - VCurrPoint.X) > 2) or
          (abs(VPrevPoint.Y - VCurrPoint.Y) > 2) then begin
          FPoints[VIndex] := VPoint;
          Inc(VIndex);
          VPrevPointIsEmpty := VCurrPointIsEmpty;
          VPrevPoint := VCurrPoint;
        end;
      end;
    end;
    Inc(i);
  end;
  Result := VIndex;
end;

procedure TMapGPSLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
