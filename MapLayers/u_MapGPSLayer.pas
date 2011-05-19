unit u_MapGPSLayer;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  t_CommonTypes,
  i_LocalCoordConverter,
  i_GPSRecorder,
  i_MapLayerGPSTrackConfig,
  i_ViewPortState,
  u_MapLayerWithThreadDraw;

type
  TMapGPSLayer = class(TMapLayerTiledWithThreadDraw)
  private
    FConfig: IMapLayerGPSTrackConfig;
    FGPSRecorder: IGPSRecorder;

    FGpsPosChangeCounter: Integer;
    FPoints: TGPSTrackPointArray;
    FPolygon: TPolygon32;
    procedure OnConfigChange(Sender: TObject);
    procedure OnGPSRecorderChange(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure DrawPath(
      AIsStop: TIsCancelChecker;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      ATrackColorer: ITrackColorerStatic;
      ALineWidth: Double;
      APointsCount: Integer
    );

  protected
    procedure DrawBitmap(AIsStop: TIsCancelChecker); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ATimerNoifier: IJclNotifier;
      AConfig: IMapLayerGPSTrackConfig;
      AGPSRecorder: IGPSRecorder
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  Graphics,
  SysUtils,
  i_CoordConverter,
  i_TileIterator,
  u_GeoFun,
  u_NotifyEventListener,
  u_TileIteratorSpiralByRect;

{ TMapGPSLayer }

constructor TMapGPSLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ATimerNoifier: IJclNotifier;
  AConfig: IMapLayerGPSTrackConfig;
  AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(AParentMap, AViewPortState, nil, ATimerNoifier, tpLower);
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnGPSRecorderChange),
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

procedure TMapGPSLayer.DrawBitmap(AIsStop: TIsCancelChecker);
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
    VPointsCount := FGPSRecorder.LastPoints(VPointsCount, FPoints);
    if (VPointsCount > 1) then begin
      if not AIsStop then begin
        VTileToDrawBmp := TCustomBitmap32.Create;
        try
          VGeoConvert := VLocalConverter.GetGeoConverter;
          VZoom := VLocalConverter.GetZoom;

          VBitmapOnMapPixelRect := VLocalConverter.GetRectInMapPixel;
          VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

          VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
          VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);
          while VTileIterator.Next(VTile) do begin
            if AIsStop then begin
              break;
            end;
            VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);

            VTilePixelsToDraw.TopLeft := Point(0, 0);
            VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
            VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

            VCurrTileOnBitmapRect.TopLeft := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
            VCurrTileOnBitmapRect.BottomRight := VLocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);

            VTileToDrawBmp.SetSize(VTilePixelsToDraw.Right, VTilePixelsToDraw.Bottom);
            VTileToDrawBmp.Clear(0);
            DrawPath(
              AIsStop,
              VTileToDrawBmp,
              CreateConverterForTileImage(VGeoConvert, VTile, VZoom),
              VTrackColorer,
              VLineWidth,
              VPointsCount
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
        finally
          VTileToDrawBmp.Free;
        end;
      end;
    end;
  end;
end;

procedure TMapGPSLayer.DrawPath(
  AIsStop: TIsCancelChecker;
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter;
  ATrackColorer: ITrackColorerStatic;
  ALineWidth: Double;
  APointsCount: Integer
);
var
  VMapPointPrev: TDoublePoint;
  VPointPrevIsEmpty: Boolean;
  VPointPrev: TDoublePoint;
  i: Integer;
  VMapPointCurr: TDoublePoint;
  VPointCurrIsEmpty: Boolean;
  VPointCurr: TDoublePoint;
  VFixedPointsPair: array [0..1] of TFixedPoint;
  VSegmentColor: TColor32;
  VIsChangePrevPoint: Boolean;
begin
  VMapPointPrev := FPoints[APointsCount - 1].Point;
  VPointPrevIsEmpty := PointIsEmpty(VMapPointPrev);
  if not VPointPrevIsEmpty then begin
    VPointPrev := ALocalConverter.LonLat2LocalPixelFloat(VMapPointPrev);
  end;
  for i := APointsCount - 2 downto 0 do begin
    VMapPointCurr := FPoints[i].Point;
    VPointCurrIsEmpty := PointIsEmpty(VMapPointCurr);
    if not VPointCurrIsEmpty then begin
      VPointCurr := ALocalConverter.LonLat2LocalPixelFloat(VMapPointCurr);
      if not VPointPrevIsEmpty then begin
        if (abs(VPointPrev.X - VPointCurr.X) > 1) or (Abs(VPointPrev.Y - VPointCurr.Y) > 1) then begin
          if (VPointPrev.x < 32767) and (VPointPrev.x > -32767) and (VPointPrev.y < 32767) and (VPointPrev.y > -32767) then begin
            VFixedPointsPair[0] := FixedPoint(VPointPrev.X, VPointPrev.Y);
            VFixedPointsPair[1] := FixedPoint(VPointCurr.X, VPointCurr.Y);
            FPolygon.Clear;
            FPolygon.AddPoints(VFixedPointsPair[0], 2);
            with FPolygon.Outline do try
              with Grow(Fixed(ALineWidth / 2), 0.5) do try
                VSegmentColor := ATrackColorer.GetColorForSpeed(FPoints[i].Speed);
                if not AIsStop then begin
                  DrawFill(ATargetBmp, VSegmentColor);
                end;
              finally
                free;
              end;
            finally
              free;
            end;
            FPolygon.Clear;
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
    if VIsChangePrevPoint then begin
      VMapPointPrev := VMapPointCurr;
      VPointPrev := VPointCurr;
      VPointPrevIsEmpty := VPointCurrIsEmpty;
    end;
    if AIsStop then begin
      Break;
    end;
  end;
  if not AIsStop then begin
    SetBitmapChanged;
  end;
end;

procedure TMapGPSLayer.OnConfigChange(Sender: TObject);
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

procedure TMapGPSLayer.OnGPSRecorderChange(Sender: TObject);
begin
  InterlockedIncrement(FGpsPosChangeCounter);
end;

procedure TMapGPSLayer.OnTimer(Sender: TObject);
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
  OnConfigChange(nil);
end;

end.
