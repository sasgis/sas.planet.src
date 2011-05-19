unit u_MapGPSLayer;

interface

uses
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
  TMapGPSLayer = class(TMapLayerWithThreadDraw)
  private
    FConfig: IMapLayerGPSTrackConfig;
    FGPSRecorder: IGPSRecorder;
    FPoints: TGPSTrackPointArray;
    FPolygon: TPolygon32;
    procedure OnConfigChange(Sender: TObject);
    procedure DrawPath(
      AIsStop: TIsCancelChecker;
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
  u_GeoFun,
  u_NotifyEventListener;

{ TMapGPSLayer }

constructor TMapGPSLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ATimerNoifier: IJclNotifier;
  AConfig: IMapLayerGPSTrackConfig;
  AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(AParentMap, AViewPortState, ATimerNoifier, tpLower);
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  FPolygon := TPolygon32.Create;
  FPolygon.Antialiased := true;
  FPolygon.AntialiasMode := am4times;
  FPolygon.Closed := false;
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
    FPoints := FGPSRecorder.LastPoints(VPointsCount);
    VPointsCount := length(FPoints);
    if (VPointsCount > 1) then begin
      DrawPath(AIsStop, VLocalConverter, VTrackColorer, VLineWidth, VPointsCount);
    end;
  end;
end;

procedure TMapGPSLayer.DrawPath(AIsStop: TIsCancelChecker;
  ALocalConverter: ILocalCoordConverter; ATrackColorer: ITrackColorerStatic;
  ALineWidth: Double; APointsCount: Integer);
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
  VPointPrev := ALocalConverter.LonLat2LocalPixelFloat(VMapPointPrev);
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
                Layer.Bitmap.Lock;
                try
                  if not AIsStop then begin
                    DrawFill(Layer.Bitmap, VSegmentColor);
                  end;
                finally
                  Layer.Bitmap.Unlock;
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

procedure TMapGPSLayer.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
