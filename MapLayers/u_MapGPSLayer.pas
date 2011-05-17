unit u_MapGPSLayer;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  t_CommonTypes,
  i_GPSRecorder,
  i_MapLayerGPSTrackConfig,
  i_ViewPortState,
  u_MapLayerWithThreadDraw;

type
  TMapGPSLayer = class(TMapLayerWithThreadDraw)
  private
    FConfig: IMapLayerGPSTrackConfig;
    FGPSRecorder: IGPSRecorder;
    procedure OnConfigChange(Sender: TObject);
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
  end;

implementation

uses
  Classes,
  Graphics,
  SysUtils,
  GR32_Polygons,
  u_GeoFun,
  i_LocalCoordConverter,
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
end;

procedure TMapGPSLayer.DrawBitmap(AIsStop: TIsCancelChecker);
var
  VTrackColorer: ITrackColorerStatic;
  VPointsCount: Integer;
  VLineWidth: Double;
  VLocalConverter: ILocalCoordConverter;
  VPoints: TGPSTrackPointArray;
  VPolygon: TPolygon32;
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
    VPoints := FGPSRecorder.LastPoints(VPointsCount);
    VPointsCount := length(VPoints);
    if (VPointsCount > 1) then begin
      VPolygon := TPolygon32.Create;
      try
        VPolygon.Antialiased := true;
        VPolygon.AntialiasMode := am4times;
        VPolygon.Closed := false;
        VMapPointPrev := VPoints[VPointsCount - 1].Point;
        VPointPrevIsEmpty := PointIsEmpty(VMapPointPrev);
        VPointPrev := VLocalConverter.LonLat2LocalPixelFloat(VMapPointPrev);
        for i := VPointsCount - 2 downto 0 do begin
          VMapPointCurr := VPoints[i].Point;
          VPointCurrIsEmpty := PointIsEmpty(VMapPointCurr);
          if not VPointCurrIsEmpty then begin
            VPointCurr := VLocalConverter.LonLat2LocalPixelFloat(VMapPointCurr);
            if not VPointPrevIsEmpty then begin
              if (abs(VPointPrev.X - VPointCurr.X) > 1) or (Abs(VPointPrev.Y - VPointCurr.Y) > 1) then begin
                if (VPointPrev.x < 32767) and (VPointPrev.x > -32767) and (VPointPrev.y < 32767) and (VPointPrev.y > -32767) then begin
                  VFixedPointsPair[0] := FixedPoint(VPointPrev.X, VPointPrev.Y);
                  VFixedPointsPair[1] := FixedPoint(VPointCurr.X, VPointCurr.Y);
                  VPolygon.AddPoints(VFixedPointsPair[0], 2);
                  with VPolygon.Outline do try
                    with Grow(Fixed(VLineWidth / 2), 0.5) do try
                      VSegmentColor := VTrackColorer.GetColorForSpeed(VPoints[i].Speed);
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
                  VPolygon.Clear;
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
      finally
        VPolygon.Free;
      end;
    end;
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
