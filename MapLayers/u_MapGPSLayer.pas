unit u_MapGPSLayer;

interface

uses
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_IGPSRecorder,
  
  
  i_IMapLayerGPSTrackConfig,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TMapGPSLayer = class(TMapLayerBasic)
  private
    FConfig: IMapLayerGPSTrackConfig;
    FGPSRecorder: IGPSRecorder;
    procedure DrawPath;
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DoRedraw; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: TMapViewPortState;
      AConfig: IMapLayerGPSTrackConfig;
      AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  Graphics,
  SysUtils,
  GR32_Polygons,
  i_ILocalCoordConverter,
  u_NotifyEventListener;

{ TMapGPSLayer }

constructor TMapGPSLayer.Create(
  AParentMap: TImage32;
  AViewPortState: TMapViewPortState;
  AConfig: IMapLayerGPSTrackConfig;
  AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

procedure TMapGPSLayer.DrawPath;
var
  j, speed: integer;
  VPolygon: TPolygon32;
  VPointPrev, VPointCurr: TDoublePoint;
  VPointsCount: Integer;
  VSegmentColor: TColor32;
  VSpeed: Extended;
  VMaxSpeed: Extended;
  VPoints: TGPSTrackPointArray;
  VLocalConverter: ILocalCoordConverter;
  VLineWidth: Double;
begin
  FConfig.LockRead;
  try
    VPointsCount := FConfig.LastPointCount;
    VLineWidth := FConfig.LineWidth;
  finally
    FConfig.UnlockRead
  end;
  VPoints := FGPSRecorder.LastPoints(VPointsCount);
  VLocalConverter := FBitmapCoordConverter;
  VPointsCount := length(VPoints);
  with FLayer.Bitmap do begin
    if (VPointsCount > 1) then begin
      VMaxSpeed := VPoints[0].Speed;
      for j := 1 to VPointsCount - 1 do begin
        if VMaxSpeed < VPoints[j].Speed then begin
          VMaxSpeed := VPoints[j].Speed;
        end;
      end;

      VPolygon := TPolygon32.Create;
      try
        VPolygon.Antialiased := true;
        VPolygon.AntialiasMode := am4times;
        VPolygon.Closed := false;
        VPointPrev := VLocalConverter.LonLat2LocalPixelFloat(VPoints[0].Point);
        for j := 1 to VPointsCount - 1 do begin
          VPointCurr := VLocalConverter.LonLat2LocalPixelFloat(VPoints[j].Point);
          VSpeed := VPoints[j - 1].Speed;
          if (VMaxSpeed > 0) then begin
            speed := round((255 * VSpeed) / VMaxSpeed);
          end else begin
            speed := 0;
          end;
          VSegmentColor := Color32(speed, 0, 256 - speed, 150);
          if (abs(VPointPrev.X - VPointCurr.X) > 1) or (Abs(VPointPrev.Y - VPointCurr.Y) > 1) then begin
            if (VPointPrev.x < 32767) and (VPointPrev.x > -32767) and (VPointPrev.y < 32767) and (VPointPrev.y > -32767) then begin
              VPolygon.Add(FixedPoint(VPointPrev.X, VPointPrev.Y));
              VPolygon.Add(FixedPoint(VPointCurr.X, VPointCurr.Y));
              with VPolygon.Outline do try
                with Grow(Fixed(VLineWidth / 2), 0.5) do try
                  DrawFill(FLayer.Bitmap, VSegmentColor);
                finally
                  free;
                end;
              finally
                free;
              end;
              VPolygon.Clear;
            end;
          end;
          VPointPrev := VPointCurr;
        end;
      finally
        VPolygon.Free;
      end;
    end;
  end;
end;

procedure TMapGPSLayer.OnConfigChange(Sender: TObject);
begin
  if FConfig.Visible then begin
    Redraw;
    Show;
  end else begin
    Hide;
  end;
end;

procedure TMapGPSLayer.DoRedraw;
begin
  inherited;
  FLayer.Bitmap.Clear(clBlack);
  DrawPath;
end;

end.
