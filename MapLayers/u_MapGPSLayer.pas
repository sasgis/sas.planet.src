unit u_MapGPSLayer;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapLayerBasic;

type
  TMapGPSLayer = class(TMapLayerBasic)
  private
    procedure DrawPath;
  protected
    procedure DoRedraw; override;
  public
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
  end;

implementation

uses
  Graphics,
  Math,
  SysUtils,
  GR32_Polygons,
  i_IGPSRecorder,
  i_ILocalCoordConverter,
  u_GlobalState;

{ TMapGPSLayer }

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
begin
  VPoints := GState.GPSpar.GPSRecorder.LastVisiblePoints;
  VLocalConverter := FBitmapCoordConverter;
  VPointsCount := length(VPoints);
  with FLayer.Bitmap do begin
    if (VPointsCount > 1) then begin
      VPolygon := TPolygon32.Create;
      try
        VPolygon.Antialiased := true;
        VPolygon.AntialiasMode := am4times;
        VPolygon.Closed := false;
        VPointPrev := VLocalConverter.LonLat2LocalPixelFloat(VPoints[0].Point);
        VMaxSpeed := GState.GPSpar.maxspeed;
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
                with Grow(Fixed(GState.GPSpar.GPS_TrackWidth / 2), 0.5) do try
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

procedure TMapGPSLayer.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    Visible := VConfigProvider.ReadBool('GPSTrack', True);
  end else begin
    Visible := True;
  end;
end;

procedure TMapGPSLayer.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VSubItem: IConfigDataWriteProvider;
begin
  inherited;
  VSubItem := AConfigProvider.GetOrCreateSubItem('VIEW');
  VSubItem.WriteBool('GPSTrack', Visible);
end;

procedure TMapGPSLayer.DoRedraw;
begin
  inherited;
  FLayer.Bitmap.Clear(clBlack);
  DrawPath;
end;

end.
