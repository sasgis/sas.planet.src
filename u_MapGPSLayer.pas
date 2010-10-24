unit u_MapGPSLayer;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TMapGPSLayer = class(TMapLayerBasic)
  private
    procedure DrawPath;
    procedure DrawArrow;
  protected
    procedure DoRedraw; override;
  public
  end;

implementation

uses
  Graphics,
  Math,
  SysUtils,
  GR32_Polygons,
  i_IGPSRecorder,
  u_GlobalState;

{ TMapGPSLayer }

procedure TMapGPSLayer.DrawArrow;
var
  VPolygonArrow: TPolygon32;
  ke, ks: TExtendedPoint;
  VArrowSize: integer;
  Angle, D, R: Extended;
  TanOfAngle: Extended;
  k1: TPoint;
  SizeTrackd2: integer;
  VLastPoint: TExtendedPoint;
  VPreLastPoint: TExtendedPoint;
  VIsArrow: Boolean;
  VMarkRect: TRect;
begin
  VIsArrow := False;
  if GState.GPSpar.GPSRecorder.GetTwoLastPoints(VLastPoint, VPreLastPoint) then begin
    try
      ke := FGeoConvert.LonLat2PixelPosFloat(VLastPoint, FZoom);
      ke := MapPixel2BitmapPixel(ke);
      ks := FGeoConvert.LonLat2PixelPosFloat(VPreLastPoint, FZoom);
      ks := MapPixel2BitmapPixel(ks);
      VArrowSize := GState.GPSpar.GPS_ArrowSize;
      D := Sqrt(Sqr(ks.X - ke.X) + Sqr(ks.Y - ke.Y));
      if D > 0.01 then begin
        R := D / 2 - (VArrowSize div 2);
        ke.x := ke.X + (ke.X - ks.X);
        ke.y := ke.y + (ke.y - ks.y);
        ke.x := Round((R * ks.x + (D - R) * kE.X) / D);
        ke.y := Round((R * ks.y + (D - R) * kE.Y) / D);
        if ks.x = ke.x then begin
          if Sign(ks.Y - ke.Y) < 0 then begin
            TanOfAngle := MinExtended / 100;
          end else begin
            TanOfAngle := MaxExtended / 100;
          end;
        end else begin
          TanOfAngle := (ks.Y - ke.Y) / (ks.X - ke.X);
        end;

        VPolygonArrow := TPolygon32.Create;
        try
          VPolygonArrow.Antialiased := true;
          VPolygonArrow.AntialiasMode := am4times;
          VPolygonArrow.Add(FixedPoint(round(ke.X), round(ke.Y)));
          Angle := ArcTan(TanOfAngle) + 0.28;
          if ((TanOfAngle < 0) and (ks.X <= ke.X)) or ((TanOfAngle >= 0) and (ks.X <= ke.X)) then begin
            Angle := Angle + Pi;
          end;
          VPolygonArrow.Add(FixedPoint(round(ke.x) + Round(VArrowSize * Cos(Angle)), round(ke.Y) + Round(VArrowSize * Sin(Angle))));
          Angle := ArcTan(TanOfAngle) - 0.28;
          if ((TanOfAngle < 0) and (ks.X <= ke.X)) or ((TanOfAngle >= 0) and (ks.X <= ke.X)) then begin
            Angle := Angle + Pi;
          end;
          VPolygonArrow.Add(FixedPoint(round(ke.X) + Round(VArrowSize * Cos(Angle)), round(ke.Y) + Round(VArrowSize * Sin(Angle))));
          VPolygonArrow.DrawFill(FLayer.Bitmap, SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 150));
          VIsArrow := true;
        finally
          VPolygonArrow.Free;
        end;
      end;
    except
    end;
    if not VIsArrow then begin
      k1 := FGeoConvert.LonLat2PixelPos(VLastPoint, FZoom);
      k1 := MapPixel2BitmapPixel(k1);
      SizeTrackd2 := GState.GPSpar.GPS_ArrowSize div 6;
      VMarkRect := Bounds(k1.x - SizeTrackd2, k1.y - SizeTrackd2, SizeTrackd2, SizeTrackd2);
      FLayer.Bitmap.FillRectS(VMarkRect, SetAlpha(clRed32, 200));
    end;
  end;
end;

procedure TMapGPSLayer.DrawPath;
var
  j, speed: integer;
  VPolygon: TPolygon32;
  VPointPrev, VPointCurr: TExtendedPoint;
  VPointsCount: Integer;
  VSegmentColor: TColor32;
  VSpeed: Extended;
  VMaxSpeed: Extended;
  VPoints: TGPSTrackPointArray;
begin
  VPoints := GState.GPSpar.GPSRecorder.LastVisiblePoints;
  
  VPointsCount := length(VPoints);
  with FLayer.Bitmap do begin
    if (VPointsCount > 1) then begin
      VPolygon := TPolygon32.Create;
      try
        VPolygon.Antialiased := true;
        VPolygon.AntialiasMode := am4times;
        VPolygon.Closed := false;
        VPointPrev := FGeoConvert.LonLat2PixelPosFloat(VPoints[0].Point, FZoom);
        VPointPrev := MapPixel2BitmapPixel(VPointPrev);
        VMaxSpeed := GState.GPSpar.maxspeed;
        for j := 1 to VPointsCount - 1 do begin
          VPointCurr := FGeoConvert.LonLat2PixelPosFloat(VPoints[j].Point, FZoom);
          VPointCurr := MapPixel2BitmapPixel(VPointCurr);
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

procedure TMapGPSLayer.DoRedraw;
begin
  inherited;
  FLayer.Bitmap.Clear(clBlack);
  if GState.GPSpar.GPS_ShowPath then begin
    DrawPath;
  end;
  DrawArrow;
end;

end.
