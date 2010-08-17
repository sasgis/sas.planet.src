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
  u_GlobalState,
  Unit1;

{ TMapGPSLayer }

procedure TMapGPSLayer.DrawArrow;
var
  Polygon: TPolygon32;
  ke,ks:TExtendedPoint;
  dl: integer;
  Angle,D,R: Currency;
  TanOfAngle:Extended;
  k1:TPoint;
  SizeTrackd2:integer;
  VLastPoint: TExtendedPoint;
  VPreLastPoint: TExtendedPoint;
  VIsArrow: Boolean;
  VPointsCount: Integer;
begin
  VIsArrow := False;
  VPointsCount := length(GState.GPS_TrackPoints);
  if VPointsCount > 0 then begin
    VLastPoint := GState.GPS_TrackPoints[VPointsCount-1];
    if VPointsCount>1 then try
      VPreLastPoint := GState.GPS_TrackPoints[VPointsCount-2];
      ke:=FGeoConvert.LonLat2ExtendedPixelPos(VLastPoint, FZoom);
      ke:=MapPixel2BitmapPixel(ke);
      ks:=FGeoConvert.LonLat2ExtendedPixelPos(VPreLastPoint, FZoom);
      ks:=MapPixel2BitmapPixel(ks);
      dl:=GState.GPS_ArrowSize;
      D:=Sqrt(Sqr(ks.X-ke.X)+Sqr(ks.Y-ke.Y));
      if D > 0.01 then begin
        R:=D/2-(dl div 2);
        ke.x:=ke.X+(ke.X-ks.X);
        ke.y:=ke.y+(ke.y-ks.y);
        ke.x:=Round((R*ks.x+(D-R)*kE.X)/D);
        ke.y:=Round((R*ks.y+(D-R)*kE.Y)/D);
        if ks.x=ke.x then begin
          if Sign(ks.Y-ke.Y)<0 then begin
            TanOfAngle:=MinExtended/100;
          end else begin
            TanOfAngle:=MaxExtended/100;
          end;
        end else begin
          TanOfAngle:=(ks.Y-ke.Y)/(ks.X-ke.X);
        end;

        Polygon := TPolygon32.Create;
        try
          Polygon.Antialiased := true;
          polygon.AntialiasMode:=am4times;
          Polygon.Add(FixedPoint(round(ke.X),round(ke.Y)));
          Angle:=ArcTan(TanOfAngle)+0.28;
          if ((TanOfAngle<0)and(ks.X<=ke.X))or((TanOfAngle>=0)and(ks.X<=ke.X)) then begin
            Angle:=Angle+Pi;
          end;
          Polygon.Add(FixedPoint(round(ke.x) + Round(dl*Cos(Angle)),round(ke.Y) + Round(dl*Sin(Angle))));
          Angle:=ArcTan(TanOfAngle)-0.28;
          if ((TanOfAngle<0)and(ks.X<=ke.X))or((TanOfAngle>=0)and(ks.X<=ke.X)) then begin
            Angle:=Angle+Pi;
          end;
          Polygon.Add(FixedPoint(round(ke.X) + Round(dl*Cos(Angle)),round(ke.Y) + Round(dl*Sin(Angle))));
          Polygon.DrawFill(FLayer.Bitmap, SetAlpha(Color32(GState.GPS_ArrowColor), 150));
          VIsArrow := true;
        finally
          Polygon.Free;
        end;
      end;
    except
    end;
    if not VIsArrow then begin
      k1:=FGeoConvert.LonLat2PixelPos(VLastPoint,FZoom);
      k1:=MapPixel2BitmapPixel(k1);
      SizeTrackd2:=GState.GPS_ArrowSize div 6;
      FLayer.Bitmap.FillRectS(k1.x-SizeTrackd2,k1.y-SizeTrackd2,k1.x+SizeTrackd2,k1.y+SizeTrackd2,SetAlpha(clRed32, 200));
    end;
  end;
end;

procedure TMapGPSLayer.DrawPath;
var
  i,speed:integer;
  polygon_line: TPolygon32;
  startrarck:integer;
  k1,k2:TPoint;
begin
  Polygon_line := TPolygon32.Create;
  try
    Polygon_line.Antialiased := true;
    Polygon_line.AntialiasMode := am4times;
    polygon_line.Closed:=false;
    startrarck:=length(GState.GPS_TrackPoints)-GState.GPS_NumTrackPoints;
    if startrarck<0 then startrarck:=0;
    with FLayer.Bitmap do begin
      if (GState.GPS_ShowPath)and(length(GState.GPS_TrackPoints)-startrarck>1) then begin
        k1:=FGeoConvert.LonLat2PixelPos(GState.GPS_TrackPoints[startrarck],FZoom);
        k1:=MapPixel2BitmapPixel(k1);
        for i:=startrarck to length(GState.GPS_TrackPoints)-2 do begin
          k2:=FGeoConvert.LonLat2PixelPos(GState.GPS_TrackPoints[i+1],FZoom);
          k2:=MapPixel2BitmapPixel(k2);
          if (GState.GPS_ArrayOfSpeed[i]>0)and(FMain.GPSpar.maxspeed>0) then begin
            speed:=round(255/(FMain.GPSpar.maxspeed/GState.GPS_ArrayOfSpeed[i]));
          end else begin
            speed:=0;
          end;
          if (k1.X<>k2.X)or(k1.Y<>k2.Y) then begin
            if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then begin
              polygon_line.Add(FixedPoint(k1));
              polygon_line.Add(FixedPoint(k2));
            end;
            with Polygon_line.Outline do try
              with Grow(Fixed(GState.GPS_TrackWidth / 2), 0.5) do try
                DrawFill(FLayer.Bitmap, SetAlpha(Color32(speed,0,256-speed,0),150));
              finally
                free;
              end;
            finally
              free;
            end;
            Polygon_line.Clear;
          end;
          k1:=k2;
        end;
      end;
    end;
  finally
    polygon_line.Free;
  end;
end;

procedure TMapGPSLayer.DoRedraw;
begin
  inherited;
  FLayer.Bitmap.Clear(clBlack);
  DrawPath;
  DrawArrow;
end;

end.
