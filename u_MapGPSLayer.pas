unit u_MapGPSLayer;

interface

uses
  GR32,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TMapGPSLayer = class(TMapLayerBasic)
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

procedure TMapGPSLayer.DoRedraw;
var i,speed,SizeTrackd2:integer;
    k1,k2:TPoint;
    ke,ks:TExtendedPoint;
    Angle,D,R: Currency;
    TanOfAngle:Extended;
    dl: integer;
    Polygon: TPolygon32;
    polygon_line: TPolygon32;
begin
  inherited;
  Polygon := TPolygon32.Create;
  Polygon.Antialiased := true;
  polygon.AntialiasMode:=am4times;
  Polygon_line := TPolygon32.Create;
  Polygon_line.Antialiased := true;
  Polygon_line.AntialiasMode := am4times;
  polygon_line.Closed:=false;
  FLayer.Bitmap.Canvas.Pen.Style:=psSolid;
  FLayer.Bitmap.Canvas.Pen.Color:=clBlue;
  FLayer.Bitmap.Clear(clBlack);

  with FLayer.Bitmap do begin
    if GState.GPS_ShowPath then begin
      for i:=0 to length(GState.GPS_TrackPoints)-2 do begin
        k1:=FGeoConvert.LonLat2PixelPos(GState.GPS_TrackPoints[i],FZoom);
        k1:=MapPixel2BitmapPixel(k1);
        k2:=FGeoConvert.LonLat2PixelPos(GState.GPS_TrackPoints[i+1],FZoom);
        k2:=MapPixel2BitmapPixel(k2);
        if (GState.GPS_ArrayOfSpeed[i]>0)and(FMain.GPSpar.maxspeed>0) then begin
          speed:=round(255/(FMain.GPSpar.maxspeed/GState.GPS_ArrayOfSpeed[i]));
        end else begin
          speed:=0;
        end;
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
    end;
  end;

  if length(GState.GPS_TrackPoints)>1 then try
    ke:=FGeoConvert.LonLat2PixelPosf(GState.GPS_TrackPoints[length(GState.GPS_TrackPoints)-1], FZoom);
    ke:=MapPixel2BitmapPixel(ke);
    ks:=FGeoConvert.LonLat2PixelPosf(GState.GPS_TrackPoints[length(GState.GPS_TrackPoints)-2], FZoom);
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
    end;
  except
  end;

  if length(GState.GPS_TrackPoints)>0 then begin
    k1:=FGeoConvert.LonLat2PixelPos(GState.GPS_TrackPoints[length(GState.GPS_TrackPoints)-1],FZoom);
    k1:=MapPixel2BitmapPixel(k1);
    SizeTrackd2:=GState.GPS_ArrowSize div 6;
    FLayer.Bitmap.FillRectS(k1.x-SizeTrackd2,k1.y-SizeTrackd2,k1.x+SizeTrackd2,k1.y+SizeTrackd2,SetAlpha(clRed32, 200));
  end;

  FreeAndNil(Polygon);
  FreeAndNil(Polygon_line);
end;

end.
