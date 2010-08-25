unit u_MapMarksLayer;

interface

uses
  GR32,
  GR32_Image,
  GR32_Polygons,
  t_GeoTypes,
  i_ICoordConverter,
  u_MapLayerBasic;

type
  TMapMarksLayer = class(TMapLayerBasic)
  protected
    FLLRect:TExtendedRect;
    procedure drawPath2Bitmap(BtmEx:TCustomBitmap32;AGeoConvert: ICoordConverter; XYPoint:TPoint;AZoomCurr:byte;pathll:TExtendedPointArray;color1,color2:TColor32;linew:integer;poly:boolean);
    procedure PreparePolygon(pathll:TExtendedPointArray; polygon: TPolygon32);
    procedure drawPath(pathll:TExtendedPointArray; color1,color2:TColor32;linew:integer;poly:boolean);
    procedure DrawMarks;
    procedure DoRedraw; override;
  public
    procedure DoRedraw2Bitmap(BtmEx:TBitmap32; AGeoConvert: ICoordConverter; ATargetRect:TRect; AZoomCurr:byte);
    constructor Create(AParentMap: TImage32; ACenter: TPoint);

  end;

implementation

uses
  Types,
  Graphics,
  Db,
  Classes,
  SysUtils,
  GR32_Resamplers,
  t_CommonTypes,
  u_GlobalState,
  Ugeofun,
  u_MarksSimple,
  u_MarksReadWriteSimple,
  u_WindowLayerBasic;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(AParentMap: TImage32; ACenter: TPoint);
begin
  inherited Create(AParentMap, ACenter);
  FLayer.Bitmap.Font.Name:='Tahoma';
  FLayer.Bitmap.Font.Style:=[];
end;

procedure TMapMarksLayer.drawPath2Bitmap(BtmEx:TCustomBitmap32; AGeoConvert: ICoordConverter;XYPoint:TPoint; AZoomCurr:byte;pathll:TExtendedPointArray;color1,color2:TColor32;linew:integer;poly:boolean);
var
  i,adp,j:integer;
  k1,k2,k4:TPoint;
  k3:TextendedPoint;
  polygon: TPolygon32;
begin
  try
    polygon:=TPolygon32.Create;
    try
      polygon.Antialiased:=true;
      polygon.AntialiasMode:=am4times;
      polygon.Closed:=poly;
      if length(pathll)>0 then begin
        for i:=0 to length(pathll)-1 do begin
          k1:=AGeoConvert.LonLat2PixelPos(pathll[i], AZoomCurr);
          k1:=Point(k1.X-XYPoint.x,k1.Y-XYPoint.y);
          if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then begin
            polygon.Add(FixedPoint(k1));
          end;
          if i<length(pathll)-1 then begin
            k2:=AGeoConvert.LonLat2PixelPos(pathll[i+1], AZoomCurr);
            k2:=Point(k2.X-XYPoint.x,k2.Y-XYPoint.y);
            if (k2.x-k1.x)>(k2.y-k1.y) then begin
              adp:=(k2.x-k1.x)div 32767+2;
            end else begin
              adp:=(k2.y-k1.y)div 32767+2;
            end;
            k3:=extPoint(((k2.X-k1.x)/adp),((k2.y-k1.y)/adp));
            if adp>2 then begin
              for j:=1 to adp-1 do begin
                k4:=Point(round(k1.x+k3.x*j),round(k1.Y+k3.y*j));
                if(k4.x<32767)and(k4.x>-32767)and(k4.y<32767)and(k4.y>-32767)then begin
                  polygon.Add(FixedPoint(k4.x,k4.y));
                end;
              end;
            end;
          end;
        end;
        if poly then begin
          Polygon.DrawFill(BtmEx, color2);
        end;
        with Polygon.Outline do try
          with Grow(Fixed(linew / 2), 0.5) do try
            FillMode := pfWinding;
            DrawFill(BtmEx, color1);
          finally
            free;
          end;
        finally
          free;
        end;
      end;
    finally
      polygon.Free;
    end;
  except
  end;
end;

procedure TMapMarksLayer.DoRedraw2Bitmap(BtmEx:TBitmap32;AGeoConvert: ICoordConverter;ATargetRect: TRect; AZoomCurr:byte);
var
  xy,xyb:Tpoint;
  btm:TCustomBitmap32;
  dLL:TExtendedPoint;
  TestArrLenP1,TestArrLenP2:TPoint;
  buf_line_arr:TExtendedPointArray;
  indexmi:integer;
  imw,texth:integer;
  LLRect: TExtendedRect;
  VIconSource: TCustomBitmap32;
  VMarksIterator: TMarksIteratorVisibleInRectIgnoreEdit;
  VMark: TMarkFull;
begin
  if (GState.show_point = mshNone) then exit;
  try
    LLRect := AGeoConvert.PixelRect2LonLatRect(ATargetRect, AZoomCurr);
    dLL:=ExtPoint((LLRect.Right-LLRect.Left)/2,(LLRect.Top-LLRect.Bottom)/2);
    LLRect.Left := LLRect.Left - dLL.X;
    LLRect.Top := LLRect.Top - dLL.Y;
    LLRect.Right := LLRect.Right + dLL.X;
    LLRect.Bottom := LLRect.Bottom + dLL.Y;

    VMarksIterator := TMarksIteratorVisibleInRectIgnoreEdit.Create(AZoomCurr, LLRect);
    try
      BtmEx.Font.Name:='Tahoma';
      BtmEx.Font.Style:=[];
      btm:=TCustomBitmap32.Create;
      try
        btm.DrawMode:=dmBlend;
        btm.Resampler:=TLinearResampler.Create;
        While VMarksIterator.Next do begin
          VMark := VMarksIterator.Current;
          buf_line_arr := VMark.Points;
          if length(buf_line_arr)>1 then begin
            TestArrLenP1:=AGeoConvert.LonLat2PixelPos(VMark.LLRect.TopLeft,(AZoomCurr));
            TestArrLenP2:=AGeoConvert.LonLat2PixelPos(VMark.LLRect.BottomRight,(AZoomCurr));
            if (abs(TestArrLenP1.X-TestArrLenP2.X)>VMark.Scale1+2)or(abs(TestArrLenP1.Y-TestArrLenP2.Y)>VMark.Scale1+2) then begin
              drawPath2Bitmap(BtmEx,AGeoConvert,ATargetRect.TopLeft,AZoomCurr, buf_line_arr,VMark.Color1,VMark.Color2,VMark.Scale1,
                (buf_line_arr[0].x=buf_line_arr[length(buf_line_arr)-1].x)and(buf_line_arr[0].y=buf_line_arr[length(buf_line_arr)-1].y));
            end;
          end;
          if length(buf_line_arr)=1 then begin
            xy:=AGeoConvert.LonLat2PixelPos(buf_line_arr[0],AZoomCurr);
            xyb:=ATargetRect.TopLeft;
            xy:=Point(xy.x - xyb.x,xy.y - xyb.y);
            imw:=VMark.Scale2;
            indexmi:=GState.MarkIcons.IndexOf(VMark.PicName);
            if(indexmi=-1)and(GState.MarkIcons.Count>0) then begin
              indexmi:=0;
            end;
            if(indexmi>-1)then begin
              VIconSource := TCustomBitmap32(GState.MarkIcons.Objects[indexmi]);
              btm.SetSize(VIconSource.Width, VIconSource.Height);
              btm.Draw(0, 0, VIconSource);
              BtmEx.Draw(bounds(xy.x-(imw div 2),xy.y-imw,imw,imw),bounds(0,0,btm.Width,btm.Height), btm);
            end;
            if VMark.Scale1>0 then begin
              BtmEx.Font.Size:=VMark.Scale1;
              texth:=BtmEx.TextHeight(VMark.name) div 2;
              BtmEx.RenderText(xy.x+(imw div 2)+2,xy.y-(imw div 2)-texth+1,VMark.name,1,VMark.Color2);
              BtmEx.RenderText(xy.x+(imw div 2)+1,xy.y-(imw div 2)-texth,VMark.name,1,VMark.Color1);
            end;
          end;
        end;
      finally
        btm.Free;
      end;
    finally
      VMarksIterator.Free;
    end;
  finally
  end;
end;

procedure TMapMarksLayer.PreparePolygon(pathll: TExtendedPointArray;
  polygon: TPolygon32);
var
  i,adp,j:integer;
  k1,k2,k4:TPoint;
  k3:TextendedPoint;
  VLonLat: TExtendedPoint;
begin
  for i:=0 to length(pathll)-1 do begin
    VLonLat := pathll[i];
    FGeoConvert.CheckLonLatPos(VLonLat);
    k1:=FGeoConvert.LonLat2PixelPos(VLonLat,FZoom);
    k1:=MapPixel2BitmapPixel(k1);
    if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then begin
      polygon.Add(FixedPoint(k1));
    end;
    if i<length(pathll)-1 then begin
      VLonLat := pathll[i+1];
      FGeoConvert.CheckLonLatPos(VLonLat);
      k2:=FGeoConvert.LonLat2PixelPos(VLonLat,FZoom);
      k2:=MapPixel2BitmapPixel(k2);
      if (k2.x-k1.x)>(k2.y-k1.y) then begin
        adp:=(k2.x-k1.x)div 32767+2;
      end else begin
        adp:=(k2.y-k1.y)div 32767+2;
      end;
      k3:=extPoint(((k2.X-k1.x)/adp),((k2.y-k1.y)/adp));
      if adp>2 then begin
        for j:=1 to adp-1 do begin
          k4:=Point(round(k1.x+k3.x*j),round(k1.Y+k3.y*j));
          if(k4.x<32767)and(k4.x>-32767)and(k4.y<32767)and(k4.y>-32767)then begin
            polygon.Add(FixedPoint(k4.x,k4.y));
          end;
        end;
      end;
    end;
  end;
end;

procedure TMapMarksLayer.drawPath(pathll:TExtendedPointArray;color1,color2:TColor32;linew:integer;poly:boolean);
var
  polygon: TPolygon32;
begin
  try
    polygon:=TPolygon32.Create;
    try
      polygon.Antialiased:=true;
      polygon.AntialiasMode:=am4times;
      polygon.Closed:=poly;
      if length(pathll)>0 then begin
        PreparePolygon(pathll, polygon);
        if poly then begin
          Polygon.DrawFill(FLayer.Bitmap, color2);
        end;
        with Polygon.Outline do try
          with Grow(Fixed(linew / 2), 0.5) do try
            FillMode := pfWinding;
            DrawFill(FLayer.Bitmap, color1);
          finally
            free;
          end;
        finally
          free;
        end;
      end;
    finally
      polygon.Free;
    end;
  except
  end;
end;

procedure TMapMarksLayer.DrawMarks;
var
  xy:Tpoint;
  btm:TCustomBitmap32;
  TestArrLenLonLatRect: TExtendedRect;
  TestArrLenPixelRect: TRect;
  buf_line_arr:TExtendedPointArray;
  indexmi:integer;
  imw,texth:integer;
  VIconSource: TCustomBitmap32;
  VScale1: Integer;
  VColor1: TColor32;
  VColor2: TColor32;
  VPointCount: Integer;
  VMarkName: string;
  VMarksIterator: TMarksIteratorVisibleInRectIgnoreEdit;
  VMark: TMarkFull;
begin
  VMarksIterator := TMarksIteratorVisibleInRectIgnoreEdit.Create(FZoom, FLLRect);
  try
    btm:=TCustomBitmap32.Create;
    try
      btm.DrawMode:=dmBlend;
      btm.Resampler:=TLinearResampler.Create;
      While VMarksIterator.Next do begin
        VMark := VMarksIterator.Current;
        VScale1 := VMark.Scale1;
        VColor1 := VMark.Color1;
        VColor2 := VMark.Color2;
        VMarkName := VMark.name;
        buf_line_arr := VMark.Points;
        VPointCount := length(buf_line_arr);
        if VPointCount>1 then begin
          TestArrLenLonLatRect := VMark.LLRect;
          FGeoConvert.CheckLonLatRect(TestArrLenLonLatRect);
          TestArrLenPixelRect := FGeoConvert.LonLatRect2PixelRect(TestArrLenLonLatRect, FZoom);
          if (abs(TestArrLenPixelRect.Left-TestArrLenPixelRect.Right)>VScale1+2)or(abs(TestArrLenPixelRect.Top-TestArrLenPixelRect.Bottom)>VScale1+2) then begin
            drawPath(buf_line_arr,VColor1,VColor2,VScale1,
              (buf_line_arr[0].x=buf_line_arr[VPointCount-1].x)and(buf_line_arr[0].y=buf_line_arr[VPointCount-1].y));
          end;
        end else if VPointCount =1 then begin
          xy:=FGeoConvert.LonLat2PixelPos(buf_line_arr[0],FZoom);
          xy := MapPixel2BitmapPixel(xy);
          imw:=VMark.Scale2;
          indexmi:=GState.MarkIcons.IndexOf(VMark.PicName);
          if(indexmi=-1)and(GState.MarkIcons.Count>0) then begin
            indexmi:=0;
          end;
          if(indexmi>-1)then begin
            VIconSource := TCustomBitmap32(GState.MarkIcons.Objects[indexmi]);
            btm.SetSize(VIconSource.Width, VIconSource.Height);
            btm.Draw(0, 0, VIconSource);
            FLayer.Bitmap.Draw(bounds(xy.x-(imw div 2),xy.y-imw,imw,imw),bounds(0,0,btm.Width,btm.Height),btm);
          end;
          if VScale1>0 then begin
            FLayer.Bitmap.Font.Size:=VScale1;
            texth:=FLayer.Bitmap.TextHeight(VMarkName) div 2;
            FLayer.Bitmap.RenderText(xy.x+(imw div 2)+2,xy.y-(imw div 2)-texth+1,VMarkName,1,VColor2);
            FLayer.Bitmap.RenderText(xy.x+(imw div 2)+1,xy.y-(imw div 2)-texth,VMarkName,1,VColor1);
          end;
        end;
      end;
    finally
      btm.Free;
    end;
  finally
    VMarksIterator.Free;
  end;
end;


procedure TMapMarksLayer.DoRedraw;
var
  VBitmapSize: TPoint;
  VRect: TRect;
begin
  inherited;
  if (GState.show_point <> mshNone) then begin
    FLayer.Bitmap.Clear(clBlack);
    VBitmapSize := GetBitmapSizeInPixel;
    VRect.TopLeft := BitmapPixel2MapPixel(Point(0,0));
    VRect.BottomRight := BitmapPixel2MapPixel(VBitmapSize);

    FGeoConvert.CheckPixelRect(VRect, FZoom, false);
    FLLRect := FGeoConvert.PixelRect2LonLatRect(VRect, FZoom);
    DrawMarks;
  end;
end;

end.
