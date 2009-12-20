unit u_MapMarksLayer;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TMapMarksLayer = class(TMapLayerBasic)
  protected
    procedure drawPath(pathll:TExtendedPointArray; color1,color2:TColor32;linew:integer;poly:boolean);
    procedure DoRedraw; override;
  public
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
  GR32_Polygons,
  pngimage,
  t_CommonTypes,
  u_GlobalState,
  Ugeofun,
  Uimgfun,
  uMapType,
  Unit1,
  UMarksExplorer,
  u_WindowLayerBasic;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(AParentMap: TImage32; ACenter: TPoint);
begin
  inherited Create(AParentMap, ACenter);
  FLayer.Bitmap.Font.Name:='Tahoma';
  FLayer.Bitmap.Font.Style:=[];
end;

procedure TMapMarksLayer.drawPath(pathll:TExtendedPointArray;color1,color2:TColor32;linew:integer;poly:boolean);
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
      FParentMap.Bitmap.BeginUpdate;
      if length(pathll)>0 then begin
        for i:=0 to length(pathll)-1 do begin
          k1:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(pathll[i],GState.zoom_size-1);
          k1:=MapPixel2BitmapPixel(k1);
          if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then begin
            polygon.Add(FixedPoint(k1));
          end;
          if i<length(pathll)-1 then begin
            k2:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(pathll[i+1],GState.zoom_size-1);
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
      FParentMap.Bitmap.endUpdate;
      FParentMap.Bitmap.Changed;
    finally
      polygon.Free;
    end;
  except
  end;
end;

procedure TMapMarksLayer.DoRedraw;
var
  LLRect:TExtendedRect;
  xy:Tpoint;
  btm:TBitmap32;
  TestArrLenP1,TestArrLenP2:TPoint;
  buf_line_arr:TExtendedPointArray;
  indexmi:integer;
  imw,texth:integer;
  marksFilter:string;
  VZoomCurr: Byte;
  VRect: TRect;
  VBitmapSize: TPoint;
begin
  inherited;
  if FMain.CDSmarks.State <> dsBrowse then exit;
  paintMark:=true;
  try
    VZoomCurr := GState.zoom_size - 1;
    VBitmapSize := GetBitmapSizeInPixel;
    VRect.TopLeft := BitmapPixel2MapPixel(Point(0,0));
    VRect.BottomRight := BitmapPixel2MapPixel(VBitmapSize);

    GState.sat_map_both.GeoConvert.CheckPixelRect(VRect, VZoomCurr, GState.CiclMap);
    LLRect := GState.sat_map_both.GeoConvert.PixelRect2LonLatRect(VRect, VZoomCurr);
    marksFilter:='';
    if GState.show_point = mshChecked then begin
      FMain.CDSKategory.Filter:='visible = 1 and ( AfterScale <= '+inttostr(GState.zoom_size)+' and BeforeScale >= '+inttostr(GState.zoom_size)+' )';
      FMain.CDSKategory.Filtered:=true;
      marksFilter:=marksFilter+'visible=1';
      FMain.CDSKategory.First;
      if FMain.CDSKategory.Eof then begin
        Visible:=false;
        FMain.CDSKategory.Filtered:=false;
        exit;
      end;
      if not(FMain.CDSKategory.Eof) then begin
        marksFilter:=marksFilter+' and (';
        while not(FMain.CDSKategory.Eof) do begin
          marksFilter:=marksFilter+'categoryid='+FMain.CDSKategory.fieldbyname('id').AsString;
          FMain.CDSKategory.Next;
          if not(FMain.CDSKategory.Eof) then begin
            marksFilter:=marksFilter+' or ';
          end;
        end;
        marksFilter:=marksFilter+')';
      end;
      FMain.CDSKategory.Filtered:=false;
      marksFilter:=marksFilter+' and ';
    end;
    marksFilter:=marksFilter+'( LonR>'+floattostr(LLRect.Left)+' and LonL<'+floattostr(LLRect.Right)+
    ' and LatB<'+floattostr(LLRect.Top)+' and LatT>'+floattostr(LLRect.Bottom)+')';
    FMain.CDSmarks.Filter:=marksFilter;
    FMain.CDSmarks.Filtered:=true;
    FMain.CDSmarks.First;
    if FMain.CDSmarks.Eof then begin
      Hide;
      FMain.CDSmarks.Filtered:=false;
      exit;
    end else begin
      FLayer.Bitmap.Clear(clBlack);
    end;
    btm:=TBitmap32.Create;
    try
      btm.DrawMode:=dmBlend;
      btm.Resampler:=TLinearResampler.Create;
      While not(FMain.CDSmarks.Eof) do begin
        buf_line_arr := Blob2ExtArr(FMain.CDSmarks.FieldByName('lonlatarr'));
        if length(buf_line_arr)>1 then begin
          TestArrLenP1:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(ExtPoint(FMain.CDSmarksLonL.AsFloat,FMain.CDSmarksLatT.AsFloat),(GState.zoom_size - 1));
          TestArrLenP2:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(ExtPoint(FMain.CDSmarksLonR.AsFloat,FMain.CDSmarksLatB.AsFloat),(GState.zoom_size - 1));
          if (abs(TestArrLenP1.X-TestArrLenP2.X)>FMain.CDSmarksScale1.AsInteger+2)or(abs(TestArrLenP1.Y-TestArrLenP2.Y)>FMain.CDSmarksScale1.AsInteger+2) then begin
            drawPath(buf_line_arr,TColor32(Fmain.CDSmarksColor1.AsInteger),TColor32(Fmain.CDSmarksColor2.AsInteger),Fmain.CDSmarksScale1.asInteger,
              (buf_line_arr[0].x=buf_line_arr[length(buf_line_arr)-1].x)and(buf_line_arr[0].y=buf_line_arr[length(buf_line_arr)-1].y));
            SetLength(buf_line_arr,0);
          end;
        end;
        if length(buf_line_arr)=1 then begin
          xy:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(buf_line_arr[0],GState.zoom_size-1);
          xy := MapPixel2BitmapPixel(xy);
          xy:=Point(xy.x,xy.y);
          imw:=FMain.CDSmarks.FieldByName('Scale2').AsInteger;
          indexmi:=GState.MarkIcons.IndexOf(FMain.CDSmarks.FieldByName('picname').AsString);
          if(indexmi=-1)and(GState.MarkIcons.Count>0) then begin
            indexmi:=0;
          end;
          if(indexmi>-1)then begin
            PNGintoBitmap32(btm,TPNGObject(GState.MarkIcons.Objects[indexmi]));
            FLayer.Bitmap.Draw(bounds(xy.x-(imw div 2),xy.y-imw,imw,imw),bounds(0,0,btm.Width,btm.Height),btm);
          end;
          if FMain.CDSmarks.FieldByName('Scale1').AsInteger>0 then begin
            FLayer.Bitmap.Font.Size:=FMain.CDSmarksScale1.AsInteger;
            texth:=FLayer.Bitmap.TextHeight(FMain.CDSmarksname.asString) div 2;
            FLayer.Bitmap.RenderText(xy.x+(imw div 2)+2,xy.y-(imw div 2)-texth+1,FMain.CDSmarksname.AsString,1,TColor32(FMain.CDSmarksColor2.AsInteger));
            FLayer.Bitmap.RenderText(xy.x+(imw div 2)+1,xy.y-(imw div 2)-texth,FMain.CDSmarksname.AsString,1,TColor32(FMain.CDSmarksColor1.AsInteger));
          end;
        end;
        FMain.CDSmarks.Next;
      end;
      FMain.CDSmarks.Filtered:=false;
    finally
      btm.Free;
    end;
  finally
    paintMark:=false;
  end;
end;

end.
