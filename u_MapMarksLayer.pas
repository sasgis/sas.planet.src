unit u_MapMarksLayer;

interface

uses
  u_MapLayerBasic;

type
  TMapMarksLayer = class(TMapLayerBasic)
  protected

  public
    procedure Redraw; override;

  end;

implementation

uses
  Types,
  Graphics,
  Db,
  Classes,
  SysUtils,
  GR32,
  GR32_Resamplers,
  pngimage,
  t_GeoTypes,
  t_CommonTypes,
  u_GlobalState,
  Ugeofun,
  Uimgfun,
  uMapType,
  Unit1,
  u_WindowLayerBasic;

{ TMapMarksLayer }

procedure TMapMarksLayer.Redraw;
var
  LLRect:TExtendedRect;
  i:integer;
  xy:Tpoint;
  btm:TBitmap32;
  TestArrLenP1,TestArrLenP2:TPoint;
  arrLL:PArrLL;
  buf_line_arr:TExtendedPointArray;
  ms:TMemoryStream;
  indexmi:integer;
  imw,texth:integer;
  marksFilter:string;
  VZoomCurr: Byte;
  VRect: TRect;
  VBitmapSize: TPoint;
begin
  inherited;
  VZoomCurr := GState.zoom_size - 1;
  VBitmapSize := GetBitmapSizeInPixel;
  VRect.TopLeft := BitmapPixel2MapPixel(Point(0,0));
  VRect.BottomRight := BitmapPixel2MapPixel(VBitmapSize);

  sat_map_both.GeoConvert.CheckPixelRect(VRect, VZoomCurr, GState.CiclMap);
  LLRect := sat_map_both.GeoConvert.PixelRect2LonLatRect(VRect, VZoomCurr);
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
  btm.DrawMode:=dmBlend;
  btm.Resampler:=TLinearResampler.Create;
  While not(FMain.CDSmarks.Eof) do begin
    ms:=TMemoryStream.Create;
    TBlobField(FMain.CDSmarksLonLatArr).SaveToStream(ms);
    ms.Position:=0;
    GetMem(arrLL,ms.size);
    ms.ReadBuffer(arrLL^,ms.size);
    if (ms.size)>24 then begin
      TestArrLenP1:=sat_map_both.GeoConvert.LonLat2PixelPos(ExtPoint(FMain.CDSmarksLonL.AsFloat,FMain.CDSmarksLatT.AsFloat),(GState.zoom_size - 1));
      TestArrLenP2:=sat_map_both.GeoConvert.LonLat2PixelPos(ExtPoint(FMain.CDSmarksLonR.AsFloat,FMain.CDSmarksLatB.AsFloat),(GState.zoom_size - 1));
      if (abs(TestArrLenP1.X-TestArrLenP2.X)>FMain.CDSmarksScale1.AsInteger+2)or(abs(TestArrLenP1.Y-TestArrLenP2.Y)>FMain.CDSmarksScale1.AsInteger+2) then begin
        SetLength(buf_line_arr,(ms.size div 24));
        for i:=0 to (ms.size div 24)-1 do begin
          buf_line_arr[i]:=arrLL^[i];
        end;
        //drawPath(buf_line_arr,false,TColor32(CDSmarksColor1.AsInteger),TColor32(CDSmarksColor2.AsInteger),CDSmarksScale1.asInteger,
        //(buf_line_arr[0].x=buf_line_arr[length(buf_line_arr)-1].x)and(buf_line_arr[0].y=buf_line_arr[length(buf_line_arr)-1].y));
        SetLength(buf_line_arr,0);
      end;
    end;
    if (ms.size)=24 then begin
      xy:=sat_map_both.FCoordConverter.LonLat2PixelPos(arrLL^[0],GState.zoom_size-1);
      xy := MapPixel2BitmapPixel(xy);
      xy:=Point(xy.x-3,xy.y-3);
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
    ms.free;
    FreeMem(arrLL);
    FMain.CDSmarks.Next;
  end;
  FMain.CDSmarks.Filtered:=false;
  btm.Free;
end;

end.
