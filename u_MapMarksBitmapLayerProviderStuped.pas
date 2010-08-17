unit u_MapMarksBitmapLayerProviderStuped;

interface

uses
  Types,
  SyncObjs,
  GR32,
  i_ICoordConverter,
  i_IBitmapLayerProvider;

type
  TMapMarksBitmapLayerProviderStuped = class(TInterfacedObject, IBitmapLayerProvider)
  private
    procedure GetBitmapRect(
      ATargetBmp: TCustomBitmap32;
      AConverter: ICoordConverter;
      ATargetRect: TRect;
      ATargetZoom: Byte
    );
  public
  end;
implementation

uses
  Classes,
  SysUtils,
  DB,
  GR32_Resamplers,
  GR32_Polygons,
  t_GeoTypes,
  t_CommonTypes,
  u_GlobalState,
  Ugeofun,
  UMarksExplorer,
  Unit1;

{ TMapMarksBitmapLayerProviderStupedThreaded }

type
  TMapMarksBitmapLayerProviderStupedThreaded = class
  private
    FTargetBmp: TCustomBitmap32;
    FConverter: ICoordConverter;
    FTargetRect: TRect;
    FTargetZoom: Byte;
    procedure drawPath2Bitmap(pathll:TExtendedPointArray; color1, color2:TColor32; linew:integer; poly:boolean);
  public
    constructor Create(
      ATargetBmp: TCustomBitmap32;
      AConverter: ICoordConverter;
      ATargetRect: TRect;
      ATargetZoom: Byte
    );
    procedure SyncGetBitmap;
  end;

constructor TMapMarksBitmapLayerProviderStupedThreaded.Create(
  ATargetBmp: TCustomBitmap32; AConverter: ICoordConverter;
  ATargetRect: TRect; ATargetZoom: Byte);
begin
  FTargetBmp := ATargetBmp;
  FConverter := AConverter;
  FTargetRect := ATargetRect;
  FTargetZoom := FTargetZoom;
end;

procedure TMapMarksBitmapLayerProviderStupedThreaded.drawPath2Bitmap(
  pathll: TExtendedPointArray; color1, color2: TColor32; linew: integer;
  poly: boolean);
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
          k1:=FConverter.LonLat2PixelPos(pathll[i], FTargetZoom);
          k1:=Point(k1.X-FTargetRect.Left,k1.Y-FTargetRect.Top);
          if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then begin
            polygon.Add(FixedPoint(k1));
          end;
          if i<length(pathll)-1 then begin
            k2:=FConverter.LonLat2PixelPos(pathll[i+1], FTargetZoom);
            k2:=Point(k2.X-FTargetRect.Left,k2.Y-FTargetRect.Top);
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
          Polygon.DrawFill(FTargetBmp, color2);
        end;
        with Polygon.Outline do try
          with Grow(Fixed(linew / 2), 0.5) do try
            FillMode := pfWinding;
            DrawFill(FTargetBmp, color1);
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

procedure TMapMarksBitmapLayerProviderStupedThreaded.SyncGetBitmap;
var
  LLRect: TExtendedRect;
  xy,xyb:Tpoint;
  btm:TCustomBitmap32;
  dLL:TExtendedPoint;
  TestArrLenLonLatRect: TExtendedRect;
  TestArrLenPixelRect: TRect;
  buf_line_arr:TExtendedPointArray;
  indexmi:integer;
  imw,texth:integer;
  marksFilter:string;

  VIconSource: TCustomBitmap32;
begin
  if (GState.show_point = mshNone)or(FMain.CDSmarks.State <> dsBrowse) then exit;
  try
    LLRect := FConverter.PixelRect2LonLatRect(FTargetRect, FTargetZoom);
    marksFilter:='';
    if GState.show_point = mshChecked then begin
      FMain.CDSKategory.Filter:='visible = 1 and ( AfterScale <= '+inttostr(FTargetZoom + 1)+' and BeforeScale >= '+inttostr(FTargetZoom + 1)+' )';
      FMain.CDSKategory.Filtered:=true;
      marksFilter:=marksFilter+'visible=1';
      FMain.CDSKategory.First;
      if FMain.CDSKategory.Eof then begin
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
    dLL:=ExtPoint((LLRect.Right-LLRect.Left)/2,(LLRect.Top-LLRect.Bottom)/2);
    marksFilter:=marksFilter+'('+
      ' LonR>'+floattostr(LLRect.Left-dLL.x)+' and'+
      ' LonL<'+floattostr(LLRect.Right+dLL.x)+' and'+
      ' LatB<'+floattostr(LLRect.Top+dLL.y)+' and'+
      ' LatT>'+floattostr(LLRect.Bottom-dLL.y)+
    ')';
    FMain.CDSmarks.Filter:=marksFilter;
    FMain.CDSmarks.Filtered:=true;
    FMain.CDSmarks.First;
    if FMain.CDSmarks.Eof then begin
      FMain.CDSmarks.Filtered:=false;
      exit;
    end;
//TODO: Сделать вывод подписей для меток.
//    BtmEx.Font.Name:='Tahoma';
//    BtmEx.Font.Style:=[];
    btm:=TCustomBitmap32.Create;
    try
      btm.DrawMode:=dmBlend;
      btm.Resampler:=TLinearResampler.Create;
      While not(FMain.CDSmarks.Eof) do begin
        buf_line_arr := Blob2ExtArr(FMain.CDSmarks.FieldByName('lonlatarr'));
        if length(buf_line_arr)>1 then begin
          TestArrLenLonLatRect.Left := FMain.CDSmarksLonL.AsFloat;
          TestArrLenLonLatRect.Top := FMain.CDSmarksLatT.AsFloat;
          TestArrLenLonLatRect.Right := FMain.CDSmarksLonR.AsFloat;
          TestArrLenLonLatRect.Bottom := FMain.CDSmarksLatB.AsFloat;
          FConverter.CheckLonLatRect(TestArrLenLonLatRect);
          TestArrLenPixelRect := FConverter.LonLatRect2PixelRect(TestArrLenLonLatRect, FTargetZoom);
          if (abs(TestArrLenPixelRect.Left-TestArrLenPixelRect.Right)>FMain.CDSmarksScale1.AsInteger+2)or(abs(TestArrLenPixelRect.Top-TestArrLenPixelRect.Bottom)>FMain.CDSmarksScale1.AsInteger+2) then begin
            drawPath2Bitmap(buf_line_arr,TColor32(Fmain.CDSmarksColor1.AsInteger),TColor32(Fmain.CDSmarksColor2.AsInteger),Fmain.CDSmarksScale1.asInteger,
              (buf_line_arr[0].x=buf_line_arr[length(buf_line_arr)-1].x)and(buf_line_arr[0].y=buf_line_arr[length(buf_line_arr)-1].y));
            SetLength(buf_line_arr,0);
          end;
        end;
        if length(buf_line_arr)=1 then begin
          xy:=FConverter.LonLat2PixelPos(buf_line_arr[0],FTargetZoom);
          xyb:=FTargetRect.TopLeft;
          xy:=Point(xy.x - xyb.x,xy.y - xyb.y);
          imw:=FMain.CDSmarks.FieldByName('Scale2').AsInteger;
          indexmi:=GState.MarkIcons.IndexOf(FMain.CDSmarks.FieldByName('picname').AsString);
          if(indexmi=-1)and(GState.MarkIcons.Count>0) then begin
            indexmi:=0;
          end;
          if(indexmi>-1)then begin
            VIconSource := TCustomBitmap32(GState.MarkIcons.Objects[indexmi]);
            btm.SetSize(VIconSource.Width, VIconSource.Height);
            btm.Draw(0, 0, VIconSource);
            FTargetBmp.Draw(bounds(xy.x-(imw div 2),xy.y-imw,imw,imw),bounds(0,0,btm.Width,btm.Height), btm);
          end;
          if FMain.CDSmarks.FieldByName('Scale1').AsInteger>0 then begin
//TODO: Сделать вывод подписей для меток.
//            BtmEx.Font.Size:=FMain.CDSmarksScale1.AsInteger;
//            texth:=BtmEx.TextHeight(FMain.CDSmarksname.asString) div 2;
//            BtmEx.RenderText(xy.x+(imw div 2)+2,xy.y-(imw div 2)-texth+1,FMain.CDSmarksname.AsString,1,TColor32(FMain.CDSmarksColor2.AsInteger));
//            BtmEx.RenderText(xy.x+(imw div 2)+1,xy.y-(imw div 2)-texth,FMain.CDSmarksname.AsString,1,TColor32(FMain.CDSmarksColor1.AsInteger));
          end;
        end;
        FMain.CDSmarks.Next;
      end;
      FMain.CDSmarks.Filtered:=false;
    finally
      btm.Free;
    end;
  finally
  end;
end;

{ TMapMarksBitmapLayerProviderStuped }

procedure TMapMarksBitmapLayerProviderStuped.GetBitmapRect(
  ATargetBmp: TCustomBitmap32; AConverter: ICoordConverter;
  ATargetRect: TRect; ATargetZoom: Byte);
var
  VWorker: TMapMarksBitmapLayerProviderStupedThreaded;
begin
  VWorker := TMapMarksBitmapLayerProviderStupedThreaded.Create(
    ATargetBmp, AConverter, ATargetRect, ATargetZoom);
  try
    TThread.Synchronize(nil, VWorker.SyncGetBitmap);
  finally
    VWorker.Free;
  end;
end;

end.
