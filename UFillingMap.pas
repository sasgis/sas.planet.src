unit UFillingMap;

interface
uses Windows,Forms,SysUtils,Classes,UMapType,UImgFun,UGeoFun,unit4,UResStrings,
     GR32,GR32_Layers,GR32_Resamplers,math,Graphics,Dialogs;

type
  TFillingMap = class(TThread)
    LayerMap:TBitmapLayer;
    done:boolean;
    needRepaint:boolean;
    stop:boolean;
  private
    DrawPos:TPoint;
    ClMZ:TColor32;
  protected
    dZoom:Byte;
    y_draw,x_draw:longint;
    Ahg_x,Ahg_y,Apr_x,Apr_y,ppaprx,ppapry:integer;
    d2562,x2:integer;
    procedure DrawPixel;
    procedure SetupLayer;
    procedure UpdateLayer;
    //procedure UpdateLayerPos;
    procedure Execute; override;
  public
    destructor destroy; override;
    constructor Create(CrSusp:Boolean);
  end;

implementation
uses unit1,USaveas;

constructor TFillingMap.Create(CrSusp:Boolean);
begin
 done:=false;
 LayerMap:=TBitmapLayer.Create(FMain.map.Layers);
 LayerMap.bitmap.DrawMode:=dmBlend;
 needRepaint:=false;
 inherited Create(CrSusp);
end;

destructor TFillingMap.destroy;
begin
// Synchronize(ClearBtmp);
 //terminate;
 //WaitFor;
 LayerMap.Free;
 inherited ;
end;

procedure TFillingMap.DrawPixel;
begin
 LayerMap.Bitmap.PixelS[DrawPos.x,DrawPos.y]:=clMZ;
end;

procedure TFillingMap.UpdateLayer;
begin
  LayerMap.Update;
end;

procedure TFillingMap.SetupLayer;
begin
 LayerMap.bitmap.Clear(clBlack);
 LayerMap.Bitmap.Width:=xhgpx;
 LayerMap.Bitmap.Height:=yhgpx;
 LayerMap.Location:=Unit1.LayerMap.Location;
 LayerMap.Visible:=true;
 dZoom:=zoom_mapzap-zoom_size;
 x2:=trunc(power(2,dZoom));
 ClMZ:=SetAlpha(Color32(MapZapColor),MapZapAlpha);
 d2562:=256 shr dZoom;
 Ahg_x:=(FMain.map.Width div d2562)+1;
 Ahg_y:=(FMain.map.Height div d2562)+1;
{ LayerMap.Bitmap.Width:=(xhgpx div d2562)+1;
 LayerMap.Bitmap.Height:=(yhgpx div d2562)+1;
 LayerMap.bitmap.Clear(clBlack);
 LayerMap.Location:=Unit1.LayerMap.Location;  }
 Apr_x:=(d2562*Ahg_x)div 2;
 Apr_y:=(d2562*Ahg_y)div 2;
 x_draw:=((d2562+((pos.x-Apr_x)mod d2562))mod d2562)-((pr_x-Apr_x));
 y_draw:=((d2562+((pos.y-Apr_y)mod d2562))mod d2562)-((pr_y-Apr_y));
 ppaprx:=pos.x-Apr_x;
 ppapry:=pos.y-Apr_y;
end;

procedure TFillingMap.Execute;
var Path:String;
    fn,fo:string;
    bo,bb:boolean;
    i,j,ii,jj:integer;
    imd256x,imd256y,xx,yy,x1,y1:longint;
begin
 repeat
 bo:=true;
 Synchronize(SetupLayer);
 ppaprx:=ppaprx*x2;
 ppapry:=ppapry*x2;
 imd256x:=0;

{ bb:=false;
 for i:=0 to Ahg_x do
  begin
   xx:=ppaprx+(imd256x shl dZoom);
   if (Terminated)or(needRepaint)or(stop)or(xx<0)or(xx>=zoom[zoom_mapzap]) then continue;
   imd256y:=0;
   for j:=0 to Ahg_y do
    begin
     yy:=ppapry+(imd256y shl dZoom);
     if (Terminated)or(needRepaint)or(stop)or(yy<0)or(yy>=zoom[zoom_mapzap]) then continue;
     Path:=ffpath(xx,yy,zoom_mapzap,sat_map_both,false);
     fn:=ExtractFilePath(path);
     if not(bb)
      then begin
            x1:=imd256x-x_draw; y1:=imd256y-y_draw;
            //ii:=i-(x_draw div d2562);jj:=j-(y_draw div d2562);
            for ii:=x1 to x1+(d2562-1) do
             for jj:=y1 to y1+(d2562-1) do
               LayerMap.Bitmap.PixelS[ii,jj]:=clMZ;
           end;
     inc(imd256y,d2562)
    end;
   if ((i+1) mod 25 = 0 ) then
    begin
     Synchronize(UpdateLayer);
    end;
   inc(imd256x,d2562)
  end;    }
 for i:=0 to Ahg_x do
  begin
   xx:=ppaprx+(imd256x shl dZoom);
   if (Terminated)or(needRepaint)or(stop)or(xx<0)or(xx>=zoom[zoom_mapzap]) then
    begin
     inc(imd256x,d2562);
     continue;
    end;
   imd256y:=0;
   for j:=0 to Ahg_y do
    begin
     yy:=ppapry+(imd256y shl dZoom);
     if (Terminated)or(needRepaint)or(stop)or(yy<0)or(yy>=zoom[zoom_mapzap]) then
      begin
       inc(imd256y,d2562);
       continue;
      end;
     Path:=ffpath(xx,yy,zoom_mapzap,sat_map_both^,false);
     fn:=ExtractFilePath(path);
     if fn=fo then if bo then bb:=TileExists(path)
                         else bb:=false
              else begin
                    bo:=DirectoryExists(fn);
                    if bo then bb:=TileExists(path)
                          else bb:=false;
                   end;
     fo:=fn;
     if not(bb)
      then begin
            x1:=imd256x-x_draw; y1:=imd256y-y_draw;
            //ii:=i-(x_draw div d2562);jj:=j-(y_draw div d2562);
            for ii:=x1 to x1+(d2562-1) do
             for jj:=y1 to y1+(d2562-1) do
               LayerMap.Bitmap.PixelS[ii,jj]:=clMZ;
           end;
     inc(imd256y,d2562)
    end;
   if ((i+1) mod 30 = 0 ) then
    begin
     Synchronize(UpdateLayer);
    end;
   inc(imd256x,d2562)
  end;
 Synchronize(UpdateLayer);
 if (stop) then Suspend;
 while (not Terminated)and(not needRepaint) do
  begin
   sleep(1);
  end;
 needRepaint:=false;
 until terminated;
end;

end.
