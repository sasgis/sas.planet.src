unit UWikiLayer;

interface

uses
  Classes,
  GR32,
  UgeoFun,
  UKmlParse,
  UMapType,
  t_GeoTypes;

type
  TWikiLayer = class
   public
    name_blok:string;
    num_blok:string;
    description:string;
    LT,RD:Tpoint;
    AarrKt:TPointArray;
  end;
  PWikiLayer = ^TWikilayer;
var WikiLayer:array of TWikiLayer;


    procedure destroyWL;
    procedure addWL(name,descript,num:string;coordinatesLT,coordinatesRD:TExtendedPoint;coordinates:  TExtendedPointArray);
    procedure loadWL(Alayer: TMapType);
    procedure MouseOnReg(var PWL:TResObj;xy:TPoint);

implementation

uses
  SysUtils,
  StrUtils,
  u_GlobalState,
  unit1;

procedure MouseOnReg(var PWL:TResObj;xy:TPoint);
var i,j:integer;
    l:integer;
begin
 for i:=0 to length(Wikilayer)-1 do
   if (xy.x>Wikilayer[i].lt.X-5)and(xy.x<Wikilayer[i].rd.X+5)and
      (xy.y>Wikilayer[i].lt.Y-5)and(xy.y<Wikilayer[i].rd.Y+5) then
   begin
    if length(Wikilayer[i].AarrKt)=1 then
     begin
      PWL.name:=Wikilayer[i].name_blok;
      PWL.descr:=Wikilayer[i].description;
      PWL.numid:=Wikilayer[i].num_blok;
      PWL.find:=true;
      exit;
     end;
    l:=length(Wikilayer[i].AarrKt)-1;
    if l<0 then continue;
    j:=1;
    if (Wikilayer[i].AarrKt[0].X<>Wikilayer[i].AarrKt[l].x)or
       (Wikilayer[i].AarrKt[0].y<>Wikilayer[i].AarrKt[l].y)then
      while (j<length(Wikilayer[i].AarrKt)) do
       begin
        if CursorOnLinie(xy.x, xy.Y, Wikilayer[i].AarrKt[j-1].x, Wikilayer[i].AarrKt[j-1].y,
                         Wikilayer[i].AarrKt[j].x, Wikilayer[i].AarrKt[j].y, 3)
           then begin
                 PWL.name:=Wikilayer[i].name_blok;
                 PWL.descr:=Wikilayer[i].description;
                 PWL.numid:=Wikilayer[i].num_blok;
                 PWL.find:=true;
                 exit;
                end;
        inc(j);
       end
     else
     if PtInRgn(Wikilayer[i].AarrKt,xy) then
      begin
       if (PolygonSquare(Wikilayer[i].AarrKt)>PWL.S)and(PWL.S<>0)
        then continue;
       PWL.S:=PolygonSquare(Wikilayer[i].AarrKt);
       PWL.name:=Wikilayer[i].name_blok;
       PWL.descr:=Wikilayer[i].description;
       PWL.numid:=Wikilayer[i].num_blok;
       PWL.find:=true;
      end
 end;
end;

procedure destroyWL;
var i:integer;
begin
 for i:=0 to length(Wikilayer)-1 do
  begin
   Wikilayer[i].Free;
   Wikilayer[i]:=nil;
  end;
 SetLength(WikiLayer,0);
 FMain.LayerMapWiki.Visible:=false;
end;

procedure loadWL(Alayer: TMapType);
var
    Ax,Ay,i,j,ii,Azoom:integer;
    APos:TPoint;
    kml:TKML;
    VSizeInTile: TPoint;
begin
 FMain.LayerMapWiki.Visible:=true;
 VSizeInTile := Fmain.LoadedSizeInTile;
 for i:=0 to VSizeInTile.X do
  for j:=0 to VSizeInTile.Y do
   begin
    Azoom:=GState.zoom_size;
    APos := GState.sat_map_both.GeoConvert.Pos2OtherMap(FMain.ScreenCenterPos, (Azoom - 1) + 8, Alayer.GeoConvert);
    if GState.CiclMap then Ax:=Fmain.X2AbsX(APos.X-pr_x+(i shl 8),GState.zoom_size)
               else Ax:=APos.X-pr_x+(i shl 8);
    Ay:=APos.y-pr_y+(j shl 8);
    KML:=TKML.Create;
    if Alayer.LoadTile(kml, Ax,Ay,Azoom, false) then
     for ii:=0 to length(KML.Data)-1 do
      addWL(KML.Data[ii].Name,KML.Data[ii].description,KML.Data[ii].PlacemarkID,KML.Data[ii].coordinatesLT,KML.Data[ii].coordinatesRD,KML.Data[ii].coordinates);
    KML.Free;
   end;
end;

procedure addWL(name,descript,num:string;coordinatesLT,coordinatesRD:TExtendedPoint;coordinates:  TExtendedPointArray);
var i,lenLay:integer;
begin
 Delete(descript,posEx('#ge',descript,0),1);
 setLength(WikiLayer,length(WikiLayer)+1);
 lenLay:=length(WikiLayer);
 WikiLayer[lenLay-1]:=TWikiLayer.Create;
 With WikiLayer[lenLay-1] do
  begin
   GState.sat_map_both.GeoConvert.CheckLonLatPos(coordinatesLT);
   LT:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(coordinatesLT,GState.zoom_size-1);
   LT := Fmain.MapPixel2LoadedPixel(LT);
   GState.sat_map_both.GeoConvert.CheckLonLatPos(coordinatesRD);
   RD:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(coordinatesRD,GState.zoom_size-1);
   RD := Fmain.MapPixel2LoadedPixel(RD);
   if coordinatesLT.X=coordinatesRD.x then begin
     LT.X:=LT.X-3;
     RD.x:=RD.x+3;
   end;
   if coordinatesLT.y=coordinatesRD.y then begin
     LT.Y:=LT.Y-3;
     RD.Y:=RD.Y+3;
   end;
   if(((RD.x-LT.x)<=1)or((RD.y-LT.y)<=1)or
     ((LT.y>(pr_y + mHd2))or(RD.y<(pr_y-mHd2))or(LT.x>(pr_x + mWd2))or(RD.x<(pr_x-mWd2)))) then begin
     exit;
   end;
   name_blok:=name;
   num_blok:=num;
   description:=descript;
   setLength(AarrKt,length(coordinates));
   if length(coordinates)=1 then
    begin
     setLength(AarrKt,5);
     GState.sat_map_both.GeoConvert.CheckLonLatPos(coordinates[0]);
     AarrKt[0]:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(coordinates[0],GState.zoom_size-1);
     AarrKt[0] := Fmain.MapPixel2LoadedPixel(AarrKt[0]);
     AarrKt[1]:=Point(AarrKt[0].x+2,AarrKt[0].y-2);
     AarrKt[2]:=Point(AarrKt[0].x+2,AarrKt[0].y+2);
     AarrKt[3]:=Point(AarrKt[0].x-2,AarrKt[0].y+2);
     AarrKt[4]:=Point(AarrKt[0].x-2,AarrKt[0].y-2);
     AarrKt[0]:=Point(AarrKt[0].x-2,AarrKt[0].y-2);
    end
   else
   for i:=0 to length(coordinates)-1 do begin
     GState.sat_map_both.GeoConvert.CheckLonLatPos(coordinates[i]);
     AarrKt[i]:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(coordinates[i],GState.zoom_size-1);
     AarrKt[i]:=Fmain.MapPixel2LoadedPixel(AarrKt[i]);
   end;
   FMain.LayerMapWiki.Bitmap.Canvas.Pen.Width:=3;
   FMain.LayerMapWiki.Bitmap.Canvas.Pen.Color:=GState.WikiMapFonColor;
   if length(coordinates)=1 then FMain.LayerMapWiki.Bitmap.Canvas.Ellipse(AarrKt[0].x,AarrKt[0].y,AarrKt[2].x,AarrKt[2].y)
                            else FMain.LayerMapWiki.Bitmap.Canvas.Polyline(AarrKt);
   FMain.LayerMapWiki.Bitmap.Canvas.Pen.Width:=1;
   FMain.LayerMapWiki.Bitmap.Canvas.Pen.Color:=GState.WikiMapMainColor;
   if length(coordinates)=1 then FMain.LayerMapWiki.Bitmap.Canvas.Ellipse(AarrKt[0].x,AarrKt[0].y,AarrKt[2].x,AarrKt[2].y)
                            else FMain.LayerMapWiki.Bitmap.Canvas.Polyline(AarrKt);
  end;
end;

end.
