unit UKMLmap;
                                  
interface
uses GR32,UgeoFun,graphics,math,UKmlParse,controls,inifiles, UKMLExplorer, UResStrings;
type
  TKMLLayer = class
   public
    name_blok:string;
    id:string;
    KMLFileName:string;
    num_blok:string;
    description:string;
    coordinates:array of TExtendedPoint;
    coordinatesLT:TExtendedPoint;
    coordinatesRD:TExtendedPoint;
    LT,RD:Tpoint;
    AarrKt:array of TPoint;
    visiblelayer:boolean;
    procedure SaveToIni;
    procedure Show_;
  end;

  TKMLFile = class(TObject)
    FileName:string;
    Name:string;
    visible:boolean;
    KMLLayer:array of TKMLLayer;
    procedure SaveToIni;
   end;

var
    KMLFile:array of TKMLFile;
    old_x,old_y:integer;
    kml:TKML;
    procedure HideKML;
    procedure ShowKML;
    procedure loadKML;
    procedure MouseOnRegKML(var PWL:TResObj;xy:TPoint);

implementation
uses unit1, SysUtils, StrUtils;

procedure MouseOnRegKML(var PWL:TResObj;xy:TPoint);
var i,j,k,DX,ShUch,DY,INCX,INCY,dist,Xerr,Yerr,t,sx,sy,ex,ey:integer;
    lastS:extended;
    l:integer;
begin
 for j:=0 to length(KMLFile)-1 do
  for i:=1 to length(KMLFile[j].KMLLayer) do
   if (KMLshow)and(KMLFile[j].visible)and(KMLFile[j].KMLLayer[i-1].visiblelayer)and
      (xy.x>KMLFile[j].KMLLayer[i-1].lt.X-3)and(xy.x<KMLFile[j].KMLLayer[i-1].rd.X+3)and
      (xy.y>KMLFile[j].KMLLayer[i-1].lt.Y-3)and(xy.y<KMLFile[j].KMLLayer[i-1].rd.Y+3) then
   begin
    if length(KMLFile[j].KMLLayer[i-1].AarrKt)=1 then
     begin
      PWL.name:=KMLFile[j].KMLLayer[i-1].name_blok;
      PWL.descr:=KMLFile[j].KMLLayer[i-1].description;
      PWL.numid:=KMLFile[j].KMLLayer[i-1].num_blok;
      PWL.find:=true;
      exit;
     end;
    l:=length(KMLFile[j].KMLLayer[i-1].AarrKt)-1;
    if l<0 then continue;
    k:=1;
    if (KMLFile[j].KMLLayer[i-1].AarrKt[0].X<>KMLFile[j].KMLLayer[i-1].AarrKt[l].x)or
       (KMLFile[j].KMLLayer[i-1].AarrKt[0].y<>KMLFile[j].KMLLayer[i-1].AarrKt[l].y)then
      while (k<length(KMLFile[j].KMLLayer[i-1].AarrKt)) do
       begin
        if CursorOnLinie(xy.x, xy.Y, KMLFile[j].KMLLayer[i-1].AarrKt[k-1].x, KMLFile[j].KMLLayer[i-1].AarrKt[k-1].y,
                         KMLFile[j].KMLLayer[i-1].AarrKt[k].x, KMLFile[j].KMLLayer[i-1].AarrKt[k].y, 3)
           then begin
                 PWL.name:=KMLFile[j].KMLLayer[i-1].name_blok;
                 PWL.descr:=KMLFile[j].KMLLayer[i-1].description;
                 PWL.numid:=KMLFile[j].KMLLayer[i-1].num_blok;
                 PWL.find:=true;
                 exit;
                end;
        inc(k);
       end
     else
     if PtInRgn(KMLFile[j].KMLLayer[i-1].AarrKt,xy) then
      begin
       if (PolygonSquare(KMLFile[j].KMLLayer[i-1].AarrKt)>PWL.S)and(PWL.S<>0)
        then continue;
       PWL.S:=PolygonSquare(KMLFile[j].KMLLayer[i-1].AarrKt);
       PWL.name:=KMLFile[j].KMLLayer[i-1].name_blok;
       PWL.descr:=KMLFile[j].KMLLayer[i-1].description;
       PWL.numid:=KMLFile[j].KMLLayer[i-1].num_blok;
       PWL.descr:=PWL.descr+'<BR>'+SAS_STR_S+': '+RoundEx(CalcS2(KMLFile[j].KMLLayer[i-1].coordinates,sat_map_both),2)+' '+SAS_UNITS_km2; //Fmain.R2ShortStr(CalcS(poly,sat_map_both),4,' '+SAS_UNITS_km+'.',' '+SAS_UNITS_m);
       PWL.find:=true;
      end
 end;
end;

function R2StrPoint(r:extended):string;
begin
 result:=floattostr(r);
 if posex(',',result,1)>0 then result[posex(',',result,1)]:='.';
end;

procedure HideKML;
begin
 LayerMapWiki.Visible:=false;
{ for j:=0 to length(KMLFile)-1 do
  begin
   len:=length(KMLFile[j].KMLLayer);
   for i:=1 to len do
    begin
     KMLFile[j].KMLLayer[i-1].Visible:=false;
     KMLFile[j].KMLLayer[i-1].SendToBack;
    end;
  end;  }
end;

procedure ShowKML;
var i,j,len:integer;
begin
 LayerMapWiki.Visible:=true;
 for j:=0 to length(KMLFile)-1 do
  begin
   len:=length(KMLFile[j].KMLLayer);
   for i:=1 to len do
    if KMLFile[j].visible then KMLFile[j].KMLLayer[i-1].Show_
  end;
end;

procedure TKMLLayer.SaveToIni;
var Ini: Tinifile;
begin
 Ini:=TiniFile.Create(GetfullKMLPath+'kml.ini');
 ini.WriteBool(KMLFileName,id,visiblelayer);
 ini.Free;
end;

procedure TKMLFile.SaveToIni;
var Ini: Tinifile;
begin
 Ini:=TiniFile.Create(GetfullKMLPath+'kml.ini');
 ini.WriteBool(FileName,'visible',visible);
 ini.Free;
end;

procedure loadKML;
var path:string;
    i,ii,KMLLen,KMLFileLen:integer;
    SearchRec: TSearchRec;
    Ini: Tinifile;
begin
 fmain.map.BeginUpdate;
 Ini:=TiniFile.Create(GetfullKMLPath+'kml.ini');
 KMLFile:=nil;
 Fmain.createdirif(GetfullKMLPath);
 if FindFirst(GetfullKMLPath+'*.kml',faAnyFile, SearchRec) = 0 then
   repeat
    KMLFileLen:=length(KMLFile);
    setLength(KMLFile,KMLFileLen+1);
    KMLFile[KMLFileLen]:=TKMLFile.Create;
    path:=GetfullKMLPath+SearchRec.Name;
    KMLFile[KMLFileLen].FileName:=SearchRec.Name;
    KMLFile[KMLFileLen].Visible:=ini.ReadBool(KMLFile[KMLFileLen].FileName,'visible',true);
    KML:=TKML.Create;
    KMLFile[KMLFileLen].KMLLayer:=nil;
    if kml.loadFromFile(path)
     then for ii:=0 to length(KML.Data)-1 do
           begin
            KMLLen:=length(KMLFile[KMLFileLen].KMLLayer);
            setLength(KMLFile[KMLFileLen].KMLLayer,KMLLen+1);
            KMLFile[KMLFileLen].KMLLayer[KMLLen]:=TKMLLayer.Create;
            KMLFile[KMLFileLen].KMLLayer[KMLLen].KMLFileName:=KMLFile[KMLFileLen].FileName;
            KMLFile[KMLFileLen].KMLLayer[KMLLen].name_blok:=KML.Data[ii].Name;
            KMLFile[KMLFileLen].KMLLayer[KMLLen].description:=KML.Data[ii].description;
            KMLFile[KMLFileLen].KMLLayer[KMLLen].num_blok:=KML.Data[ii].PlacemarkID;
            SetLength(KMLFile[KMLFileLen].KMLLayer[KMLLen].coordinates,length(KML.Data[ii].coordinates));
            for i:=0 to length(KML.Data[ii].coordinates)-1 do
              KMLFile[KMLFileLen].KMLLayer[KMLLen].coordinates[i]:=KML.Data[ii].coordinates[i];
            KMLFile[KMLFileLen].KMLLayer[KMLLen].coordinatesLT:=KML.Data[ii].coordinatesLT;
            KMLFile[KMLFileLen].KMLLayer[KMLLen].coordinatesRD:=KML.Data[ii].coordinatesRD;
            KMLFile[KMLFileLen].KMLLayer[KMLLen].id:=KMLFile[KMLFileLen].KMLLayer[KMLLen].name_blok+'_'+
              inttostr(round(KMLFile[KMLFileLen].KMLLayer[KMLLen].coordinatesLT.X))+'_'+
              inttostr(round(KMLFile[KMLFileLen].KMLLayer[KMLLen].coordinatesLT.Y))+'_'+
              inttostr(round(KMLFile[KMLFileLen].KMLLayer[KMLLen].coordinatesRD.X))+'_'+
              inttostr(round(KMLFile[KMLFileLen].KMLLayer[KMLLen].coordinatesRD.Y));
            KMLFile[KMLFileLen].KMLLayer[KMLLen].visiblelayer:=ini.ReadBool(KMLFile[KMLFileLen].FileName,
                  KMLFile[KMLFileLen].KMLLayer[KMLLen].id,true);
            //KMLFile[KMLFileLen].KMLLayer[KMLLen].show_;
           end
     else ;
    KML.Free;
   until FindNext(SearchRec) <> 0;
 Ini.Free;
 ShowKML;
 Fmain.map.EndUpdate;
 Fmain.map.Refresh;   
end;

procedure TKMLLayer.Show_;
var i:integer;
begin
 RD.x:=Fmain.Lon2X(coordinatesRD.x);
 LT.X:=Fmain.Lon2X(coordinatesLT.X);
 LT.Y:=Fmain.Lat2y(coordinatesLT.Y);
 RD.Y:=Fmain.Lat2y(coordinatesRD.Y);
 if (length(coordinates)=1) then
  begin
   LT:=Point(LT.X-4,LT.Y-4);
   RD:=Point(RD.X+4,RD.Y+4);
  end;
 if(((RD.x-LT.x)<=1)and((RD.y-LT.y)<=1)or
   ((LT.y>Fmain.map.Height)or(RD.y<0)or(LT.x>Fmain.map.Width)or(RD.x<0)))or(visiblelayer=false)  then
     begin
      LT.X:=LT.X+(pr_x-mWd2);
      RD.x:=RD.x+(pr_x-mWd2);
      LT.Y:=LT.Y+(pr_y-mHd2);
      RD.Y:=RD.Y+(pr_y-mHd2);
      exit;
     end;
 LT.X:=LT.X+(pr_x-mWd2);
 RD.x:=RD.x+(pr_x-mWd2);
 LT.Y:=LT.Y+(pr_y-mHd2);
 RD.Y:=RD.Y+(pr_y-mHd2);
 if RD.X=LT.X then RD.X:=RD.X+1;
 if RD.Y=LT.Y then RD.Y:=RD.Y+1;
 setLength(AarrKt,length(coordinates));
 for i:=0 to length(coordinates)-1 do
  begin
   AarrKt[i].X:=Fmain.Lon2X(coordinates[i].X)+(pr_x-mWd2);
   AarrKt[i].Y:=Fmain.Lat2Y(coordinates[i].Y)+(pr_y-mHd2);
  end;
 LayerMapWiki.Bitmap.Canvas.Pen.Color:=$101010;
 LayerMapWiki.Bitmap.Canvas.Brush.Color:=clWhite;
 LayerMapWiki.Bitmap.Canvas.Pen.Width:=3;
 if (length(AarrKt)>1)and(((RD.x-LT.x)>5)or((RD.y-LT.y)>5))
  then LayerMapWiki.Bitmap.Canvas.Polyline(AarrKt)
  else begin
        LayerMapWiki.Bitmap.Canvas.Brush.Color:=clBlue;
        LayerMapWiki.Bitmap.Canvas.Ellipse(AarrKt[0].X-2,AarrKt[0].Y-2,AarrKt[0].X+2,AarrKt[0].Y+2);
       end;
 LayerMapWiki.Bitmap.Canvas.Pen.Color:=clWhite;
 LayerMapWiki.Bitmap.Canvas.Brush.Color:=clWhite;
 LayerMapWiki.Bitmap.Canvas.Pen.Width:=1;
 if (length(AarrKt)>1)and(((RD.x-LT.x)>5)or((RD.y-LT.y)>5))
 then LayerMapWiki.Bitmap.Canvas.Polyline(AarrKt)
 else begin
       LayerMapWiki.Bitmap.Canvas.Brush.Color:=clBlue;
       LayerMapWiki.Bitmap.Canvas.Ellipse(AarrKt[0].X-2,AarrKt[0].Y-2,AarrKt[0].X+2,AarrKt[0].Y+2);
      end;
end;

end.
