unit UWikiLayer;

interface

uses
  Classes,
  GR32,
  UgeoFun,
  UMapType,
  t_GeoTypes,
  u_KmlInfoSimple;

type
  TWikiLayerElement = class
   public
    name_blok:string;
    num_blok:string;
    description:string;
    LT,RD:Tpoint;
    AarrKt:TPointArray;
    constructor Create;
    destructor Destroy; override;
  end;

  TWikiLayer = class
  private
    FWikiLayerElments: array of TWikiLayerElement;
    procedure addWL(name,descript,num:string;coordinatesLT,coordinatesRD:TExtendedPoint;coordinates:  TExtendedPointArray);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddFromLayer(Alayer: TMapType);
    procedure MouseOnReg(var PWL:TResObj;xy:TPoint);
  end;

implementation

uses
  SysUtils,
  StrUtils,
  i_ICoordConverter,
  u_GlobalState,
  unit1;

{ TWikiLayerElement }

constructor TWikiLayerElement.Create;
begin
  name_blok := '';
  num_blok := '';
  description := '';
  AarrKt := nil;
end;

destructor TWikiLayerElement.Destroy;
begin
  name_blok := '';
  num_blok := '';
  description := '';
  AarrKt := nil;
  inherited;
end;

{ TWikiLayer }

procedure TWikiLayer.AddFromLayer(Alayer: TMapType);
var
    Ax,Ay,i,j,ii,Azoom:integer;
    APos:TPoint;
    kml:TKmlInfoSimple;
    VSizeInTile: TPoint;
begin
 FMain.LayerMapWiki.Visible:=true;
 VSizeInTile := Fmain.LoadedSizeInTile;
 Fmain.LayerMapWiki.BeginUpdate;
 for i:=0 to VSizeInTile.X do
  for j:=0 to VSizeInTile.Y do
   begin
    Azoom:=GState.zoom_size;
    APos := GState.sat_map_both.GeoConvert.Pos2OtherMap(FMain.ScreenCenterPos, (Azoom - 1) + 8, Alayer.GeoConvert);
    if GState.CiclMap then Ax:=Fmain.X2AbsX(APos.X-pr_x+(i shl 8),GState.zoom_size)
               else Ax:=APos.X-pr_x+(i shl 8);
    Ay:=APos.y-pr_y+(j shl 8);
    KML:=TKmlInfoSimple.Create;
    try
      if Alayer.LoadTile(kml, Ax,Ay,Azoom, false) then
       for ii:=0 to length(KML.Data)-1 do
        addWL(KML.Data[ii].Name,KML.Data[ii].description,KML.Data[ii].PlacemarkID,KML.Data[ii].coordinatesLT,KML.Data[ii].coordinatesRD,KML.Data[ii].coordinates);
    finally
      KML.Free;
    end;
   end;
 Fmain.LayerMapWiki.EndUpdate;
end;

procedure TWikiLayer.Clear;
var
  i: integer;
begin
  for i := 0 to length(FWikiLayerElments)-1 do begin
    FreeAndNil(FWikiLayerElments[i]);
  end;
  FWikiLayerElments := nil;
  FMain.LayerMapWiki.Visible:=false;
end;

constructor TWikiLayer.Create;
begin
  FWikiLayerElments := nil;
end;

destructor TWikiLayer.Destroy;
begin
  Clear;
  FWikiLayerElments := nil;
  inherited;
end;

procedure TWikiLayer.MouseOnReg(var PWL: TResObj; xy: TPoint);
var i,j:integer;
    l:integer;
begin
 for i:=0 to length(FWikiLayerElments)-1 do
   if (xy.x>FWikiLayerElments[i].lt.X-5)and(xy.x<FWikiLayerElments[i].rd.X+5)and
      (xy.y>FWikiLayerElments[i].lt.Y-5)and(xy.y<FWikiLayerElments[i].rd.Y+5) then
   begin
    if length(FWikiLayerElments[i].AarrKt)=1 then
     begin
      PWL.name:=FWikiLayerElments[i].name_blok;
      PWL.descr:=FWikiLayerElments[i].description;
      PWL.numid:=FWikiLayerElments[i].num_blok;
      PWL.find:=true;
      exit;
     end;
    l:=length(FWikiLayerElments[i].AarrKt)-1;
    if l<0 then continue;
    j:=1;
    if (FWikiLayerElments[i].AarrKt[0].X<>FWikiLayerElments[i].AarrKt[l].x)or
       (FWikiLayerElments[i].AarrKt[0].y<>FWikiLayerElments[i].AarrKt[l].y)then
      while (j<length(FWikiLayerElments[i].AarrKt)) do
       begin
        if CursorOnLinie(xy.x, xy.Y, FWikiLayerElments[i].AarrKt[j-1].x, FWikiLayerElments[i].AarrKt[j-1].y,
                         FWikiLayerElments[i].AarrKt[j].x, FWikiLayerElments[i].AarrKt[j].y, 3)
           then begin
                 PWL.name:=FWikiLayerElments[i].name_blok;
                 PWL.descr:=FWikiLayerElments[i].description;
                 PWL.numid:=FWikiLayerElments[i].num_blok;
                 PWL.find:=true;
                 exit;
                end;
        inc(j);
       end
     else
     if PtInRgn(FWikiLayerElments[i].AarrKt,xy) then
      begin
       if (PolygonSquare(FWikiLayerElments[i].AarrKt)>PWL.S)and(PWL.S<>0)
        then continue;
       PWL.S:=PolygonSquare(FWikiLayerElments[i].AarrKt);
       PWL.name:=FWikiLayerElments[i].name_blok;
       PWL.descr:=FWikiLayerElments[i].description;
       PWL.numid:=FWikiLayerElments[i].num_blok;
       PWL.find:=true;
      end
 end;
end;

procedure TWikiLayer.addWL(name,descript,num:string;coordinatesLT,coordinatesRD:TExtendedPoint;coordinates:  TExtendedPointArray);
var
  i,lenLay:integer;
  VConverter: ICoordConverter;
begin
 VConverter := GState.sat_map_both.GeoConvert;
 Delete(descript,posEx('#ge',descript,0),1);
 setLength(FWikiLayerElments,length(FWikiLayerElments)+1);
 lenLay:=length(FWikiLayerElments);
 FWikiLayerElments[lenLay-1]:=TWikiLayerElement.Create;
 With FWikiLayerElments[lenLay-1] do
  begin
   VConverter.CheckLonLatPos(coordinatesLT);
   LT:=VConverter.LonLat2PixelPos(coordinatesLT,GState.zoom_size-1);
   LT := Fmain.MapPixel2LoadedPixel(LT);
   VConverter.CheckLonLatPos(coordinatesRD);
   RD:=VConverter.LonLat2PixelPos(coordinatesRD,GState.zoom_size-1);
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
     VConverter.CheckLonLatPos(coordinates[0]);
     AarrKt[0]:=VConverter.LonLat2PixelPos(coordinates[0],GState.zoom_size-1);
     AarrKt[0] := Fmain.MapPixel2LoadedPixel(AarrKt[0]);
     AarrKt[1]:=Point(AarrKt[0].x+2,AarrKt[0].y-2);
     AarrKt[2]:=Point(AarrKt[0].x+2,AarrKt[0].y+2);
     AarrKt[3]:=Point(AarrKt[0].x-2,AarrKt[0].y+2);
     AarrKt[4]:=Point(AarrKt[0].x-2,AarrKt[0].y-2);
     AarrKt[0]:=Point(AarrKt[0].x-2,AarrKt[0].y-2);
    end
   else
   for i:=0 to length(coordinates)-1 do begin
     VConverter.CheckLonLatPos(coordinates[i]);
     AarrKt[i]:=VConverter.LonLat2PixelPos(coordinates[i],GState.zoom_size-1);
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
