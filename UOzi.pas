unit UOzi;

interface

uses
  Types,
  SysUtils,
  classes,
  GR32,
  
  Ugeofun,
  UMapType;

type
  TPrType = (ptMap,ptTab,ptW,ptDat,ptKml);
  TPrTypeArray = array of TPrType;

procedure toOziMap(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);
procedure toTabMap(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);
procedure toWorldFiles(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);
procedure toPrj(fname:string;Atype:TMapType);
procedure toAuxXml(fname:string;Atype:TMapType);
procedure toDat(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);
procedure toKml(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);

implementation

uses
  unit1,
  u_GeoToStr,
  t_GeoTypes;

procedure toOziMap(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);
var f:TextFile;
    xy:TPoint;
    rad:real;
    lat,lon:array[1..3] of real;
    i:integer;
    lats,lons:array[1..3] of string;

    function fs(r:real):string;
    var i:integer;
    begin
     result:=floattostr(r);
     for i:=1 to length(result) do
      if result[i]=',' then begin result[i]:='.'; break; end;
     result:=copy(result,1,i+8);
    end;
begin
 assignfile(f,ChangeFileExt(fname,'.map'));
 rewrite(f);
 writeln(f,'OziExplorer Map Data File Version 2.2');
 writeln(f,'Created by SAS.Planet');
 writeln(f,ExtractFileName(fname));
 writeln(f,'1 ,Map Code,'+#13#10+'WGS 84,,   0.0000,   0.0000,WGS 84'+#13#10+'Reserved 1'+#13#10+
           'Reserved 2'+#13#10+'Magnetic Variation,,,E'+#13#10+'Map Projection,Mercator,PolyCal,No,AutoCalOnly,No,BSBUseWPX,No');

 lon[1]:=Atype.GeoConvert.Pos2LonLat(xy1,(Azoom - 1) + 8).X;
 lat[1]:=Atype.GeoConvert.Pos2LonLat(xy1,(Azoom - 1) + 8).Y;
 lon[3]:=Atype.GeoConvert.Pos2LonLat(xy2,(Azoom - 1) + 8).X;
 lat[3]:=Atype.GeoConvert.Pos2LonLat(xy2,(Azoom - 1) + 8).Y;
 lon[2]:=lon[3]-(lon[3]-lon[1])/2;
 xy.Y:=(xy2.y-((xy2.Y-xy1.Y)div 2));
 xy.X:=(xy2.x-((xy2.x-xy1.x)div 2));
 lat[2]:=Atype.GeoConvert.Pos2LonLat(xy,(Azoom - 1) + 8).Y;

 for i:=1 to 3 do
  begin
   lons[i]:=inttostr(trunc(abs(lon[i])))+', '+fs(Frac(abs(lon[i]))*60);
   if lon[i]<0 then lons[i]:=lons[i]+',W'
               else lons[i]:=lons[i]+',E';
  end;
 for i:=1 to 3 do
  begin
   lats[i]:=inttostr(trunc(abs(lat[i])))+', '+fs(Frac(abs(lat[i]))*60);
   if lat[i]<0 then lats[i]:=lats[i]+',S'
               else lats[i]:=lats[i]+',N';
  end;

 xy2:=Point(xy2.X-xy1.X,xy2.y-xy1.y);
 xy1:=Point(0,0);

 writeln(f,'Point01,xy,    '+inttostr(xy1.x)+', '+inttostr(xy1.y)+',in, deg, '+lats[1]+', '+lons[1]+', grid,   ,           ,           ,N');
 writeln(f,'Point02,xy,    '+inttostr(xy2.x)+', '+inttostr(xy2.y)+',in, deg, '+lats[3]+', '+lons[3]+', grid,   ,           ,           ,N');
 writeln(f,'Point03,xy,    '+inttostr(xy1.x)+', '+inttostr(xy2.y)+',in, deg, '+lats[3]+', '+lons[1]+', grid,   ,           ,           ,N');
 writeln(f,'Point04,xy,    '+inttostr(xy2.x)+', '+inttostr(xy1.y)+',in, deg, '+lats[1]+', '+lons[3]+', grid,   ,           ,           ,N');
 writeln(f,'Point05,xy,    '+inttostr((xy2.x-xy1.X)div 2)+', '+inttostr((xy2.y-xy1.y)div 2)+',in, deg, '+lats[2]+', '+lons[2]+', grid,   ,           ,           ,N');
 writeln(f,'Point06,xy,    '+inttostr((xy2.x-xy1.X)div 2)+', '+inttostr(xy1.y)+',in, deg, '+lats[1]+', '+lons[2]+', grid,   ,           ,           ,N');
 writeln(f,'Point07,xy,    '+inttostr(xy1.x)+', '+inttostr((xy2.y-xy1.y)div 2)+',in, deg, '+lats[2]+', '+lons[1]+', grid,   ,           ,           ,N');
 writeln(f,'Point08,xy,    '+inttostr(xy2.x)+', '+inttostr((xy2.y-xy1.y)div 2)+',in, deg, '+lats[2]+', '+lons[3]+', grid,   ,           ,           ,N');
 writeln(f,'Point09,xy,    '+inttostr((xy2.x-xy1.X)div 2)+', '+inttostr(xy2.y)+',in, deg, '+lats[3]+', '+lons[2]+', grid,   ,           ,           ,N');
 for i:=10 to 30 do
  writeln(f,'Point'+inttostr(i)+',xy,     ,     ,in, deg,    ,        ,N,    ,        ,W, grid,   ,           ,           ,N');

 writeln(f,'Projection Setup,,,,,,,,,,'+#13#10+'Map Feature = MF ; Map Comment = MC     These follow if they exist'+#13#10+'Track File = TF      These follow if they exist'
           +#13#10+'Moving Map Parameters = MM?    These follow if they exist'+#13#10+'MM0,Yes'+#13#10+'MMPNUM,4');
 writeln(f,'MMPXY,1,'+inttostr(xy1.X)+','+inttostr(xy1.y));
 writeln(f,'MMPXY,2,'+inttostr(xy2.X)+','+inttostr(xy1.y));
 writeln(f,'MMPXY,3,'+inttostr(xy2.X)+','+inttostr(xy2.y));
 writeln(f,'MMPXY,4,'+inttostr(xy1.X)+','+inttostr(xy2.y));

 writeln(f,'MMPLL,1, '+fs(lon[1])+', '+fs(lat[1]));
 writeln(f,'MMPLL,2, '+fs(lon[3])+', '+fs(lat[1]));
 writeln(f,'MMPLL,3, '+fs(lon[3])+', '+fs(lat[3]));
 writeln(f,'MMPLL,4, '+fs(lon[1])+', '+fs(lat[3]));

 rad:=Atype.GeoConvert.GetSpheroidRadius;

 writeln(f,'MM1B,'+fs(1/((zoom[Azoom]/(2*PI))/(rad*cos(lat[2]*D2R)))));
 writeln(f,'MOP,Map Open Position,0,0');
 writeln(f,'IWH,Map Image Width/Height,'+inttostr(xy2.X)+','+inttostr(xy2.y));

 closefile(f);
end;

function GetProjByEPSG(ACode: Integer): string;
begin
  case ACode of
    3785: begin
      Result :=
        'PROJCS["Popular Visualisation CRS / Mercator",' + #13#10 +
        'GEOGCS["Popular Visualisation CRS",' + #13#10 +
        'DATUM["Popular_Visualisation_Datum",' + #13#10 +
        'SPHEROID["Popular Visualisation Sphere",6378137,0,' + #13#10 +
        'AUTHORITY["EPSG","7059"]],' + #13#10 +
        'TOWGS84[0,0,0,0,0,0,0],' + #13#10 +
        'AUTHORITY["EPSG","6055"]],' + #13#10 +
        'PRIMEM["Greenwich",0,' + #13#10 +
        'AUTHORITY["EPSG","8901"]],' + #13#10 +
        'UNIT["degree",0.01745329251994328,' + #13#10 +
        'AUTHORITY["EPSG","9122"]],' + #13#10 +
        'AUTHORITY["EPSG","4055"]],' + #13#10 +
        'UNIT["metre",1,' + #13#10 +
        'AUTHORITY["EPSG","9001"]],' + #13#10 +
        'PROJECTION["Mercator_1SP"],' + #13#10 +
        'PARAMETER["central_meridian",0],' + #13#10 +
        'PARAMETER["scale_factor",1],' + #13#10 +
        'PARAMETER["false_easting",0],' + #13#10 +
        'PARAMETER["false_northing",0],' + #13#10 +
        'AUTHORITY["EPSG","3785"],' + #13#10 +
        'AXIS["X",EAST],' + #13#10 +
        'AXIS["Y",NORTH]]';
    end;
    53004: begin
      Result :=
        'PROJCS["Sphere_Mercator",' + #13#10 +
        'GEOGCS["GCS_Sphere",' + #13#10 +
        'DATUM["Not_specified_based_on_Authalic_Sphere",' + #13#10 +
        'SPHEROID["Sphere",6371000,0]],' + #13#10 +
        'PRIMEM["Greenwich",0],' + #13#10 +
        'UNIT["Degree",0.017453292519943295]],' + #13#10 +
        'PROJECTION["Mercator_1SP"],' + #13#10 +
        'PARAMETER["False_Easting",0],' + #13#10 +
        'PARAMETER["False_Northing",0],' + #13#10 +
        'PARAMETER["Central_Meridian",0],' + #13#10 +
        'PARAMETER["Standard_Parallel_1",0],' + #13#10 +
        'UNIT["Meter",1],' + #13#10 +
        'AUTHORITY["EPSG","53004"]]';
    end;
    3395: begin
      Result :=
        'PROJCS["WGS 84 / World Mercator",' + #13#10 +
        'GEOGCS["WGS 84",' + #13#10 +
        'DATUM["WGS_1984",' + #13#10 +
        'SPHEROID["WGS 84",6378137,298.257223563,' + #13#10 +
        'AUTHORITY["EPSG","7030"]],' + #13#10 +
        'AUTHORITY["EPSG","6326"]],' + #13#10 +
        'PRIMEM["Greenwich",0,' + #13#10 +
        'AUTHORITY["EPSG","8901"]],' + #13#10 +
        'UNIT["degree",0.01745329251994328,' + #13#10 +
        'AUTHORITY["EPSG","9122"]],' + #13#10 +
        'AUTHORITY["EPSG","4326"]],' + #13#10 +
        'UNIT["metre",1,' + #13#10 +
        'AUTHORITY["EPSG","9001"]],' + #13#10 +
        'PROJECTION["Mercator_1SP"],' + #13#10 +
        'PARAMETER["central_meridian",0],' + #13#10 +
        'PARAMETER["scale_factor",1],' + #13#10 +
        'PARAMETER["false_easting",0],' + #13#10 +
        'PARAMETER["false_northing",0],' + #13#10 +
        'AUTHORITY["EPSG","3395"],' + #13#10 +
        'AXIS["Easting",EAST],' + #13#10 +
        'AXIS["Northing",NORTH]]'
    end;
    4326: begin
      Result :=
        'GEOGCS["WGS 84",' + #13#10 +
        'DATUM["WGS_1984",' + #13#10 +
        'SPHEROID["WGS 84",6378137,298.257223563,' + #13#10 +
        'AUTHORITY["EPSG","7030"]],' + #13#10 +
        'AUTHORITY["EPSG","6326"]],' + #13#10 +
        'PRIMEM["Greenwich",0,' + #13#10 +
        'AUTHORITY["EPSG","8901"]],' + #13#10 +
        'UNIT["degree",0.01745329251994328,' + #13#10 +
        'AUTHORITY["EPSG","9122"]],' + #13#10 +
        'AUTHORITY["EPSG","4326"]]'
    end;
  else
    Result := '';
  end;
end;

procedure toAuxXml(fname:string;Atype:TMapType);
var
  AuxXmkfile:TMemoryStream;
  str:UTF8String;
  VprojInfo: String;
begin
  AuxXmkfile:=TMemoryStream.create;
  str:=AnsiToUtf8('<PAMDataset>'+#13#10+'<SRS>');
  VprojInfo := GetProjByEPSG(Atype.GeoConvert.GetProjectionEPSG);
  str:=str+AnsiToUtf8(VprojInfo);
  str:=str+AnsiToUtf8('</SRS>'+#13#10+'<Metadata>'+#13#10+'<MDI key="PyramidResamplingType">NEAREST</MDI>'+#13#10+'</Metadata>'+#13#10+'</PAMDataset>');
  AuxXmkfile.Write(str[1],length(str));
  AuxXmkfile.SaveToFile(fname+'.aux.xml');
  AuxXmkfile.Free;
end;

procedure toPrj(fname:string;Atype:TMapType);
var
  f:TextFile;
  VprojInfo: String;
begin
 assignfile(f,ChangeFileExt(fname,'.prj'));
 rewrite(f);
 VprojInfo := GetProjByEPSG(Atype.GeoConvert.GetProjectionEPSG);
 writeln(f, VprojInfo);
 closefile(f);
end;

procedure toDat(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);
var f:TextFile;
    LL1,LL2:TExtendedPoint;
begin
 assignfile(f,ChangeFileExt(fname,'.dat'));
 rewrite(f);
 writeln(f,'2');
 LL1:=Atype.GeoConvert.PixelPos2LonLat(xy1,Azoom-1);
 LL2:=Atype.GeoConvert.PixelPos2LonLat(xy2,Azoom-1);
 writeln(f,R2StrPoint(LL1.x)+','+R2StrPoint(LL1.y));
 writeln(f,R2StrPoint(LL2.x)+','+R2StrPoint(LL1.y));
 writeln(f,R2StrPoint(LL2.x)+','+R2StrPoint(LL2.y));
 writeln(f,R2StrPoint(LL1.x)+','+R2StrPoint(LL2.y));
 writeln(f,Atype.name+' (SASPlanet)');
 closefile(f);
end;

procedure toKml(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);
var f:TextFile;
    LL1,LL2:TExtendedPoint;
    str:UTF8String;
begin
 assignfile(f,ChangeFileExt(fname,'.kml'));
 rewrite(f);
 str:=ansiToUTF8('<?xml version="1.0" encoding="UTF-8"?>'+#13#10);
 str:=str+ansiToUTF8('<kml><GroundOverlay><name>'+ExtractFileName(fname)+'</name><color>88ffffff</color><Icon>'+#13#10);
 str:=str+ansiToUTF8('<href>'+ExtractFileName(fname)+'</href>'+#13#10);
 str:=str+ansiToUTF8('<viewBoundScale>0.75</viewBoundScale></Icon><LatLonBox>'+#13#10);
 LL1:=Atype.GeoConvert.PixelPos2LonLat(xy1,Azoom-1);
 LL2:=Atype.GeoConvert.PixelPos2LonLat(xy2,Azoom-1);
 str:=str+ansiToUTF8('<north>'+R2StrPoint(LL1.y)+'</north>'+#13#10);
 str:=str+ansiToUTF8('<south>'+R2StrPoint(LL2.y)+'</south>'+#13#10);
 str:=str+ansiToUTF8('<east>'+R2StrPoint(LL2.x)+'</east>'+#13#10);
 str:=str+ansiToUTF8('<west>'+R2StrPoint(LL1.x)+'</west>'+#13#10);
 str:=str+ansiToUTF8('</LatLonBox></GroundOverlay></kml>');
 writeln(f,str);
 closefile(f);
end;

procedure toTabMap(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);
var f:TextFile;
    xy:TPoint;
    lat,lon:array[1..3] of real;
begin
 assignfile(f,ChangeFileExt(fname,'.tab'));
 rewrite(f);
 writeln(f,'!table');
 writeln(f,'!version 300');
 writeln(f,'!charset WindowsCyrillic'+#13#10);
 writeln(f,'Definition Table');
 writeln(f,'File "'+copy(fname,LastDelimiter('\',fname)+1,length(fname)-1)+'"');
 writeln(f,'Type "RASTER"');

 lon[1]:=Atype.GeoConvert.Pos2LonLat(xy1,(Azoom - 1) + 8).X;
 lat[1]:=Atype.GeoConvert.Pos2LonLat(xy1,(Azoom - 1) + 8).Y;
 lon[3]:=Atype.GeoConvert.Pos2LonLat(xy2,(Azoom - 1) + 8).X;
 lat[3]:=Atype.GeoConvert.Pos2LonLat(xy2,(Azoom - 1) + 8).Y;
 lon[2]:=lon[3]-(lon[3]-lon[1])/2;
 xy.Y:=(xy2.y-((xy2.Y-xy1.Y)div 2));
 xy.X:=(xy2.x-((xy2.x-xy1.x)div 2));
 lat[2]:=Atype.GeoConvert.Pos2LonLat(xy,(Azoom - 1) + 8).Y;

 xy2:=Point(xy2.X-xy1.X,xy2.y-xy1.y);
 xy1:=Point(0,0);

 writeln(f,'('+R2StrPoint(lon[1])+','+R2StrPoint(lat[1])+') ('+inttostr(xy1.x)+', '+inttostr(xy1.y)+') Label "Точка 1",');
 writeln(f,'('+R2StrPoint(lon[3])+','+R2StrPoint(lat[3])+') ('+inttostr(xy2.x)+', '+inttostr(xy2.y)+') Label "Точка 2",');
 writeln(f,'('+R2StrPoint(lon[1])+','+R2StrPoint(lat[3])+') ('+inttostr(xy1.x)+', '+inttostr(xy2.y)+') Label "Точка 3",');
 writeln(f,'('+R2StrPoint(lon[3])+','+R2StrPoint(lat[1])+') ('+inttostr(xy2.x)+', '+inttostr(xy1.y)+') Label "Точка 4",');
 writeln(f,'('+R2StrPoint(lon[2])+','+R2StrPoint(lat[2])+') ('+inttostr((xy2.x-xy1.X)div 2)+', '+inttostr((xy2.y-xy1.y)div 2)+') Label "Точка 5",');
 writeln(f,'('+R2StrPoint(lon[2])+','+R2StrPoint(lat[1])+') ('+inttostr((xy2.x-xy1.X)div 2)+', '+inttostr(xy1.y)+') Label "Точка 6",');
 writeln(f,'('+R2StrPoint(lon[1])+','+R2StrPoint(lat[2])+') ('+inttostr(xy1.x)+', '+inttostr((xy2.y-xy1.y)div 2)+') Label "Точка 7",');
 writeln(f,'('+R2StrPoint(lon[3])+','+R2StrPoint(lat[2])+') ('+inttostr(xy2.x)+', '+inttostr((xy2.y-xy1.y)div 2)+') Label "Точка 8",');
 writeln(f,'('+R2StrPoint(lon[2])+','+R2StrPoint(lat[3])+') ('+inttostr((xy2.x-xy1.X)div 2)+', '+inttostr(xy2.y)+') Label "Точка 9"');

 writeln(f,'CoordSys Earth Projection 1, 104');
 writeln(f,'Units "degree"');
 closefile(f);
end;

procedure toWorldFiles(fname:string;xy1,xy2:TPoint;Azoom:byte;Atype:TMapType);
var f:TextFile;
    ll1,ll2:TExtendedPoint;
    CellX,CellY,OrigX,OrigY:extended;
begin
 fname:=fname+'w';
 ll1:=Atype.GeoConvert.Pos2LonLat(xy1,(Azoom - 1) + 8);
 ll2:=Atype.GeoConvert.Pos2LonLat(xy2,(Azoom - 1) + 8);
 CalculateWFileParams(ll1,ll2,xy2.X-xy1.X,xy2.Y-xy1.Y,Atype,CellX,CellY,OrigX,OrigY);
 assignfile(f,fname);
 rewrite(f);
 writeln(f,R2StrPoint(CellX));
 writeln(f,'0');
 writeln(f,'0');
 writeln(f,R2StrPoint(CellY));
 writeln(f,R2StrPoint(OrigX));
 writeln(f,R2StrPoint(OrigY));
 closefile(f);
end;

end.
 