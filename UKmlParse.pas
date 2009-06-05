unit UKmlParse;
interface
uses Classes,SysUtils,GR32,StrUtils,Ugeofun,Windows, UResStrings;

type
 TStyle = class
  id:string;
  colorline:TColor32;
  colorfill:TColor32;
  bitm:TBitmap32;
 end;

 TStyleMap = class
  id:string;
  normal:TStyle;
  hightLight:TStyle;
 end;

 TKMLData = record
  PlacemarkID:string;
  Name:string;
  description:string;
  LatLonAltBoxLT:TRealpoint;
  LatLonAltBoxRD:TRealpoint;
  coordinates:array of TRealpoint;
  coordinatesLT:TRealpoint;
  coordinatesRD:TRealpoint;
 end;
 TKML = class
  Data: Array of TKMLData;
  Styles:TStringList;
  StyleMaps:TStringList;
  Error_:string;
  function loadFromFile(FileName:string):boolean;
 end;

var Style:TStyle;
    StyleMap:TStyleMap;
implementation
uses unit1;


function TKML.loadFromFile(FileName:string):boolean;
var buffer,koord:string;
    buff:array [0..64535]of char;
    position,PosStartPlace,PosTag1,PosTag2,PosEndPlace,size,ii,jj,placeN,iip:integer;
    str:TMemoryStream;
    pb:integer;
begin
  result:=true;
  error_:='';
  if not(FileExists(FileName)) then
   begin
    result:=false;
    error_:=SAS_ERR_FileNotFound;
    exit;
   end;

  str:=TMemoryStream.Create;
  str.LoadFromFile(FileName);
  str.Position:=0;
  fillchar(buff,sizeof(buff),0);
  buffer:='';
  repeat
   size:=str.Read(buff,sizeOf(buff));
   buffer:=buffer+buff;
  until size<sizeOf(buff);
  position:=1;
  PosStartPlace:=1;
  PosEndPlace:=1;
  ii:=0;
  placeN:=length(buffer);
  placeN:=0;
  Data:=nil;
{  While (position>0)and(PosStartPlace>0)and(PosEndPlace>0)and(result) do
   try
    Style.Create;
    PosStartPlace:=PosEx('<StyleMap',buffer,position);
    if PosStartPlace<1 then continue;
    PosEndPlace:=PosEx('</<StyleMap>',buffer,PosStartPlace);
    if PosEndPlace<1 then continue;
    position:=PosEx('id=',buffer,PosStartPlace);
    if (position<PosEndPlace)and(position>PosStartPlace)
     then Style.ID:=copy(buffer,position+4,PosEx('">',buffer,position)-(position+6))
     else continue;
     normal
    Styles=
   except
   end;    }
  While (position>0)and(PosStartPlace>0)and(PosEndPlace>0)and(result) do
   try
    SetLength(Data,placeN+1);
    With Data[PlaceN] do
    begin
    PosStartPlace:=PosEx('<Placemark',buffer,position);
    if PosStartPlace<1 then continue;
    PosEndPlace:=PosEx('</Placemark>',buffer,PosStartPlace);
    if PosEndPlace<1 then continue;
    position:=PosEx('id=',buffer,PosStartPlace);
    if (position<PosEndPlace)and(position>PosStartPlace)
     then Data[PlaceN].PlacemarkID:=copy(buffer,position+6,PosEx('">',buffer,position)-(position+6))
     else Data[PlaceN].PlacemarkID:='';
    PosTag1:=PosEx('<name>',buffer,PosStartPlace); PosTag2:=PosEx('</name>',buffer,PosTag1);
    if (PosTag1>PosStartPlace)and(PosTag1<PosEndPlace)and(PosTag2>PosStartPlace)and(PosTag2<PosEndPlace)and(PosTag2>PosTag1)
     then Data[PlaceN].Name:=Utf8ToAnsi(copy(buffer,PosTag1+6,PosTag2-(PosTag1+6)))
     else Data[PlaceN].Name:='';
    PosTag1:=PosEx('<description>',buffer,PosStartPlace); PosTag2:=PosEx('</description>',buffer,PosTag1);
    if (PosTag1>PosStartPlace)and(PosTag1<PosEndPlace)and(PosTag2>PosStartPlace)and(PosTag2<PosEndPlace)and(PosTag2>PosTag1)
     then Data[PlaceN].description:=Utf8ToAnsi(copy(buffer,PosTag1+13,PosTag2-(PosTag1+13)))
     else Data[PlaceN].description:='';
    pb:=PosEx('<![CDATA[',Data[PlaceN].description);
    if pb>0 then Data[PlaceN].description:=copy(Data[PlaceN].description,pb+9,PosEx(']]>',Data[PlaceN].description)-(pb+9));
{    PosTag1:=PosEx('<LatLonAltBox',buffer,PosStartPlace); PosTag2:=PosEx('</LatLonAltBox>',buffer,PosTag1);
    if (PosTag1>PosStartPlace)and(PosTag1<PosEndPlace)and(PosTag2>PosStartPlace)and(PosTag2<PosEndPlace)and(PosTag2>PosTag1)
     then begin
           PosTag1:=PosEx('<north>',buffer,PosStartPlace); PosTag2:=PosEx('</north>',buffer,PosTag1);
           if (PosTag1>PosStartPlace)and(PosTag1<PosEndPlace)and(PosTag2>PosStartPlace)and(PosTag2<PosEndPlace)and(PosTag2>PosTag1)
            then LatLonAltBoxLT.Y:=form1.str2r(copy(buffer,PosTag1+7,PosTag2-(PosTag1+7)))
            else LatLonAltBoxLT.Y:=-1000;
           PosTag1:=PosEx('<south>',buffer,PosStartPlace); PosTag2:=PosEx('</south>',buffer,PosTag1);
           if (PosTag1>PosStartPlace)and(PosTag1<PosEndPlace)and(PosTag2>PosStartPlace)and(PosTag2<PosEndPlace)and(PosTag2>PosTag1)
            then LatLonAltBoxRD.Y:=form1.str2r(copy(buffer,PosTag1+7,PosTag2-(PosTag1+7)))
            else LatLonAltBoxRD.Y:=-1000;
           PosTag1:=PosEx('<east>',buffer,PosStartPlace); PosTag2:=PosEx('</east>',buffer,PosTag1);
           if (PosTag1>PosStartPlace)and(PosTag1<PosEndPlace)and(PosTag2>PosStartPlace)and(PosTag2<PosEndPlace)and(PosTag2>PosTag1)
            then LatLonAltBoxRD.X:=form1.str2r(copy(buffer,PosTag1+6,PosTag2-(PosTag1+6)))
            else LatLonAltBoxRD.X:=-1000;
           PosTag1:=PosEx('<west>',buffer,PosStartPlace); PosTag2:=PosEx('</west>',buffer,PosTag1);
           if (PosTag1>PosStartPlace)and(PosTag1<PosEndPlace)and(PosTag2>PosStartPlace)and(PosTag2<PosEndPlace)and(PosTag2>PosTag1)
            then LatLonAltBoxLT.X:=form1.str2r(copy(buffer,PosTag1+6,PosTag2-(PosTag1+6)))
            else LatLonAltBoxLT.X:=-1000;
          end;    }
    PosTag1:=PosEx('<coordinates',buffer,PosStartPlace); PosTag2:=PosEx('</coordinates>',buffer,PosTag1);
    if (PosTag1>PosStartPlace)and(PosTag1<PosEndPlace)and(PosTag2>PosStartPlace)and(PosTag2<PosEndPlace)and(PosTag2>PosTag1)
     then begin
           koord:=copy(buffer,PosTag1+13,PosTag2-(PosTag1+13));
           ii:=1;
           jj:=0;
           while ii<=length(koord) do
            begin
             while (not(koord[ii]in ['0'..'9','-']))and(ii<=length(koord)) do inc(ii);
             if ii>length(koord) then continue;
             setLength(coordinates,jj+1);
             iip:=posEx(',',koord,ii);
             coordinates[jj].x:=Fmain.str2r(copy(koord,ii,iip-ii));
             ii:=iip+1;
             iip:=posEx(',',koord,ii);
             if iip=0 then iip:=length(koord);
             coordinates[jj].y:=Fmain.str2r(copy(koord,ii,iip-ii));
             ii:=iip+1;
             while ((koord[ii]in ['0'..'9','e','.','-'])) do inc(ii);
             inc(jj);
            end;
           if length(coordinates)>0 then
            begin
             coordinatesLT:=coordinates[0];
             coordinatesRD:=coordinates[0];
             for ii:=0 to length(coordinates)-1 do
              begin
                if coordinatesLT.x>coordinates[ii].X then coordinatesLT.x:=coordinates[ii].X;
                if coordinatesRD.x<coordinates[ii].X then coordinatesRD.x:=coordinates[ii].X;
                if coordinatesLT.y<coordinates[ii].y then coordinatesLT.y:=coordinates[ii].y;
                if coordinatesRD.y>coordinates[ii].y then coordinatesRD.y:=coordinates[ii].y;
              end;
            end
           else result:=false;
          end
     else result:=false;
    end;
    inc(placeN);
    position:=PosEndPlace+1;
   except
    Result:=false;
    error_:=SAS_ERR_Read;
   end;
 SetLength(Data,length(Data)-1);
 str.Free;
end;

end.
