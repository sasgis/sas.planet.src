unit UPLT;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  GR32,
  unit1,
  t_GeoTypes;

type
 TPLTData = record
  Name:string;
  description:string;
  coordinates:array of TExtendedpoint;
  coordinatesLT:TExtendedPoint;
  coordinatesRD:TExtendedPoint;
 end;

 TPLT = class
  Data: Array of TPLTData;
  function loadFromFile(FileName:string):boolean;
 end;

implementation

function GetWord(Str, Smb: string; WordNmbr: Byte): string;
var SWord: string;
    StrLen, N: Byte;
begin
  StrLen := SizeOf(Str);
  N := 1;
  while ((WordNmbr >= N) and (StrLen <> 0)) do
  begin
    StrLen := System.Pos(Smb, str);
    if StrLen <> 0 then
    begin
      SWord := Copy(Str, 1, StrLen - 1);
      Delete(Str, 1, StrLen);
      Inc(N);
    end
    else SWord := Str;
  end;
  if WordNmbr <= N then Result := SWord
                   else Result := '';
end;

function TPLT.loadFromFile(FileName:string):boolean;
var pltstr:TStringList;
    str,trackname:string;
    i,j:integer;
begin
 Result := false;
 if FileExists(FileName) then
  begin
   pltstr:=TStringList.Create;
   pltstr.LoadFromFile(FileName);
   trackname:=copy(ExtractFileName(FileName),1,length(ExtractFileName(FileName))-4);
   for i:=6 to pltstr.Count-1 do
    try
     j:=1;
     str:=pltstr[i];
     while j<length(str) do
      if str[j]=' ' then delete(str,j,1)
                    else inc(j);
     if (GetWord(pltstr[i], ',', 3)='1')or(i=6) then
      begin
       SetLength(Data,length(Data)+1);
       Data[length(Data)-1].Name:=trackname+', section '+inttostr(length(Data));
      end;
     SetLength(Data[length(Data)-1].coordinates,length(Data[length(Data)-1].coordinates)+1);
     Data[length(Data)-1].coordinates[length(Data[length(Data)-1].coordinates)-1].y:=Fmain.str2r(GetWord(str, ',', 1));
     Data[length(Data)-1].coordinates[length(Data[length(Data)-1].coordinates)-1].x:=Fmain.str2r(GetWord(str, ',', 2));
    except
    end;
  end;
end;

end.
