unit u_PLT;

interface

uses
  Classes,
  SysUtils,
  t_GeoTypes;

type
 TPLTData = record
  Name:string;
  description:string;
  coordinates:TArrayOfDoublePoint;
  coordinatesLT:TDoublePoint;
  coordinatesRD:TDoublePoint;
  function IsEmpty: Boolean;
  function IsPoint: Boolean;
  function IsLine: Boolean;
  function IsPoly: Boolean;
 end;

 TPLT = class
  Data: Array of TPLTData;
  function loadFromFile(FileName:string):boolean;
 end;

implementation

uses
  u_GeoToStr;

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
  if FileExists(FileName) then begin
    pltstr:=TStringList.Create;
    try
      pltstr.LoadFromFile(FileName);
      trackname:=copy(ExtractFileName(FileName),1,length(ExtractFileName(FileName))-4);
      for i:=6 to pltstr.Count-1 do begin
        try
          j:=1;
          str:=pltstr[i];
          while j<length(str) do begin
            if str[j]=' ' then begin
              delete(str,j,1);
            end else begin
              inc(j);
            end;
          end;
          if (GetWord(pltstr[i], ',', 3)='1')or(i=6) then begin
            SetLength(Data,length(Data)+1);
            Data[length(Data)-1].Name:=trackname+', section '+inttostr(length(Data));
          end;
          SetLength(Data[length(Data)-1].coordinates,length(Data[length(Data)-1].coordinates)+1);
          Data[length(Data)-1].coordinates[length(Data[length(Data)-1].coordinates)-1].y:=str2r(GetWord(str, ',', 1));
          Data[length(Data)-1].coordinates[length(Data[length(Data)-1].coordinates)-1].x:=str2r(GetWord(str, ',', 2));
        except
        end;
      end;
    finally
      pltstr.Free;
    end;
  end;
end;

{ TPLTData }

function TPLTData.IsEmpty: Boolean;
begin
  Result := Length(coordinates) = 0;
end;

function TPLTData.IsLine: Boolean;
var
  VPointCount: Integer;
begin
  VPointCount := Length(coordinates);
  if VPointCount > 1 then begin
    Result := (coordinates[0].X <> coordinates[VPointCount - 1].X) or
      (coordinates[0].Y <> coordinates[VPointCount - 1].Y);
  end else begin
    Result := False;
  end;
end;

function TPLTData.IsPoint: Boolean;
begin
  Result := Length(coordinates) = 1;
end;

function TPLTData.IsPoly: Boolean;
var
  VPointCount: Integer;
begin
  VPointCount := Length(coordinates);
  if VPointCount > 1 then begin
    Result := (coordinates[0].X = coordinates[VPointCount - 1].X) and
      (coordinates[0].Y = coordinates[VPointCount - 1].Y);
  end else begin
    Result := False;
  end;
end;

end.
