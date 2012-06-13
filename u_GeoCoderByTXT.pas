{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GeoCoderByTXT;

interface

uses
  Classes,
  sysutils,
  i_OperationNotifier,
  i_LocalCoordConverter,
  u_GeoCoderLocalBasic;

type
  EGeoCoderERR = class(Exception);
  EDirNotExist = class(EGeoCoderERR);

  TGeoCoderByTXT = class(TGeoCoderLocalBasic)
  private
    FLock: IReadWriteSync;
  procedure SearchInTXTFile(
    const AFile : string ;
    const ASearch : widestring;
    Vlist : IInterfaceList;
    var Vcnt : integer
  );
  protected
    function ParseResultToPlacemarksList(
      const ASearch: WideString
    ): IInterfaceList; override;
  public
    constructor Create();
  end;

implementation

uses
  ActiveX,
  windows,
  StrUtils,
  t_GeoTypes,
  i_GeoCoder,
  i_BinaryData,
  i_StringlistStatic,
  u_ResStrings,
  u_GeoCodePlacemark,
  u_GeoCodeResult,
  u_BinaryDataByMemStream,
  u_StringListStatic,
  u_GeoToStr;

{ TGeoCoderByTXT }

function GetNsubstring( const A_String,A_Separator:string; const N:integer):string;
var
i, j, cnt: integer;
begin
j:=0;
i:=posex(a_separator,a_string);
cnt:=1;
while (i<>0) and (cnt<N) do
 begin
     Inc(cnt);
     j:= i;
     i := PosEx(A_Separator, A_String, i + Length(A_Separator));
 end;
if (cnt>=n)and(i=0) then i:=length(A_String)+1;
result := copy(A_String,j+1,(i-j-1));
end;

function ItemExist(
  AValue:IGeoCodePlacemark;
  AList:igeocoderesult
  ):boolean;
var
i: Cardinal;
skip : boolean;
VPlacemark: IGeoCodePlacemark;
VEnum: IEnumUnknown;
j : integer;
str1,str2 : string;
begin
 skip := false;
 venum := Alist.GetPlacemarks;
 i := 0;
  while (VEnum.Next(1, VPlacemark, @i) = S_OK )and( skip = false )do
  begin
   j:= posex(')',VPlacemark.GetAddress);
   str1 := copy(VPlacemark.GetAddress,j,length(VPlacemark.GetAddress)-(j+1));
   j:= posex(')',Avalue.GetAddress);
   str2 := copy(Avalue.GetAddress,j,length(Avalue.GetAddress)-(j+1));
   if str1=str2 then
    if
      abs(VPlacemark.GetPoint.x-avalue.GetPoint.x) +
      abs(VPlacemark.GetPoint.Y-avalue.GetPoint.Y) < 0.05  then
    skip := true
  end;
result := skip;
end;

procedure TGeoCoderByTXT.SearchInTXTFile(
  const AFile : string ;
  const ASearch : widestring;
  Vlist : IInterfaceList;
  var Vcnt : integer
  );
var
 VFormatSettings : TFormatSettings;
 VPlace : IGeoCodePlacemark;
 VPoint : TDoublePoint;
 slat, slon, sname, sdesc, sfulldesc : string;
 VLinkErr : boolean;
 i, k, l: integer;
 VStr: string;
 VSearch : widestring;
 V_StrData : string;
 Skip: boolean;
 VStream: TFileStream;
begin
 VStream := TFileStream.Create(AFile, fmOpenRead);
 VFormatSettings.DecimalSeparator := '.';
 VSearch := AnsiUpperCase(ASearch);
 while PosEx('  ',VSearch)>0 do VSearch := ReplaceStr(VSearch,'  ',' ');
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
    try
    FLock.BeginRead;
     try
      SetLength(Vstr,VStream.Size);
      VStream.ReadBuffer(VStr[1],VStream.Size);
      VStream := nil;
     finally
    FLock.EndRead;
    end;
   finally
   VStream.Free;
   end;
  Vstr := AnsiUpperCase(Vstr);
  // ищем вхождение, затем бежим назад до начала блока
  i:=1;
  while (PosEx(VSearch, VStr, i)>i) {and (i>0) }do begin
    VLinkErr := false;
    i := PosEx(VSearch, VStr, i);
    k := PosEx(#$A, VStr, i); // конец блока найденных данных.
    l := i;
    while (copy(Vstr,l,1)<>#$A) and (l>0) do dec(l); // начало блока с найденными данными
     V_StrData := Copy(VStr, l+1 , k-l);
     sdesc := '';
     sdesc := sdesc + GetNsubstring(V_StrData,#09,18) + #$D#$A;
     sdesc := sdesc + GetNsubstring(V_StrData,#09,2) + #$D#$A;
     sdesc := sdesc + GetNsubstring(V_StrData,#09,3) + #$D#$A;
     sdesc := sdesc + GetNsubstring(V_StrData,#09,4) + #$D#$A;
     sdesc := sdesc + GetNsubstring(V_StrData,#09,19);
     slat := GetNsubstring(V_StrData,#09,5);
     slon := GetNsubstring(V_StrData,#09,6);
    if (slat='') or (slon='') then VLinkErr := true;
    if VLinkErr <> true then begin
     try
       VPoint.Y := StrToFloat(slat, VFormatSettings);
       VPoint.X := StrToFloat(slon, VFormatSettings);
     except
       raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
     end;
      sname := inttostr(Vcnt)+') '+ASearch;
      sdesc := sdesc + #$D#$A +'[ ' + slon + ' , ' + slat + ' ]';
      sdesc := sdesc + #$D#$A + ExtractFileName(AFile);
      sfulldesc := sname + #$D#$A+ sdesc;

      VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
      // если закометировать условие то не будет производиться фильтрация одинаковых элементов
      skip := ItemExist(Vplace,TGeoCodeResult.Create(VSearch, 200, '', VList));
      if skip = false then
       begin
        inc(Vcnt);
        VList.Add(VPlace);
       end;
    end;
    i:=k;
  end;
end;

constructor TGeoCoderByTXT.Create();
begin
  if not DirectoryExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))+'userdata\txt')) then
    raise EDirNotExist.Create('not found .\userdata\txt\! skip GeoCoderByTXT');
end;

function TGeoCoderByTXT.ParseResultToPlacemarksList(
  const ASearch: WideString
): IInterfaceList;
var
VList: IInterfaceList;
vpath : string;
Vcnt : integer;
VFolder: string;
SearchRec: TSearchRec;
begin
 Vcnt := 1;
 VList := TInterfaceList.Create;
 VFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))+'userdata\txt\');
 if FindFirst(VFolder + '*.txt', faAnyFile, SearchRec) = 0 then begin
  repeat
    if (SearchRec.Attr and faDirectory) = faDirectory then begin
      continue;
    end;
    vpath:= VFolder+SearchRec.Name;
    SearchInTXTFile(Vpath , Asearch , vlist , Vcnt);
  until FindNext(SearchRec) <> 0;
 end;
 Result := VList;
end;
end.
