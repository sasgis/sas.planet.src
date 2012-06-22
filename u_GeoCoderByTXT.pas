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
  i_ValueToStringConverter,
  u_GeoCoderLocalBasic;

type
  EGeoCoderERR = class(Exception);
  EDirNotExist = class(EGeoCoderERR);

  TGeoCoderByTXT = class(TGeoCoderLocalBasic)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FLock: IReadWriteSync;
    function deg2strvalue(
      aDeg: Double;
      Alat, NeedChar: boolean
      ):string;
  procedure SearchInTXTFile(
    const ACancelNotifier: IOperationNotifier;
    AOperationID: Integer;
    const AFile : string ;
    const ASearch : widestring;
    Vlist : IInterfaceList;
    var Vcnt : integer
  );
  protected
    function DoSearch(
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceList; override;
  public
    constructor Create(const AValueToStringConverterConfig: IValueToStringConverterConfig);
  end;

implementation

uses
  ActiveX,
  windows,
  StrUtils,
  t_GeoTypes,
  t_CommonTypes,
  i_GeoCoder,
  u_ResStrings,
  u_GeoCodePlacemark,
  u_GeoCodeResult;

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

function TGeoCoderByTXT.deg2strvalue( aDeg:Double; Alat,NeedChar:boolean):string;
var
  VDegr: Double;
  VInt: Integer;
  VValue: Integer;
begin
  VDegr := abs(ADeg);

  case FValueToStringConverterConfig.DegrShowFormat of
    dshCharDegrMinSec, dshSignDegrMinSec: begin
      VValue := Trunc(VDegr * 60 * 60 * 100 + 0.005);
      VInt := Trunc(VValue / (60 * 60 * 100));
      VValue := VValue - VInt * (60 * 60 * 100);
      result := IntToStr(VInt) + '°';

      VInt := Trunc(VValue / (60 * 100));
      VValue := VValue - VInt * (60 * 100);

      if VInt < 10 then begin
        Result := result + '0' + IntToStr(VInt) + '''';
      end else begin
        Result := result + IntToStr(VInt) + '''';
      end;

      Result := Result + FormatFloat('00.00', VValue / 100) + '"';
    end;
    dshCharDegrMin, dshSignDegrMin: begin
      VValue := Trunc(VDegr * 60 * 10000 + 0.00005);
      VInt := Trunc(VValue / (60 * 10000));
      VValue := VValue - VInt * (60 * 10000);
      Result := IntToStr(VInt) + '°';
      Result := Result + FormatFloat('00.0000', VValue / 10000) + '''';
    end;
    dshCharDegr, dshSignDegr: begin
      Result := FormatFloat('0.000000', VDegr) + '°';
    end;
  end;

   if NeedChar then
    if Alat then begin
    if aDeg>0 then Result := 'N'+ Result else Result := 'S'+ Result ;
    end else
    if aDeg>0 then Result := 'E'+ Result else Result := 'W'+ Result ;
end;

function ItemExist(
  const AValue: IGeoCodePlacemark;
  const AList: igeocoderesult
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
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
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
 VFormatSettings.DecimalSeparator := '.';
 VSearch := AnsiUpperCase(ASearch);
 FLock.BeginRead;
 try
  VStream := TFileStream.Create(AFile, fmOpenRead);
   try
    SetLength(Vstr,VStream.Size);
    VStream.ReadBuffer(VStr[1],VStream.Size);
   finally
   VStream.Free;
   end;
  finally
  FLock.EndRead;
 end;
  Vstr := AnsiUpperCase(utf8toansi(Vstr));
  // ищем вхождение, затем бежим назад до начала блока
  i:=1;
  while (PosEx(VSearch, VStr, i)>i) {and (i>0) }do begin
    VLinkErr := false;
    i := PosEx(VSearch, VStr, i);
    k := PosEx(#$A, VStr, i); // конец блока найденных данных.
    l := i;
    while (copy(Vstr,l,1)<>#$A) and (l>0) do dec(l); // начало блока с найденными данными
    V_StrData := Copy(VStr, l+1 , k-l);
    if Vcnt mod 5 =0 then
     if ACancelNotifier.IsOperationCanceled(AOperationID) then
       Exit;
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
      if FValueToStringConverterConfig.IsLatitudeFirst = true then
         sdesc := sdesc + '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
          sdesc := sdesc + '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
      sdesc := sdesc + #$D#$A + ExtractFileName(AFile);
      sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');

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

constructor TGeoCoderByTXT.Create;
begin
  inherited Create;
  if not DirectoryExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))+'userdata\txt')) then
    raise EDirNotExist.Create('not found .\userdata\txt\! skip GeoCoderByTXT');
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function TGeoCoderByTXT.DoSearch(
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceList;
var
VList: IInterfaceList;
vpath : string;
Vcnt : integer;
VFolder: string;
SearchRec: TSearchRec;
MySearch : string;
begin
 Vcnt := 1;
 MySearch := Asearch;
 while PosEx('  ',MySearch)>0 do MySearch := ReplaceStr(MySearch,'  ',' ');
 VList := TInterfaceList.Create;
 VFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))+'userdata\txt\');
 if FindFirst(VFolder + '*.txt', faAnyFile, SearchRec) = 0 then begin
  repeat
    if (SearchRec.Attr and faDirectory) = faDirectory then begin
      continue;
    end;
    vpath:= VFolder+SearchRec.Name;
    SearchInTXTFile(ACancelNotifier, AOperationID, Vpath , MySearch, vlist , Vcnt);
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
  until FindNext(SearchRec) <> 0;
 end;
 Result := VList;
end;
end.
