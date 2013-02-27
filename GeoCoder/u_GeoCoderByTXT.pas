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
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_ValueToStringConverter,
  u_GeoCoderLocalBasic,
  u_MappedFile;

type
  EGeoCoderERR = class(Exception);
  EDirNotExist = class(EGeoCoderERR);

  TGeoCoderByTXT = class(TGeoCoderLocalBasic)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FLock: IReadWriteSync;
  procedure SearchInTXTFile(
    const ACancelNotifier: INotifierOperation;
    AOperationID: Integer;
    const AFile : string ;
    const ASearch : widestring;
    Alist : IInterfaceList;
    var Acnt : integer
  );
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceList; override;
  public
    constructor Create(const AValueToStringConverterConfig: IValueToStringConverterConfig);
  end;

implementation

uses
  StrUtils,
  t_GeoTypes,
  i_GeoCoder,
  u_ResStrings,
  u_Synchronizer,
  u_GeoCodePlacemark;

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
  const AValue: IGeoCodePlacemark;
  const AList: IInterfaceList
):boolean;
var
  i: Integer;
  VPlacemark: IGeoCodePlacemark;
  j : integer;
  str1,str2 : string;
begin
  Result := false;
  for i := 0 to AList.Count - 1 do begin
    VPlacemark := IGeoCodePlacemark(AList.Items[i]);
    j:= posex(')',VPlacemark.Name);
    str1 := copy(VPlacemark.Name,j,length(VPlacemark.Name)-(j+1));
    j:= posex(')',AValue.Name);
    str2 := copy(AValue.Name,j,length(AValue.Name)-(j+1));
    if str1=str2 then begin
      if
        abs(VPlacemark.GetPoint.x-AValue.GetPoint.x) +
        abs(VPlacemark.GetPoint.Y-AValue.GetPoint.Y) < 0.05
      then begin
        Result := true;
        Break;
      end;
    end;
  end;
end;

procedure TGeoCoderByTXT.SearchInTXTFile(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AFile : string ;
  const ASearch : widestring;
  Alist : IInterfaceList;
  var Acnt : integer
  );
var
 VFormatSettings : TFormatSettings;
 VPlace : IGeoCodePlacemark;
 VPoint : TDoublePoint;
 slat, slon, sname, sdesc, sfulldesc : string;
 VLinkErr : boolean;
 i, j, l: integer;
 VStr: AnsiString;
 VSearch : widestring;
 V_StrData : string;
 Skip: boolean;
 VValueConverter: IValueToStringConverter;
 VFile: TMappedFile;
 VData: PByte;
begin
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  VFormatSettings.DecimalSeparator := '.';
  VSearch := AnsiUpperCase(ASearch);
  VFile := TMappedFile.Create(AFile);
  VData := PByte(VFile.Content);
  for j := 0 to VFile.Size - 1 do begin
    VStr := VStr + chr(VData^);
    if VData^ = $0A then begin // найден конец строки
      V_StrData := AnsiUpperCase(utf8toansi(VStr));
      if (PosEx(VSearch, V_StrData,1)>1) {and (i>0) }then begin
        VLinkErr := false;
        V_StrData := utf8toansi(VStr);
        if Acnt mod 5 =0 then
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

          sname := GetNsubstring(V_StrData,#09,4);
          if sname ='' then sname := GetNsubstring(V_StrData,#09,3);
          l := length(sname);
          while (copy(sname,l,1)<>',') and (l>0) do dec(l);
          sname := copy(sname,l+1,length(sname)-l);
          if PosEx('?',sname)>0 then begin
            l := length(sname);
            while (copy(sname,l,1)<>#09) and (l>0) do dec(l);
            i := PosEx(VSearch , V_StrData);
            l := PosEx(#09 , V_StrData,i);
            sname := Copy(V_StrData, i, l - i);
            if  PosEx(',',sname)>0 then begin
              i:=0;
              l := PosEx(',' , sname);
              sname := Copy(sname, i, l - (i+1));
            end;
          end;

          sdesc := sdesc + '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sdesc := sdesc + #$D#$A + ExtractFileName(AFile);
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');

          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          // если закометировать условие то не будет производиться фильтрация одинаковых элементов
          skip := ItemExist(Vplace, Alist);
          if not skip then begin
            inc(Acnt);
            Alist.Add(VPlace);
          end;
        end;
      end;
    VStr:='';
    end;
  Inc(VData);
  end;
VFile.Free;
end;

constructor TGeoCoderByTXT.Create;
begin
  inherited Create;
  if not DirectoryExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))+'userdata\txt')) then
    raise EDirNotExist.Create('not found .\userdata\txt\! skip GeoCoderByTXT');
  FLock := MakeSyncRW_Std(Self, False);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function TGeoCoderByTXT.DoSearch(
  const ACancelNotifier: INotifierOperation;
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
 MySearch := ASearch;
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
