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

type
  TTabArray = array [1..19] of AnsiString;

{ TGeoCoderByTXT }

procedure StringToTabArray(const AString: AnsiString; var AArray: TTabArray);
const
  cTabChar: AnsiChar = #09;
  cTabCharSize = 1;
var
  I, J, K: Integer;
begin
  I := PosEx(cTabChar, AString);
  J := 0;
  K := 1;
  while I > 0 do begin
    Inc(J);
    if J > Length(AArray) then begin
      Break;
    end else begin
      AArray[J] := Copy(AString, K, (I - K));
      K := I + 1;
      I := PosEx(cTabChar, AString, K);
    end;
  end;
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
  const ASearch : WideString;
  AList : IInterfaceList;
  var ACnt : integer
);
var
  VFormatSettings : TFormatSettings;
  VPlace : IGeoCodePlacemark;
  VPoint : TDoublePoint;
  slat, slon, sname, sdesc, sfulldesc : string;
  i, j, l: integer;
  VSearch : WideString;
  VValueConverter: IValueToStringConverter;
  VFile: TMappedFile;
  VData, VLineStart: PByte;
  VLineSize: Integer;
  VAnsiLine: AnsiString;
  VAnsiLineUpper: AnsiString;
  VTabArray: TTabArray;
begin
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  VFormatSettings.DecimalSeparator := '.';
  VSearch := AnsiUpperCase(ASearch);
  for I := Low(VTabArray) to High(VTabArray) do begin
    VTabArray[I] := '';
  end;
  VFile := TMappedFile.Create(AFile);
  try
    VData := PByte(VFile.Content);
    VLineStart := VData;
    for I := 0 to VFile.Size - 1 do begin
      if VData^ = $0A then begin // найден конец строки
        VLineSize := Cardinal(VData) - Cardinal(VLineStart);
        SetLength(VAnsiLine, VLineSize);
        Move(VLineStart^, VAnsiLine[1], VLineSize);
        VAnsiLine := Utf8ToAnsi(VAnsiLine);
        VAnsiLineUpper := AnsiUpperCase(VAnsiLine);
        if Pos(VSearch, VAnsiLineUpper) > 1 then begin
          if ACnt mod 5 = 0 then begin
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              Exit;
            end;
          end;

          StringToTabArray(VAnsiLine, VTabArray);

          sdesc :=
            VTabArray[18] + #$D#$A +
            VTabArray[02] + #$D#$A +
            VTabArray[03] + #$D#$A +
            VTabArray[04] + #$D#$A +
            VTabArray[19] + #$D#$A;

          slat := VTabArray[5];
          slon := VTabArray[6];

          if (slat <> '') and (slon <> '') then begin
            try
              VPoint.Y := StrToFloat(slat, VFormatSettings);
              VPoint.X := StrToFloat(slon, VFormatSettings);
            except
              raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
            end;

            sname := VTabArray[4];
            if sname = '' then begin
              sname := VTabArray[3];
            end;

            l := length(sname);
            while (copy(sname,l,1)<>',') and (l>0) do dec(l);
            sname := copy(sname,l+1,length(sname)-l);

            if PosEx('?',sname)>0 then begin
              l := length(sname);
              while (Copy(sname,l,1)<>#09) and (l>0) do dec(l);
              j := PosEx(VSearch , VAnsiLineUpper);
              l := PosEx(#09 , VAnsiLine, j);
              sname := Copy(VAnsiLine, j, l - j);
              if  PosEx(',',sname)>0 then begin
                j := 0;
                l := PosEx(',' , sname);
                sname := Copy(sname, j, l - (j+1));
              end;
            end;

            sdesc := sdesc + '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
            sdesc := sdesc + #$D#$A + ExtractFileName(AFile);
            sfulldesc :=  ReplaceStr(sname + #$D#$A + sdesc, #$D#$A, '<br>');

            VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
            if not ItemExist(VPlace, AList) then begin
              Inc(ACnt);
              AList.Add(VPlace);
            end;
          end;
        end;
        Inc(VData);
        VLineStart := VData;
      end else begin
        Inc(VData);
      end;
    end;
  finally
    VFile.Free;
  end;
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
