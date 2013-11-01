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
  i_GeoCoder,
  i_InterfaceListSimple,
  i_NotifierOperation,
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
  procedure SearchInTXTFile(
    const ACancelNotifier: INotifierOperation;
    AOperationID: Integer;
    const AFile : string ;
    const ASearch : widestring;
    const Alist : IInterfaceListSimple;
    var Acnt : integer
  );
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
    constructor Create(
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  StrUtils,
  t_GeoTypes,
  u_InterfaceListSimple,
  u_ResStrings,
  u_Synchronizer;

{ TGeoCoderByTXT }

function ItemExist(
  const AValue: IGeoCodePlacemark;
  const AList: IInterfaceListSimple
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
  const AList : IInterfaceListSimple;
  var ACnt : integer
);
var
  VFormatSettings : TFormatSettings;
  VPlace : IGeoCodePlacemark;
  VPoint : TDoublePoint;
  slat, slon: string;
  sname, sdesc, sfulldesc : string;
  j, l: integer;
  VSearch : string;
  VValueConverter: IValueToStringConverter;
  VAnsi: AnsiString;
  VLine: string;
  VLineUpper: string;
  VTabArray: TStringList;
  VTextFile: Textfile;
begin
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  VFormatSettings.DecimalSeparator := '.';
  VSearch := AnsiUpperCase(ASearch);
  Assign(VTextFile, AFile);
  {$I-} // отключение контроля ошибок ввода-вывода
  Reset(VTextFile);
  {$I+} // включение контроля ошибок ввода-вывода
  if IOResult = 0 then // если нет ошибка открытия, то
  begin
    VTabArray := TStringList.Create;
    try
      VTabArray.LineBreak := #09;
      while not EOF(VTextFile) do begin
       Readln(VTextFile, VAnsi);
       VLine := Utf8ToAnsi(VAnsi);
        VLineUpper := AnsiUpperCase(VLine);
        if Pos(VSearch, VLineUpper) > 1 then begin
          if ACnt mod 5 = 0 then begin
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              Exit;
            end;
          end;
          VTabArray.Text := VLine;
          sdesc :=
            VTabArray.Strings[17] + #$D#$A +
            VTabArray.Strings[01] + #$D#$A +
            VTabArray.Strings[02] + #$D#$A +
            VTabArray.Strings[03] + #$D#$A +
            VTabArray.Strings[18] + #$D#$A;

          slat := VTabArray.Strings[4];
          slon := VTabArray.Strings[5];

          sname := VTabArray.Strings[3];
          if sname = '' then begin
            sname := VTabArray.Strings[2];
          end;
          VTabArray.Text := '';

          if (slat <> '') and (slon <> '') then begin
            try
              VPoint.Y := StrToFloat(slat, VFormatSettings);
              VPoint.X := StrToFloat(slon, VFormatSettings);
            except
              raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
            end;

            l := length(sname);
            while (copy(sname,l,1)<>',') and (l>0) do dec(l);
            sname := copy(sname,l+1,length(sname)-l);

            if PosEx('?',sname)>0 then begin
              l := length(sname);
              while (Copy(sname,l,1)<>#09) and (l>0) do dec(l);
                j := PosEx(VSearch , VLineUpper);
                l := PosEx(#09 , VLine, j);
                sname := Copy(VLine, j, l - j);
              if  PosEx(',',sname)>0 then begin
                j := 0;
                l := PosEx(',' , sname);
                sname := Copy(sname, j, l - (j+1));
              end;
            end;

            sdesc := sdesc + '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
            sdesc := sdesc + #$D#$A + ExtractFileName(AFile);
            sfulldesc :=  ReplaceStr(sname + #$D#$A + sdesc, #$D#$A, '<br>');

            VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
            if not ItemExist(VPlace, AList) then begin
              Inc(ACnt);
              AList.Add(VPlace);
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(VTabArray);
    end;
    CloseFile(VTextFile);
  end;
end;

constructor TGeoCoderByTXT.Create(
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(APlacemarkFactory);
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
): IInterfaceListSimple;
var
VList: IInterfaceListSimple;
vpath : string;
Vcnt : integer;
VFolder: string;
SearchRec: TSearchRec;
MySearch : string;
begin
 Vcnt := 1;
 MySearch := ASearch;
 while PosEx('  ',MySearch)>0 do MySearch := ReplaceStr(MySearch,'  ',' ');
 VList := TInterfaceListSimple.Create;
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
