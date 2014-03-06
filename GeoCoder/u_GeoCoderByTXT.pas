{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
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
  i_VectorItemSubsetBuilder,
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
      const AFile: string;
      const ASearch: widestring;
      const Alist: IInterfaceListSimple;
      var Acnt: integer
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
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  StrUtils,
  t_GeoTypes,
  i_VectorDataItemSimple,
  u_InterfaceListSimple,
  u_ResStrings,
  u_Synchronizer;

{ TGeoCoderByTXT }

constructor TGeoCoderByTXT.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory);
  if not DirectoryExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'userdata\txt')) then
    raise EDirNotExist.Create('not found .\userdata\txt\! skip GeoCoderByTXT');
  FLock := MakeSyncRW_Std(Self, False);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function ItemExist(
  const AValue: IVectorDataItemPoint;
  const AList: IInterfaceListSimple
):boolean;
var
  I, J: Integer;
  VPlacemark: IVectorDataItemPoint;
  VStr1, VStr2: string;
begin
  Result := False;
  for I := 0 to AList.Count - 1 do begin
    VPlacemark := IVectorDataItemPoint(AList.Items[I]);
    J:= PosEx(')',VPlacemark.Name);
    VStr1 := copy(VPlacemark.Name, J, Length(VPlacemark.Name) - (J + 1));
    J:= PosEx(')',AValue.Name);
    VStr2 := copy(AValue.Name, J, Length(AValue.Name) - (J + 1));
    if VStr1=VStr2 then begin
      if
        abs(VPlacemark.GetPoint.Point.x - AValue.GetPoint.Point.x) +
        abs(VPlacemark.GetPoint.Point.Y - AValue.GetPoint.Point.Y) < 0.05
      then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TGeoCoderByTXT.SearchInTXTFile(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AFile: string;
  const ASearch: WideString;
  const AList: IInterfaceListSimple;
  var ACnt: Integer
);
var
  VFormatSettings: TFormatSettings;
  VPlace: IVectorDataItemPoint;
  VPoint: TDoublePoint;
  VLatStr, VLonStr: string;
  VSName, VSDesc, VSFullDesc: string;
  I: Integer;
  VSearch: string;
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
          VTabArray.LineBreak := #09;
          VTabArray.Text := VLine;

          if VTabArray.Count < 19 then break;

          VSDesc :=
            VTabArray.Strings[17] + #$D#$A +
            VTabArray.Strings[01] + #$D#$A +
            VTabArray.Strings[02] + #$D#$A +
            VTabArray.Strings[03] + #$D#$A +
            VTabArray.Strings[18] + #$D#$A;

          VLatStr := VTabArray.Strings[4];
          VLonStr := VTabArray.Strings[5];

          VSName := VTabArray.Strings[3];
          if VSName = '' then begin
            VSName := VTabArray.Strings[2];
          end;

          VTabArray.Clear;
          VTabArray.LineBreak := ',';
          VTabArray.Text := VSName;

          for I := 0 to VTabArray.Count - 1 do begin
            if Length(VTabArray.Strings[I]) <> 0 then begin
              if Pos(VSearch, AnsiUpperCase(VTabArray.Strings[I])) > 0 then begin
                VSName := VTabArray.Strings[I];
                break;
              end;
            end;
          end;
          VTabArray.Clear;

          if (VLatStr <> '') and (VLonStr <> '') then begin
            try
              VPoint.Y := StrToFloat(VLatStr, VFormatSettings);
              VPoint.X := StrToFloat(VLonStr, VFormatSettings);
            except
              raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [VLatStr, VLonStr]);
            end;

            VSDesc := VSDesc + '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
            VSDesc := VSDesc + #$D#$A + ExtractFileName(AFile);
            VSFullDesc :=  ReplaceStr(VSName + #$D#$A + VSDesc, #$D#$A, '<br>');

            VPlace := PlacemarkFactory.Build(VPoint, VSName, VSDesc, VSFullDesc, 4);
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

function TGeoCoderByTXT.DoSearch(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  VList: IInterfaceListSimple;
  vpath: string;
  Vcnt: Integer;
  VFolder: string;
  SearchRec: TSearchRec;
  MySearch: string;
begin
  Vcnt := 1;
  MySearch := ASearch;
  while PosEx('  ',MySearch) > 0 do MySearch := ReplaceStr(MySearch, '  ', ' ');
  VList := TInterfaceListSimple.Create;
  VFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'userdata\txt\');
  if FindFirst(VFolder + '*.txt', faAnyFile, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then begin
        Continue;
      end;
      vpath := VFolder + SearchRec.Name;
      SearchInTXTFile(ACancelNotifier, AOperationID, Vpath, MySearch, vlist, Vcnt);
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
    until FindNext(SearchRec) <> 0;
  end;
  Result := VList;
end;
end.
