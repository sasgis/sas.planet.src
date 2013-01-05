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

unit u_GeoCoderByRosreestr;

interface

uses
  Classes,
  i_InetConfig,
  i_DownloadResult,
  i_DownloadRequest,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_DownloadResultFactory,  
  i_ValueToStringConverter,
  u_GeoCoderBasic;

type
  TGeoCoderByRosreestr = class(TGeoCoderBasic)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
  protected
    function PrepareRequest(
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IDownloadRequest; override;
    function ParseResultToPlacemarksList(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceList; override;
  public
    constructor Create(
      const AInetSettings: IInetConfig;
      const AGCNotifier: INotifierTime;
      const AResultFactory: IDownloadResultFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    );

  end;

implementation

uses
  SysUtils,
  StrUtils,
  RegExprUtils,
  t_GeoTypes,
  i_GeoCoder,
  i_CoordConverter,
  u_ResStrings,
  u_GeoCodePlacemark;

{ TGeoCoderByRosreestr }

  procedure QuickSort(
  var AList:IInterfaceList;
  var ADist: array of Double;
    L, R: Integer
  );
  var
    I, J: Integer;
    P: Double;
    TD: Double;

  begin
    repeat
      I := L;
      J := R;
      P := ADist[(L + R) shr 1];
      repeat
        while ADist[I] < P do begin
          Inc(I);
        end;
        while ADist[J] > P do begin
          Dec(J);
        end;
        if I <= J then begin
          TD := ADist[I];

          ADist[I] := ADist[J];
          ADist[J] := TD;
          AList.Exchange(I,J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then begin
        QuickSort(AList, ADist, L, J);
      end;
      L := I;
    until I >= R;
end;

procedure SortIt(
  var AList:IInterfaceList;
  const ALocalConverter: ILocalCoordConverter
    );
var
  i: integer;
  VMark: IGeoCodePlacemark;
  VDistArr: array of Double;
begin
   setlength(VDistArr,AList.Count);
   for i := 0 to AList.GetCount-1 do begin
      VMark := IGeoCodePlacemark(AList.Items[i]);
      VDistArr[i]:=ALocalConverter.GetGeoConverter.Datum.CalcDist(ALocalConverter.GetCenterLonLat,VMark.GetPoint);
   end;
  QuickSort(AList,VDistArr,0,AList.GetCount-1);
end;

procedure meters_to_lonlat(
  in_x, in_y: Double;
  out outout: TDoublePoint
);
begin
  outout.X := in_X / 6378137 * 180 / Pi;
  outout.Y := ((arctan(exp(in_Y / 6378137)) - Pi / 4) * 360) / Pi;
end;

function GetBetween(const ASTR,AFrom,ATo:string ; var VRes:string;APtr:integer):integer;
var
 i,j:integer;
begin
  i:=PosEx(AFrom, ASTR, Aptr);
  if i>0 then
   begin
   j:= PosEx(ATo, ASTR,i+length(AFrom));
   if j>i then begin
     VRes := Copy(AStr, i + Length(AFrom), j - (i + length(AFrom)));
     Result := j;
   end else
   begin
     VRes :='';
     Result := Aptr;
   end
  end else
  begin
   VRes :='';
   Result := Aptr;
  end;
end;
constructor TGeoCoderByRosreestr.Create(const AInetSettings: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AResultFactory: IDownloadResultFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(AInetSettings, AGCNotifier, AResultFactory);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;


function TGeoCoderByRosreestr.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceList;
var
  slat, slon, sname, sdesc, sfulldesc, VtempString: string;
  i, j: integer;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
  VValueConverter: IValueToStringConverter;
  VStr: string;
  VNeedSort: boolean;
begin
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  sfulldesc := '';
  sdesc := '';
  VtempString := '';
  VNeedSort :=false;

  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  SetLength(Vstr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, Vstr[1], AResult.Data.Size);
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceList.Create;
  i := PosEx('_jsonpCallback', VStr);
  VStr := ReplaceStr(VStr, '\"', '''');
  VStr := ReplaceStr(VStr, '\/', '/');
  VStr := ReplaceStr(VStr, '&quot;', '''');

  //по кадастровому номеру
  while (PosEx('{"attributes"', VStr, i) > i) and (i > 0) do begin
    j := PosEx('{"attributes"', VStr, i);
    sdesc := '';
    sname := '';
    j := GetBetween(Vstr,'"CAD_NUM":"','"',sname,j);

    j := GetBetween(Vstr,'"OBJECT_ADDRESS":"','"',sdesc,j);
    sdesc := Utf8ToAnsi(sdesc);
    
    if sdesc='' then begin
      j := GetBetween(Vstr,'"OBJECT_PLACE":"','"',sfulldesc,j);
      j := GetBetween(Vstr,'"OBJECT_LOCALITY":"','"',sdesc,j);
      sdesc := Utf8ToAnsi(sfulldesc+ ' ' +sdesc);
    end;

    j := GetBetween(Vstr,'"XC":',',',slon,j);
    j := GetBetween(Vstr,'"YC":',',',slat,j);

    try
      meters_to_lonlat(StrToFloat(slon, VFormatSettings), StrToFloat(slat, VFormatSettings), Vpoint);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
    end;
    i := (PosEx('}}', VStr, j));
    sdesc := sdesc + #$D#$A + '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
    sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
    VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
    VList.Add(VPlace);
  end;

  if 0 = VList.GetCount then VNeedSort := true;
  // по наименованию
  while (PosEx('address', VStr, i) > i) and (i > 0) do begin
    j := i;

    i := PosEx('"address":"', VStr, j);
    j := PosEx('"', VStr, i + 11);
    sname := Utf8ToAnsi(Copy(VStr, i + 11, j - (i + 11)));

    i := PosEx('"x":', VStr, j);
    j := PosEx('.', VStr, i + 4);
    slon := Copy(VStr, i + 4, j - (i + 4));

    i := PosEx('"y":', VStr, j);
    j := PosEx('.', VStr, i + 4);
    slat := Copy(VStr, i + 4, j - (i + 4));

    i := PosEx('"ParentName":"', VStr, j);
    j := PosEx('"', VStr, i + 14);
    sdesc := Utf8ToAnsi(Copy(VStr, i + 14, j - (i + 14)));
    try
      meters_to_lonlat(StrToFloat(slon, VFormatSettings), StrToFloat(slat, VFormatSettings), Vpoint);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
    end;
    i := (PosEx('}}', VStr, i));
    sdesc := sdesc + #$D#$A + '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
    sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
    VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
    VList.Add(VPlace);
  end;
  if (VNeedSort) and (VList.GetCount>1) then SortIt(VList ,ALocalConverter); // only for names

  Result := VList;
end;

function TGeoCoderByRosreestr.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
var
  VSearch: String;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  i: integer;
  s1, s2, s3, s4: string;
  i1, i2, i3, i4: integer;
begin
  VSearch := ASearch;
  VConverter := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
  VSearch := ReplaceStr(ReplaceStr(VSearch, '*', ''), ':', '');// убираем * и : из строки кадастрового номера

  if '' = RegExprReplaceMatchSubStr(VSearch, '[0-9]', '') then begin //cadastre number
    VSearch := ASearch;
    i := PosEx(':', VSearch, 1);
    s1 := copy(VSearch, 1, i - 1);
    VSearch := copy(VSearch, i + 1, length(VSearch) - i + 1);
    i1 := strtoint(s1);

    i := PosEx(':', VSearch, 1);
    s2 := copy(VSearch, 1, i - 1);
    VSearch := copy(VSearch, i + 1, length(VSearch) - i + 1);
    i2 := strtoint(s2);

    i := PosEx(':', VSearch, 1);
    if i = 0 then begin
      i := length(VSearch) + 1;
    end;
    s3 := copy(VSearch, 1, i - 1);
    VSearch := copy(VSearch, i + 1, length(VSearch) - i + 1);
    try
      i3 := strtoint(s3);
    except
      i3 := 0;
    end;

    s4 := VSearch;
    try
      i4 := strtoint(s4);
    except
      i4 := 0;
    end;

    if ('' = RegExprReplaceMatchSubStr(s1, '[0-9]', '')) and (i1 > 0) then begin
      while length(s1) < 2 do begin
        s1 := '0' + s1;
      end;
    end;
    if ('' = RegExprReplaceMatchSubStr(s2, '[0-9]', '')) and (i2 > 0) then begin
      while length(s2) < 2 do begin
        s2 := '0' + s2;
      end;
    end;
    if ('' = RegExprReplaceMatchSubStr(s3, '[0-9]', '')) and (i3 > 0) then begin
      while length(s3) < 7 do begin
        s3 := '0' + s3;
      end;
    end;
    if ('' = RegExprReplaceMatchSubStr(s4, '[0-9]', '')) and (i4 > 0) then begin
      while length(s4) < 5 do begin
        s4 := '0' + s4;
      end;
    end;

    VSearch := s1 + s2 + s3 + s4;

    if PosEx('*', VSearch, 1) > 0 then begin
      VSearch := ReplaceStr(VSearch, '*', '');// убираем * из строки кадастрового номера
      Result :=
        PrepareRequestByURL(
//          'http://maps.rosreestr.ru/ArcGIS/rest/services/Cadastre/CadastreInfo/MapServer/2/query?f=json&where=PARCELID%20like%20''' + URLEncode(AnsiToUtf8(VSearch)) + '%25''&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript23._jsonpCallback'
          'http://maps.rosreestr.ru/ArcGIS/rest/services/CadastreNew/Cadastre/MapServer/exts/GKNServiceExtension/online/parcel/find?cadNum='+URLEncode(AnsiToUtf8(ASearch))+'&onlyAttributes=false&returnGeometry=true&f=json&callback=dojo.io.script.jsonp_dojoIoScript24._jsonpCallback'
        );
    end else if i4 = 0 then // Кварталы
    begin
      Result :=
        PrepareRequestByURL(
//          'http://maps.rosreestr.ru/ArcGIS/rest/services/Cadastre/CadastreInfo/MapServer/6/query?f=json&where=KVARTALID%20like%20''' + URLEncode(AnsiToUtf8(VSearch)) + '%25''&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript40._jsonpCallback'
          'http://maps.rosreestr.ru/ArcGIS/rest/services/CadastreNew/Cadastre/MapServer/12/query?f=json&where=PKK_ID%20like%20'''+ URLEncode(AnsiToUtf8(VSearch)) +'%25''&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript21._jsonpCallback'
        );
    end else // участки
    begin
      Result :=
        PrepareRequestByURL(
          'http://maps.rosreestr.ru/ArcGIS/rest/services/CadastreNew/Cadastre/MapServer/exts/GKNServiceExtension/online/parcel/find?cadNum='+ URLEncode(AnsiToUtf8(ASearch)) +'&onlyAttributes=false&returnGeometry=true&f=json&callback=dojo.io.script.jsonp_dojoIoScript48._jsonpCallback'
        );
    end;

  end else begin //name
    VSearch := ASearch;
    Result :=
      PrepareRequestByURL(
        'http://maps.rosreestr.ru/ArcGIS/rest/services/Address/Locator_Composite/GeocodeServer/findAddressCandidates?SingleLine=' + URLEncode(AnsiToUtf8(VSearch)) + '&f=json&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript21._jsonpCallback'
      );
  end;

end;

end.